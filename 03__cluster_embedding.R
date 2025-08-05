#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
# Aim: Hierarchical Clustering of Greek Lemmas.
#
################################################################################

# Load required libraries
library( 'fastcluster' )    # Fast hierarchical clustering
library( 'cluster' )        # For PAM (medoids) and silhouette analysis
library( 'factoextra' )     # For optimal cluster number determination
library( 'parallel' )       # For parallel processing where possible
library( 'dplyr' )          # For data manipulation
library( 'ggplot2' )        # For plotting
library( 'jsonlite' )       # For json output
################################################################################

###
# Find medoid within each cluster
##
find_cluster_medoids <- function( data, clusters ) 
{
    medoids <- list()
    medoid_indices <- numeric(max(clusters))
    
    for (cluster_id in unique(clusters)) {
        cluster_indices <- which(clusters == cluster_id)
        if (length(cluster_indices) == 1) {
            medoids[[cluster_id]] <- rownames(data)[cluster_indices]
            medoid_indices[cluster_id] <- cluster_indices
        } else {
            # Calculate pairwise distances within cluster
            cluster_data <- data[cluster_indices, , drop = FALSE]
            cluster_dist <- as.matrix(dist(cluster_data))
            
            # Find point with minimum average distance to all other points in cluster
            avg_distances <- rowMeans(cluster_dist)
            medoid_local_idx <- which.min(avg_distances)
            medoid_global_idx <- cluster_indices[medoid_local_idx]
            
            medoids[[cluster_id]] <- rownames(data)[medoid_global_idx]
            medoid_indices[cluster_id] <- medoid_global_idx
        }
    }
    
    return(list(medoids = medoids, indices = medoid_indices))
}

###
# Calculate intra-cluster distance fit (recommended primary metric)
##
calculate_intracluster_fit <- function(embedding_data, cluster_assignments) {
    fit_scores <- numeric(length(cluster_assignments))
    
    for(cluster_id in unique(cluster_assignments)) {
        cluster_mask <- cluster_assignments == cluster_id
        cluster_indices <- which(cluster_mask)
        
        if(length(cluster_indices) == 1) {
            fit_scores[cluster_mask] <- 100
        } else {
            cluster_data <- embedding_data[cluster_mask, , drop = FALSE]
            
            # Calculate average distance to all other points in cluster
            avg_distances <- numeric(nrow(cluster_data))
            for(i in 1:nrow(cluster_data)) {
                distances_to_others <- apply(cluster_data[-i, , drop = FALSE], 1, 
                                           function(x) sqrt(sum((cluster_data[i,] - x)^2)))
                avg_distances[i] <- mean(distances_to_others)
            }
            
            # Convert to percentiles (lower distance = higher fit)
            ranks <- rank(-avg_distances)  # negative so closer points get higher ranks
            percentiles <- (ranks - 1) / (length(ranks) - 1) * 100
            fit_scores[cluster_mask] <- percentiles
        }
    }
    
    return(data.frame(
        lemma = rownames(embedding_data),
        intracluster_fit_percentage = round(fit_scores, 1)
    ))
}

################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.03.cluster.embedding'
dir.create( outdir, showWarnings = FALSE )

# Load the glove model (embedding)
load( 'out.02.make.glove/saved_glove__5.RData' )

# only keep LXX-NT lemmas
bib <- readr::read_tsv( 'out.01.select.lemmas/unique_lemmas_LXX_NT.tsv', show_col_types = FALSE )
embedding <- embedding[ rownames( embedding ) %in% bib$lemma, ]

# Prepare vars
embedding_sample <- embedding
lemma_names_sample <- rownames( embedding )
sample_indices <- 1:nrow( embedding )

# Calculate distance matrix
dist_matrix <- dist( embedding_sample, method = "euclidean" )

# Perform hierarchical clustering
hc_result <- fastcluster::hclust( dist_matrix, method = "ward.D2" )


# Test different numbers of clusters (reasonable range for interpretation)
# Louw-Nida has 93 semantic domains, so start at 90 and run to 180
# to be sure to capture a reasonable range
k_range <- 90:180

# Silhouette method
silhouette_scores <- numeric( length( k_range ) )
wss_scores <- numeric( length( k_range ) )

for (i in seq_along( k_range)) 
{
    k <- k_range[ i ]
    print( paste("Testing k =", k ) )
    
    # Cut dendrogram
    clusters <- cutree( hc_result, k = k )
    
    # Calculate silhouette score
    sil <- silhouette( clusters, dist_matrix )
    silhouette_scores[ i ] <- mean( sil[, 3] )
    
    # Calculate within-cluster sum of squares
    wss <- 0
    for ( cluster_id in unique( clusters ) ) 
    {
        cluster_points <- embedding_sample[clusters == cluster_id, , drop = FALSE]
        if (nrow(cluster_points) > 1) 
        {
            cluster_center <- colMeans(cluster_points)
            wss <- wss + sum(apply(cluster_points, 1, function(x) sum((x - cluster_center)^2)))
        }
    }
    wss_scores[ i ] <- wss
}

# Find optimal k
optimal_k_sil <- k_range[ which.max( silhouette_scores ) ]

paste( "Optimal k (Silhouette):", optimal_k_sil ) # 155
paste( "Max Silhouette Score:", round( max( silhouette_scores ), 4 ) ) 

#######
## PLOT
#######

# silhouette analysis
df_sil <- data.frame( x = k_range, y = silhouette_scores )

# Silhouette plot
p_sil <- ggplot( df_sil, aes( x = x, y = y ) ) +
        geom_line(color = "#2C3E50", linewidth = 1, alpha = 0.5) +
        geom_point(color = "#2980B9", size = 2, alpha = 0.5 ) +
        geom_vline( xintercept = optimal_k_sil, linetype = "dashed", color = "gray30" ) +
        scale_x_continuous( breaks = seq( 90, 180, 10 ) ) +
        labs( x = "Number of Clusters (k)", y = "Average Silhouette Score" ) +
    theme_minimal(base_size = 14) +
    theme_bw()

# Save the plot
ggsave( p_sil, file = file.path( outdir, "silhouette.png" ), width = 6, height = 4, dpi = 600, bg = 'white' )

# Find medoids for optimal clustering
optimal_clusters <- cutree( hc_result, k = optimal_k_sil )
medoid_result <- find_cluster_medoids( embedding_sample, optimal_clusters )

# Calculate intracluster fit for all lemmas
intracluster_fit <- calculate_intracluster_fit( embedding_sample, optimal_clusters )

# Cluster summary
cluster_summary <- data.frame(
    cluster_id = 1:optimal_k_sil,
    size = as.numeric( table( optimal_clusters ) ),
    medoid = unlist(medoid_result$medoids ),
    medoid_index = medoid_result$indices[ 1:optimal_k_sil ]
)

# sort from large to small
cluster_summary <- cluster_summary[ order( cluster_summary$size, decreasing = TRUE ), ]

########

# Create a comprehensive data frame with all lemmas and their cluster assignments
lemma_cluster_df <- data.frame(
    lemma = lemma_names_sample,
    original_cluster_id = optimal_clusters,
    stringsAsFactors = FALSE
)

# Calculate cluster sizes and create mapping from original to size-ordered cluster IDs
cluster_sizes <- table( optimal_clusters )
cluster_size_df <- data.frame(
    original_cluster_id = as.numeric(names(cluster_sizes)),
    cluster_size = as.numeric(cluster_sizes)
)

# Sort clusters by size (largest first) and create new cluster IDs
cluster_size_df <- cluster_size_df[order(cluster_size_df$cluster_size, decreasing = TRUE), ]
cluster_size_df$new_cluster_id <- 1:nrow(cluster_size_df)

# Create mapping for cluster ID conversion
cluster_mapping <- setNames(cluster_size_df$new_cluster_id, cluster_size_df$original_cluster_id)

# Add the size-ordered cluster IDs to the lemma data frame
lemma_cluster_df$cluster_id <- cluster_mapping[as.character(lemma_cluster_df$original_cluster_id)]

# Add cluster size information
lemma_cluster_df <- merge(lemma_cluster_df, 
                          cluster_size_df[, c("new_cluster_id", "cluster_size")], 
                          by.x = "cluster_id", by.y = "new_cluster_id")

# Sort the final data frame by cluster ID (which is now ordered by size)
lemma_cluster_df <- lemma_cluster_df[order(lemma_cluster_df$cluster_id), ]

# Clean up and reorder columns
lemma_cluster_df <- lemma_cluster_df[, c("lemma", "cluster_id", "cluster_size")]

# Add intracluster fit scores to the main data frame
lemma_cluster_df <- merge(lemma_cluster_df, intracluster_fit, by = "lemma")

# Reset row names
rownames(lemma_cluster_df) <- NULL

# Update cluster_summary to use the new cluster IDs and sort by size
cluster_summary_ordered <- data.frame(
    cluster_id = cluster_size_df$new_cluster_id,
    size = cluster_size_df$cluster_size,
    medoid = sapply(cluster_size_df$original_cluster_id, function(x) medoid_result$medoids[[x]]),
    medoid_index = sapply(cluster_size_df$original_cluster_id, function(x) medoid_result$indices[x])
)

# Add average intracluster fit per cluster to summary
cluster_fit_summary <- lemma_cluster_df %>%
    group_by(cluster_id) %>%
    summarise(avg_intracluster_fit = round(mean(intracluster_fit_percentage), 1), .groups = 'drop')

cluster_summary_ordered <- merge(cluster_summary_ordered, cluster_fit_summary, by = "cluster_id")


# Save the results
save( lemma_cluster_df, cluster_summary_ordered, hc_result, optimal_k_sil, 
     file = file.path( outdir, "lemma_clusters_results.RData" ) )

# Export to TSV files
readr::write_tsv( lemma_cluster_df, file.path( outdir, "lemma_clusters.tsv" ), quote = 'all' )
readr::write_tsv( cluster_summary_ordered, file.path( outdir, "cluster_summary.tsv" ), quote = 'all' )

# Create a detailed cluster breakdown showing all lemmas per cluster with fit scores
cluster_breakdown <- lemma_cluster_df %>%
    select(cluster_id, lemma, intracluster_fit_percentage) %>%
    arrange(cluster_id, desc(intracluster_fit_percentage))

# write to disk
readr::write_tsv( cluster_breakdown, file.path( outdir, "cluster_breakdown.tsv" ), quote = 'all' )

# Plot cluster size distribution
p_cluster_sizes <- ggplot( cluster_summary_ordered, aes( x = cluster_id, y = size ) ) +
    geom_bar(stat = "identity", fill = "#3498DB", color = 'gray30', alpha = 0.9, size = 0.3 ) +
    labs( x = "Cluster ID (ordered by size)", y = "Number of Lemmas" ) +
    scale_x_continuous( n.breaks = 20 ) +
    scale_y_continuous( n.breaks = 10 ) +    
    theme_minimal(base_size = 12) +
    theme_bw()

# Save the plot
ggsave( p_cluster_sizes, file = file.path( outdir, "cluster_sizes.png" ), width = 9, height = 4, dpi = 600, bg = 'white' )


##############
# save to json
##############

# Create a list where each medoid contains all its cluster members with fit scores
medoid_clusters_json <- list()

for( i in 1:nrow( cluster_summary_ordered ) ) 
{
    cluster_id <- cluster_summary_ordered$cluster_id[ i ]
    medoid <- cluster_summary_ordered$medoid[ i ]
    
    # Get all lemmas in this cluster with their fit scores
    cluster_data <- lemma_cluster_df[ lemma_cluster_df$cluster_id == cluster_id, ]
    cluster_data <- cluster_data[ order( cluster_data$intracluster_fit_percentage, decreasing = TRUE ), ]
    
    # Create member list with fit scores - explicitly include intracluster_fit_percentage
    members_with_fit <- lapply( 1:nrow( cluster_data ), function( j ) {
        list(
            lemma = cluster_data$lemma[ j ],
            intracluster_fit_percentage = cluster_data$intracluster_fit_percentage[ j ]
        )
    })
    
    # Create cluster key and structure with ID, medoid, and members
    cluster_key <- paste0( "cluster_", cluster_id )
    medoid_clusters_json[[ cluster_key ]] <- list(
        cluster_id = cluster_id,
        medoid = medoid,
        size = nrow( cluster_data ),
        members = members_with_fit
    )
}

# Convert to JSON and save
json_output <- toJSON( medoid_clusters_json, pretty = TRUE, auto_unbox = FALSE )
writeLines( json_output, file.path( outdir, "medoid_clusters.json" ) )

# required post-processing (remove "σ"):
# "0527-013" "21" "21.13" "σας" "σ" "a-p---fg-" "Septuaginta" "Regnorum iii" --> σος 
# I.e.,:   "cluster_id": "11",
#          "medoid": "Ἀρεοπαγίτης",
#          "size": 143,
#          "label": "Civic and Religious Roles"


