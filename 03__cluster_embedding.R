#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
# Aim: cluster embedding.
# Hierarchical Clustering of Greek Lemmas with Progress Monitoring

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
# Use cosine distance which is often better for embeddings
# Custom cosine distance function for better memory efficiency
##
cosine_dist <- function( x ) 
{
    # Normalize rows
    x_norm <- x / sqrt( rowSums( x^2 ) )
    # Calculate cosine similarity then convert to distance
    cos_sim <- tcrossprod( x_norm )
    cos_dist <- as.dist(1 - cos_sim )
    return( cos_dist )
}

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

################################################################################

# output dir
outdir <- 'out.03.cluster.embedding'
dir.create( outdir, showWarnings = FALSE )

# Load your data (assuming it's already loaded)
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
# Louw-nida has 93 semantic domains, so start at 90 and run to 300 
# to be sure to capture a reasonable range
k_range <- 90:300

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
optimal_k_elbow <- k_range[ which.min( diff( diff( wss_scores ) ) ) + 1]  # Elbow method

paste( "Optimal k (Silhouette):", optimal_k_sil ) # 190
paste( "Optimal k (Elbow):", optimal_k_elbow ) # 210
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
        scale_x_continuous( breaks = seq( 80, 360, 20 ) ) +
        labs( x = "Number of Clusters (k)", y = "Average Silhouette Score" ) +
    theme_minimal(base_size = 14) +
    theme_bw()

# elbow analysis
df_elbow <- data.frame( x = k_range, y = wss_scores )

# Elbow plot
p_elbow <- ggplot( df_elbow, aes( x = x, y = y ) ) +
    geom_line(color = "#2C3E50", linewidth = 1, alpha = 0.5) +
    geom_point(color = "#2980B9", size = 2, alpha = 0.5 ) +
    geom_vline( xintercept = optimal_k_elbow, linetype = "dashed", color = "gray30" ) +
    scale_x_continuous( breaks = seq( 80, 360, 20 ) ) +
    labs( x = "Number of Clusters (k)", y = "Within-cluster Sum of Squares" ) +
    theme_minimal(base_size = 14) +
    theme_bw()




# Find medoids for optimal clustering
optimal_clusters <- cutree( hc_result, k = optimal_k_sil )
medoid_result <- find_cluster_medoids( embedding_sample, optimal_clusters )

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

# Reset row names
rownames(lemma_cluster_df) <- NULL

# Update cluster_summary to use the new cluster IDs and sort by size
cluster_summary_ordered <- data.frame(
    cluster_id = cluster_size_df$new_cluster_id,
    size = cluster_size_df$cluster_size,
    medoid = sapply(cluster_size_df$original_cluster_id, function(x) medoid_result$medoids[[x]]),
    medoid_index = sapply(cluster_size_df$original_cluster_id, function(x) medoid_result$indices[x])
)

# Display summary statistics
cat("Clustering Results Summary:\n")
cat("Total number of lemmas:", nrow(lemma_cluster_df), "\n")
cat("Number of clusters:", optimal_k_sil, "\n")
cat("Largest cluster size:", max(lemma_cluster_df$cluster_size), "\n")
cat("Smallest cluster size:", min(lemma_cluster_df$cluster_size), "\n")
cat("Average cluster size:", round(mean(lemma_cluster_df$cluster_size), 2), "\n")

# Display first few rows of each cluster
cat("\nFirst 10 clusters with their lemmas:\n")
for(i in 1:min(10, optimal_k_sil)) {
    cluster_lemmas <- lemma_cluster_df$lemma[lemma_cluster_df$cluster_id == i]
    cat("Cluster", i, "(size:", length(cluster_lemmas), "):", 
        paste(head(cluster_lemmas, 5), collapse = ", "))
    if(length(cluster_lemmas) > 5) cat(", ...")
    cat("\n")
}

# Save the results
save( lemma_cluster_df, cluster_summary_ordered, hc_result, optimal_k_sil, 
     file = file.path( outdir, "lemma_clusters_results.RData" ) )

# Export to TSV files
readr::write_tsv( lemma_cluster_df, file.path( outdir, "lemma_clusters.tsv" ), quote = 'all' )

readr::write_tsv( cluster_summary_ordered, file.path( outdir, "cluster_summary.tsv" ), quote = 'all' )


# Create a detailed cluster breakdown showing all lemmas per cluster
cluster_breakdown <- split(lemma_cluster_df$lemma, lemma_cluster_df$cluster_id)
cluster_breakdown_df <- data.frame(
    cluster_id = rep(names(cluster_breakdown), sapply(cluster_breakdown, length)),
    lemma = unlist(cluster_breakdown, use.names = FALSE),
    stringsAsFactors = FALSE
)
cluster_breakdown_df$cluster_id <- as.numeric(cluster_breakdown_df$cluster_id)

# write to disk
readr::write_tsv( cluster_breakdown_df, file.path( outdir, "cluster_breakdown.tsv" ), quote = 'all' )

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

# Create a list where each medoid contains all its cluster members
medoid_clusters_json <- list()

for( i in 1:nrow( cluster_summary_ordered ) ) 
{
    cluster_id <- cluster_summary_ordered$cluster_id[ i ]
    medoid <- cluster_summary_ordered$medoid[ i ]
    
    # Get all lemmas in this cluster
    cluster_members <- lemma_cluster_df$lemma[ lemma_cluster_df$cluster_id == cluster_id ]
    
    # Create cluster key and structure with ID, medoid, and members
    cluster_key <- paste0( "cluster_", cluster_id )
    medoid_clusters_json[[ cluster_key ]] <- list(
        cluster_id = cluster_id,
        medoid = medoid,
        size = length( cluster_members ),
        members = cluster_members
    )
}

# Convert to JSON and save
json_output <- toJSON( medoid_clusters_json, pretty = TRUE, auto_unbox = FALSE )
writeLines( json_output, file.path( outdir, "medoid_clusters.json" ) )

