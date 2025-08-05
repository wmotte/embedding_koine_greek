#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umutrecht.nl)
#
# Merge cluster information with actual clusters into a single JSON
################################################################################
library( 'jsonlite' )
library( 'purrr' )

# output dir
outdir <- 'out.04.merge.jsons'
dir.create( outdir, showWarnings = FALSE )

# Load the two JSON files
clusters <- fromJSON( "out.03.cluster.embedding/medoid_clusters.json", simplifyVector = FALSE )
validation <- fromJSON( "misc/cluster_information_with_meta.json", simplifyVector = FALSE )
 
# Helper: Create lookup from cluster_id to cluster data
cluster_lookup <- setNames(clusters, map_chr(clusters, ~ as.character( .x$cluster_id[[1]] ) ) )

###
# Merge into original meta_groups structure
##
merged_meta_groups <- map( validation$meta_groups, function( meta_group ) 
{
    enriched_clusters <- map( meta_group$clusters, function(cluster) 
    {
        cluster_id <- as.character(cluster$cluster_id)
        original_cluster <- cluster_lookup[[cluster_id]]
        
        if (!is.null(original_cluster)) {
            # Enrich the cluster
            cluster$members <- original_cluster$members
            cluster$size <- original_cluster$size[[1]]
            cluster$medoid <- original_cluster$medoid[[1]]
        }
        
        cluster
    })
    
    # Return full meta_group
    list(
        meta_group_id = meta_group$meta_group_id,
        meta_label = meta_group$meta_label,
        description = meta_group$description,
        clusters = enriched_clusters
    )
})

################################################################################

# Final object
final_output <- list( meta_groups = merged_meta_groups )

# Write merged JSON to file
write_json( final_output, file.path( outdir, "merged_clusters.json" ), pretty = TRUE, auto_unbox = TRUE)
