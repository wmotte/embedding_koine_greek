#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umcutrecht.nl
#
# Merge first encounter of lemma in LXX-NT with json data.
################################################################################

# Load required libraries
library( 'jsonlite' )
library( 'dplyr' )

###
# Function to merge TSV data with JSON clusters
##
merge_lemma_data <- function(tsv_file, json_file, output_file) 
{
    # Read the TSV file
    lemma_data <- read.delim(tsv_file, sep = "\t", stringsAsFactors = FALSE)
    
    # Read the JSON file with specific settings to preserve structure
    json_data <- fromJSON(json_file, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    
    # Function to merge lemma info for a single member
    merge_member_info <- function(member) 
    {
        # Handle different possible structures for lemma
        lemma_value <- NULL
        if (is.list(member$lemma)) {
            lemma_value <- member$lemma[[1]]
        } else if (is.vector(member$lemma)) {
            lemma_value <- member$lemma[1]
        } else {
            lemma_value <- member$lemma
        }
        
        # Find matching rows in the TSV data
        matching_rows <- lemma_data[lemma_data$lemma == lemma_value, ]
        
        # Add the matching TSV data to the member
        if (nrow(matching_rows) > 0) {
            # Convert to list format for JSON output
            member$tsv_data <- split(matching_rows, seq(nrow(matching_rows)))
        } else {
            # If no match found, add empty list
            member$tsv_data <- list()
        }
        
        return(member)
    }
    
    # Function to process a single cluster
    process_cluster <- function(cluster) 
    {
        if (!is.null(cluster$members) && length(cluster$members) > 0) {
            # Apply the merge function to each member
            cluster$members <- lapply(cluster$members, merge_member_info)
        }
        return(cluster)
    }
    
    # Function to process a single meta_group
    process_meta_group <- function(meta_group) 
    {
        if (!is.null(meta_group$clusters) && length(meta_group$clusters) > 0) {
            # Apply the cluster processing to each cluster
            meta_group$clusters <- lapply(meta_group$clusters, process_cluster)
        }
        return(meta_group)
    }
    
    # Process all meta_groups
    if (!is.null(json_data$meta_groups) && length(json_data$meta_groups) > 0) {
        json_data$meta_groups <- lapply(json_data$meta_groups, process_meta_group)
    }
    
    # Write the merged JSON to output file
    write_json(json_data, output_file, pretty = TRUE, auto_unbox = TRUE)

    # Return some statistics
    total_lemmas <- nrow(lemma_data)
    unique_lemmas <- length(unique(lemma_data$lemma))

    return(json_data)
}

###
# Function to preview the results
##
preview_merged_data <- function(merged_data, n_examples = 3) 
{
    if (!is.null(merged_data$meta_groups) && length(merged_data$meta_groups) > 0) 
    {
        for (mg_idx in seq_len(min(2, length(merged_data$meta_groups)))) 
        {
            meta_group <- merged_data$meta_groups[[mg_idx]]
         
            if (!is.null(meta_group$clusters) && length(meta_group$clusters) > 0) 
            {
                for (cl_idx in seq_len(min(1, length(meta_group$clusters)))) {
                    cluster <- meta_group$clusters[[cl_idx]]
                    
                    if (!is.null(cluster$members) && length(cluster$members) > 0) {
                        for (mb_idx in seq_len(min(n_examples, length(cluster$members)))) 
                        {
                            member <- cluster$members[[mb_idx]]
                            
                            # Handle different lemma structures
                            lemma_val <- NULL
                            if (is.list(member$lemma)) {
                                lemma_val <- member$lemma[[1]]
                            } else if (is.vector(member$lemma)) {
                                lemma_val <- member$lemma[1]
                            } else {
                                lemma_val <- member$lemma
                            }
                            
                            if (!is.null(member$tsv_data) && length(member$tsv_data) > 0) 
                            {
                                if (length(member$tsv_data) > 0) {
                                    first_match <- member$tsv_data[[1]]
                                    cat("      Example: TLG=", first_match$TLG, 
                                        ", chapter.verse=", first_match$chapter.verse,
                                        ", source=", first_match$source,
                                        ", book=", first_match$book, "\n")
                                }
                            } else {
                                print(" No TSV matches found!" )
                            }
                        }
                    }
                }
            }
        }
    }
}

################################################################################

# output dir
outdir <- 'out.05.merge.json.with.lxx.nt.source'
dir.create( outdir, showWarnings = FALSE )

# Define file paths
tsv_file <- "out.01.select.lemmas/unique_lemmas_LXX_NT.tsv"
json_file <- "out.04.merge.jsons/merged_clusters.json"
output_file <- file.path( outdir, "merged_clusters_with_tsv_data.json" )
  
# Check if files exist
if (!file.exists(tsv_file)) {
stop("TSV file not found: ", tsv_file)
}

if (!file.exists(json_file)) {
stop("JSON file not found: ", json_file)
}

# Perform the merge
merged_data <- merge_lemma_data(tsv_file, json_file, output_file)

# Show preview
preview_merged_data(merged_data)

