# Embedding Koine Greek

This repository contains the R scripts used to generate and analyze word embeddings for Koine Greek lemmas. The project follows a six-step pipeline, starting with corpus selection and ending with the integration of lemma source data with hierarchical clustering results.

The methodology is detailed across six R scripts, which are designed to be run in sequential order.

## Project Pipeline

### Step 1: Corpus Selection

The initial step involves selecting the texts to be included in the corpus. The script `00__select_koine_greek_texts.R` defines the Koine Greek period from 330 BCE to 330 CE. This historical range is based on the conquests of Alexander the Great and the founding of Constantinople by Constantine, respectively. The script reads a `metadata.txt` file and filters for texts that overlap with this period.

Key outputs from this step include:
* A TSV file (`selected_texts_koine_greek.tsv`) containing the metadata for the selected texts.
* Plots visualizing the distribution of texts and tokens over time.

### Step 2: Lemma Extraction

The script `01__select_lemmas.R` is responsible for processing the selected texts to extract lemmas (the dictionary form of a word). It uses the `xml2` library to parse XML treebank files, extracting the lemma, part-of-speech tag, and other relevant information for each word.

The script then performs the following actions:
* It combines the lemmas from all selected texts into a single, large corpus.
* It updates the metadata with the true word count (number of lemmas) for each text.
* It generates a plot showing the total number of words per 100-year bin.
* It extracts and analyzes lemmas specifically from Biblical Greek texts (Septuaginta and Novum Testamentum) to prepare for a later comparative analysis.

### Step 3: GloVe Embedding

The core of the project is the creation of a GloVe (Global Vectors for Word Representation) embedding model. The script `02__make_glove.R` uses the processed lemma corpus to train the model.

The key parameters for the GloVe model are:
* **Window Size:** A symmetric skip-gram window of 5 words.
* **Vector Size:** 150 dimensions.
* **Iterations:** 100 iterations for training.
* **Vocabulary Pruning:** Lemmas with a frequency of less than 5 are removed from the vocabulary.

This step generates the final embedding matrix, which is saved as an RData file (`saved_glove__5.RData`). It also produces plots illustrating the prevalence of lemmas in the corpus, following Zipf's law.

### Step 4: Hierarchical Clustering

The script `03__cluster_embedding.R` involves clustering the generated word embeddings to identify semantic groupings. This analysis focuses specifically on the embeddings for lemmas found in the Biblical Greek texts (Septuaginta and Novum Testamentum).

The clustering process uses hierarchical clustering with the Ward.D2 method. The optimal number of clusters is determined programmatically using the silhouette and elbow methods. An optimal number of 190 clusters was determined by the silhouette method. For each cluster, a medoid (the most representative lemma) is identified.

The results of the clustering are saved in multiple formats for easy access and analysis:
* `lemma_clusters.tsv`: A TSV file listing each lemma and its cluster assignment.
* `cluster_summary.tsv`: A summary of each cluster, including its size and medoid.
* `medoid_clusters.json`: A JSON file that organizes all lemmas by their cluster and medoid.
* A plot showing the distribution of cluster sizes is also generated.

### Step 5: JSON Merging

The script `04__merge_jsons.R` consolidates cluster information with the actual cluster data into a single, comprehensive JSON structure. This step takes the medoid clusters from Step 4 and enriches them with meta-group information and cluster descriptions.

The merging process:
* Combines `medoid_clusters.json` from the clustering step with `cluster_information_with_meta.json` containing meta-group definitions.
* Preserves the hierarchical meta-group structure while adding detailed cluster membership data.
* Enriches each cluster with member information, size, and medoid data.

The output is a unified JSON file (`merged_clusters.json`) that maintains the organizational structure while providing complete cluster details.

### Step 6: Source Data Integration

The final step, performed by `05__merge_json_with_lxx_nt_source.R`, integrates the clustered lemma data with source text information from the Septuaginta and New Testament. This creates a comprehensive dataset linking semantic clusters to their textual origins.

The integration process:
* Merges TSV data containing lemma source information (TLG references, chapter/verse, book, source) with the JSON cluster structure.
* Handles various lemma data structures to ensure robust matching between cluster members and source data.
* Adds detailed provenance information for each lemma, enabling analysis of how semantic clusters relate to specific texts and passages.
* Provides preview functionality to validate the merging process and display examples of matched data.

The final output (`merged_clusters_with_tsv_data.json`) combines semantic clustering results with detailed source attribution, enabling comprehensive analysis of Koine Greek lemma usage across different texts and contexts.

## Repository Contents

* **`00__select_koine_greek_texts.R`**: Script for selecting and filtering texts based on a defined Koine Greek period.
* **`01__select_lemmas.R`**: Script for parsing XML treebanks, extracting lemmas, and creating the corpus.
* **`02__make_glove.R`**: Script for training the GloVe word embedding model.
* **`03__cluster_embedding.R`**: Script for hierarchically clustering the embeddings, focusing on Biblical Greek lemmas.
* **`04__merge_jsons.R`**: Script for merging cluster information with actual clusters into a unified JSON structure.
* **`05__merge_json_with_lxx_nt_source.R`**: Script for integrating cluster data with lemma source information from LXX-NT texts.
* **`out.00.select.koine.greek.texts/`**: Output directory for corpus selection results.
* **`out.01.select.lemmas/`**: Output directory for lemma extraction and corpus creation.
* **`out.02.make.glove/`**: Output directory for GloVe model and related plots.
* **`out.03.cluster.embedding/`**: Output directory for clustering results, including TSV and JSON files.
* **`out.04.merge.jsons/`**: Output directory for merged cluster JSON structure.
* **`out.05.merge.json.with.lxx.nt.source/`**: Output directory for final integrated dataset with source information.
* **`misc/metadata.txt`**: Metadata file for the corpus (required for the first script).
* **`misc/cluster_information_with_meta.json`**: Meta-group and cluster information (required for the fifth script).
* **`xml/`**: Directory containing the raw XML treebank files (required for the second script).

## Dependencies

This project relies on the following R libraries:
* `readr`
* `ggplot2`
* `dplyr`
* `xml2`
* `text2vec`
* `fastcluster`
* `cluster`
* `factoextra`
* `parallel`
* `jsonlite`
* `purrr`

## How to Run

1.  Ensure all R dependencies are installed.
2.  Place the `metadata.txt` file and `cluster_information_with_meta.json` in a `misc/` directory and the raw XML files in an `xml/` directory.
3.  Run the scripts in the following order:
    1.  `Rscript 00__select_koine_greek_texts.R`
    2.  `Rscript 01__select_lemmas.R`
    3.  `Rscript 02__make_glove.R`
    4.  `Rscript 03__cluster_embedding.R`
    5.  `Rscript 04__merge_jsons.R`
    6.  `Rscript 05__merge_json_with_lxx_nt_source.R`