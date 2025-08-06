#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
# Clean Greek Lemma Clusters - Postprocessing Script
#
# This script performs postprocessing on semantic clusters containing Greek
# lemmas by removing function words and single-character lemmas according to
# the methodology: "we applied a postprocessing step to remove from the 
# clusters a curated list of single character lemmas and function words"
#
# BEHAVIOR:
# 1. Reads a JSON file containing semantic clusters with Greek lemmas
# 2. Removes ALL single-letter lemmas, including:
#    - Simple single characters (α, β, γ, etc.)
#    - Single letters with diacritics that may appear as 2+ chars (ἆ, ἕ, ἒ, Ἤ, ᾗ, Ἡ, etc.)
#    - Articles and letters with breathing marks, accents, iota subscripts
# 3. Removes specified multi-character function words with robust matching:
#    - Original list: καί, ἀλλά, γάρ, οὖν, ἵνα, ἐάν, ὅτι, δέ, μή, οὐ, οὐκ, οὐχ, τε, μήποτε
#    - Plus all capitalization variants (Καί, ΚΑΙ, etc.)
#    - Plus diacritical variants (καὶ, καί, και, etc.)
#    - Plus additional common function words and prepositions
#    - Uses fuzzy matching to catch variants with different accents
# 4. Updates cluster size statistics after removal
# 5. Reports all removed lemmas with detailed information
# 6. Validates that all targeted lemmas were successfully removed
# 7. Optionally saves the cleaned data to a new JSON file
#
#
# DEPENDENCIES:
#   Required: jsonlite
#   Optional: tidyverse, conflicted
#   For better Unicode handling (recommended): stringi
#
#   The script handles Greek letters with diacritics that may be encoded as
#   multiple Unicode codepoints (e.g., ἆ, ἕ, Ἤ, ᾗ). These are correctly
#   identified as single letters and removed. If you encounter any issues
#   with Greek character matching, consider installing the 'stringi' package
#   for enhanced Unicode normalization support: install.packages("stringi")
#
################################################################################
library( "jsonlite" )

# Suppress package startup messages and handle conflicted package
suppressPackageStartupMessages({
  # Handle conflicted package if present
  if (requireNamespace("conflicted", quietly = TRUE)) {
    conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
    conflicted::conflict_prefer("lag", "dplyr", quiet = TRUE)
  }
  
  # Load tidyverse if available (optional)
  if (requireNamespace("tidyverse", quietly = TRUE)) {
    library(tidyverse)
  }
})

###
# Function to process the JSON data and remove function words and single-character lemmas
##
clean_lemma_clusters <- function(json_file, output_file = NULL) 
{
  
  # Read the JSON file
  cat("Reading JSON file...\n")
  data <- fromJSON(json_file, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  
  # Check structure
  if (!"meta_groups" %in% names(data)) {
    stop("JSON file does not contain 'meta_groups' field")
  }
  
  # Define the list of function words to remove (multi-character only)
  # Note: ALL single-character lemmas (including ὁ, ἡ, ἤ, etc.) are removed automatically
  # This comprehensive list includes:
  #   - Original function words from the paper
  #   - Capitalized variants (first letter and all caps)
  #   - Different diacritical marks and accents
  #   - Common spelling variations
  #   - Additional common Greek function words and prepositions
  function_words_base <- c(
    # Conjunctions (multi-character)
    "καί", "Καί", "ΚΑΙ", "και", "καὶ", "Καὶ",  # και with various diacritics
    "ἀλλά", "Ἀλλά", "ἀλλὰ", "Ἀλλὰ", "ΑΛΛΑ", "ἀλλ'", "Ἀλλ'",  # αλλα variants
    "γάρ", "Γάρ", "γὰρ", "Γὰρ", "ΓΑΡ",  # γαρ variants
    "οὖν", "Οὖν", "ΟΥΝ", "ουν",  # ουν variants
    "ἵνα", "Ἵνα", "ἱνα", "Ἱνα", "ἰνα", "ΙΝΑ",  # ινα variants
    "ἐάν", "Ἐάν", "ἐὰν", "Ἐὰν", "ΕΑΝ", "ἄν", "Ἄν", "ἂν", "Ἂν",  # εαν and αν variants
    "ὅτι", "Ὅτι", "ὁτι", "Ὁτι", "ΟΤΙ",  # οτι variants
    # Particles (multi-character)
    "δέ", "Δέ", "δὲ", "Δὲ", "ΔΕ", "δ'", "Δ'",  # δε variants
    "μή", "Μή", "μὴ", "Μὴ", "ΜΗ",  # μη variants
    "οὐ", "Οὐ", "οὑ", "Οὑ", "ΟΥ", "ου",  # ου variants
    "οὐκ", "Οὐκ", "οὑκ", "Οὑκ", "ΟΥΚ", "ουκ",  # ουκ variants
    "οὐχ", "Οὐχ", "οὑχ", "Οὑχ", "ΟΥΧ", "ουχ", "οὐχὶ", "Οὐχὶ", "οὐχί", "Οὐχί",  # ουχ variants
    "τέ", "Τέ", "τὲ", "Τὲ", "τε", "Τε", "ΤΕ",  # τε variants
    "μήποτε", "Μήποτε", "μὴποτε", "Μὴποτε", "ΜΗΠΟΤΕ",  # μηποτε variants
    "μήπως", "Μήπως", "μὴπως", "Μὴπως",  # μηπως variants (similar to μηποτε)
    "μά", "μα",
    # Additional common function words
    "εἰ", "Εἰ", "ΕΙ", "εἰς", "Εἰς", "ΕΙΣ",  # ει/εις variants
    "ἐν", "Ἐν", "ΕΝ", "ἑν", "Ἑν",  # εν variants
    "τίς", "Τίς", "τὶς", "Τὶς", "τις", "Τις", "ΤΙΣ",  # τις variants
    "τῶν", "Τῶν", "των", "Των", "ΤΩΝ",  # των variants
    "μέν", "Μέν", "μὲν", "Μὲν", "μεν", "Μεν", "ΜΕΝ",  # μεν variants
    "ἐπί", "Ἐπί", "ἐπὶ", "Ἐπὶ", "επι", "Επι", "ΕΠΙ",  # επι variants
    "πρός", "Πρός", "πρὸς", "Πρὸς", "προς", "Προς", "ΠΡΟΣ",  # προς variants
    "διά", "Διά", "διὰ", "Διὰ", "δια", "Δια", "ΔΙΑ",  # δια variants
    "κατά", "Κατά", "κατὰ", "Κατὰ", "κατα", "Κατα", "ΚΑΤΑ",  # κατα variants
    "μετά", "Μετά", "μετὰ", "Μετὰ", "μετα", "Μετα", "ΜΕΤΑ",  # μετα variants
    "παρά", "Παρά", "παρὰ", "Παρὰ", "παρα", "Παρα", "ΠΑΡΑ",  # παρα variants
    "ὑπό", "Ὑπό", "ὑπὸ", "Ὑπὸ", "υπο", "Υπο", "ΥΠΟ",  # υπο variants
    "ἀπό", "Ἀπό", "ἀπὸ", "Ἀπὸ", "απο", "Απο", "ΑΠΟ",  # απο variants
    "περί", "Περί", "περὶ", "Περὶ", "περι", "Περι", "ΠΕΡΙ",  # περι variants
    "ὡς", "Ὡς", "ὠς", "Ὠς", "ως", "Ως", "ΩΣ",  # ως variants
    "ἕως", "Ἕως", "ἑως", "Ἑως", "εως", "Εως", "ΕΩΣ",  # εως variants
    "ἄρα", "Ἄρα", "ἀρα", "Ἀρα", "αρα", "Αρα", "ΑΡΑ",  # αρα variants
    "οὔτε", "Οὔτε", "οὐτε", "Οὐτε", "ουτε", "Ουτε", "ΟΥΤΕ",  # ουτε variants
    "μήτε", "Μήτε", "μὴτε", "Μὴτε", "μητε", "Μητε", "ΜΗΤΕ",  # μητε variants
    "εἴτε", "Εἴτε", "εἰτε", "Εἰτε", "ειτε", "Ειτε", "ΕΙΤΕ"  # ειτε variants
  )
  
  # Create a unique set and normalize
  function_words <- unique(function_words_base)
  
  # Function to check if a lemma is effectively a single character
  # This handles Greek letters with diacritics that might be encoded as multiple codepoints
  is_single_greek_letter <- function(word) {
    # First check simple single character
    if (nchar(word) == 1) return(TRUE)
    
    # List of single Greek letters with diacritics that might appear as 2+ characters
    # These are essentially single letters with breathing marks, accents, or iota subscripts
    single_letter_variants <- c(
      # Specific cases mentioned by user
      "ἆ", "ἕ", "ἒ", "Ἤ", "ᾗ", "Ἡ",
      # Articles and single letters with diacritics
      "ὁ", "ἡ", "ὅ", "ἥ", "ὃ", "ἣ", "ᾧ", "ᾗ", "οἷ", "αἷ",
      "Ὁ", "Ἡ", "Ὅ", "Ἥ", "Ὃ", "Ἣ",
      # Single letters with various diacritics (comprehensive list)
      # Alpha variants
      "ἀ", "ἁ", "ἂ", "ἃ", "ἄ", "ἅ", "ἆ", "ἇ", "ᾶ", "ὰ", "ά", "ᾳ", "ᾷ", "ᾴ", "ᾲ", "ᾀ", "ᾁ", "ᾂ", "ᾃ", "ᾄ", "ᾅ", "ᾆ", "ᾇ",
      "Ἀ", "Ἁ", "Ἂ", "Ἃ", "Ἄ", "Ἅ", "Ἆ", "Ἇ", "Ὰ", "Ά", "Α", "Ᾱ", "Ᾰ", "ᾼ",
      # Epsilon variants
      "ἐ", "ἑ", "ἒ", "ἓ", "ἔ", "ἕ", "ὲ", "έ", "ὲ", "έ",
      "Ἐ", "Ἑ", "Ἒ", "Ἓ", "Ἔ", "Ἕ", "Ὲ", "Έ", "Ε",
      # Eta variants
      "ἠ", "ἡ", "ἢ", "ἣ", "ἤ", "ἥ", "ἦ", "ἧ", "ῆ", "ὴ", "ή", "ῃ", "ῇ", "ῄ", "ῂ", "ᾐ", "ᾑ", "ᾒ", "ᾓ", "ᾔ", "ᾕ", "ᾖ", "ᾗ",
      "Ἠ", "Ἡ", "Ἢ", "Ἣ", "Ἤ", "Ἥ", "Ἦ", "Ἧ", "Ὴ", "Ή", "Η", "ῌ",
      # Iota variants
      "ἰ", "ἱ", "ἲ", "ἳ", "ἴ", "ἵ", "ἶ", "ἷ", "ῖ", "ὶ", "ί", "ῒ", "ΐ", "ῗ", "ΐ",
      "Ἰ", "Ἱ", "Ἲ", "Ἳ", "Ἴ", "Ἵ", "Ἶ", "Ἷ", "Ὶ", "Ί", "Ι", "Ῑ", "Ῐ",
      # Omicron variants
      "ὀ", "ὁ", "ὂ", "ὃ", "ὄ", "ὅ", "ὸ", "ό",
      "Ὀ", "Ὁ", "Ὂ", "Ὃ", "Ὄ", "Ὅ", "Ὸ", "Ό", "Ο",
      # Upsilon variants
      "ὐ", "ὑ", "ὒ", "ὓ", "ὔ", "ὕ", "ὖ", "ὗ", "ῦ", "ὺ", "ύ", "ῢ", "ΰ", "ῧ", "ΰ",
      "Ὑ", "Ὓ", "Ὕ", "Ὗ", "Ὺ", "Ύ", "Υ", "Ῡ", "Ῠ",
      # Omega variants
      "ὠ", "ὡ", "ὢ", "ὣ", "ὤ", "ὥ", "ὦ", "ὧ", "ῶ", "ὼ", "ώ", "ῳ", "ῷ", "ῴ", "ῲ", "ᾠ", "ᾡ", "ᾢ", "ᾣ", "ᾤ", "ᾥ", "ᾦ", "ᾧ", "ᾤα", "ω̅",
      "Ὠ", "Ὡ", "Ὢ", "Ὣ", "Ὤ", "Ὥ", "Ὦ", "Ὧ", "Ὼ", "Ώ", "Ω", "ῼ",
      # Rho variants
      "ῤ", "ῥ", "ῤ", "ῥ",
      "Ῥ", "Ρ",
      # Other single letters
      "β", "Β", "γ", "Γ", "δ", "Δ", "ζ", "Ζ", "θ", "Θ", "κ", "Κ", "λ", "Λ", 
      "μ", "Μ", "ν", "Ν", "ξ", "Ξ", "π", "Π", "σ", "ς", "Σ", "τ", "Τ", 
      "φ", "Φ", "χ", "Χ", "ψ", "Ψ"
    )
    
    if (word %in% single_letter_variants) return(TRUE)
    
    # Also check if stripping all diacritics results in a single character
    # This is a more aggressive approach
    word_stripped <- gsub("[΄`´'ʹ͂̀́̂̃̈̓̔͂ͅ῾᾿ῥῤ῀῁ῆῇῒΐῗῢΰῧῶῷ]", "", word)
    word_stripped <- gsub("[ᾶὰάᾳᾷᾴᾲἀἁἂἃἄἅἆἇᾀᾁᾂᾃᾄᾅᾆᾇ]", "α", word_stripped)
    word_stripped <- gsub("[ἈἉἊἋἌἍἎἏᾺΆΑᾹᾸ]", "Α", word_stripped)
    word_stripped <- gsub("[ὲέἐἑἒἓἔἕ]", "ε", word_stripped)
    word_stripped <- gsub("[ῈΈΕἘἙἚἛἜἝ]", "Ε", word_stripped)
    word_stripped <- gsub("[ῆὴήῃῇῄῂἠἡἢἣἤἥἦἧᾐᾑᾒᾓᾔᾕᾖᾗ]", "η", word_stripped)
    word_stripped <- gsub("[ῊΉΗἨἩἪἫἬἭἮἯ]", "Η", word_stripped)
    word_stripped <- gsub("[ῖὶίῒΐῗἰἱἲἳἴἵἶἷ]", "ι", word_stripped)
    word_stripped <- gsub("[ῚΊΙἸἹἺἻἼἽἾἿῙῘ]", "Ι", word_stripped)
    word_stripped <- gsub("[ὸόὀὁὂὃὄὅ]", "ο", word_stripped)
    word_stripped <- gsub("[ῸΌΟὈὉὊὋὌὍ]", "Ο", word_stripped)
    word_stripped <- gsub("[ῦὺύῢΰῧὐὑὒὓὔὕὖὗ]", "υ", word_stripped)
    word_stripped <- gsub("[ῪΎΥὙὛὝὟῩῨ]", "Υ", word_stripped)
    word_stripped <- gsub("[ῶὼώῳῷῴῲὠὡὢὣὤὥὦὧᾠᾡᾢᾣᾤᾥᾦᾧ]", "ω", word_stripped)
    word_stripped <- gsub("[ῺΏΩὨὩὪὫὬὭὮὯ]", "Ω", word_stripped)
    word_stripped <- gsub("[ῤῥ]", "ρ", word_stripped)
    word_stripped <- gsub("[Ῥ]", "Ρ", word_stripped)
    
    if (nchar(word_stripped) == 1) return(TRUE)
    
    return(FALSE)
  }
  
  # Also create a function to check with Unicode normalization
  is_function_word <- function(word) {
    # Direct match
    if (word %in% function_words) return(TRUE)
    
    # Try lowercase comparison (though Greek lowercase is complex)
    word_lower <- tolower(word)
    if (word_lower %in% tolower(function_words)) return(TRUE)
    
    # Strip accents and diacritics for comparison (basic approach)
    # This is a simplified approach - for production, consider using stringi package
    word_stripped <- gsub("[΄`´'ʹ͂̀́̂̃̈̓̔͂ͅ]", "", word)
    for (fw in function_words) {
      fw_stripped <- gsub("[΄`´'ʹ͂̀́̂̃̈̓̔͂ͅ]", "", fw)
      if (word_stripped == fw_stripped) return(TRUE)
    }
    
    return(FALSE)
  }
  
  # Initialize counters for reporting
  total_removed <- 0
  removed_lemmas <- list()
  
  # Process each meta_group
  for (i in seq_along(data$meta_groups)) {
    meta_group <- data$meta_groups[[i]]
    
    # Safely get meta_group properties
    meta_group_id <- if (!is.null(meta_group$meta_group_id)) meta_group$meta_group_id else i
    meta_label <- if (!is.null(meta_group$meta_label)) meta_group$meta_label else paste("Group", i)
    
    # Check if clusters exist
    if (is.null(meta_group$clusters)) {
      cat("  No clusters found in this meta group\n")
      next
    }
    
    # Process each cluster in the meta_group
    for (j in seq_along(meta_group$clusters)) {
      cluster <- meta_group$clusters[[j]]
      
      # Safely get cluster properties
      cluster_id <- if (!is.null(cluster$cluster_id)) cluster$cluster_id else j
      original_size <- if (!is.null(cluster$size)) cluster$size else length(cluster$members)
      
      cat(sprintf("\n  Cluster %s (original size: %d):\n", cluster_id, original_size))
      
      # Track removed lemmas for this cluster
      cluster_removed <- character()
      
      # Filter members
      new_members <- list()
      
      # Check if members exist
      if (is.null(cluster$members) || length(cluster$members) == 0) {
        cat("    No members in this cluster\n")
        data$meta_groups[[i]]$clusters[[j]]$size <- 0
        next
      }
      
      for (k in seq_along(cluster$members)) {
        member <- cluster$members[[k]]
        
        # Get the lemma(s) - handle different possible structures
        lemmas <- if (!is.null(member$lemma)) {
          if (is.list(member$lemma)) {
            unlist(member$lemma)
          } else {
            member$lemma
          }
        } else {
          character()
        }
        
        # Check each lemma
        should_remove <- FALSE
        for (lemma in lemmas) {
          # First check if it's a single Greek letter (including those with diacritics)
          if (is_single_greek_letter(lemma)) {
            should_remove <- TRUE
            cluster_removed <- c(cluster_removed, lemma)
            cat(sprintf("    - Removing: '%s' (single letter/character)\n", lemma))
          }
          # Then check if it's in the multi-character function words list
          else if (is_function_word(lemma)) {
            should_remove <- TRUE
            cluster_removed <- c(cluster_removed, lemma)
            cat(sprintf("    - Removing: '%s' (function word)\n", lemma))
          }
        }
        
        # Keep the member if it shouldn't be removed
        if (!should_remove) {
          new_members[[length(new_members) + 1]] <- member
        }
      }
      
      # Update the cluster
      data$meta_groups[[i]]$clusters[[j]]$members <- new_members
      
      # Update the size
      new_size <- length(new_members)
      data$meta_groups[[i]]$clusters[[j]]$size <- new_size
      
      # Report changes for this cluster
      if (length(cluster_removed) > 0) {
        removed_lemmas[[paste0("cluster_", cluster_id)]] <- unique(cluster_removed)
        total_removed <- total_removed + length(unique(cluster_removed))
        cat(sprintf("    Updated size: %d -> %d (removed %d lemmas)\n", 
                    original_size, new_size, original_size - new_size))
      } else {
        cat("    No lemmas removed\n")
      }
    }
  }
  
  if (length(removed_lemmas) > 0) {
    cat("\nRemoved lemmas by cluster:\n")
    for (cluster_name in names(removed_lemmas)) {
      cat(sprintf("  %s: %s\n", 
                  cluster_name, 
                  paste(removed_lemmas[[cluster_name]], collapse = ", ")))
    }
  }
  
  # Save the cleaned data if output file is specified
  if (!is.null(output_file)) {
    cat(sprintf("\nSaving cleaned data to: %s\n", output_file))
    write_json(data, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat("File saved successfully!\n")
  }
  
  return(data)
}

##
# Function to validate the cleaning results
###
validate_cleaning <- function(cleaned_data)
{

  # Define the same comprehensive function words list for validation
  function_words_base <- c(
    # Conjunctions
    "καί", "Καί", "ΚΑΙ", "και", "καὶ", "Καὶ",
    "ἀλλά", "Ἀλλά", "ἀλλὰ", "Ἀλλὰ", "ΑΛΛΑ", "ἀλλ'", "Ἀλλ'",
    "γάρ", "Γάρ", "γὰρ", "Γὰρ", "ΓΑΡ",
    "οὖν", "Οὖν", "ΟΥΝ", "ουν",
    "ἵνα", "Ἵνα", "ἱνα", "Ἱνα", "ἰνα", "ΙΝΑ",
    "ἐάν", "Ἐάν", "ἐὰν", "Ἐὰν", "ΕΑΝ", "ἄν", "Ἄν", "ἂν", "Ἂν",
    "ὅτι", "Ὅτι", "ὁτι", "Ὁτι", "ΟΤΙ",
    # Particles
    "δέ", "Δέ", "δὲ", "Δὲ", "ΔΕ", "δ'", "Δ'",
    "μή", "Μή", "μὴ", "Μὴ", "ΜΗ",
    "οὐ", "Οὐ", "οὑ", "Οὑ", "ΟΥ", "ου",
    "οὐκ", "Οὐκ", "οὑκ", "Οὑκ", "ΟΥΚ", "ουκ",
    "οὐχ", "Οὐχ", "οὑχ", "Οὑχ", "ΟΥΧ", "ουχ", "οὐχὶ", "Οὐχὶ", "οὐχί", "Οὐχί",
    "τέ", "Τέ", "τὲ", "Τὲ", "τε", "Τε", "ΤΕ",
    "μήποτε", "Μήποτε", "μὴποτε", "Μὴποτε", "ΜΗΠΟΤΕ",
    "μήπως", "Μήπως", "μὴπως", "Μὴπως",
    # Additional function words
    "εἰ", "Εἰ", "ΕΙ", "εἰς", "Εἰς", "ΕΙΣ",
    "ἐν", "Ἐν", "ΕΝ", "ἑν", "Ἑν",
    "τίς", "Τίς", "τὶς", "Τὶς", "τις", "Τις", "ΤΙΣ",
    "τῶν", "Τῶν", "των", "Των", "ΤΩΝ",
    "μέν", "Μέν", "μὲν", "Μὲν", "μεν", "Μεν", "ΜΕΝ",
    "ἐπί", "Ἐπί", "ἐπὶ", "Ἐπὶ", "επι", "Επι", "ΕΠΙ",
    "πρός", "Πρός", "πρὸς", "Πρὸς", "προς", "Προς", "ΠΡΟΣ",
    "διά", "Διά", "διὰ", "Διὰ", "δια", "Δια", "ΔΙΑ",
    "κατά", "Κατά", "κατὰ", "Κατὰ", "κατα", "Κατα", "ΚΑΤΑ",
    "μετά", "Μετά", "μετὰ", "Μετὰ", "μετα", "Μετα", "ΜΕΤΑ",
    "παρά", "Παρά", "παρὰ", "Παρὰ", "παρα", "Παρα", "ΠΑΡΑ",
    "ὑπό", "Ὑπό", "ὑπὸ", "Ὑπὸ", "υπο", "Υπο", "ΥΠΟ",
    "ἀπό", "Ἀπό", "ἀπὸ", "Ἀπὸ", "απο", "Απο", "ΑΠΟ",
    "περί", "Περί", "περὶ", "Περὶ", "περι", "Περι", "ΠΕΡΙ",
    "ὡς", "Ὡς", "ὠς", "Ὠς", "ως", "Ως", "ΩΣ",
    "ἕως", "Ἕως", "ἑως", "Ἑως", "εως", "Εως", "ΕΩΣ",
    "ἄρα", "Ἄρα", "ἀρα", "Ἀρα", "αρα", "Αρα", "ΑΡΑ",
    "οὔτε", "Οὔτε", "οὐτε", "Οὐτε", "ουτε", "Ουτε", "ΟΥΤΕ",
    "μήτε", "Μήτε", "μὴτε", "Μὴτε", "μητε", "Μητε", "ΜΗΤΕ",
    "εἴτε", "Εἴτε", "εἰτε", "Εἰτε", "ειτε", "Ειτε", "ΕΙΤΕ"
  )
  
  function_words <- unique(function_words_base)
  
  # Same function to check single Greek letters (including with diacritics)
  is_single_greek_letter_validation <- function(word) {
    if (nchar(word) == 1) return(TRUE)
    
    single_letter_variants <- c(
      "ἆ", "ἕ", "ἒ", "Ἤ", "ᾗ", "Ἡ",
      "ὁ", "ἡ", "ὅ", "ἥ", "ὃ", "ἣ", "ᾧ", "ᾗ", "οἷ", "αἷ",
      "Ὁ", "Ἡ", "Ὅ", "Ἥ", "Ὃ", "Ἣ",
      "ἀ", "ἁ", "ἂ", "ἃ", "ἄ", "ἅ", "ἆ", "ἇ", "ᾶ", "ὰ", "ά", "ᾳ", "ᾷ", "ᾴ", "ᾲ", "ᾀ", "ᾁ", "ᾂ", "ᾃ", "ᾄ", "ᾅ", "ᾆ", "ᾇ",
      "Ἀ", "Ἁ", "Ἂ", "Ἃ", "Ἄ", "Ἅ", "Ἆ", "Ἇ", "Ὰ", "Ά", "Α", "Ᾱ", "Ᾰ", "ᾼ",
      "ἐ", "ἑ", "ἒ", "ἓ", "ἔ", "ἕ", "ὲ", "έ", "ὲ", "έ",
      "Ἐ", "Ἑ", "Ἒ", "Ἓ", "Ἔ", "Ἕ", "Ὲ", "Έ", "Ε",
      "ἠ", "ἡ", "ἢ", "ἣ", "ἤ", "ἥ", "ἦ", "ἧ", "ῆ", "ὴ", "ή", "ῃ", "ῇ", "ῄ", "ῂ", "ᾐ", "ᾑ", "ᾒ", "ᾓ", "ᾔ", "ᾕ", "ᾖ", "ᾗ",
      "Ἠ", "Ἡ", "Ἢ", "Ἣ", "Ἤ", "Ἥ", "Ἦ", "Ἧ", "Ὴ", "Ή", "Η", "ῌ",
      "ἰ", "ἱ", "ἲ", "ἳ", "ἴ", "ἵ", "ἶ", "ἷ", "ῖ", "ὶ", "ί", "ῒ", "ΐ", "ῗ", "ΐ",
      "Ἰ", "Ἱ", "Ἲ", "Ἳ", "Ἴ", "Ἵ", "Ἶ", "Ἷ", "Ὶ", "Ί", "Ι", "Ῑ", "Ῐ",
      "ὀ", "ὁ", "ὂ", "ὃ", "ὄ", "ὅ", "ὸ", "ό",
      "Ὀ", "Ὁ", "Ὂ", "Ὃ", "Ὄ", "Ὅ", "Ὸ", "Ό", "Ο",
      "ὐ", "ὑ", "ὒ", "ὓ", "ὔ", "ὕ", "ὖ", "ὗ", "ῦ", "ὺ", "ύ", "ῢ", "ΰ", "ῧ", "ΰ",
      "Ὑ", "Ὓ", "Ὕ", "Ὗ", "Ὺ", "Ύ", "Υ", "Ῡ", "Ῠ",
      "ὠ", "ὡ", "ὢ", "ὣ", "ὤ", "ὥ", "ὦ", "ὧ", "ῶ", "ὼ", "ώ", "ῳ", "ῷ", "ῴ", "ῲ", "ᾠ", "ᾡ", "ᾢ", "ᾣ", "ᾤ", "ᾥ", "ᾦ", "ᾧ",
      "Ὠ", "Ὡ", "Ὢ", "Ὣ", "Ὤ", "Ὥ", "Ὦ", "Ὧ", "Ὼ", "Ώ", "Ω", "ῼ",
      "ῤ", "ῥ", "ῤ", "ῥ",
      "Ῥ", "Ρ",
      "β", "Β", "γ", "Γ", "δ", "Δ", "ζ", "Ζ", "θ", "Θ", "κ", "Κ", "λ", "Λ", 
      "μ", "Μ", "ν", "Ν", "ξ", "Ξ", "π", "Π", "σ", "ς", "Σ", "τ", "Τ", 
      "φ", "Φ", "χ", "Χ", "ψ", "Ψ"
    )
    
    if (word %in% single_letter_variants) return(TRUE)
    
    # Check if stripping diacritics results in single character
    word_stripped <- gsub("[΄`´'ʹ͂̀́̂̃̈̓̔͂ͅ῾᾿ῥῤ῀῁ῆῇῒΐῗῢΰῧῶῷ]", "", word)
    word_stripped <- gsub("[ᾶὰάᾳᾷᾴᾲἀἁἂἃἄἅἆἇᾀᾁᾂᾃᾄᾅᾆᾇ]", "α", word_stripped)
    word_stripped <- gsub("[ἈἉἊἋἌἍἎἏᾺΆΑᾹᾸ]", "Α", word_stripped)
    word_stripped <- gsub("[ὲέἐἑἒἓἔἕ]", "ε", word_stripped)
    word_stripped <- gsub("[ῈΈΕἘἙἚἛἜἝ]", "Ε", word_stripped)
    word_stripped <- gsub("[ῆὴήῃῇῄῂἠἡἢἣἤἥἦἧᾐᾑᾒᾓᾔᾕᾖᾗ]", "η", word_stripped)
    word_stripped <- gsub("[ῊΉΗἨἩἪἫἬἭἮἯ]", "Η", word_stripped)
    word_stripped <- gsub("[ῖὶίῒΐῗἰἱἲἳἴἵἶἷ]", "ι", word_stripped)
    word_stripped <- gsub("[ῚΊΙἸἹἺἻἼἽἾἿῙῘ]", "Ι", word_stripped)
    word_stripped <- gsub("[ὸόὀὁὂὃὄὅ]", "ο", word_stripped)
    word_stripped <- gsub("[ῸΌΟὈὉὊὋὌὍ]", "Ο", word_stripped)
    word_stripped <- gsub("[ῦὺύῢΰῧὐὑὒὓὔὕὖὗ]", "υ", word_stripped)
    word_stripped <- gsub("[ῪΎΥὙὛὝὟῩῨ]", "Υ", word_stripped)
    word_stripped <- gsub("[ῶὼώῳῷῴῲὠὡὢὣὤὥὦὧᾠᾡᾢᾣᾤᾥᾦᾧ]", "ω", word_stripped)
    word_stripped <- gsub("[ῺΏΩὨὩὪὫὬὭὮὯ]", "Ω", word_stripped)
    word_stripped <- gsub("[ῤῥ]", "ρ", word_stripped)
    word_stripped <- gsub("[Ῥ]", "Ρ", word_stripped)
    
    if (nchar(word_stripped) == 1) return(TRUE)
    
    return(FALSE)
  }
  
  # Validation function for checking function words
  is_function_word_validation <- function(word) {
    if (word %in% function_words) return(TRUE)
    word_lower <- tolower(word)
    if (word_lower %in% tolower(function_words)) return(TRUE)
    word_stripped <- gsub("[΄`´'ʹ͂̀́̂̃̈̓̔͂ͅ]", "", word)
    for (fw in function_words) {
      fw_stripped <- gsub("[΄`´'ʹ͂̀́̂̃̈̓̔͂ͅ]", "", fw)
      if (word_stripped == fw_stripped) return(TRUE)
    }
    return(FALSE)
  }
  
  issues_found <- FALSE
  
  if (!is.null(cleaned_data$meta_groups)) {
    for (meta_group in cleaned_data$meta_groups) {
      if (!is.null(meta_group$clusters)) {
        for (cluster in meta_group$clusters) {
          if (!is.null(cluster$members)) {
            for (member in cluster$members) {
              lemmas <- if (!is.null(member$lemma)) {
                if (is.list(member$lemma)) {
                  unlist(member$lemma)
                } else {
                  member$lemma
                }
              } else {
                character()
              }
              
              for (lemma in lemmas) {
                # Check for ANY single letter (including those with diacritics)
                if (is_single_greek_letter_validation(lemma)) {
                  cluster_id <- if (!is.null(cluster$cluster_id)) cluster$cluster_id else "unknown"
                  cat(sprintf("WARNING: Single letter lemma still present: '%s' in cluster %s\n", 
                              lemma, cluster_id))
                  issues_found <- TRUE
                }
                # Check for multi-character function words
                if (is_function_word_validation(lemma)) {
                  cluster_id <- if (!is.null(cluster$cluster_id)) cluster$cluster_id else "unknown"
                  cat(sprintf("WARNING: Function word still present: '%s' in cluster %s\n", 
                              lemma, cluster_id))
                  issues_found <- TRUE
                }
              }
            }
          }
        }
      }
    }
  }
  
  if (!issues_found) {
    cat("✓ All single-letter lemmas (including those with diacritics) and function words successfully removed!\n")
  }
  
  # Report cluster size statistics
  cat("\nCluster size statistics after cleaning:\n")
  sizes <- c()
  
  if (!is.null(cleaned_data$meta_groups)) {
    for (meta_group in cleaned_data$meta_groups) {
      if (!is.null(meta_group$clusters)) {
        for (cluster in meta_group$clusters) {
          if (!is.null(cluster$size)) {
            sizes <- c(sizes, cluster$size)
          }
        }
      }
    }
  }
  
  if (length(sizes) > 0) {
    cat(sprintf("  Total clusters: %d\n", length(sizes)))
    cat(sprintf("  Min size: %d\n", min(sizes)))
    cat(sprintf("  Max size: %d\n", max(sizes)))
    cat(sprintf("  Mean size: %.2f\n", mean(sizes)))
    cat(sprintf("  Median size: %.1f\n", median(sizes)))
  } else {
    cat("  No clusters found with size information\n")
  }
}

###
# Function to print all function words being checked
##
print_function_words <- function() 
{
  # Same list as in the main function
  function_words_base <- c(
    "καί", "Καί", "ΚΑΙ", "και", "καὶ", "Καὶ",
    "ἀλλά", "Ἀλλά", "ἀλλὰ", "Ἀλλὰ", "ΑΛΛΑ", "ἀλλ'", "Ἀλλ'",
    "γάρ", "Γάρ", "γὰρ", "Γὰρ", "ΓΑΡ",
    "οὖν", "Οὖν", "ΟΥΝ", "ουν",
    "ἵνα", "Ἵνα", "ἱνα", "Ἱνα", "ἰνα", "ΙΝΑ",
    "ἐάν", "Ἐάν", "ἐὰν", "Ἐὰν", "ΕΑΝ", "ἄν", "Ἄν", "ἂν", "Ἂν",
    "ὅτι", "Ὅτι", "ὁτι", "Ὁτι", "ΟΤΙ",
    "δέ", "Δέ", "δὲ", "Δὲ", "ΔΕ", "δ'", "Δ'",
    "μή", "Μή", "μὴ", "Μὴ", "ΜΗ",
    "οὐ", "Οὐ", "οὑ", "Οὑ", "ΟΥ", "ου",
    "οὐκ", "Οὐκ", "οὑκ", "Οὑκ", "ΟΥΚ", "ουκ",
    "οὐχ", "Οὐχ", "οὑχ", "Οὑχ", "ΟΥΧ", "ουχ", "οὐχὶ", "Οὐχὶ", "οὐχί", "Οὐχί",
    "τέ", "Τέ", "τὲ", "Τὲ", "τε", "Τε", "ΤΕ",
    "μήποτε", "Μήποτε", "μὴποτε", "Μὴποτε", "ΜΗΠΟΤΕ",
    "μήπως", "Μήπως", "μὴπως", "Μὴπως",
    "εἰ", "Εἰ", "ΕΙ", "εἰς", "Εἰς", "ΕΙΣ",
    "ἐν", "Ἐν", "ΕΝ", "ἑν", "Ἑν",
    "τίς", "Τίς", "τὶς", "Τὶς", "τις", "Τις", "ΤΙΣ",
    "τῶν", "Τῶν", "των", "Των", "ΤΩΝ",
    "μέν", "Μέν", "μὲν", "Μὲν", "μεν", "Μεν", "ΜΕΝ",
    "ἐπί", "Ἐπί", "ἐπὶ", "Ἐπὶ", "επι", "Επι", "ΕΠΙ",
    "πρός", "Πρός", "πρὸς", "Πρὸς", "προς", "Προς", "ΠΡΟΣ",
    "διά", "Διά", "διὰ", "Διὰ", "δια", "Δια", "ΔΙΑ",
    "κατά", "Κατά", "κατὰ", "Κατὰ", "κατα", "Κατα", "ΚΑΤΑ",
    "μετά", "Μετά", "μετὰ", "Μετὰ", "μετα", "Μετα", "ΜΕΤΑ",
    "παρά", "Παρά", "παρὰ", "Παρὰ", "παρα", "Παρα", "ΠΑΡΑ",
    "ὑπό", "Ὑπό", "ὑπὸ", "Ὑπὸ", "υπο", "Υπο", "ΥΠΟ",
    "ἀπό", "Ἀπό", "ἀπὸ", "Ἀπὸ", "απο", "Απο", "ΑΠΟ",
    "περί", "Περί", "περὶ", "Περὶ", "περι", "Περι", "ΠΕΡΙ",
    "ὡς", "Ὡς", "ὠς", "Ὠς", "ως", "Ως", "ΩΣ",
    "ἕως", "Ἕως", "ἑως", "Ἑως", "εως", "Εως", "ΕΩΣ",
    "ἄρα", "Ἄρα", "ἀρα", "Ἀρα", "αρα", "Αρα", "ΑΡΑ",
    "οὔτε", "Οὔτε", "οὐτε", "Οὐτε", "ουτε", "Ουτε", "ΟΥΤΕ",
    "μήτε", "Μήτε", "μὴτε", "Μὴτε", "μητε", "Μητε", "ΜΗΤΕ",
    "εἴτε", "Εἴτε", "εἰτε", "Εἰτε", "ειτε", "Ειτε", "ΕΙΤΕ"
  )
  
  function_words <- sort(unique(function_words_base))
  
  # Group by base form (simplified grouping)
  cat("\nTotal unique forms:", length(function_words), "\n")
  cat("\nAll forms (alphabetically sorted):\n")
  
  # Print in columns for better readability
  ncol <- 5
  nwords <- length(function_words)
  nrows <- ceiling(nwords / ncol)
  
  for (i in 1:nrows) {
    row_words <- character()
    for (j in 0:(ncol-1)) {
      idx <- i + j * nrows
      if (idx <= nwords) {
        row_words <- c(row_words, sprintf("%-15s", function_words[idx]))
      }
    }
    cat(paste(row_words, collapse = " "), "\n")
  }
  
  cat("\nNote: Additionally, ALL single-letter lemmas are automatically removed, including:\n")
  cat("  - Simple single characters (α, β, γ, etc.)\n")
  cat("  - Single letters with diacritics that may appear as 2+ chars (ἆ, ἕ, ἒ, Ἤ, ᾗ, Ἡ, etc.)\n")
  cat("  - Articles with breathing marks (ὁ, ἡ, etc.)\n")
  cat("  - Any Greek letter with accents, breathing marks, or iota subscripts\n")
  cat("\nThe matching also uses fuzzy comparison to catch variants with different diacritics.\n")
}

###
# Function to diagnose JSON structure (helpful for debugging)
##
diagnose_json_structure <- function(json_file) 
{
  cat("Diagnosing JSON structure...\n")
  
  # Try different parsing options
  cat("\nAttempting to parse with simplifyVector=FALSE...\n")
  tryCatch({
    data <- fromJSON(json_file, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    cat("✓ Successfully parsed with simplifyVector=FALSE\n")
    
    # Check top-level structure
    cat("\nTop-level keys:", paste(names(data), collapse = ", "), "\n")
    
    if ("meta_groups" %in% names(data)) {
      cat("Number of meta_groups:", length(data$meta_groups), "\n")
      
      if (length(data$meta_groups) > 0) {
        # Check first meta_group structure
        first_mg <- data$meta_groups[[1]]
        cat("\nFirst meta_group keys:", paste(names(first_mg), collapse = ", "), "\n")
        
        if (!is.null(first_mg$clusters) && length(first_mg$clusters) > 0) {
          cat("Number of clusters in first meta_group:", length(first_mg$clusters), "\n")
          
          # Check first cluster structure
          first_cluster <- first_mg$clusters[[1]]
          cat("\nFirst cluster keys:", paste(names(first_cluster), collapse = ", "), "\n")
          
          if (!is.null(first_cluster$members) && length(first_cluster$members) > 0) {
            cat("Number of members in first cluster:", length(first_cluster$members), "\n")
            
            # Check first member structure
            first_member <- first_cluster$members[[1]]
            cat("\nFirst member keys:", paste(names(first_member), collapse = ", "), "\n")
            
            if (!is.null(first_member$lemma)) {
              cat("First member lemma type:", class(first_member$lemma), "\n")
              if (is.list(first_member$lemma)) {
                cat("First member lemma content:", unlist(first_member$lemma), "\n")
              } else {
                cat("First member lemma content:", first_member$lemma, "\n")
              }
            }
          }
        }
      }
    }
    
    return(data)
  }, error = function(e) {
    cat("✗ Failed to parse:", e$message, "\n")
    return(NULL)
  })
}

################################################################################
# End functions
################################################################################

# Main execution
# Usage examples:
#   Interactive R session:
#     # Clean the data
#     cleaned_data <- clean_lemma_clusters("data.json", "data_cleaned.json")
#     validate_cleaning(cleaned_data)
#     
#     # List all function words being removed
#     print_function_words()
#     
#     # If you encounter errors, diagnose the JSON structure:
#     diagnose_json_structure("data.json")

# If you want to run it directly from command line:
if (!interactive()) 
{
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    cat("Usage: Rscript clean_lemmas.R <input.json> [output.json]\n")
    cat("   Or: Rscript clean_lemmas.R --diagnose <input.json>\n")
    cat("   Or: Rscript clean_lemmas.R --list-words\n")
    quit(status = 1)
  }
  
  # Check for list-words flag
  if (args[1] == "--list-words") {
    print_function_words()
    quit(status = 0)
  }
  
  # Check for diagnose flag
  if (args[1] == "--diagnose") {
    if (length(args) < 2) {
      cat("Error: Please specify input file for diagnosis\n")
      quit(status = 1)
    }
    input_file <- args[2]
    if (!file.exists(input_file)) {
      cat(sprintf("Error: Input file '%s' not found\n", input_file))
      quit(status = 1)
    }
    diagnose_json_structure(input_file)
    quit(status = 0)
  }
  
  input_file <- args[1]
  output_file <- if (length(args) >= 2) args[2] else NULL
  
  # Check if input file exists
  if (!file.exists(input_file)) {
    cat(sprintf("Error: Input file '%s' not found\n", input_file))
    quit(status = 1)
  }
  
  tryCatch({
    cleaned_data <- clean_lemma_clusters(input_file, output_file)
    validate_cleaning(cleaned_data)
  }, error = function(e) {
    cat(sprintf("\nError processing file: %s\n", e$message))
    cat("\nTry running with --diagnose flag to check JSON structure:\n")
    cat(sprintf("  Rscript clean_lemmas.R --diagnose %s\n", input_file))
    quit(status = 1)
  })
}

