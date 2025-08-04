#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
# Select books within the Biblical Greek period.
#
#
# The verb is the PoS annotated for person, number, tense, mood, and voice (and gender and case if the verb is a participle), 
# and is lemmatized as such in the LSJ dictionary. 
#
################################################################################

library( "xml2" )
library( "dplyr" )


################################################################################

###
# Parse treebank
##
parse_treebank <- function( xml_file )
{
    # Read XML
    xml_data <- read_xml( xml_file )
    
    # Extract word nodes
    words <- xml_find_all( xml_data, "//word" )
    
    # Create data frame
    df <- data.frame(
        id = xml_attr( words, "id" ),
        form = xml_attr( words, "form" ),
        lemma = xml_attr( words, "lemma" ),
        postag = xml_attr( words, "postag" ),
        head = xml_attr( words, "head" ),
        relation = xml_attr( words, "relation" ),
        chapter = xml_attr( words, "div_chapter" ),
        section = xml_attr( words, "div_section" )
    )

    df$source <- gsub( "\\.xml", "", gsub( "xml/", "", xml_file ) )
        
    # remove non-required columns, sort and rename 'source'
    df <- df[, c( 'source', 'chapter', 'section', 'form', 'lemma', 'postag' ) ]

    
    # remove .,;
    df <- df[ df$postag != 'u--------', ]
    
    # remove end of chapter stuff
    df <- df[ df$postag != "z--------", ]
    
    # remove NA's in lemma (i.e., diacritical stuff)
    df <- df[ !is.na( df$lemma ), ]

    # some lemmas have spaces i.e. in 0081-001: "Κοριλ ́λα"
    df$lemma <- gsub( " ", "", df$lemma )

    # if chapter is NA set to 0
    if( sum( is.na( df$chapter ) ) == nrow( df ) )
        df$chapter <- 0
        
    rownames( df ) <- NULL
    
    return( df )
}

 


################################################################################

# output dir
outdir <- 'out.01.select.lemmas'
dir.create( outdir, showWarnings = FALSE )

# input meta data
meta <- readr::read_tsv( 'out.00.select.koine.greek.texts/selected_texts_koine_greek.tsv', show_col_types = FALSE )

# total tokens [18.2M] however, tokens are not always words, so explicitly count words after reading
print( total_tokens <- sum( meta$tokens ) )
 
i <- 1

# word statistics
meta$true_n_words <- NA

# container to store all lemmas
full <- NULL

# each text seperately
for( i in 1:nrow( meta ) )
{
    tlg <- meta[ i, 'tlg' ]
    author <- meta[ i, 'author' ]  
    title <- meta[ i, 'title' ]  
    xml_file <- paste0( 'xml/', tlg, '.xml' )

    outfile_df <- paste0( outdir, '/book_', tlg, '__', gsub( ' ', '_', title ), '.tsv.gz' )
    outfile_lemma <- paste0( outdir, '/book_', tlg, '__', gsub( ' ', '_', title ), '__lemma_sentence.tsv.gz' )
    
    # only if xml is available and outfile not   
    if( file.exists( xml_file ) & !file.exists( outfile_df ) )
    {
        print( paste0( "*** PROCESSING: ", i, ' from ', nrow( meta ), ': - ', author, ' -- ', title ) )
        
        # single
        df <- parse_treebank( xml_file )
        df$author <- as.character( author )
        df$title <- as.character( title )
        
        # merge
        full <- rbind( full, df )
        
        # make single sentence of all the book text
        sentence_lemma <- paste0( df[ , 'lemma' ], collapse = ' ' )    

        # write df to disk
        readr::write_tsv( df, file = gzfile( outfile_df ), quote = 'all' )
                
        # write lemma sentence to disk
        readr::write_tsv( data.frame( text = sentence_lemma ), file = gzfile( outfile_lemma ), quote = 'all' )
        
        # add true words
        meta[ i, 'true_n_words' ] <- nrow( df )
        
    }
}

# get number of unique lemmas
full_lemmas <- full[ !duplicated( full$lemma ), ]

# write df to disk
full_lemma_outputfile <- gzfile( file.path( outdir, 'full_unique_lemma_list.tsv.gz' ) )
readr::write_tsv( full_lemmas, file = full_lemma_outputfile, quote = 'all' )

# nrows
nrow( full_lemmas )


################################################
# Make plot of true number of words (lemma's)
################################################

# total number of words (or lemma's): 15_882_313
sum( meta$true_n_words )

# write updated meta to disk
readr::write_tsv( meta, file = file.path( outdir, 'meta_with_true_word_count.tsv' ), quote = 'all' )

# Create 100-year bins
bin_width <- 100
koine_binned <- meta %>%
    mutate( date_bin = bin_width * ( 1 + floor( date_mean / bin_width ) ) ) %>%
    group_by( date_bin ) %>%
    summarise( total_words = sum( true_n_words ), .groups = "drop" )

# write words per bin to disk
readr::write_tsv( koine_binned, file = file.path( outdir, 'words_per_100_year_bin.tsv' ), quote = 'all' )

# Plot total words in Koine Texts per bin
p_words <- ggplot( koine_binned, aes( x = date_bin, y = total_words ) ) +
    geom_col( fill = "#67a9cf", color = "gray30", width = bin_width * 0.95 ) +
    labs( x = "Average Date (CE)", y = "Number of Words" ) +
    scale_x_continuous(
        breaks = seq(min(koine_binned$date_bin), max(koine_binned$date_bin), by = 100 ),
        minor_breaks = seq(min(koine_binned$date_bin), max(koine_binned$date_bin), by = 50 )
    ) +
    scale_y_continuous( breaks = scales::pretty_breaks( n = 12 ), labels = scales::comma ) +
    theme_minimal()

# save to disk
ggsave( p_words, file = file.path( outdir, 'plot_total_words_selected_texts_koine_greek.png' ), width = 5, height = 5, dpi = 600, bg = 'white' )


###################################
################ MERGE ############
###################################

# Get all lemma .tsv.gz files in the output directory
converted_files <- list.files( outdir, pattern = "lemma_sentence\\.tsv\\.gz$", full.names = TRUE )

# output file
outfile <- paste0( outdir, '/ALL_lemma_sentences.tsv.gz' )

if( !file.exists( outfile ) )
{
    # Read and merge all files into a single data.frame
    merged_data <- do.call( rbind, lapply( converted_files, function( file ) {
        print( file )
        data <- readr::read_tsv( file, show_col_types = FALSE )
        return( data )
    } ) )
    
    # View the merged data
    head( merged_data )
    tail( merged_data )
    
    # write to disk
    readr::write_tsv( merged_data, file = gzfile( outfile ), quote = 'all' )
}

#################################
# Select unique lemmas in LXX-NT
#################################

# get Biblical texts only [n=84]
meta_bib <- meta[ meta$author %in% c( 'Septuaginta', 'Novum Testamentum' ), ]

# total words = 749_678
sum( meta_bib$true_n_words )

## get all lemma's in LXX + NT
all <- NULL
i <- 1

# each text seperately
for( i in 1:nrow( meta_bib ) )
{
    tlg <- meta_bib[ i, 'tlg' ]
    author <- meta_bib[ i, 'author' ]  
    title <- meta_bib[ i, 'title' ]  
    xml_file <- paste0( 'xml/', tlg, '.xml' )
    
    # only if xml is available and outfile not   
    if( file.exists( xml_file ) )
    {
        print( paste0( "*** PROCESSING: ", i, ' from ', nrow( meta_bib ), ': - ', author, ' -- ', title ) )
        
        # single
        df <- parse_treebank( xml_file )
        df$author <- as.character( author )
        df$title <- as.character( title )
        
        # merge
        all <- rbind( all, df )
    }
}

# sort, such that LXX precedes NT
all <- all[ order( all$author, decreasing = TRUE ), ]
unique( all$title )
all$form <- NULL
all$postag <- NULL
all$chapter <- NULL

colnames( all ) <- c( 'TLG', 'chapter.verse', 'lemma', 'source', 'book' )
head( all )
tail( all )

# get unique lemmas with first position in LXX-NT
# Novum Testamentum       Septuaginta 
#              1475             15493 
df_lemmas <- all[ !duplicated( all$lemma ), ]
summary( as.factor( df_lemmas$source ) )

# write to disk
readr::write_tsv( df_lemmas, file.path( outdir, 'unique_lemmas_LXX_NT.tsv' ), quote = 'all' )

# for NT only
nt <- all[ all$source == 'Novum Testamentum', ]
nt <- nt[ !duplicated( nt$lemma ), ]

# Novum Testamentum 
#              5315 
summary( as.factor( nt$source ) )

# 72.2% already in LXX
round( 100 - 100 * ( summary( as.factor( df_lemmas$source ) )[ 1 ] / summary( as.factor( nt$source ) ) ), 1 )

