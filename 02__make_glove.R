#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
# Aim: Embed lemmas into glove model.
#
################################################################################
library( "text2vec" )
library( "ggplot2" )

################################################################################

###
# Return tokens, it, and vectorizer and save token freq
##
get_preparation <- function( sentence, outdir, term_count_min )
{
    # Create iterator over tokens
    tokens <- space_tokenizer( sentence )
    
    # Create vocabulary. Terms will be unigrams (simple words).
    it <- itoken( tokens, progressbar = TRUE )
    vocab <- create_vocabulary( it )
    
    # prune to get rid of maximal "@" BE CAREFUL! 'term_count_max' will remove everything above freq of @!!
    #vocab <- prune_vocabulary( vocab, term_count_max = vocab[ vocab$term == '@', 'term_count' ] - 1 )
    

    terms_to_remove <- c(     
        "𐅻",
        "ς#",
        "⋖ʹ",
        "ϟʹ",
        "𝈒𝈩",
        ",",
        "𝈚𝈿",
        "Ϡ",
        "_",
        "⩹ʹ",
        "Ϟʹ",
        "Ι𝈶",
        "Ϡʹ",
        "𐅻̅",
        "𐆄",
        "γ#",
        "Ιϡ",
        "Φ𝈓̓" ,
        "̓",
        "Ε𝈈",
        "Μ𝈳",
        "Σϡ",
        "ϡ",
        "ʹ",
        "∠ʹ",
        "𝈖𝈪",
        "Ρ𝈱",
        "͵ε",
        "𝈛𝈾",
        "α#",
        "ε#",
        "Ζ𝈸",
        "Θ𝈍" )
    
    for( term in terms_to_remove )
    {
        # remove @
        vocab <- vocab[ vocab$term != term, ]
        rownames( vocab ) <- NULL
    }
    
    # prune to remove all tokens with too few occurances
    vocab <- prune_vocabulary( vocab, term_count_min = term_count_min )
    vocab$doc_count <- NULL
    
    # Use our filtered vocabulary
    vectorizer <- vocab_vectorizer( vocab )
    
    # output list
    output <- list( vocab = vocab, it = it, vectorizer = vectorizer )
    
    return( output )
}

################################################################################

# outdir
outdir <- 'out.02.make.glove'
dir.create( outdir, showWarnings = FALSE )

# read texts
df <- readr::read_tsv( 'out.01.select.lemmas/ALL_lemma_sentences.tsv.gz', show_col_types = FALSE )

# Input to GloVe is a single line of texts, however, we do not want word counts 
# to influence at boundaries therefore, we concat with a dummy term in between.
list_separator <- paste0( " ", paste0( rep( "@", 10 ), collapse = ' ' ), " " )

# get single string, separated by list_separator [197.1 MB]
sentence <- paste( df$text, collapse = list_separator )

# split sentences into words
words <- unlist( strsplit( sentence, " " ) )

# minimal frequency to be included
term_count_min <- 5

# vectorizer
preparation <- get_preparation( sentence, outdir, term_count_min )
vocabs <- preparation$vocab[ order( preparation$vocab$term_count, decreasing = TRUE ), ]

# make Zipf's law plot
p_freq <- ggplot( vocabs, aes( x = 1:nrow( vocabs ), y = term_count ) ) +
    geom_hline(yintercept = 5, color = "gray", linetype = "dashed") +
    geom_line(color = "steelblue", linewidth = 1.1 ) +
    scale_y_log10( labels = scales::label_comma(),  breaks = c(5, 10, 50, 100, 500, 1000, 5000, 10000, 5e5, 1e5, 5e5, 1e6) ) +
    annotation_logticks( sides = "l" ) +
    scale_x_continuous( breaks = c( 1, 4000, 8000, 12000, 16000, 20000, 24000, 28000, 32000, 36000, 39913 ) ) +
    labs( x = "Lemma (sorted from high to low prevalence)", y = "Prevalence" ) +
    theme_bw() 

# save to disk
ggsave( p_freq, file = file.path( outdir, 'Figure_lemma_prevalence_koine_greek.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )


## make prevalence plot for lemmas inside/outside lxx-nt
bib <- readr::read_tsv( 'out.01.select.lemmas/unique_lemmas_LXX_NT.tsv', show_col_types = FALSE )

vocabs$x <- 1:nrow( vocabs )
vocabs$group <- 'outside-lxx-nt'
vocabs[ vocabs$term %in% bib$lemma, 'group' ] <- 'lxx-nt'

# lxx-nt outside-lxx-nt 
# 10678           29235 
summary( as.factor( vocabs$group ) )

# make Zipf's law plot
p_freq2 <- 
    ggplot( vocabs, aes( x = x, y = term_count ) ) +
    geom_hline( yintercept = 5, color = "gray", linetype = "dashed" ) +
    geom_line( aes( color = group) , linewidth = 0.5, alpha = 1 ) +
    geom_point( size = 0.75, shape = 21, color = "gray40", stroke = 0.1, alpha = 0.5 ) +
    facet_wrap( ~group, nrow = 2 ) +
    scale_y_log10( labels = scales::label_comma(), breaks = c(5, 10, 50, 100, 500, 1000, 5000, 10000, 5e5, 1e5, 5e5, 1e6) ) +
    annotation_logticks( sides = "l" ) +
    scale_x_continuous( labels = scales::label_comma(), breaks = c( 1, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 39913 ) ) +
    labs( x = "Lemma (sorted from high to low prevalence)", y = "Lemma Prevalence" ) +
    theme_bw() +
    theme( legend.position = 'none' ) +
    scale_fill_manual( values = c( '#ef8a62', '#67a9cf' ) ) 

ggsave( p_freq2, file = file.path( outdir, 'Figure_lemma_prevalence_koine_greek__split.png' ), width = 6, height = 8, dpi = 900, bg = 'white' )

##############
# GLOVE model
##############

set.seed( 999 )

# window on left and right
nwindow <- 5

# term-co-occurrence matrix (TCM)
tcm <- create_tcm( preparation$it, preparation$vectorizer, skip_grams_window = nwindow, skip_grams_window_context = "symmetric" )
dim( tcm ) # 39.9k x 39.9k

# vector length
rank <- 150

# number of iterations
n_iter <- 100

# glove model
glove <- GlobalVectors$new( rank = rank, x_max = 100 )
wv_main <- glove$fit_transform( tcm, n_iter = n_iter, convergence_tol = 0.00001, n_threads = 8 )

# get context matrix
wv_context <- glove$components

# 150 x 39944
dim( wv_context )

# combine main embedding and context embedding (sum) into one matrix
# 39944 x 150
dim( embedding <- wv_main + t( wv_context ) )

# save files to disk
save( embedding, file = paste0( outdir, "/saved_glove__", nwindow, ".RData" ) )

# check how embedding rows correspond with frequency (for debugging)
rr <- rownames( embedding )
idx <- vocabs$term %in% rr

vocabs$included <- FALSE
vocabs[ idx, 'included' ] <- TRUE

# check two terms
term1 <- "πνεῦμα"	
term2 <- "γράμμα"	

vocabs[ vocabs$term == term1, ] # 7241
vocabs[ vocabs$term == term2, ] # 2745

# write to disk
readr::write_tsv( vocabs, file.path( outdir, 'frequencies_in_corpus__min_5.tsv' ), quote = 'all' )

