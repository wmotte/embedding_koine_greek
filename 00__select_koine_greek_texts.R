#!/usr/bin/env Rscript
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
#
# Aim: Select all texts relevant to Koine
# We exclude clearly pre-Koine texts (e.g., Homer, Hesiod) and (proto)Byzantium texts.
#
# Ref: Geoffrey Horrocks 'Greek: A History of the Language and its Speakers' (2nd edition)', Wiley-Blackwell, 2010:
#
# p. 79:
# "During the latter half of the 4th century Bc the kingdom of Macedonia first became the controlling power in mainland Greece; 
# and then, through the spectacular conquests of Alexander III (*the Great', 356-323 BC), acquired control of the whole of 
# the eastern Mediterranean, including Asia Minor, Syria and Egypt, and finally extended its rule throughout the former 
# Persian empire to the borders of India. Great new cities were founded in the conquered territories, most notably 
# Alexandria in Egypt, Pergamum in Asia Minor, and Antioch in Syria, and Greek culture and language were spread as far as the plains of the Punjab."
#
# p. 193: 
# "When Constantine founded his 'New Rome' on the site of the old Greek city of Byzantium on the Bosporus in AD May 330, 
# therefore, it was not merely as a centre of Roman culture and Latinity in the east but also as a capital city that was to be
# imbued from the outset with the spirit of the Christian faith. In due course it provided the physical and spiritual 
# centre for the medieval Byzantine state."
################################################################################

# Load necessary library
library( "readr" )

# Output directory
outdir <- 'out.00.select.koine.greek.texts'
dir.create( outdir, showWarnings = FALSE )

# Read the metadata
meta <- readr::read_tsv( "misc/metadata.txt", show_col_types = FALSE )

# Define Koine Greek period range (Early + Middle): 330 BCE to 200 CE
koine_start <- -330
koine_end   <- 330 # Constantine founded his 'New Rome' on the site of the old Greek city of Byzantium on the Bosporus in AD May 330.

# Select texts overlapping with Koine period
koine_texts <- meta[ meta$ENDDATE >= koine_start & meta$STARTDATE <= koine_end, ]

# average date
koine_texts$mean_date <- ( koine_texts$STARTDATE + koine_texts$ENDDATE ) / 2

# clean
remove_cols <- c( 'GLAUX_TEXT_ID', 'SOURCE_LICENSE', 'SOURCE_FORMAT', 'TM_TEXT' )
koine_texts <- koine_texts[ , !colnames( koine_texts ) %in% remove_cols ]

colnames( koine_texts ) <- c( 'tlg', 'date_lower_bound', 'date_upper_bound', 'author', 'title', 'genre', 'dialect', 'source', 'words', 'date_mean' )

# total number of words 18,204,974
sum( koine_texts$words )

# View the selection
nrow( koine_texts ) # 1255 texts

# Optionally write to file
write_tsv( koine_texts, file.path( outdir, "selected_texts_koine_greek.tsv" ), quote = 'all' )

library( "ggplot2" )
library( "dplyr" )

# bin width
bin_width <- 100

# Plot distribution of text dating over time
p <- ggplot( koine_texts, aes( x = date_mean ) ) +
        geom_histogram( binwidth = bin_width, fill = "#67a9cf", color = "gray30" ) +
        labs( x = "Average Date (CE)", y = "Number of Texts" ) +
        scale_x_continuous( breaks = scales::pretty_breaks( n = 10 ) ) +
        scale_y_continuous( breaks = scales::pretty_breaks( n = 12 ) ) + 
        theme_minimal()

# save to disk
ggsave( p, file = file.path( outdir, 'plot_average_date_selected_texts_koine_greek.png' ), width = 5, height = 5, dpi = 600, bg = 'white' )


# Create 100-year bins

koine_binned <- koine_texts %>%
    mutate( date_bin = bin_width * ( 1 + floor( date_mean / bin_width ) ) ) %>%
    group_by( date_bin ) %>%
    summarise( total_words = sum( words ), .groups = "drop" )

# Plot total number of words in Koine Texts per bin
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

