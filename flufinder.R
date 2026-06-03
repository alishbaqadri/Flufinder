#Function 3:

split_peptides <- function(peptides) {
  
  # Opening stringr for simple string manipulation
  library(stringr)
  
  # Splitting peptides into individual amino acids using str_split;
  # generates a list of lists of amino acids for each peptide
  lapply(peptides, str_split, pattern = "")
}



#Function 6
ggbarplot <- function(peptide_counts_table) {
  
  library(ggplot2)
  
  # Generating a barplot from the peptide counts dataframe
  ggplot(peptide_counts_table) +
    aes(rownames(peptide_counts_table), peptide_counts) +
    geom_col(fill = "blue", width = 0.5) +
    theme_bw() +
    labs(x = "Flu Strain", y = "Peptide Counts")
  
}


