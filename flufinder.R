# Function 1
upload_fasta <- function(testfasta.txt) {
  ## Open package
  library(seqinr)
  
  ## read file
  read.fasta(file = testfasta.txt, 
             seqtype = "AA", 
             as.string = TRUE, 
             set.attributes = FALSE)
}

#Function 3:
split_peptides <- function(peptides) {
  # Opening stringr for simple string manipulation
  library(stringr)
  # Splitting peptides into individual amino acids using str_split;
  # generates a list of lists of amino acids for each peptide
  lapply(peptides, str_split, pattern = "")
}


# Function 4
splitpeptides_to_masses <- function(aa) {
  ## Generating a vector of masses for each amino acid
  aa_masses <- c(A=71.037, R=156.101, N=114.042, D=115.026, C=103.009,
                 Q=128.058, E=129.042, G=57.021, H=137.058, I=113.084, L=113.084,
                 K=128.094, M=131.040, F=147.068, P=97.052, S=87.032, T=101.047,
                 W=186.079, Y=163.063, V=99.068)
  
  ## Looping over each list of peptides and mapping amino acids to masses, returning the sum as peptide masses
  peptide_masses <- aa
  for(i in 1:length(aa)) {
    peptide_masses[[i]] <- lapply(aa[[i]],
                                  function(x) sum(aa_masses[x]))
  }
  
  ## Unlisting the inner lists to generate a list of vectors of masses for each protein
  return(lapply(peptide_masses, unlist))
  
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

#Using a function that simulates trypsin digestion of protein sequences
trypsinize <- function(proteins) {
  #Loading stringr package 
  library(stringr)
  #Splitting each protein sequence after every R (arginine) or K (lysine)
  # (?<=R|K) is a regular expression that splits immediately after R or K
  lapply(proteins, str_split_1, pattern = "(?<=R|K)")
}

#Using the function that counts how many masses in a sample match each protein
count_matching_masses <- function(protein_masses, sample) {
  
  #Counting matches between sample and each protein
  df <- as.data.frame(
    sapply(protein_masses, function(x)
      sum(as.character(sample) %in% as.character(x)))
  )
  
  #Renaming the column
  names(df) <- "peptide_counts"
  
  #Returning the data frame
  return(df)
}
