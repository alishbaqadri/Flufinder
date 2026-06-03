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


