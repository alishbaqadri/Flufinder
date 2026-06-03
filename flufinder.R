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

aa <- list(A=list(c("L","V","K"),
                  c("L","H","H","I","I","F","E","S","M","L","K"), c("D","M","Q","R")),
           B=list(c("A","D","E","F","Q","G","S","M","Q","K"),
                  c("I","E","A","C","W","Q","S","Y","D","V","Q","F")),
           C=list(c("M","I","N","E","P","F","S","W","R"),
                  c("L","E","F","H","L","S","E","R"), c("Y","D","E","I","M")))