
print("Running script: 8 - matrix_of_survival_probabilities.R")

# The function creates is a matrix of the survival probabilities of children. 
# There is one matrix for each birth cohort of mothers showing the probability
# that a child will reach a certain age.

# Function based on Sarah's original script.
# Note that this function has to be run with life table for both sexes combined!!
# LTC is a df of country cohort life tables

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created in this script can be loaded as:
if(F) {
  sex <- "F"
  
  # Preferred:
  # Using a normal PC not in the shared U-drive
  # NOTE: this only works outsdie Hydra as it referes to the C Drive
  
  (files <- list.files(path = "../../Data/derived", pattern = "^lx.kids.arr_"))
  files2 <- paste0("../../Data/derived/", files)[52:55]
  arr_l <- lapply(files2, function(f) {
    readRDS(f)
  })  
  
}
# ~~~~~~~~~~~~~~~~~~~~~


# 1. Define function ----
# ~~~~~~~~~~~~~~~~~~~

# Note:
# THIS SCRIPT SHOULD BE RUN ON A HPC USING MULTIPLE CORES AS IT IS TOO LENGHTY
# FOR A PC

matrix_of_survival_probabilities <- function(LTC, run_checks = F, cos, xs, mas, numCores) {
  
  # 1. Get matrix of survival probabilities of kids ----
  
  LTC_l <-  split(LTC, LTC$Country)
  
  # If done together, this returns a list sized 1.04 GB, which is inconvenient
  # This functino splits the data and saves it into a determined number of chunks
  # It return no object but saves no_chunks number of files that can later be loaded into R
  
  # Takes 15 MINUTES USING 25 CORES on a HPC.
  
  closeAllConnections()
  
  print("Getting matrix of survival probs and saving as separate files...")
  
  survival_probs_parallel(
    l = LTC_l
    , xs
    , mas
    , cos
    , numCores = numCores
  )
  
  
  # 3. Explore ----
  
  if(run_checks) {
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # LTC can be read from the file created in the previous script as 
    sex <- "F"
    
    # Preferred:
    # Using a normal PC not in the shared U-drive
    # NOTE: this only works outsdie Hydra as it referes to the C Drive
    
    (files <- list.files(path = "../../Data/derived", pattern = "^lx.kids.arr_"))
    files2 <- paste0("../../Data/derived/", files)[52:55]
    arr_l <- lapply(files2, function(f) {
      readRDS(f)
    })  
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # NOT RUN
    # Check created arrays
    # a <- arr_l[[2]]
    # str(a)
    # dimnames(a)
    # a[,,50] %>% View
    
    # Check if all countries are in estimtes
    # Check!
    # Something weird is going on here as LTC has both a "Europe" and a "EUROPE" 'country'
    # It is not an issue as the file is simply overwritten but it is annoying
    
    # Check that the two are equivalent
    # df <- LTC %>% 
      # filter(Country %in% c("EUROPE", "Europe")) %>% 
      # filter(Cohort == 1951) %>% 
      # filter(Age == 0)
    
    # Get files
    f <- gsub("lx.kids.arr_", "", files)
    f <- gsub(".RDS", "", f)
    
    c <- unique(LTC$Country)
    
    c[!c %in% f]
    
  }
}

# 2. Run for both sexes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xs are allowed reproductive ages
# This must be the same as the age groups in the asfr data
cos <- c(1950:2099) # cohorts
xs <- c(13:54) # reproductive age
mas<-c(13:100) # mother ages

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print('8. Getting matrix of survival probabilities for both sexes.')
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

numCores <- ifelse(detectCores() > 8, 25, detectCores())

matrix_of_survival_probabilities(
  LTC = LTCB
  , run_checks = F
  , cos = cos
  , xs = xs
  , mas = mas
  , numCores = numCores
  )

print("8 - matrix_survival_probabilities: sucess!")
