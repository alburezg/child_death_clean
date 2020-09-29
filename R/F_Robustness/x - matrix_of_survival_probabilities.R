
print("Running script: 8 - matrix_of_survival_probabilities.R")

# The function creates a matrix of the survival probabilities of children. 
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
  files2 <- paste0("../../Data/derived/", files)[1]
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



# 2. Run for both sexes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# xs are allowed reproductive ages
# This must be the same as the age groups in the asfr data
cos <- c(1950:2100) # cohorts
xs <- c(15:49) # reproductive age
mas<-c(15:100) # mother ages

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

print("8 - matrix_survival_probabilities: success!")
