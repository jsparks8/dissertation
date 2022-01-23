# This script will create the folder structure for my simulation
# The folder naming convention is as follows:
# [Cell #] + [# Response Categories] + [# Items] + [Sample Size] + [Reps]
# Each folder will contain the data and results for all each replication:

# Base folder:
# # Dataset [1-10]

# # GGUM/GGUM-RT directory:
# # # Saved JAGS environment
# # # Comparison plot
# # # Trace plot
# # # Autocorrelation plot
# # # Gelman Rubin index
# # # Time to estimate
# # # EAP estimates

# Specify working directory -----
starting.directory <- getwd()

cat <- c(6, 2)
items <- c(10, 30)
n <- c(500, 2000)
rep <- c(1:10)
cell <- 1

for (j in 1:length(n)) {
  
  for (k in 1:length(cat)) {
    for (i in 1:length(items)) {
      
      for (r in 1:length(rep)) {
        folder <- paste0("CELL", cell, "_CAT", cat[k], "_I", items[i], "_N", n[j], "_REP", rep[r])
        output_dir <- file.path(starting.directory, "Simulation", folder, fsep = "/")
        
        # Create output directory if it does not exist ----
        if (!dir.exists(output_dir)) {
          dir.create(output_dir)
        } else {
          print(paste0("Dir ", folder, " already exists!"))
        }
      }
      
      cell <- cell+1
    }
  }
}
