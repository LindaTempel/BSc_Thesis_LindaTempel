##### ##### #####     Analysis scrips for behavioral data   ##### ##### #####
#                                 May 2018 
#                                     


# Load nescessary packages
require(dplyr, plyr)

# ----- 1) Read in the data ---------------------

path <- c("Desktop/DPX_Baseline/Behave_Logs/") # <- location of files

paths <- dir(path = path, full.names = T, pattern = "\\.txt$")
names(paths) <- basename(paths)

# ----- 2) Create data frame containing all files and observations

# **  RUN CODE to get dataframe with behavioral data form each individual
# **  The code cuts the dataframe down to a tidy version containing relevant variables 
# **  with the right attributues.

# Read in files
All_RT <- plyr::ldply(paths, read.table, sep =",", dec = ".", header=F)
rm(paths)

# Rename variables
