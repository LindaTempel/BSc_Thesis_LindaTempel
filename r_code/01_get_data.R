##### ##### #####     Analysis scripts for behavioral data   ##### ##### #####
#                                 May 2018 
#                                     


# Load helper functions
setwd("n")
source('./r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr')
getPacks(pkgs)
rm(pkgs)

# ----- 1) Read in the data -------------------------------------------

# # Set path before start
path <- c("") # <- location of files

paths <- dir(path = path, full.names = T, pattern = "\\.txt$")
names(paths) <- basename(paths)



# ----- 2) Create data frame containing all files and observations------

# Read in files
Data_behav <- plyr::ldply(paths, read.table, sep =",", dec = ".", header=F)
rm(paths)

# Delete variables

Data_behav<- Data_behav %>% dplyr::select(.id, V2, V4, V6, V8, V10, V12)

# Rename variables

names(Data_behav) <- c('VP', 'Block', 'Trial', 'Deck', 'Value', 'RT', 'Payoff')

# Clean String Variables

Data_behav$VP<- gsub(Data_behav$VP, pattern = "rawdata.txt", replacement = "")

#------3) Create new variables-----------------------------------------

# Split file into blocks 

Data_B1<-dplyr::filter(Data_behav, Block==1)
unique(Data_B1$Block)
Data_B2<-dplyr::filter(Data_behav, Block==2)
unique(Data_B2$Block)
Data_B3<-dplyr::filter(Data_behav, Block==3)
unique(Data_B3$Block)

# Add variable CARD
Data_B1$Deck <- factor(Data_B1$Deck)
Data_B1$Card <- plyr::revalue(Data_B1$Deck, c('1' = 'A', '2' = 'D', '3' = 'C', '4' = 'B'))

Data_B2$Deck <- factor(Data_B2$Deck)
Data_B2$Card <- plyr::revalue(Data_B2$Deck, c('1' = 'B', '2' = 'C', '3' = 'A', '4' = 'D'))

Data_B3$Deck <- factor(Data_B3$Deck)
Data_B3$Card <- plyr::revalue(Data_B3$Deck, c('1' = 'D', '2' = 'A', '3' = 'B', '4' = 'C'))

# ------ Add GAIN & LOSS columns for Block 1 and variable NET_PAYOFF-----

for (i in 1:nrow(Data_B1)) {  
  
  if (Data_B1[i, 8] == 'A' ) {
    
    if (Data_B1[i, 5] == 1) {
      Data_B1[i, 9] <- 100
      Data_B1[i, 10] <- 0
      Data_B1[i, 11] <- 100
    } else if (Data_B1[i, 5] == 2) {
      Data_B1[i,9] <- 100
      Data_B1[i,10] <- -150
      Data_B1[i,11]<- -50
    } else if (Data_B1[i, 5]  == 3) {
      Data_B1[i,9] <- 100
      Data_B1[i,10] <- -200 
      Data_B1[i,11] <- -100
    } else if (Data_B1[i, 5]  == 4) {
      Data_B1[i,9] <- 100
      Data_B1[i,10] <- -250
      Data_B1[i,11] <- -150
    } else if (Data_B1[i, 5]  == 5) {
      Data_B1[i,9] <- 100
      Data_B1[i,10] <- -300
      Data_B1[i,11] <- -200
    } else if (Data_B1[i, 5]  == 6) {
      Data_B1[i,9] <- 100
      Data_B1[i,10] <- -350
      Data_B1[i,11] <- -250
    }
    
  } else if  (Data_B1[i, 8] == 'B' ) {
    
    if (Data_B1[i, 5] == 1) {
      Data_B1[i, 9] <- 100
      Data_B1[i, 10] <- 0
      Data_B1[i, 11] <- 100
    } else if (Data_B1[i, 5] == 2) {
      Data_B1[i,9] <- 100
      Data_B1[i,10] <- -1250
      Data_B1[i,11] <- -1150
    }
    
  } else if (Data_B1[i, 8] == 'C' ) {
    
    if (Data_B1[i, 5] == 1) {
      Data_B1[i, 9] <- 50
      Data_B1[i, 10] <- 0
      Data_B1[i,11] <- 50
    } else if (Data_B1[i, 5] == 2) {
      Data_B1[i,9] <- 50
      Data_B1[i,10] <- -25
      Data_B1[i,11] <- 25
    } else if (Data_B1[i, 5] == 3) {
      Data_B1[i,9] <- 50
      Data_B1[i,10] <- -50
      Data_B1[i,11] <- 0
    } else if (Data_B1[i, 5] == 4) {
      Data_B1[i,9] <- 50
      Data_B1[i,10] <- -75
      Data_B1[i,11] <- -25
    }
    
 } else if (Data_B1[i, 8] == 'D' ) {
    
    if (Data_B1[i, 5] == 1) {
      Data_B1[i, 9] <- 50
      Data_B1[i, 10] <- 0
      Data_B1[i, 11] <- 50
    } else if (Data_B1[i, 5] == 2) {
      Data_B1[i,9] <- 50
      Data_B1[i,10] <- -250
      Data_B1[i,11] <- -200
    }
    
  }
  
}

rm(i)
names(Data_B1)[9:11] <- c('gain','loss', 'net_payoff')

# ------ Add GAIN & LOSS columns for Block 2 and variable NET_PAYOFF------

for (i in 1:nrow(Data_B2)) {  
  
  if (Data_B2[i, 8] == 'A' ) {
    
    if (Data_B2[i, 5] == 1) {
      Data_B2[i, 9] <- 200
      Data_B2[i, 10] <- -100
      Data_B2[i, 11] <- 100
    } else if (Data_B2[i, 5] == 2) {
      Data_B2[i,9] <- 50
      Data_B2[i,10] <- -100
      Data_B2[i,11]<- -50
    } else if (Data_B2[i, 5]  == 3) {
      Data_B2[i,9] <- 50
      Data_B2[i,10] <- -150
      Data_B2[i,11] <- -100
    } else if (Data_B2[i, 5]  == 4) {
      Data_B2[i,9] <- 50
      Data_B2[i,10] <- -200
      Data_B2[i,11] <- -150
    } else if (Data_B2[i, 5]  == 5) {
      Data_B2[i,9] <- 150
      Data_B2[i,10] <- -350
      Data_B2[i,11] <- -200
    } else if (Data_B2[i, 5]  == 6) {
      Data_B2[i,9] <- 150
      Data_B2[i,10] <- -400
      Data_B2[i,11] <- -250
    }
    
  } else if  (Data_B2[i, 8] == 'B' ) {
    
    if (Data_B2[i, 5] == 1) {
      Data_B2[i, 9] <- 200
      Data_B2[i, 10] <- -100
      Data_B2[i, 11] <- 100
    } else if (Data_B2[i, 5] == 2) {
      Data_B2[i,9] <- 200
      Data_B2[i,10] <- -1350
      Data_B2[i,11] <- -1150
    }
    
  } else if (Data_B2[i, 8] == 'C' ) {
    
    if (Data_B2[i, 5] == 1) {
      Data_B2[i, 9] <- 100
      Data_B2[i, 10] <- -50
      Data_B2[i,11] <- 50
    } else if (Data_B2[i, 5] == 2) {
      Data_B2[i,9] <- 100
      Data_B2[i,10] <- -75
      Data_B2[i,11] <- 25
    } else if (Data_B2[i, 5] == 3) {
      Data_B2[i,9] <- 100
      Data_B2[i,10] <- -100
      Data_B2[i,11] <- 0
    } else if (Data_B2[i, 5] == 4) {
      Data_B2[i,9] <- 100
      Data_B2[i,10] <- -125
      Data_B2[i,11] <- -25
    }
    
  } else if (Data_B2[i, 8] == 'D' ) {
    
    if (Data_B2[i, 5] == 1) {
      Data_B2[i, 9] <- 100
      Data_B2[i, 10] <- -50
      Data_B2[i, 11] <- 50
    } else if (Data_B2[i, 5] == 2) {
      Data_B2[i,9] <- 100
      Data_B2[i,10] <- -300
      Data_B2[i,11] <- -200
    }
    
  }
  
}

rm(i)
names(Data_B2)[9:11] <- c('gain','loss', 'net_payoff')

# ------ Add GAIN & LOSS columns for Block 3 and variable NET_PAYOFF------

for (i in 1:nrow(Data_B3)) {  
  
  if (Data_B3[i, 8] == 'A' ) {
    
    if (Data_B3[i, 5] == 1) {
      Data_B3[i, 9] <- 125
      Data_B3[i, 10] <- -25
      Data_B3[i, 11] <- 100
    } else if (Data_B3[i, 5] == 2) {
      Data_B3[i,9] <- 75
      Data_B3[i,10] <- -125
      Data_B3[i,11]<- -50
    } else if (Data_B3[i, 5]  == 3) {
      Data_B3[i,9] <- 75
      Data_B3[i,10] <- -175 
      Data_B3[i,11] <- -100
    } else if (Data_B3[i, 5]  == 4) {
      Data_B3[i,9] <- 75
      Data_B3[i,10] <- -225
      Data_B3[i,11] <- -150
    } else if (Data_B3[i, 5]  == 5) {
      Data_B3[i,9] <- 175
      Data_B3[i,10] <- -375
      Data_B3[i,11] <- -200
    } else if (Data_B3[i, 5]  == 6) {
      Data_B3[i,9] <- 175
      Data_B3[i,10] <- -425
      Data_B3[i,11] <- -250
    }
    
  } else if  (Data_B3[i, 8] == 'B' ) {
    
    if (Data_B3[i, 5] == 1) {
      Data_B3[i, 9] <- 125
      Data_B3[i, 10] <- 25
      Data_B3[i, 11] <- 100
    } else if (Data_B3[i, 5] == 2) {
      Data_B3[i,9] <- 125
      Data_B3[i,10] <- -1275
      Data_B3[i,11] <- -1150
    }
    
  } else if (Data_B3[i, 8] == 'C' ) {
    
    if (Data_B3[i, 5] == 1) {
      Data_B3[i, 9] <- 200
      Data_B3[i, 10] <- -150
      Data_B3[i,11] <- 50
    } else if (Data_B3[i, 5] == 2) {
      Data_B3[i,9] <- 200
      Data_B3[i,10] <- -175
      Data_B3[i,11] <- 25
    } else if (Data_B3[i, 5] == 3) {
      Data_B3[i,9] <- 200
      Data_B3[i,10] <- -200
      Data_B3[i,11] <- 0
    } else if (Data_B3[i, 5] == 4) {
      Data_B3[i,9] <- 200
      Data_B3[i,10] <- -225
      Data_B3[i,11] <- -25
    }
    
  } else if (Data_B3[i, 8] == 'D' ) {
    
    if (Data_B3[i, 5] == 1) {
      Data_B3[i, 9] <- 200
      Data_B3[i, 10] <- -150
      Data_B3[i, 11] <- 50
    } else if (Data_B3[i, 5] == 2) {
      Data_B3[i,9] <- 200
      Data_B3[i,10] <- -400
      Data_B3[i,11] <- -200
    }
    
  }
  
}

rm(i)
names(Data_B3)[9:11] <- c('gain','loss', 'net_payoff')


#-----4) Create new data frame containing all blocks-------------------

Data_card<-rbind(Data_B1, Data_B2, Data_B3)
rm(Data_B1, Data_B2, Data_B3)
print(levels(Data_card$Card)) 
Data_card$Card = factor(Data_card$Card,levels(Data_card$Card)[c(2,5,4,3,1)])
