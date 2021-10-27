##Masters Thesis Script - Logistic growth B. muricatum
##Rachael Brock, 28/10/21

#SCRIPT 1: Sets default values for parameters

#Load Packages
library(tidyverse)


#load data from Roviana lagoon
size_freq <- as.matrix(read_csv("Data/size_freq_table.csv"))

#set initial abundance per size class. 
#60-70% of population are female, so multiply by 0.65
##Surveyed 72 sites at 500m*20m = 720,000m^2 --> multiply by 100/72 to get km^2.
N0 <- size_freq[,4]*0.65*100/72 #abundance in 2000/2001

N18 <-size_freq[,5]*0.65*100/27 #abundance in 2018

nYears <- 100 #number of years to run 

min_age_at_fishing <- 7 #min age at fishing capture

maxage <- 29 #maximum age

R0 <- 2000 #Maximum observed number of recruits per km2 - calculated using data provided by R. Hamilton from surveys in Kia, Solomon Islands
      
aMat <- 7 #age at sexual maturity

mr <- 0.169 ##instantaneous natural mortality estimate

CR <- 10 #Goodyear compensation ratio estimate

dispersal <- 4.7 #The ratio of adult:juvenile habitat observed in Kia. Using as a baseline to account for adults dispersing into outer reef from juvenile zones

FishedBiomass <-  20365/16*12*0.96/177*0.65 #baseline estimate of fished biomass using Kia data. 20,365kg caught over 16 months (16/12) where 96% were B muri, and 65% are female

save(N0, N18, nYears, min_age_at_fishing, maxage, 
                          aMat, rf, mr, CR, FishedBiomass, file="Data/BMuri_life_parameters.RDA") ##RDA is for multiple objects, RDS for one

