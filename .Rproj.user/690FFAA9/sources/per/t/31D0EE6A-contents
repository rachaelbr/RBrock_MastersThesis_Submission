##Masters Thesis Script - Logistic growth B. muricatum
##Rachael Brock, 28/10/21

#SCRIPT 5: The script produces the data to analyse

library(ggplot2)
library(tidyverse)

#set up input for sensitivity - things  vary across each run
  data_in <- expand.grid(frates = c(seq(0, 500, length.out = 11),325),
                         hab_loss = seq(0, 1, length.out = 11), 
                         min_age = c(2,7,8,9,10,11,12),
                         age_at_maturity = c(6,7,8,9,10),
                         CR = c(10,20,30,40,50),
                         mr = c(seq(0.1, 0.2, length.out = 6),0.169),
                         R0 = c(2000))


  irows <- 1:nrow(data_in)

  #2000 start point
          #run the model for combinations of each variable, defined in data_in. Output will be 
            #dat.return (from section 3) in point 1, Nt in point 2, and harvest in point 3.
          ModelOutput_2001 <- lapply(irows, function(i) {runmodel(data_in$frates[i],
                                                     data_in$hab_loss[i], 
                                                     data_in$min_age[i],
                                                     data_in$age_at_maturity[i],
                                                     data_in$CR[i],
                                                     data_in$mr[i],
                                                     data_in$R0[i])})
          
          #Check
          length(ModelOutput_2001)
          
          #Extract just data from dat.return by taking the first item in ModelOutput_2001
          model_results <- lapply(ModelOutput_2001, function(x) x[[1]])
          
          # Bind list object together into a dataframe
          model_results <- bind_rows(model_results)
          
          model_results <- filter(model_results, t<=100)
          
        
        
  ##2018 start point
          ModelOutput_2018 <- lapply(irows, function(i) {runmodel2018(data_in$frates[i],
                                                                      data_in$hab_loss[i], 
                                                                      data_in$min_age[i],
                                                                      data_in$age_at_maturity[i],
                                                                      data_in$CR[i],
                                                                      data_in$mr[i],
                                                                      data_in$R0[i])})
          
          #Check
          length(ModelOutput_2018)
          
          #Extract just data from dat.return by taking the first item in ModelOutput_2001
          model_results2018 <- lapply(ModelOutput_2018, function(x) x[[1]])
          
          # Bind list object together into a dataframe
          model_results2018 <- bind_rows(model_results2018)
          
          model_results2018 <- filter(model_results2018, t<=100)
          
          
          