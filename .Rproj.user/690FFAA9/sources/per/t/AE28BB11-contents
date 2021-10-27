##Masters Thesis Script - Logistic growth B. muricatum
##Rachael Brock, 28/10/21

#SCRIPT 2: script creates the matrices and calculates constants using defined values from 1-Bolbo-Parameters.



        
        
#Calculate weight at ages for biomasses
        #STEP 1: Calculate legnths at each age using *female* life parameters from Taylor et al. (2018)
            # Linf = max size 
            # k is the coefficient in growth function
            # t0 is the hypothetical age at 0mm. 
                
            compute_lt <- function(t, Linf = 1047, k = 0.150, t0 = 0.102) { 
              Linf *(1-exp(-k*(t-t0))) 
            }
            Lt <- matrix(compute_lt(t = 1:29), ncol = 1)     #store the length values at each age in a matrix
        
            plot(Lt) #check
        
        #STEP 2: Calculate weight for each age - using female equation from taylor 2018 -- in kg!!
            #W=a*[TL]^b
            Wt <- matrix(0, nrow=maxage, ncol=1)
            for (t in 1:29) {
              a <- 3.469 * 10^(-6)
              b <- 3.246
              Wt[t,] <- a/1000*Lt[t,]^b
            }
            plot(Wt) #check