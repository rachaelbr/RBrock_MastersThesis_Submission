##Masters Thesis Script - Logistic growth B. muricatum
##Rachael Brock, 28/10/21

#SCRIPT 4: The script writes a function for the model with 2018 abundance.


runmodel2018 <- function(FishedBiomass = 0, Habitat_loss = 0, min_age_at_fishing=7, aMat =7, CR=10, mr=0.169, R0=400){
  
  
  #calculate age specific mortality using paper Age-specific natural mortality rates in stock assessments: 
  #size-based vs. density-dependent Joseph E. Powers 
  
  #Calculate natural mortality at max size (Minf) 
  #can estimate using average instant nat mortality (rm) by using equation:
  #mortality at time t (Mt) = Minf(1-exp((-k*(a-t0)))^(-b*0.305)
  #Therefore: Minf = max age * mr/[(sum for age 1 to max age)(1-exp(-k*(a-t0)))^(-b*0.305)] 
  
  #STEP 1: first compute the sigma part of the equation (within square brackets above) using a fn
  compute_Minf <- function(a, mr=0.169, k=0.150, t0=0.102, b=3.246, maxage=29){
    (1-exp(-k*(a-t0)))^(-b*0.305)
  }
  
  #STEP 2: then calc Minf by taking the sum
  Minf <- maxage*mr/colSums(matrix(compute_Minf(a=1:29),ncol=1))
  
  #STEP 3: now using the Minf value, calc varied mortality using equation (Mt) = Minf(1-exp((-k*(a-t0)))^(-b*0.305)
  compute_varied_mortality <- function(t, Minf, k=0.150, t0=0.102, b=3.246) { 
    Minf *(1-exp(-k*(t-t0)))^(-b*0.305)
  }
  
  #save as a martix for instant mortality at different ages
  MortalityVaried <- matrix(compute_varied_mortality(t = 1:29,Minf=Minf), ncol = 1)
  
  #check works out to average instant mortality
  colSums(MortalityVaried)/maxage
  
  #Create a survival rates matrix by converting instant nat mortality (MortalityVaried) to discrete
  Survival <- exp(-MortalityVaried)
  
  
  
  #calculate alpha and beta for the Beverton Holt function.  
  #STEP 1: Calculate equilibrium numbers
  #create a storage matrix for equilibrium population distribution
  equilib <- matrix(0,nrow=maxage,ncol=1)
  
  #Calc numbers in each age classes at equilibrium        
  #assuming natural mortality is constant, and no fishing --- 
  #sum of Geometric sequence =a(1-r^n)/(1-r)
  
  for (i in 2:maxage) {
    equilib[1,1] <- R0
    equilib[i,1] <- equilib[i-1,1]*(Survival[[i-1]])
  }
  
  
  #STEP 2: convert to biomass of each stage at equilibrium
  Bequilib <- equilib*Wt
  
  #STEP 3: calculate the total reproductive biomass at equilibrium:
  Sequilib <- sum(Bequilib[aMat:maxage])
  
  #STEP 4: Using known equations from Walters 2006, calculate alpha and beta for the bev holt fn --moved to BolboModel
  alpha <- CR * R0/Sequilib
  beta <- (CR-1)/Sequilib
  
  
  
  ##Create storage arrays for the matrices in the for loops
  ##build a storage array for all years of the abundance (Nt) and Recruitment (Rt)
  Nt <- matrix(0, nrow=maxage, ncol=nYears+1) #abundance at time t in numbers
  Rt <- matrix(0, nrow=nYears+1, ncol=1) #recruits at time t in numbers
  Ft <- matrix(0,nrow=maxage,ncol=nYears+1) #fishing at time t in numbers
  Over_aMat <- matrix(0,nrow=(maxage-aMat+1),ncol=nYears+1) #number of adult fish at time t
  Over_twos <- matrix(0,nrow=(maxage-1),ncol=nYears+1) #number of fish aged 2+ at time t
  
  
  #Create a zeros matrix for the max function to compare with later, so fishing does not become negative
  Zeros <- matrix(0, nrow=28,ncol=1) 
  
  #storage arrays for biomass
  SpawnerBiom <- matrix(0,nrow=1,ncol=nYears+1) #biomass of spawners
  FishingBiom_t <- matrix(0,nrow=1,ncol=nYears+1) #biomass of fish within 'fishable' size
  Harvest_t <- matrix(0,nrow=29,ncol=nYears+1) #harvest at time t in biomass
  Biomass <- matrix(0,nrow=29,ncol=nYears+1) # overall biomass
  Ht <- matrix(0,nrow=29,ncol=nYears+1) #harvest at time t in biomass
  
  #Percentage change in population from t=1 to t       
  PopChangePercent <- matrix(0,nrow=1,ncol=nYears+1) #biomass of spawners
  
  
  #Set t=0 abundance
  Nt[,1] <- N18
  
  
  
  ##Running loops
  
  for(t in 2:(nYears+1)){ #run for the number of years
    Biomass[,t-1] <- Nt[1:maxage,t-1]* Wt[1:maxage,1] #calc biomass of fish in each age class
    SpawnerBiom[1,t-1] <- sum(Biomass[aMat:maxage,t-1]) #calc biomass of spawners
    Rt[t-1,1] <- 0.65*alpha * (1-Habitat_loss) * SpawnerBiom[1,t-1]/(1+beta*SpawnerBiom[1,t-1]) ##calculate the recruits produced -- multiply by 0.65 to get females only!
    Nt[1,t] <- Rt[t-1,1] #insert recruits into Nt row 1 for age 1
    
    FishingBiom_t[1,t-1] <- sum(Biomass[min_age_at_fishing:maxage,t-1]) #calc fishable biomass (where min_age_at_fishing is the minimum capture age)
    
    
    #Calculate how many fish were caught in each age class 
    #to distribute fishing, the biomass of caught fish will be distributed in the same 
    #proportions as biomass of 'fishable' fish
    ##example. 20kg of fish are caught. there are 50kg of fish. 5kg of the fish (10%) are aged 7. So 20kg*10%=2kg of fish aged 7.
    #therefore:
    
    if (FishingBiom_t[1,t-1]>0){ ##set up if loop as can't divide by 0 as would be the case is fishing biomass=0
      Ft[min_age_at_fishing:maxage,t-1] <- (FishedBiomass*(Biomass[min_age_at_fishing:maxage,t-1]
                                                           /FishingBiom_t[1,t-1])/Wt[min_age_at_fishing:maxage,1]) #calcualate the proportion fished from each age class -- divide by wt to convert back to numbers
      
    } else {
      Ft[(min_age_at_fishing-1):28,t-1] <-0 #if there was nothing within the "fishable" biomass to catch, they catch nothing. 
    }
    
    
    #calc Nt - fishing (minusing as hyperstable) 
    Nt[2:29,t] <- pmax(Zeros,((Nt[1:28,t-1]-Ft[1:28,t-1])* Survival[1:28,1]))  #take Nt-fishing, then multiply by survival. If any value is less than 0, take 0 instead.
    #NOTE: Assumes fish over 29 years die 
    
    #calculate amount of fish caught
    for(a in min_age_at_fishing:maxage){
      if (Ft[a-1,t-1] < (Nt[a-1,t-1])) { #if the amount being fished is less than the population at time t
        Ht[a-1,t-1] <- Ft[a-1,t-1]  #then they catch the desired amount
        
      } else { #if the population is less than the target catch
        
        Ht[a-1,t-1] <- (Nt[a-1,t-1]) #they catch all the fish in that age class
      }
      
      ##outputs:
      Over_aMat[,t-1] <- Nt[aMat:maxage,t-1] #provide the number of individuals aged over aMat as an output
      Over_twos[,t-1] <- Nt[2:maxage,t-1] #provide the number of individuals ages over 2 as an output
      
      #calculate the % change in population from starting year to current year
      if (sum(Biomass[1:maxage,t-1])>0){
        PopChangePercent[1,t-1] <- (sum(Biomass[1:maxage,t-1])/sum(Biomass[1:maxage,1])-1)*100 #calc biomass of spawners
      } else {
        PopChangePercent[1,t-1] <- -100
      }
    }
  }
  
  
  
  #calculate totals from variables that were divided by age class
  BiomassTotal <- colSums(Biomass) #calculate total biomass
  MatureFish <- colSums(Over_aMat) #calculate number of sexually mature fish
  DispersedAdults <- colSums(Over_twos)/dispersal  #calculate number of fish aged 2+
  IUCN <- colSums(PopChangePercent) #fixes formatting for export
  
  
  #This defines what outputs to provide from the function:  
  datreturn <- data.frame(FishedBiomass = FishedBiomass,
                          Habitat_loss = Habitat_loss,
                          t = 1:(nYears+1), 
                          BiomassTotal=BiomassTotal,
                          DispersedAdults=DispersedAdults,
                          MatureFish=MatureFish,
                          min_age_at_fishing=min_age_at_fishing,
                          aMat=aMat,
                          R0=R0,
                          CR=CR,
                          mr=mr,
                          IUCN=IUCN
  )
  
  
  #Return a list that includes the datreturn data, the population (Nt), and the harvest
  
  return(list(datreturn = datreturn, Nt = Nt, Harvest_t = Harvest_t))
  
}
