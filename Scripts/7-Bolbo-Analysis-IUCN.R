##Masters Thesis Script - Logistic growth B. muricatum
##Rachael Brock, 28/10/21

#SCRIPT 7: This script calculates the risk of extinction under different scenarios according to the IUCN redlist criteria




##PLAYING WITH RUNNING THE MODEL FOR 2500 COMBOS OF FISHING RATE + HABITAT LOSS        
#set up input for sensitivity - things  vary across each run
    data_in_IUCN <- expand.grid(frates = seq(0, 350, length.out = 40),
                                      hab_loss = seq(0, 1, length.out = 40),
                                      min_age_at_fishing=c(7,11),
                                      mr = c(0.1, 0.169, 0.2),
                                      R0 = c(2000))
    
    
    irows <- 1:nrow(data_in_IUCN)
    

    #run the model for each row of data_in (ie different perams)
    ModelOutput_long <- lapply(irows, function(i) {runmodel2018(data_in_IUCN$frates[i],
                                                                data_in_IUCN$hab_loss[i], 
                                                                data_in_IUCN$min_age[i],
                                                                7,
                                                                10,
                                                                data_in_IUCN$mr[i],
                                                                data_in_IUCN$R0[i])})  
#CHECK HOW MANY PARAMS --> 4 hab loss by 6 frates =24
length(ModelOutput_long)

#Extract just data from spawner biomasses by taking the first item in ModelOutput
HabLossANDFrates <- lapply(ModelOutput_long, function(x) x[[1]])


#set the generation time
G<-8

# Bind list object together into a dataframe
HabLossANDFrates <- bind_rows(HabLossANDFrates)
HabLossANDFratesG <- filter(HabLossANDFrates, t==(3*G))

HabLossANDFratesG$Risk <- cut(HabLossANDFratesG$IUCN, 
                             breaks=c(-100000000000,-80,-70,-30,0, 100,100000000000))




 ggplot(filter(HabLossANDFratesG, mr==0.1, min_age_at_fishing==7), aes(x = Habitat_loss, y = FishedBiomass, fill=Risk)) +
  geom_tile() +
  theme_classic()+
   geom_point(aes(x=1, y=150), colour="black")+
   geom_point(aes(x=0.9, y=200), colour="black")+
   geom_point(aes(x=0.8, y=250), colour="black")+
   geom_point(aes(x=0.7, y=275), colour="black")+
   geom_point(aes(x=0.6, y=300), colour="black")+
  geom_point(aes(x=0.5, y=312.5), colour="black")+
  geom_point(aes(x=0.4, y=325), colour="black")+
  geom_point(aes(x=0.3, y=350), colour="black")+
   # geom_point(aes(x=0.2, y=362.5), colour="black")+
   # geom_point(aes(x=0.1, y=375), colour="black")+
   # geom_point(aes(x=0, y=400), colour="black")+
   
  scale_fill_manual(breaks = levels(HabLossANDFrates$groups),
                    values = alpha(c("darkred", "red", "orange", "yellow", "blue", "darkblue"),1))+
   xlab("Habitat Loss")+
   ylab("Fished Biomass (kg/km2/yr)")
 
          # subtitle="Dark Red indicates Critically Endangered, Red indicates Endangered, Orange is vulnerable,
          # Yellow indicates decline but not at risk, blue indicates growth)")


 
 ggplot(filter(HabLossANDFratesG, mr==0.169, min_age_at_fishing==7), aes(x = Habitat_loss, y = FishedBiomass, fill=Risk)) +
   geom_tile() +
   theme_classic()+
   geom_point(aes(x=1, y=50), colour="black")+
   geom_point(aes(x=0.9, y=100), colour="black")+
   geom_point(aes(x=0.8, y=150), colour="black")+
   geom_point(aes(x=0.7, y=175), colour="black")+
   geom_point(aes(x=0.6, y=200), colour="black")+
   geom_point(aes(x=0.5, y=225), colour="black")+
   geom_point(aes(x=0.4, y=250), colour="black")+
   geom_point(aes(x=0.3, y=275), colour="black")+
   geom_point(aes(x=0.2, y=300), colour="black")+
   geom_point(aes(x=0.1, y=312.5), colour="black")+
   geom_point(aes(x=0, y=325), colour="black")+
   
   scale_fill_manual(breaks = levels(HabLossANDFrates$groups),
                     values = alpha(c("darkred", "red", "orange", "yellow", "blue", "darkblue"),1))+
   xlab("Habitat Loss")+
   ylab("Fished Biomass (kg/km2/yr)")
 
 
 
 
 
 
 ggplot(filter(HabLossANDFratesG, mr==0.2, min_age_at_fishing==7), aes(x = Habitat_loss, y = FishedBiomass, fill=Risk)) +
  geom_tile() +
  theme_classic()+
   geom_point(aes(x=1, y=0), colour="black")+
   geom_point(aes(x=0.9, y=50), colour="black")+
   geom_point(aes(x=0.8, y=100), colour="black")+
   geom_point(aes(x=0.7, y=125), colour="black")+
   geom_point(aes(x=0.6, y=150), colour="black")+
   geom_point(aes(x=0.5, y=175), colour="black")+
   geom_point(aes(x=0.4, y=200), colour="black")+
   geom_point(aes(x=0.3, y=225), colour="black")+
   geom_point(aes(x=0.2, y=250), colour="black")+
   geom_point(aes(x=0.1, y=275), colour="black")+
   geom_point(aes(x=0, y=300), colour="black")+
  scale_fill_manual(breaks = levels(HabLossANDFrates$groups),
                    values = alpha(c("darkred", "red", "orange", "yellow", "blue", "darkblue"),1))+
   xlab("Habitat Loss")+
   ylab("Fished Biomass (kg/km2/yr)")
 
 
          # subtitle="Dark Red indicates Critically Endangered, Red indicates Endangered, Orange is vulnerable,
          # Yellow indicates decline but not at risk, blue indicates growth)")
# 
# mr0.25 <- ggplot(filter(HabLossANDFratesG, mr==0.169,min_age_at_fishing==7), aes(x = Habitat_loss, y = FishedBiomass, fill=Risk)) +
#   geom_tile() +
#   geom_point(aes(x=0.1, y=100), colour="black")+
#   geom_point(aes(x=0.2, y=150), colour="black")+
#   geom_point(aes(x=0.3, y=175), colour="black")+
#   geom_point(aes(x=0.4, y=225), colour="black")+
#   geom_point(aes(x=0.5, y=225), colour="black")+
#   geom_point(aes(x=0.6, y=275), colour="black")+
#   geom_point(aes(x=0.7, y=275), colour="black")+
#   geom_point(aes(x=0.8, y=300), colour="black")+
#   geom_point(aes(x=0.9, y=325), colour="black")+
#   geom_point(aes(x=1, y=325), colour="black")+  theme_classic()+
#   scale_fill_manual(breaks = levels(HabLossANDFrates$groups),
#                     values = alpha(c("darkred", "red", "orange", "yellow", "blue", "darkblue"),1))+
#   xlab("Habitat Impact")+
#   ylab("Fished Biomass")+
#   ggtitle("Instantaneous Natural Mortality 
#           of 0.169")
          # subtitle="Dark Red indicates Critically Endangered, Red indicates Endangered, Orange is vulnerable,
          # Yellow indicates decline but not at risk, blue indicates growth)")

 

gridExtra::grid.arrange(mr0.15, mr0.2, ncol=2)
G












