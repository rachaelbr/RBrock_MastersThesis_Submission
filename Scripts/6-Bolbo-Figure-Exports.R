##Masters Thesis Script - Logistic growth B. muricatum
##Rachael Brock, 28/10/21

#SCRIPT 6: This script produces the figures for the analysis

#Figure 13: Overall trends -- REDO
            #scenario 1: bad h + low fish
            Fishing_habitat_rm <- filter(model_results, aMat == 7, t<100, Habitat_loss==0.8, CR==10,
                                         min_age_at_fishing ==2, FishedBiomass==0) ##restricting which inputs from the spawner biomasses vector, then plot my filtered data!
            
            #scenario 2: bad h + high fish
            Fishing_habitat_rm <- filter(model_results, aMat == 7, t<100, Habitat_loss==0.8, CR==10,
                                         min_age_at_fishing ==2, FishedBiomass==300) ##restricting which inputs from the spawner biomasses vector, then plot my filtered data!
            
            
            #scenario 3: good h + low fish
            Fishing_habitat_rm <- filter(model_results, aMat == 7, t<100, Habitat_loss==0.2, CR==10,
                                         min_age_at_fishing ==2, FishedBiomass==0) ##restricting which inputs from the spawner biomasses vector, then plot my filtered data!
            
            #scenario 4: good h + high fish
            Fishing_habitat_rm <- filter(model_results, aMat == 7, t<100, Habitat_loss==0.2, CR==10,
                                         min_age_at_fishing ==2, FishedBiomass==300) ##restricting which inputs from the spawner biomasses vector, then plot my filtered data!
            
            
            
            #Fishing --> Habitat + rm 
            ggplot(Fishing_habitat_rm) + #plot the data from model_results
              aes(x = t, y = BiomassTotal, color=factor(mr)) + #factor: makes the data discrete (grouped) not continuous--choose what the x and y axis is
              #facet_grid(Habitat_loss ~ FishedBiomass) + #add another element (the facet grid)
              theme_bw()+
              geom_line() + 
              #geom_point(aes(x=18, y=948), colour="blue") + ##add the starting biomass
              #geom_point(aes(x=0, y=3312.63), colour="blue") + #add the biomass at 18 years
              xlab("Years")+
              ylab("Female Biomass per km2 of Nursery Habitat")+
              labs(color='Natural 
Mortality 
Rate') 
            
            
            

#Figure 14: natural mortality
          #Habitat --> habitat + rm ---
          Habitat_CR_rm <- filter(model_results, R0==2000, FishedBiomass==0, CR==10, t ==100, Habitat_loss==0, aMat==7, min_age_at_fishing ==2)
          
          #Habitat --> Habitat + rm 
          ggplot(Habitat_CR_rm) + #plot the data from model_results
            aes(x = mr, y = DispersedAdults*4.7)+ #factor: makes the data discrete (grouped) not continuous--choose what the x and y axis is
            #facet_grid(.~CR) + #add another element (the facet grid)
            theme_bw()+
            geom_line(colour="blue") + 
            #geom_hline(yintercept=631, colour="red") +
            geom_hline(yintercept=2850*0.65, colour="red") +
            
            ##add the starting biomass
            xlab("Instantaneous Natural Mortality Rate (per year)")+
             ylab("Juvenile and Adult Fish (Numbers) per km2 ")
          
          
          

#Figure 15: Age at maturity
          aMat_Fishing_mr_habitat <- filter(model_results, t==100, CR==10, FishedBiomass <355, mr==0.169, min_age_at_fishing==2, Habitat_loss >0, Habitat_loss <0.8)
          
          ggplot(aMat_Fishing_mr_habitat) + 
            aes(x = aMat, y = BiomassTotal, color = factor(FishedBiomass)) + 
            facet_grid(.~Habitat_loss) +
            #geom_hline(yintercept=950, colour="blue") +
            theme_bw()+
            geom_point(size=2.5) +
            xlab("Age at Maturity")+
            ylab("Biomass (kg/km2)") +
            labs(color='Fished Biomass 
     (kg/km2)') 
          
        

#Figure 16:Goodyear compensation ratio
          #Healthy habitat (20% loss)
            CR_Fishing_mr_habitat <- filter(model_results, t==100, Habitat_loss<0.7, Habitat_loss >0.15,aMat==7, mr==0.169, min_age_at_fishing==2)
            ggplot(CR_Fishing_mr_habitat) + 
              aes(x = CR, y = BiomassTotal, color = factor(FishedBiomass)) + 
              theme_bw()+
              facet_grid(.~Habitat_loss) +
              geom_vline(xintercept=15, colour="gray") +
              geom_vline(xintercept=25, colour="gray") +
              geom_vline(xintercept=35, colour="gray") +
              geom_vline(xintercept=45, colour="gray") +
              geom_point(size = 2, position=position_jitter(width=1, height=0)) +
              xlab("Goodyear Compensation Ratio")+
              ylab("Equilibrium Biomass (kg/km2/yr)")+
              labs(color='Fished Biomass 
     (kg/km2)') 
            
           



#Figure 17: fishing + habitat loss possible combos
            min <- read_csv("Data/minagedata_usingtable4.csv")
            
              ggplot(min, aes(Habitat_loss)) + 
              #plot the data from model_results
              geom_ribbon(aes(ymin=min, ymax=mr_0.1), fill="lightblue")+
              theme_bw()+ 
                geom_line(aes(y=mr_0.2), linetype="dashed", color="blue") +  
                geom_line(aes(y=mr_0.1), linetype="dashed", color="blue") +  
                geom_line(aes(y=mr_0.169), linetype="dashed", color="red") +  
              xlab("Proportion of Nursery Habitat Loss")+
              ylab("Fishing Pressure per km2")
            

#Figure 18:[INSERT FIGURE HERE OF MIN CATCH SIZE + LOW FISHING + HEALTHY HABITAT]
              catchsize <- filter(model_results2018, t==100,  CR==10, FishedBiomass <355, mr==0.2, aMat==7)
              
              
              ggplot(catchsize) + 
                aes(x = min_age_at_fishing, y = BiomassTotal, color = factor(FishedBiomass)) + 
                theme_bw()+
                facet_grid(.~Habitat_loss) +
                geom_point(size=1.5) +
                geom_line() +
                xlab("Minimum Age at First Capture")+
                ylab("Biomass (kg/km2)") +
                labs(color='Fished Biomass 
     (kg/km2)') 
              
             
              

#Figure 19: IUCN min catch 7
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
              
#Figure 20: IUCN min catch 11
              ggplot(filter(HabLossANDFratesG, mr==0.2, min_age_at_fishing==11), aes(x = Habitat_loss, y = FishedBiomass, fill=Risk)) +
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
              