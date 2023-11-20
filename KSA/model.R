###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
# https://towardsdatascience.com/parallelization-caveats-in-r-1-the-basics-multiprocessing-and-multithreading-performance-eb584b7e850ehttps://towardsdatascience.com/parallelization-caveats-in-r-1-the-basics-multiprocessing-and-multithreading-performance-eb584b7e850e
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws, ggplot2) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("PreppedData2023_KSA.Rda")
source("../utils/demmod_KSA.R")

#write.csv(int.df%>%filter(iso3=="SAU"), "interventions.csv", row.names = F)
int.df<-read.csv("int_formatted.csV", stringsAsFactors = F)%>%
  rename(`Baseline Coverage` = Baseline.Coverage,
         `Mortality reduction` = Mortality.reduction)%>%
  filter(agel>=25, ageu<=80)

age.code<-int.df%>%select(ageu, agel, Code)

###############################################################################################################################

all.locs<-"Saudi Arabia"
#should be 117 countries
interventions <- unique(int.df$Code)
total         <- length(interventions) #* ength(all.locs)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

all.pin.l     <- all.dalys.l <- all.q30.l <-dadt.all.l<- list(total)

for (is in all.locs){
  k = 1
  dadt.all.l<-all.pin.l     <- all.dalys.l <- all.q30.l <- list(total)
  
  for (inter in interventions){

    projection <- project_pop(0.5, is, inter, 0, 1, 
                              "varying", 2022, 2023,
                              sel.cse, "no", "no", "no", 1)
    
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    dadt.all.l[[k]]   = data.table(projection$DAdt) %>% mutate(Code = inter)
    
    k = k + 1
  }
  
  all.dalys     <- rbindlist(all.dalys.l)
  dadt.all      <- rbindlist(dadt.all.l)
  
  save(all.dalys, dadt.all, file = paste0("output2050_KSA.Rda"))
}

load("output2050_KSA.Rda")


match<-data.frame(Code = c(1.1, 2.1, 3.1, 4.1, 5.1,
                           1.2, 2.2, 3.2, 4.2, 5.2),
                  Scenario = c(rep("Intervention", 5),
                               rep("Ideal", 5)),
                  cause = c("Breast cancer", "Colon and rectum cancer", "Ischemic heart disease",
                            "Ischemic stroke", "Diabetes mellitus type 2",
                            "Breast cancer", "Colon and rectum cancer", "Ischemic heart disease",
                            "Ischemic stroke", "Diabetes mellitus type 2")
)

#TABLES

DA<-dadt.all%>%filter(year_id<=2040)%>%
  left_join(., match)%>%
  group_by(Scenario)%>%
  summarise(Deaths.Averted = sum(Deaths.Avert, na.rm = T))

DALYS<-all.dalys%>%filter(year_id<=2040)%>%
  left_join(., match)%>%
  group_by(Scenario)%>%
  summarise(Dalys.Averted = sum(DALY.ave, na.rm = T))

df<-left_join(DA, DALYS%>%mutate(GDP = 20110.32))%>%
  mutate(Econ.impact = 2.3*GDP*Dalys.Averted)

write.csv(df, "Table1.csv", row.names = F)

#by age
DA<-dadt.all%>%filter(year_id<=2040)%>%
  left_join(., match)%>%
  group_by(Scenario, age_group)%>%
  summarise(Deaths.Averted = sum(Deaths.Avert, na.rm = T))

DALYS<-all.dalys%>%filter(year_id<=2040)%>%
  left_join(., match)%>%
  group_by(Scenario, age_group)%>%
  summarise(Dalys.Averted = sum(DALY.ave, na.rm = T))

df<-left_join(DA, DALYS%>%mutate(GDP = 20110.32))%>%
  mutate(Econ.impact = 2.3*GDP*Dalys.Averted)

write.csv(df, "Table1_by_age.csv", row.names = F)


#PLOTS

DA2<-dadt.all%>%filter(year_id<=2040)%>%
  group_by(Code, year_id)%>%
  summarise(Deaths.Averted = sum(Deaths.Avert, na.rm = T))

ggplot(DA2%>%filter(year_id<=2040),
       aes(x=year_id, y= Deaths.Averted, color=Code, groups=Code))+
  geom_point()



