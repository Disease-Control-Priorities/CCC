rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws, ggplot2) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

DA<-dadt.all%>%filter(year_id<=2040)%>%
  left_join(., match)%>%
  group_by(Scenario)%>%
  summarise(Deaths.Averted = sum(Deaths.Avert, na.rm = T))

DALYS<-all.dalys%>%filter(year_id<=2040)%>%
  group_by(Code)%>%
  summarise(Dalys.Averted = sum(DALY.ave, na.rm = T))




#Plots
DA2<-dadt.all%>%filter(year_id<=2040)%>%
  group_by(Code, year_id)%>%
  summarise(Deaths.Averted = sum(Deaths.Avert, na.rm = T))

ggplot(DA2%>%filter(year_id<=2040),
       aes(x=year_id, y= Deaths.Averted, color=Code, groups=Code))+
  geom_point()



