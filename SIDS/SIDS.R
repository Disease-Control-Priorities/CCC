
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#################################################################
#load('SIDS_intersectoral.Rda')   
#load("SIDS_base.Rda")

#all.pin<-bind_rows(all.pin, clinical.pin2%>%mutate(year_id = as.numeric(year_id), Coverage = as.numeric(Coverage)))
#unique(all.pin$Code)
#all.dalys<-bind_rows(all.dalys, clinical.dalys2)
#unique(all.dalys$Code)
#dadt.all<-bind_rows(dadt.all, clinical.dadt2)
#all.q30<-bind_rows(all.q30, clinical.q302)

#save(all.pin, all.dalys, all.q30, dadt.all, file = "SIDS.Rda")

#################################################################

load("SIDS.Rda")
uc<-read.csv("../output/unit_costs/Maldives_adjusted_uc_2020.csv",stringsAsFactors = F)%>%mutate(location_name = "Maldives")%>%
  bind_rows(., read.csv("../output/unit_costs/Timor-Leste_adjusted_uc_2020.csv",stringsAsFactors = F)%>%
              mutate(location_name = "Timor-Leste"))
unique(uc$NCD)
load("../output/goal.q30.covid_0830.Rda")

unique(dadt.all$Code)

cost<-left_join(all.pin%>%
                  mutate(unique_id = paste0("C",Code,"_", sub_id))%>%
                  select(-sub_id), uc)%>%
  mutate(cost = adjusted_uc*pin)%>%
  group_by(Code, group, year_id, Intervention, location_name)%>%
  summarise(cost = sum(cost))%>%
  spread(group, cost)%>%
  rename(Adjusted.cost = Adjusted,
         Baseline.cost = Baseline)%>%
  mutate(Incremental.cost = Adjusted.cost - Baseline.cost)


tab<-dadt.all%>%
  group_by(location_name, Code)%>%
  summarise(Deaths.averted = sum(Deaths.Avert, na.rm=T))%>%
  left_join(., all.dalys%>%group_by(location_name, Code)%>%
              summarise(DALYs.averted = sum(DALY.ave, na.rm=T)))%>%
  mutate(DALYs.averted = ifelse(DALYs.averted<1,0,DALYs.averted))%>%
  left_join(., cost%>%
              group_by(location_name, Code, Intervention)%>%
              summarise(Incremental.cost = sum(Incremental.cost)))%>%
  mutate(ICER = Incremental.cost/DALYs.averted)%>%
  arrange(location_name, ICER)


write.csv(tab, "SIDS_ICERS.csv", row.names = F)

#40q30 plot
locs<-c("Maldives", "Timor-Leste")
load("../output/SIDS_all.Rda")

baseline<-bind_rows(q30.opt%>%mutate(year_id=as.numeric(year_id)), 
                    goal.q30.out%>%select(location_name, Baseline_2015)%>%unique()%>%
                      mutate(year_id=2015)%>%rename(Baseline = Baseline_2015))%>%
  filter(location_name %in% locs)%>%
  rename(x40q30 = Baseline)%>%
  mutate(Scenario = "Baseline")


full<-q30.opt%>%
  select(location_name, Adjusted, year_id)%>%
  mutate(year_id = as.numeric(year_id))%>%
  rename(x40q30 = Adjusted)%>%
  mutate(Scenario = "80% Target coverage")

plot<-bind_rows(baseline, full)%>%
  filter(Scenario=="Baseline" | year_id>=2022)%>%
  mutate(target = ifelse(year_id==2015, x40q30*(2/3), NA))%>%
  mutate(Scenario = factor(Scenario, levels=c("Baseline", "80% Target coverage")))

library(ggplot2)
library(tidyr)

ggplot(plot, aes(x=year_id, y=x40q30, color= Scenario))+
  geom_line(size=1)+
  geom_abline()+
  facet_wrap(~location_name)+
  geom_hline(data = plot, aes(yintercept = target), color="darkgrey", lty=2)+
  theme_bw()+
  ylim(0,30)+
  ylab("40q30 (%)")+
  xlab("Year")+
  theme(axis.text.x=element_text(angle=45))

ggsave("40q30_SIDS.png", height = 4, width = 8, units = "in")

## all interventions table ##

tab2<-dadt.opt%>%
  group_by(location_name)%>%
  summarise(Deaths.averted = sum(Deaths.Avert))%>%
  left_join(.,dalys.opt%>%
              group_by(location_name)%>%
              summarise(DALYs.averted = sum(DALY.ave)))%>%
  left_join(., cost%>%group_by(location_name)%>%
              summarise(Incremental.cost = sum(Incremental.cost)))%>%
  mutate(ICER = Incremental.cost/DALYs.averted)

write.csv(tab2, "all_ints_SIDS.csv")

