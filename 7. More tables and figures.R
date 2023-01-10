rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

WB<-read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  select(iso3, wb2021, location_gbd)%>%rename(location_name = location_gbd)
lics<-WB%>%filter(wb2021=="LIC")%>%pull(location_name)

codes<-read.csv("Figures/clinical_full_2023_2030.csv", stringsAsFactors = F)%>%
  filter(BCR>=15, Code>5)%>%
  select(WB_Region, Code, BCR)

### Figure 2 ###
#Add intersectoral impacts#
load("for_parallel_processing/output2023_target_intersectoral.Rda")

int.dadt<-dadt.all
int.dalys<-all.dalys

best.int<-all.q30%>%mutate(keep = ifelse(location_name %in% lics & Code>=5.5, 0 , 1))%>%
  filter(keep==1)%>%
  group_by(location_name, year_id)%>%
  summarise(int.eff = sum(q30.ave))

int.q30<-all.q30%>%
  group_by(location_name, year_id)%>%
  summarise(int.eff = sum(q30.ave))

load("output/results_target_all.Rda")
all.q30<-q30.opt
all.dadt<-dadt.all.opt
all.dalys<-dalys.opt

load("output/results_target_best.Rda")
best.q30<-q30.opt
best.dadt<-dadt.all.opt
best.dalys<-dalys.opt

best.q30<-left_join(best.q30, best.int)%>%
  mutate(q30.ave = q30.ave + int.eff,
         Adjusted = Baseline - q30.ave)%>%
  select(-int.eff)

all.q30<-left_join(all.q30, int.q30)%>%
  mutate(q30.ave = q30.ave + int.eff,
         Adjusted = Baseline - q30.ave)%>%
  select(-int.eff)

#pull targets#
load("output/goal.q30.covid_0830.Rda")

locs<-unique(all.q30$location_name)

pop<-read.csv("new_inputs/wb_pop.csv", stringsAsFactors = F, skip=4)%>% 
  pivot_longer(X1960:X2021) %>% 
  group_by(Country.Code) %>% 
  fill(value, .direction = "down") %>% 
  pivot_wider(Country.Code)%>%
  select(Country.Code, X2021)%>%
  rename(iso3 = Country.Code, pop = X2021)%>%
  left_join(., WB)

#Figure 2
#The second figure will be a plot of 40q30-NCD4 over time at the global level, with the business as usual scenario as well as 
## (1) a scenario modeling the impact of the full list of interventions (table 2 above) and 
## (2) a scenario modeling the impact of the list of most cost-beneficial interventions (table 3 above)

baseline<-bind_rows(q30.opt%>%select(location_name, Baseline, year_id)%>%
                      mutate(year_id = as.numeric(year_id)), 
                    goal.q30.out%>%select(location_name, Baseline_2015)%>%unique()%>%
                      mutate(year_id=2015)%>%rename(Baseline = Baseline_2015))%>%
  filter(location_name %in% locs)%>%
  rename(x40q30 = Baseline)%>%
  mutate(Scenario = "Baseline")


full<-all.q30%>%select(location_name, Adjusted, year_id)%>%
  filter(location_name%in%locs)%>%
  mutate(year_id = as.numeric(year_id))%>%
  rename(x40q30 = Adjusted)%>%
  mutate(Scenario = "All interventions and policies")

best<-best.q30%>%select(location_name, Adjusted, year_id)%>%
  filter(location_name%in%locs)%>%
  mutate(year_id = as.numeric(year_id))%>%
  rename(x40q30 = Adjusted)%>%
  mutate(Scenario = "Best investment package (BCR>15)")
  

plot<-bind_rows(baseline, full, best)%>%
  left_join(.,WB)%>%
  left_join(.,pop)%>%
  group_by(year_id, Scenario)%>%
  summarise(x40q30 = weighted.mean(x40q30, pop))%>%
  mutate(Scenario = factor(Scenario, levels= c("Baseline",
                                               "Best investment package (BCR>15)",
                                               "All interventions and policies")))%>%
  filter(Scenario=="Baseline" | year_id>=2022)

plot$x40q30[plot$year_id==2015]*(2/3)

library(ggplot2)
library(tidyr)

plot2<-bind_rows(baseline, full, best)%>%
  left_join(.,WB)%>%
  left_join(.,pop)%>%
  group_by(year_id, Scenario, wb2021)%>%
  summarise(x40q30 = weighted.mean(x40q30, pop))%>%
  filter(wb2021!="UMIC")%>%
  bind_rows(., plot%>%mutate(wb2021="LIC+LMC"))%>%
  mutate(wb2021 = ifelse(wb2021=="LMIC", "LMC", wb2021))%>%
  mutate(wb2021 = factor(wb2021, levels=c("LIC", "LMC", "LIC+LMC")))%>%
  mutate(target = ifelse(year_id==2015, x40q30*(2/3), NA))%>%
  group_by(wb2021)%>%
  fill(target)%>%
  mutate(Scenario = factor(Scenario, levels= c("Baseline",
                                               "Best investment package (BCR>15)",
                                               "All interventions and policies")))%>%
  filter(Scenario=="Baseline" | year_id>=2022)

ggplot(plot2, aes(x=year_id, y=x40q30, color= Scenario))+
  geom_line(size=1)+
  geom_abline()+
  facet_wrap(~wb2021)+
  geom_hline(data = plot2, aes(yintercept = target), color="darkgrey", lty=2)+
  theme_bw()+
  ylim(0,30)+
  ylab("40q30 (%)")+
  xlab("Year")+
  theme(axis.text.x=element_text(angle=45))
  

ggsave("Figures/Figure2.png", height = 4, width = 8, units = "in")


########################
##TABLE 2/3##
########################
###
#Total pop 2022
pop<-read.csv("ccc_pop.csv", stringsAsFactors = F)%>%
  gather(year, pop, -region)%>%
  mutate(year = as.numeric(gsub("X", "", year)),
         region = ifelse(region=="LMC", "LMIC", region),
         pop = pop*1e6)%>%
  filter(region%in%c("LIC", "LMIC"))

tab<-pop%>%filter(year==2022)
add<-tab%>%group_by(year)%>%summarise(pop = sum(pop))%>%mutate(region = "LIC+LMIC")

tab<-bind_rows(tab, add)%>%
  select(-year)%>%
  mutate(pop = pop/1e6)%>%
  rename(pop_2022 = pop)

#Spending in 2022
cost.df<-read.csv("output/cost_outputs.csv", stringsAsFactors = F)

add<-cost.df%>%filter(year_id==2022, wb2021!="LIC/LMC")%>%
  group_by(wb2021)%>%
  summarise(base.cost =  sum(Baseline.cost, na.rm=T)/1e9)

add<-add%>%bind_rows(.,add%>%summarise(base.cost = sum(base.cost))%>%mutate(wb2021="LIC+LMIC"))%>%
  rename(region = wb2021)

tab<-left_join(tab, add)%>%arrange(pop_2022)

#NCD deaths 2022 (n=4)
load("output/results_q30.Rda")
sum(ncds.opt$V4)/1e6
ded<-left_join(ncds.opt%>%rename(location_name = location), WB)%>%group_by(wb2021)%>%summarise(deaths = sum(V4)/1e6)
add<-ded%>%bind_rows(., ded%>%summarise(deaths = sum(deaths))%>%mutate(wb2021="LIC+LMIC"))%>%
  rename(region = wb2021,
         deaths_2022 = deaths)

tab<-left_join(tab, add)%>%arrange(pop_2022)


#NCD deaths 2030 (n=12)
sum(ncds.opt$V12)/1e6
ded<-left_join(ncds.opt%>%rename(location_name = location), WB)%>%group_by(wb2021)%>%summarise(deaths = sum(V12)/1e6)
add<-ded%>%bind_rows(., ded%>%summarise(deaths = sum(deaths))%>%mutate(wb2021="LIC+LMIC"))%>%
  rename(region = wb2021, 
         deaths_2030 = deaths)

tab<-left_join(tab, add)%>%arrange(pop_2022)

write.csv(tab, "Figures/base_table.csv", row.names = F)

#################
##best investment##
#################

codes<-read.csv("Figures/clinical_full_2023_2030.csv", stringsAsFactors = F)%>%
  filter(BCR>=15, WB_Region!="LIC/LMC")%>%
  select(WB_Region, Code)%>%
  mutate(keep=1,
         WB_Region = ifelse(WB_Region=="LMC", "LMIC", "LIC"))%>%
  rename(wb2021 = WB_Region)

tab<-left_join(cost.df%>%filter(wb2021!="LIC/LMC"), codes)%>%filter(keep== 1, year_id>2022)%>%
  group_by(wb2021)%>%
  summarise(inc.cost = sum(Incremental.cost, na.rm=T)/1e9)

add<-tab%>%summarise(inc.cost = sum(inc.cost))%>%mutate(wb2021 = "LIC+LMIC")

tab<-bind_rows(tab, add)

#annual pc cost
pop2030<-read.csv("ccc_pop.csv", stringsAsFactors = F)%>%
  gather(year, pop, -region)%>%
  mutate(year = as.numeric(gsub("X", "", year)),
         region = ifelse(region=="LMC", "LMIC", region),
         pop = pop*1e6)%>%
  filter(region%in%c("LIC", "LMIC"), year==2030)%>%
  select(-year)%>%
  rename(wb2021 = region)

pop2030<-bind_rows(pop2030, pop2030%>%summarise(pop = sum(pop))%>%mutate(wb2021="LIC+LMIC"))

add<-left_join(cost.df%>%filter(wb2021!="LIC/LMC"), codes)%>%filter(keep== 1, year_id==2030)%>%
  group_by(wb2021)%>%
  summarise(total.cost = sum(Adjusted.cost, na.rm=T))

add<-bind_rows(add,add%>%summarise(total.cost = sum(total.cost))%>%mutate(wb2021 = "LIC+LMIC"))%>%
  left_join(., pop2030)%>%
  mutate(cost.pc = total.cost/pop)%>%
  select(wb2021, cost.pc)
  
tab<-left_join(tab, add)

# % reached #
q30.best<-left_join(best.q30, goal.q30.out%>%select(location_name, Baseline_2015, Target_40q30)%>%unique())%>%
  mutate(reached = ifelse(Adjusted - Target_40q30<=0,1,0))%>%
  filter(year_id==2030)%>%
  left_join(., WB)%>%
  mutate(all = 1)%>%
  group_by(wb2021)%>%
  summarise(prop_reached = sum(reached, na.rm=T)/sum(all, na.rm=T))

best.lmic.q30<-left_join(best.q30, goal.q30.out%>%select(location_name, Baseline_2015, Target_40q30)%>%unique())%>%
  mutate(reached = ifelse(Adjusted - Target_40q30<=0,1,0))%>%
  filter(year_id==2030)%>%
  left_join(., WB)%>%
  mutate(all = 1)%>%
  summarise(prop_reached = sum(reached, na.rm=T)/sum(all, na.rm=T))%>%
  mutate(wb2021 = "LIC+LMIC")

add<-bind_rows(q30.best, best.lmic.q30)%>%
  mutate(prop_reached = paste0(prop_reached*100, "%"))

tab<-left_join(tab, add)

#deaths averted
dadt<-int.dadt%>%mutate(keep = ifelse(location_name %in% lics & Code>=5.5,0,1))%>%
  filter(keep==1)%>%
  left_join(., WB)%>%
  group_by(wb2021)%>%
  summarise(deaths.avert = sum(Deaths.Avert))%>%
  bind_rows(., best.dadt%>%left_join(., WB)%>%group_by(wb2021)%>%summarise(deaths.avert = sum(Deaths.Avert)))%>%
  group_by(wb2021)%>%
  summarise(deaths.avert = sum(deaths.avert))

add<-bind_rows(dadt, dadt%>%summarise(deaths.avert = sum(deaths.avert))%>%mutate(wb2021 = "LIC+LMIC"))

tab<-left_join(tab, add%>%mutate(deaths.avert = deaths.avert/1e6))

#dalys averted (discounted*)
dalys<-int.dalys%>%mutate(keep = ifelse(location_name %in% lics & Code>=5.5,0,1))%>%
  mutate(year_id = as.numeric(year_id))%>%
  filter(keep==1, year_id>2022)%>%
  mutate(DALY.ave = DALY.ave*((1-0.08)^(year_id-2022)))%>%
  left_join(., WB)%>%
  group_by(wb2021)%>%
  summarise(dalys.avert = sum(DALY.ave))%>%
  bind_rows(., best.dalys%>%
              mutate(year_id = as.numeric(year_id))%>%
              filter(year_id>2022)%>%
              mutate(DALY.ave = DALY.ave*((1-0.08)^(year_id-2022)))%>%
              left_join(., WB)%>%group_by(wb2021)%>%
              summarise(dalys.avert = sum(DALY.ave))
            )%>%
  group_by(wb2021)%>%
  summarise(dalys.avert = sum(dalys.avert))

add<-bind_rows(dalys, dalys%>%summarise(dalys.avert = sum(dalys.avert))%>%mutate(wb2021 = "LIC+LMIC"))

tab<-left_join(tab, add%>%mutate(dalys.avert = dalys.avert/1e6))

#economic benefits (discounted)
dalys<-int.dalys%>%mutate(keep = ifelse(location_name %in% lics & Code>=5.5,0,1))%>%
  mutate(year_id = as.numeric(year_id))%>%
  filter(keep==1, year_id>2022)%>%
  mutate(DALY.ave = DALY.ave*((1-0.08)^(year_id-2022)))%>%
  left_join(., WB)%>%
  group_by(wb2021, year_id, Code)%>%
  summarise(dalys.avert = sum(DALY.ave))%>%
  bind_rows(., best.dalys%>%
              mutate(year_id = as.numeric(year_id))%>%
              filter(year_id>2022)%>%
              mutate(DALY.ave = DALY.ave*((1-0.08)^(year_id-2022)))%>%
              left_join(., WB)%>%group_by(wb2021, year_id)%>%
              summarise(dalys.avert = sum(DALY.ave))
  )%>%
  mutate(dalys.avert = ifelse(dalys.avert<0,0,dalys.avert))

ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)))

#use LIC/LMC value of a DAYL
both.dalys<-dalys%>%
  group_by(year_id,Code)%>%
  summarise(dalys.avert = sum(dalys.avert, na.rm=T))%>%
  mutate(wb2021 = "LIC/LMC")

dalys<-bind_rows(dalys, both.dalys)

add<-left_join(dalys, ccc.vsl)%>%
  filter(year_id>=2022)%>%
  mutate(Gross.benefits = val*dalys.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
  )%>%group_by(wb2021)%>%
  summarise(benefit = sum(Gross.benefits)/1e9,
            forgone.surplus = sum(Forgone.surplus)/1e9)%>%
  mutate(wb2021 = ifelse(wb2021 == "LIC/LMC", "LIC+LMIC", wb2021))


tab<-left_join(tab, add)

#BCR
tab<-tab%>%mutate(BCR = benefit / (inc.cost + forgone.surplus))

write.csv(tab, "Figures/best_table.csv", row.names=F)

#################
# All ints #
#################

tab<-cost.df%>%filter(year_id>2022, wb2021!="LIC/LMC")%>%
  group_by(wb2021)%>%
  summarise(inc.cost = sum(Incremental.cost, na.rm=T)/1e9)

add<-tab%>%summarise(inc.cost = sum(inc.cost))%>%mutate(wb2021 = "LIC+LMIC")

tab<-bind_rows(tab, add)

#annual pc cost
add<-cost.df%>%filter(year_id==2030, wb2021!="LIC/LMC")%>%
  group_by(wb2021)%>%
  summarise(total.cost = sum(Adjusted.cost, na.rm=T))

add<-bind_rows(add,add%>%summarise(total.cost = sum(total.cost))%>%mutate(wb2021 = "LIC+LMIC"))%>%
  left_join(., pop2030)%>%
  mutate(cost.pc = total.cost/pop)%>%
  select(wb2021, cost.pc)

tab<-left_join(tab, add)

# % reached #

q30<-left_join(all.q30, goal.q30.out%>%select(location_name, Baseline_2015, Target_40q30)%>%unique())%>%
  mutate(reached = ifelse(Adjusted - Target_40q30<=0,1,0))%>%
  filter(year_id==2030)%>%
  left_join(., WB)%>%
  mutate(all = 1)%>%
  group_by(wb2021)%>%
  summarise(prop_reached = sum(reached, na.rm=T)/sum(all, na.rm=T))

lmic.q30<-left_join(all.q30, goal.q30.out%>%select(location_name, Baseline_2015, Target_40q30)%>%unique())%>%
  mutate(reached = ifelse(Adjusted - Target_40q30<=0,1,0))%>%
  filter(year_id==2030)%>%
  left_join(., WB)%>%
  mutate(all = 1)%>%
  summarise(prop_reached = sum(reached, na.rm=T)/sum(all, na.rm=T))%>%
  mutate(wb2021 = "LIC+LMIC")

add<-bind_rows(q30, lmic.q30)%>%
  mutate(prop_reached = paste0(prop_reached*100, "%"))

tab<-left_join(tab, add)

#deaths averted
dadt<-int.dadt%>%
  left_join(., WB)%>%
  group_by(wb2021)%>%
  summarise(deaths.avert = sum(Deaths.Avert))%>%
  bind_rows(., all.dadt%>%left_join(., WB)%>%group_by(wb2021)%>%summarise(deaths.avert = sum(Deaths.Avert)))%>%
  group_by(wb2021)%>%
  summarise(deaths.avert = sum(deaths.avert))

add<-bind_rows(dadt, dadt%>%summarise(deaths.avert = sum(deaths.avert))%>%mutate(wb2021 = "LIC+LMIC"))

tab<-left_join(tab, add%>%mutate(deaths.avert = deaths.avert/1e6))

#dalys averted (discounted*)
dalys<-int.dalys%>%
  mutate(year_id = as.numeric(year_id))%>%
  filter(year_id>2022)%>%
  mutate(DALY.ave = DALY.ave*((1-0.08)^(year_id-2022)))%>%
  left_join(., WB)%>%
  group_by(wb2021)%>%
  summarise(dalys.avert = sum(DALY.ave))%>%
  bind_rows(., all.dalys%>%
              mutate(year_id = as.numeric(year_id))%>%
              filter(year_id>2022)%>%
              mutate(DALY.ave = DALY.ave*((1-0.08)^(year_id-2022)))%>%
              left_join(., WB)%>%group_by(wb2021)%>%
              summarise(dalys.avert = sum(DALY.ave))
  )%>%
  group_by(wb2021)%>%
  summarise(dalys.avert = sum(dalys.avert))

add<-bind_rows(dalys, dalys%>%summarise(dalys.avert = sum(dalys.avert))%>%mutate(wb2021 = "LIC+LMIC"))

tab<-left_join(tab, add%>%mutate(dalys.avert = dalys.avert/1e6))

#economic benefits (discounted)
dalys<-int.dalys%>%
  mutate(year_id = as.numeric(year_id))%>%
  filter(year_id>2022)%>%
  mutate(DALY.ave = DALY.ave*((1-0.08)^(year_id-2022)))%>%
  left_join(., WB)%>%
  group_by(wb2021, year_id, Code)%>%
  summarise(dalys.avert = sum(DALY.ave))%>%
  bind_rows(., all.dalys%>%
              mutate(year_id = as.numeric(year_id))%>%
              filter(year_id>2022)%>%
              mutate(DALY.ave = DALY.ave*((1-0.08)^(year_id-2022)))%>%
              left_join(., WB)%>%group_by(wb2021, year_id)%>%
              summarise(dalys.avert = sum(DALY.ave))
  )%>%
  mutate(dalys.avert = ifelse(dalys.avert<0,0,dalys.avert))

ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)))


#use LIC/LMC value of a DAYL
both.dalys<-dalys%>%
  group_by(year_id,Code)%>%
  summarise(dalys.avert = sum(dalys.avert, na.rm=T))%>%
  mutate(wb2021 = "LIC/LMC")

dalys<-bind_rows(dalys, both.dalys)

add<-left_join(dalys, ccc.vsl)%>%
  filter(year_id>=2022)%>%
  mutate(Gross.benefits = val*dalys.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
  )%>%group_by(wb2021)%>%
  summarise(benefit = sum(Gross.benefits)/1e9,
            forgone.surplus = sum(Forgone.surplus)/1e9)%>%
  mutate(wb2021 = ifelse(wb2021 == "LIC/LMC", "LIC+LMIC", wb2021))


tab<-left_join(tab, add)

#BCR
tab<-tab%>%mutate(BCR = benefit / (inc.cost + forgone.surplus))

write.csv(tab, "Figures/all_table.csv", row.names=F)

###


### 
#Appendix tables
###

#8% 
atab1<-read.csv("Figures/clinical_full_2023_2030.csv")%>%
  select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
  gather(Metric, Base, -WB_Region, -Code, -Intervention)%>%
  left_join(., 
            read.csv("Figures/clinical_full_2023_2030_pesm.csv")%>%
              select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
              gather(Metric, Pessimistic, -WB_Region, -Code, -Intervention)
  )%>%
  left_join(., 
            read.csv("Figures/clinical_full_2023_2030_optm.csv")%>%
              select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
              gather(Metric, Optimistic, -WB_Region, -Code, -Intervention)
            )%>%
  
  arrange(WB_Region, Intervention, Metric, -Base)

write.csv(atab1, "Figures/Appendix_Table_8percent.csv", row.names = F)

#5%
atab2<-read.csv("Figures/clinical_full_2023_2030_5perc.csv")%>%
  select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
  gather(Metric, Base, -WB_Region, -Code, -Intervention)%>%
  left_join(., 
            read.csv("Figures/clinical_full_2023_2030_pesm_5perc.csv")%>%
              select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
              gather(Metric, Pessimistic, -WB_Region, -Code, -Intervention)
  )%>%
  left_join(., 
            read.csv("Figures/clinical_full_2023_2030_optm_5perc.csv")%>%
              select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
              gather(Metric, Optimistic, -WB_Region, -Code, -Intervention)
  )%>%
  
  arrange(WB_Region, Intervention, Metric, -Base)

write.csv(atab2, "Figures/Appendix_Table_5percent.csv", row.names = F)

#14%
atab3<-read.csv("Figures/clinical_full_2023_2030_14perc.csv")%>%
  select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
  gather(Metric, Base, -WB_Region, -Code, -Intervention)%>%
  left_join(., 
            read.csv("Figures/clinical_full_2023_2030_pesm_14perc.csv")%>%
              select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
              gather(Metric, Pessimistic, -WB_Region, -Code, -Intervention)
  )%>%
  left_join(., 
            read.csv("Figures/clinical_full_2023_2030_optm_14perc.csv")%>%
              select(WB_Region, Code, Intervention, DALYS.avert, BCR)%>%
              gather(Metric, Optimistic, -WB_Region, -Code, -Intervention)
  )%>%
  
  arrange(WB_Region, Intervention, Metric, -Base)

write.csv(atab3, "Figures/Appendix_Table_14percent.csv", row.names = F)



#####################
#India
#####################

load("for_parallel_processing/output2023_target_intersectoral.Rda")

all.dalys%>%
  filter(location_name=="India", Code==5.1)%>%
  summarise(DALYS.ave = sum(DALY.ave))

dalys<-all.dalys%>%
  filter(location_name=="India", Code==5.1)%>%
  group_by(year_id)%>%
  summarise(DALYS.ave = sum(DALY.ave))%>%
  mutate(year_id = as.numeric(year_id))

dadt.all%>%
  filter(location_name=="India", Code==5.1)%>%
  summarise(Deaths.Avert = sum(Deaths.Avert))

deaths<-dadt.all%>%
  filter(location_name=="India", Code==5.1)%>%
  group_by(year_id)%>%
  summarise(Deaths.Avert = sum(Deaths.Avert))

uc<-read.csv("output/unit_costs/India_adjusted_uc_2020.csv")%>%
  filter(NCD==5.1)%>%pull(adjusted_uc)

cost<-all.pin%>%filter(Code==5.1, location_name == "India")%>%
  mutate(cost = pin*uc)%>%
  select(year_id, group, cost)%>%
  spread(group, cost)%>%
  filter(year_id>=2023)%>%
  rename(Adjusted.cost = Adjusted,
         Baseline.cost = Baseline)%>%
  left_join(., dalys)%>%
  left_join(., deaths)

write.csv(cost, "output/India_crude_tob_tax.csv")

