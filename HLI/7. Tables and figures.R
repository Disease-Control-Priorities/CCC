rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

#########################
# Table 1
# No discounting
########################

WB<-read.csv("../new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  select(iso3, location_gbd, HLI_group)%>%
  rename(location_name = location_gbd)%>%
  left_join(., read.csv("../new_inputs/HS123.csv", stringsAsFactors = F)%>%
              select(ISO3, HSP)%>%
              rename(iso3 = ISO3))%>%
  mutate(HSP = gsub(" ", "", HSP))%>%
  filter(HSP!="NA")

load("output/results_target_all.Rda")
t_deaths<-dadt.all.opt
t_dalys<-dalys.opt
t_pin<-all.pin.opt

#add intersectoral results#
load("../for_parallel_processing/output2023_target_intersectoral.Rda")
int_deaths<-dadt.all
int_dalys<-all.dalys
int_pin<-all.pin
#add UMICs
load("../for_parallel_processing/output2023_target_intersectoral_UMICs.Rda")
int_deaths<-bind_rows(int_deaths, dadt.all)
int_dalys<-bind_rows(int_dalys, all.dalys)
int_pin<-bind_rows(int_pin, all.pin)

cost<-read.csv("output/all_costs.csv", stringsAsFactors = F)
exclude<-c("Moldova", "Turkey", "Maldives", "Vanuatu")
all.locs<-unique(cost$location_name[cost$location_name%!in%exclude])

DA<-bind_rows(t_deaths%>%select(location_name, Deaths.Avert), 
              int_deaths%>%select(location_name, Deaths.Avert))%>%
  filter(location_name %in% all.locs)%>%
  left_join(., WB)%>%group_by(HLI_group)%>%
  summarise(Cumulative.deaths.averted = sum(Deaths.Avert, na.rm=T))

DALYS<-bind_rows(t_dalys%>%select(location_name, DALY.ave), 
                 int_dalys%>%select(location_name, DALY.ave))%>%
  filter(location_name %in% all.locs)%>%
  left_join(., WB)%>%group_by(HLI_group)%>%
  summarise(Cumulative.DALYs.averted = sum(DALY.ave))

inc.cost<-left_join(cost, WB)%>%filter(location_name %in% all.locs)%>%
  group_by(HLI_group)%>%
  summarise(Cumulative.incremental.cost = sum(Incremental.cost, na.rm=T))

cost.2030<-left_join(cost, WB)%>%filter(year_id==2030)%>%
  filter(location_name %in% all.locs)%>%
  group_by(HLI_group)%>%
  summarise(Total.cost.2030 = sum(Adjusted.cost, na.rm=T))

gghed<-read.csv("gghe/gghed_data.csv",stringsAsFactors = F)%>%filter(year==2030)%>%
  rename(iso3 = ISO)%>%
  left_join(., WB)%>%
  filter(location_name %in% all.locs)%>%
  group_by(HLI_group)%>%
  summarise(PopTotal = sum(PopTotal),
            GGHED = sum(GGHED))

table1<-left_join(DA, DALYS)%>%
  left_join(., inc.cost)%>%
  left_join(., cost.2030)%>%
  left_join(gghed)%>%
  mutate(Cost.per.capita = Total.cost.2030/PopTotal,
         Cost.per.GGHED = Total.cost.2030/GGHED)

write.csv(table1, "figures/Table1.csv", row.names = F)

##############################################
#Table 3
##############################################

load("output/results_target_best.Rda")
unique(dadt.all.opt$location_name)
########################
#Cost
########################

pin<-bind_rows(all.pin.opt, int_pin)%>%
  mutate(unique_id = paste0("C",Code,"_", sub_id))%>%
  select(-sub_id)

unique(pin$Code)

uc<-read.csv("../output/unit_costs/Afghanistan_adjusted_uc_2020.csv", stringsAsFactors = F)%>%
  mutate(location_name = "Afghanistan")

for(i in all.locs[2:117]){
  uc<-bind_rows(uc, read.csv(paste0("../output/unit_costs/",i,"_adjusted_uc_2020.csv"), stringsAsFactors = F)%>%
                  mutate(location_name = i))
}

cost2<-left_join(pin, uc)%>%
  mutate(cost = adjusted_uc*pin)%>%
  group_by(Code, group, year_id, Intervention, location_name)%>%
  summarise(cost = sum(cost))%>%
  spread(group, cost)%>%
  rename(Adjusted.cost = Adjusted,
         Baseline.cost = Baseline)%>%
  mutate(Incremental.cost = Adjusted.cost - Baseline.cost)

########################

DA2<-bind_rows(dadt.all.opt%>%select(location_name, Deaths.Avert), 
               int_deaths%>%select(location_name, Deaths.Avert))%>%
  filter(location_name %in% all.locs)%>%
  left_join(., WB)%>%group_by(HLI_group)%>%
  summarise(Cumulative.deaths.averted = sum(Deaths.Avert))%>%
  na.omit()

DALYS2<-bind_rows(dalys.opt%>%select(location_name, DALY.ave), 
                 int_dalys%>%select(location_name, DALY.ave))%>%
  filter(location_name %in% all.locs)%>%
  left_join(., WB)%>%group_by(HLI_group)%>%
  summarise(Cumulative.DALYs.averted = sum(DALY.ave))%>%
  na.omit()

inc.cost2<-left_join(cost2, WB)%>%group_by(HLI_group)%>%
  summarise(Cumulative.incremental.cost = sum(Incremental.cost, na.rm=T))%>%
  na.omit()

cost.20302<-left_join(cost2, WB)%>%filter(year_id==2030)%>%
  group_by(HLI_group)%>%
  summarise(Total.cost.2030 = sum(Adjusted.cost, na.rm=T))%>%
  na.omit()

table3<-left_join(DA2, DALYS2)%>%
  left_join(., inc.cost2)%>%
  left_join(., cost.20302)%>%
  left_join(., gghed)%>%
  mutate(Cost.per.capita = Total.cost.2030/PopTotal,
         Cost.per.GGHED = Total.cost.2030/GGHED)

write.csv(table3, "figures/Table3.csv", row.names = F)


plot3<-read.csv("figures/Table1.csv", stringsAsFactors = F)%>%
  select(HLI_group, Cumulative.incremental.cost, Cumulative.DALYs.averted)%>%
  mutate(Scenario = "All interventions")%>%
  bind_rows(., read.csv("figures/Table3.csv", stringsAsFactors = F)%>%
              select(HLI_group, Cumulative.incremental.cost, Cumulative.DALYs.averted)%>%
              mutate(Scenario = "High priority interventions"))

plot3<-plot3%>%
  select(Scenario, HLI_group, Cumulative.incremental.cost, Cumulative.DALYs.averted)%>%
  mutate(Cumulative.DALYs.averted = Cumulative.DALYs.averted/1e6,
         Cumulative.incremental.cost = Cumulative.incremental.cost/1e9)%>%
  gather(Measure, value, -HLI_group, -Scenario)%>%
  mutate(Measure = ifelse(Measure=="Cumulative.DALYs.averted",
                          "Cumulative DALYs \naverted (millions)",
                          "Cumulative incremental \ncost (billions $USD)"))%>%
  mutate(Measure = factor(Measure,levels=c("Cumulative incremental \ncost (billions $USD)",
                                           "Cumulative DALYs \naverted (millions)")))


library("ggpattern")

ggplot(plot3, aes(x=HLI_group, y=value, alpha=Scenario, fill=Measure, group=Measure))+
  geom_bar(position = "dodge", 
           stat = "identity")+
  xlab("")+
  ylab("")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.5, 0.9))+
  scale_fill_manual(values=c("#4A889A", "#BB667D"))

ggsave("figures/Figure4.jpeg", height=6, width=9)


################
# Figure 3
# all cause mx by age
################

load("../new_inputs/PreppedData2023c.Rda")
#grab Nx from wpp.in

load("output/results_q30.Rda")

mx0<-D0.opt%>%
  group_by(location_name)%>%
  mutate(age = row_number(),
         sex = ifelse(age>86, "Female", "Male"),
         age = ifelse(age>86, age-86, age),
         age= age-1)%>%
  gather(year, deaths, -location_name, -sex, -age)%>%
  mutate(year = as.numeric(gsub("V","", year)),
         year = year+2018)%>%
  mutate(Scenario = "Baseline")

mxall<-D1.opt%>%
  group_by(location_name)%>%
  mutate(age = row_number(),
         sex = ifelse(age>86, "Female", "Male"),
         age = ifelse(age>86, age-86, age),
         age= age-1)%>%
  gather(year, deaths, -location_name, -sex, -age)%>%
  mutate(year = as.numeric(gsub("V","", year)),
         year = year+2018)%>%
  mutate(Scenario = "All interventions")


load("output/results_q30_best.Rda")

mxhpp<-D1.opt%>%
  group_by(location_name)%>%
  mutate(age = row_number(),
         sex = ifelse(age>86, "Female", "Male"),
         age = ifelse(age>86, age-86, age),
         age= age-1)%>%
  gather(year, deaths, -location_name, -sex, -age)%>%
  mutate(year = as.numeric(gsub("V","", year)),
         year = year+2018)%>%
  mutate(Scenario = "High-priority interventions")

mxfront<-mxhpp%>%mutate(deaths = 0.7*deaths, Scenario = "Frontier mortality")

plot4<-bind_rows(mx0, mxall, mxhpp, mxfront)%>%
  left_join(., WB)%>%
  left_join(., wpp.in%>%rename(age=age_name, sex = sex_name)%>%select(age, sex, year, iso3, Nx))%>%
  group_by(year, sex, age, Scenario)%>%
  summarise(deaths = sum(deaths),
            pop = sum(Nx))%>%
  mutate(mx = 1e5*(deaths/pop))

ggplot(plot4%>%filter(year==2030), aes(x=age, y=mx, color=Scenario))+
  geom_line(size=1)+
  facet_wrap(~sex)+
  ylab("All-cause mortality rate (per 100,000)")+
  xlab("Age")+
  theme_bw()

ggsave("figures/Figure3.jpeg", height=4, width=8)

