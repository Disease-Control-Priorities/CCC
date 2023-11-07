rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, ggpattern, tidyverse, broom, readxl)
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

#########################
#Figure 2
#########################
order<-read.csv("hpp_order.csv")%>%
  select(order, Intervention)

fig2<-read.csv("hpp.csv", stringsAsFactors = F)%>%
  mutate(HSP = ifelse(HSP=="conflict", "Conflict", HSP),
         HSP = ifelse(HSP=="fragile", "Fragile", HSP),
         Priorty = ifelse(Code>5, "High", Priorty))%>%
  left_join(., order)%>%
  mutate(Priorty = factor(Priorty, levels=c("High", "Medium", "Low")))

unique(fig2$HSP)

ggplot(fig2%>%mutate(HSP = factor(HSP, levels = c("Conflict", "Fragile","HS1","HS2","HS3"))), 
       aes(x=HSP, y=reorder(Intervention, -order), fill=Priorty))+
  geom_tile(color="white", size=0.5)+
  theme_classic()+
  theme(axis.text.x=element_text(size=12, angle=45, hjust=0),    
        axis.text.y=element_text(size=12))+
  scale_fill_manual(values=c("#5d9976", "#feea83","#f8696b"))+
  scale_x_discrete(position="top")+
  #labs(fill="ICER (as a proportion \nof GDP per capita)")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

ggsave("figures/Figure2.png", height = 10, width = 12, units = "in")

write.csv(fig2, "figures/figure2_data.csv", row.names = F)

#########################
# Table 2
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

#Add WB2023 class
class<-read_excel("../new_inputs/CLASS.xlsx")%>%
  select(Code, `Income group`)%>%
  rename(iso3 = Code,
         wb2023 = `Income group`)%>%
  mutate(wb2023 = ifelse(wb2023=="High income", "HIC", wb2023),
         wb2023 = ifelse(wb2023=="Low income", "LIC", wb2023),
         wb2023 = ifelse(wb2023=="Lower middle income", "LMIC", wb2023),
         wb2023 = ifelse(wb2023=="Upper middle income", "UMIC", wb2023),
         wb2023 = ifelse(iso3=="VEN", "UMIC", wb2023))
#venezuela is NA? #assign to UMIC (per 2021 data)

WB<-left_join(WB, class)

load("output/results_target_all.Rda")
t_deaths<-dadt.all.opt
t_dalys<-dalys.opt
t_pin<-all.pin.opt
NCD0_all<-d0.opt
NCD1_all<-d1.opt

#add intersectoral results#
load("../for_parallel_processing/output2050_target_intersectoral.Rda")
int_deaths<-dadt.all
int_dalys<-all.dalys
int_pin<-all.pin
NCD0_int<-d0.all
NCD1_int<-d1.all

cost<-read.csv("output/all_costs.csv", stringsAsFactors = F)
all.locs<-unique(cost$location_name)

DA2050<-bind_rows(t_deaths%>%select(location_name, Deaths.Avert, year_id), 
                  int_deaths%>%select(location_name, Deaths.Avert, year_id))%>%
  filter(location_name %in% all.locs, year_id==2050)%>%
  left_join(., WB)%>%group_by(wb2023, year_id)%>%
  summarise(Deaths.averted = sum(Deaths.Avert, na.rm=T))%>%
  mutate(Scenario = "All interventions")

max(int_deaths$year)

DA<-bind_rows(t_deaths%>%select(location_name, Deaths.Avert), 
              int_deaths%>%select(location_name, Deaths.Avert))%>%
  filter(location_name %in% all.locs)%>%
  left_join(., WB)%>%group_by(wb2023)%>%
  summarise(Cumulative.deaths.averted = sum(Deaths.Avert, na.rm=T))

DALYS<-bind_rows(t_dalys%>%select(location_name, DALY.ave), 
                 int_dalys%>%select(location_name, DALY.ave))%>%
  filter(location_name %in% all.locs)%>%
  left_join(., WB)%>%group_by(wb2023)%>%
  summarise(Cumulative.DALYs.averted = sum(DALY.ave))

inc.cost<-left_join(cost, WB)%>%filter(location_name %in% all.locs)%>%
  group_by(wb2023)%>%
  summarise(Cumulative.incremental.cost = sum(Incremental.cost, na.rm=T))

tot.cost<-left_join(cost, WB)%>%filter(location_name %in% all.locs)%>%
  group_by(wb2023, year_id)%>%
  summarise(Total.cost.all = sum(Adjusted.cost, na.rm=T),
            Total.cost.base = sum(Baseline.cost, na.rm=T))

cost.2050<-left_join(cost, WB)%>%filter(year_id==2050)%>%
  filter(location_name %in% all.locs)%>%
  group_by(wb2023)%>%
  summarise(Total.cost.2050 = sum(Adjusted.cost, na.rm=T))

gghed<-read.csv("gghe/gghed_data_2050.csv",stringsAsFactors = F)%>%filter(year==2050)%>%
  rename(iso3 = ISO)%>%
  left_join(., WB)%>%
  filter(location_name %in% all.locs)%>%
  group_by(wb2023)%>%
  summarise(PopTotal = sum(PopTotal),
            GGHED = sum(GGHED))

#output GDP for Daphne
gpd4d<-read.csv("gghe/gghed_data_2050.csv",stringsAsFactors = F)%>%
  filter(year>=2020 & year<=2050)%>%
  rename(iso3 = ISO)%>%
  left_join(., WB)%>%
  filter(location_name %in% all.locs)%>%
  group_by(wb2023, year)%>%
  summarise(PopTotal = sum(PopTotal),
            GDP = sum(GDP))

table1<-left_join(DA, DALYS)%>%
  left_join(., inc.cost)%>%
  left_join(., cost.2050)%>%
  left_join(gghed)%>%
  mutate(Cost.per.capita = Total.cost.2050/PopTotal,
         Cost.per.GGHED = Total.cost.2050/GGHED)

#Add all countries
table1_all<-left_join(DA, DALYS)%>%
  left_join(., inc.cost)%>%
  left_join(., cost.2050)%>%
  left_join(.,gghed)%>%
  summarise(Cumulative.deaths.averted = sum(Cumulative.deaths.averted),
            Cumulative.DALYs.averted = sum(Cumulative.DALYs.averted),
            Cumulative.incremental.cost = sum(Cumulative.incremental.cost),
            Total.cost.2050 = sum(Total.cost.2050),
            PopTotal = sum(PopTotal),
            GGHED = sum(GGHED))%>%
  mutate(Cost.per.capita = Total.cost.2050/PopTotal,
         Cost.per.GGHED = Total.cost.2050/GGHED,
         wb2023 = "All countries")

write.csv(bind_rows(table1, table1_all), "figures/Table2.csv", row.names = F)

##############################################
#Table 5
##############################################
rm(dalys.opt, dadt.all.opt)

load("output/results_target_best.Rda")
NCD0_best<-d0.opt
NCD1_best<-d1.opt

########################
#Cost
########################
selects<-left_join(read.csv("hpp.csv", stringsAsFactors = F), WB, relationship = "many-to-many")%>%
  select(Priorty, location_name, Code)%>%
  mutate(Priorty = ifelse(Code>5, "High", Priorty))

cost2<-read.csv("output/all_costs.csv", stringsAsFactors = F)%>%
  left_join(., selects)%>%
  filter(Priorty == "High")

########################

DA2050<-bind_rows(dadt.all.opt%>%select(location_name, Deaths.Avert, year_id), 
                  int_deaths%>%select(location_name, Deaths.Avert, year_id))%>%
  filter(location_name %in% all.locs, year_id==2050)%>%
  left_join(., WB)%>%group_by(wb2023, year_id)%>%
  summarise(Deaths.averted = sum(Deaths.Avert, na.rm=T))%>%
  mutate(Scenario = "High-priority interventions")%>%
  bind_rows(., DA2050)

DA2050<-DA2050%>%group_by(year_id, Scenario)%>%summarise(Deaths.averted = sum(Deaths.averted))%>%
  mutate(wb2023 = "All LMIC")%>%
  bind_rows(., DA2050)

write.csv(DA2050%>%arrange(Scenario), "deaths_averted_2050.csv", row.names = F)

DA2<-bind_rows(dadt.all.opt%>%select(location_name, Deaths.Avert), 
               int_deaths%>%select(location_name, Deaths.Avert))%>%
  filter(location_name %in% all.locs)%>%
  left_join(., WB)%>%group_by(wb2023)%>%
  summarise(Cumulative.deaths.averted = sum(Deaths.Avert))%>%
  na.omit()

DALYS2<-bind_rows(dalys.opt%>%select(location_name, DALY.ave), 
                 int_dalys%>%select(location_name, DALY.ave))%>%
  filter(location_name %in% all.locs)%>%
  left_join(., WB)%>%group_by(wb2023)%>%
  summarise(Cumulative.DALYs.averted = sum(DALY.ave))%>%
  na.omit()

inc.cost2<-left_join(cost2, WB)%>%group_by(wb2023)%>%
  summarise(Cumulative.incremental.cost = sum(Incremental.cost, na.rm=T))%>%
  na.omit()

tot.cost2<-left_join(cost2, WB)%>%filter(location_name %in% all.locs)%>%
  group_by(wb2023, year_id)%>%
  summarise(Total.cost.hpp = sum(Adjusted.cost, na.rm=T))

cost.20502<-left_join(cost2, WB)%>%filter(year_id==2050)%>%
  group_by(wb2023)%>%
  summarise(Total.cost.2050 = sum(Adjusted.cost, na.rm=T))%>%
  na.omit()

table3<-left_join(DA2, DALYS2)%>%
  left_join(., inc.cost2)%>%
  left_join(., cost.20502)%>%
  left_join(., gghed)%>%
  mutate(Cost.per.capita = Total.cost.2050/PopTotal,
         Cost.per.GGHED = Total.cost.2050/GGHED)


table3_all<-left_join(DA2, DALYS2)%>%
  left_join(., inc.cost2)%>%
  left_join(., cost.20502)%>%
  left_join(., gghed)%>%
  summarise(Cumulative.deaths.averted = sum(Cumulative.deaths.averted),
            Cumulative.DALYs.averted = sum(Cumulative.DALYs.averted),
            Cumulative.incremental.cost = sum(Cumulative.incremental.cost),
            Total.cost.2050 = sum(Total.cost.2050),
            PopTotal = sum(PopTotal),
            GGHED = sum(GGHED))%>%
  mutate(Cost.per.capita = Total.cost.2050/PopTotal,
         Cost.per.GGHED = Total.cost.2050/GGHED,
         wb2023 = "All countries")

write.csv(bind_rows(table3,table3_all), "figures/Table5.csv", row.names = F)

##Table for Daphne

tabxx<-left_join(tot.cost, tot.cost2)%>%
  left_join(., gpd4d%>%rename(year_id = year))%>%
  group_by(year_id)%>%
  summarise(Total.cost.base = sum(Total.cost.base),
            Total.cost.all = sum(Total.cost.all),
            Total.cost.hpp = sum(Total.cost.hpp),
            PopTotal = sum(PopTotal),
            GDP = sum(GDP))

write.csv(tabxx, "gdp_total_cost.csv", row.names = F)

################
#Table 6
################

gghed<-read.csv("gghe/gghed_data_2050.csv",stringsAsFactors = F)%>%
  filter(year==2050 | year==2040 | year==2030)%>%
  rename(iso3 = ISO)%>%
  left_join(., WB)%>%
  filter(location_name %in% all.locs)%>%
  group_by(wb2023, year)%>%
  summarise(PopTotal = sum(PopTotal),
            GGHED = sum(GGHED))%>%
  rename(year_id = year)

cost3<-read.csv("output/all_costs.csv", stringsAsFactors = F)%>%
  left_join(., selects)%>%
  filter(Priorty != "Low")

table6<-bind_rows(cost2%>%mutate(pkg = "High"),
                  cost3%>%mutate(pkg = "High + Medium"))%>%
  filter(year_id==2030 | year_id==2040 | year_id==2050)%>%
  left_join(., WB)%>%
  group_by(wb2023, year_id, pkg)%>%
  summarise(Totalcost = sum(Adjusted.cost))%>%
  left_join(., gghed)%>%
  mutate(costpc = Totalcost/PopTotal)%>%
  mutate(column = paste0(year_id, ", pkg:", pkg))%>%
  ungroup()%>%
  select(-pkg, -year_id, -PopTotal, -GGHED, -Totalcost)%>%
  spread(column, costpc)

totalpop<-gghed%>%group_by(year_id)%>%summarise(PopTotal = sum(PopTotal))
  
all6<-bind_rows(cost2%>%mutate(pkg = "High"),
                cost3%>%mutate(pkg = "High + Medium"))%>%
  filter(year_id==2030 | year_id==2040 | year_id==2050)%>%
  group_by(year_id, pkg)%>%
  summarise(Totalcost = sum(Adjusted.cost))%>%
  left_join(., totalpop)%>%
  mutate(costpc = Totalcost/PopTotal)%>%
  mutate(column = paste0(year_id, ", pkg:", pkg))%>%
  ungroup()%>%
  select(-pkg, -year_id, -PopTotal, -Totalcost)%>%
  spread(column, costpc)%>%
  mutate(wb2023 = "All countries")

tab6<-bind_rows(table6, all6)

write.csv(tab6, "figures/Table6.csv", row.names = F)

################
# Figure 3
# NCD mx by age
################

load("../new_inputs/PreppedData2023c_2050.Rda")
#grab Nx from wpp.in

mx0<-NCD0_all%>%
  group_by(location_name)%>%
  mutate(age = row_number(),
         sex = ifelse(age>86, "Male", "Female"),
         age = ifelse(age>86, age-86, age),
         age= age-1)%>%
  gather(year, deaths, -location_name, -sex, -age)%>%
  mutate(year = as.numeric(gsub("V","", year)),
         year = year+2018)%>%
  mutate(Scenario = "Baseline")

mx_int<-NCD0_int%>%
  group_by(location_name, Code)%>%
  mutate(age = row_number(),
         sex = ifelse(age>86, "Male", "Female"),
         age = ifelse(age>86, age-86, age),
         age= age-1)%>%
  gather(year, deaths, -location_name, -sex, -age, -Code)%>%
  mutate(year = as.numeric(gsub("V","", year)),
         year = year+2018)%>%
  rename(deaths0 = deaths)%>%
  left_join(., NCD1_int%>%
              group_by(location_name, Code)%>%
              mutate(age = row_number(),
                     sex = ifelse(age>86, "Male", "Female"),
                     age = ifelse(age>86, age-86, age),
                     age= age-1)%>%
              gather(year, deaths, -location_name, -sex, -age, -Code)%>%
              mutate(year = as.numeric(gsub("V","", year)),
                     year = year+2018)%>%
              rename(deaths1 = deaths))%>%
  mutate(DA = deaths0 - deaths1)%>%
  group_by(location_name, year, age, sex)%>%
  summarise(DA = sum(DA))

mxall<-NCD1_all%>%
  group_by(location_name)%>%
  mutate(age = row_number(),
         sex = ifelse(age>86, "Male", "Female"),
         age = ifelse(age>86, age-86, age),
         age= age-1)%>%
  gather(year, deaths, -location_name, -sex, -age)%>%
  mutate(year = as.numeric(gsub("V","", year)),
         year = year+2018)%>%
  mutate(Scenario = "All interventions")%>%
  left_join(., mx_int)%>%
  mutate(deaths = deaths - DA)%>%
  select(-DA)


#load("output/results_q30_best.Rda")

mxhpp<-NCD1_best%>%
  group_by(location_name)%>%
  mutate(age = row_number(),
         sex = ifelse(age>86, "Male", "Female"),
         age = ifelse(age>86, age-86, age),
         age= age-1)%>%
  gather(year, deaths, -location_name, -sex, -age)%>%
  mutate(year = as.numeric(gsub("V","", year)),
         year = year+2018)%>%
  mutate(Scenario = "High-priority interventions")%>%
  left_join(., mx_int)%>%
  mutate(deaths = deaths - DA)%>%
  select(-DA)

#Take 20th percentile in 2040
mxfront<-read.csv("for-David_2023-05-16.csv", stringsAsFactors = F)%>%
  filter(year==2040)%>%
  group_by(year, age, ncd_mxn, iso3)%>%
  summarise(ncd_mxn = mean(ncd_mxn)*1e5)%>%
  mutate(Scenario = "Frontier mortality 2040")%>% 
  group_by(age, year, Scenario)%>%
  summarise(ncd_mxn = quantile(ncd_mxn, probs=c(0.2)))


plot4<-bind_rows(mx0, mxall, mxhpp)%>%
  filter(year==2040)%>%
  left_join(., WB)%>%
  left_join(., wpp.in%>%
              rename(age=age_name, sex = sex_name)%>%
              select(age, sex, year, iso3, Nx))%>%
  group_by(year, age, Scenario)%>%
  summarise(deaths = sum(deaths),
            pop = sum(Nx))%>%
  mutate(ncd_mxn = 1e5*(deaths/pop))%>%
  bind_rows(., mxfront)%>%
  mutate(Scenario = factor(Scenario, levels = c("Baseline", "High-priority interventions",
                                                "All interventions", "Frontier mortality 2040")))


#rates
tabx<-plot4%>%filter(year==2040, age %in% c(40,60,80))%>%
  select(-deaths, -pop)%>%
  mutate(age = paste("age", age))%>%
  spread(age, ncd_mxn)

#write.csv(tabx, "ncd_mx.csv")


## NCD death rates for Daphne
#AARC: log((well19/pop19)/(well14/pop14))/5
tabx2<-bind_rows(mx0, mxall, mxhpp)%>%
  left_join(., WB)%>%
  left_join(., wpp.in%>%
              rename(age=age_name, sex = sex_name)%>%
              select(age, sex, year, iso3, Nx))%>%
  group_by(year, age, Scenario)%>%
  summarise(deaths = sum(deaths),
            pop = sum(Nx))%>%
  mutate(ncd_mxn = 1e5*(deaths/pop))%>%
  mutate(Scenario = factor(Scenario, levels = c("Baseline", "High-priority interventions","All interventions")))%>%
  mutate(age.group = ifelse( age<30, "0-30",
                                          ifelse(age>=30 & age<49, "30-49",
                                                 ifelse(age>=50 & age<70, "50-69",
                                                        ifelse(age>=70 & age<85, "70-84", "85+")))))%>%
  filter(year==2020 | year==2050)%>%
  group_by(age.group, Scenario, year)%>%
  summarise(deaths = sum(deaths),
            pop = sum(pop))%>%
  mutate(ncd_mx = 1e5*(deaths/pop),
         Scenario = paste(Scenario, "in", year))%>%
  filter(age.group !="85+", age.group!="0-30")%>%
  select(-deaths, -pop, -year)%>%
  spread(Scenario, ncd_mx)%>%
  select(-`High-priority interventions in 2020`,
         -`All interventions in 2020`)%>%
  mutate(`hpp_aarc (%)` = 100*log(`High-priority interventions in 2050`/`Baseline in 2020`)/30,
         `allint_aarc (%)` = 100*log(`All interventions in 2050`/`Baseline in 2020`)/30)


write.csv(tabx2, "ncd_deaths_per_100k.csv", row.names = F)
  

#all cause mx?


ggplot(plot4%>%filter(year==2040), aes(x=age, y=ncd_mxn, color=Scenario))+
  geom_smooth(se=FALSE)+
  ylab("NCD mortality rate (per 100,000)")+
  xlab("Age")+
  theme_bw()+
  scale_y_continuous(trans='log10')

#ggsave("figures/Figure3.jpeg", height=4, width=8)

ggplot(plot4%>%filter(year==2040, age>30), aes(x=age, y=ncd_mxn, color=Scenario))+
  geom_smooth(se=FALSE)+
  ylab("NCD mortality rate (per 100,000)")+
  xlab("Age")+
  theme_bw()+
  scale_y_continuous(trans='log10')

ggsave("figures/Figure3_over30.jpeg", height=4, width=8)
write.csv(plot4%>%filter(year==2040, age>30), "figures/figure3_data.csv", row.names = F)


ggplot(plot4%>%filter(year==2040,age>30), aes(x=age, y=ncd_mxn, color=Scenario))+
  geom_smooth(se=FALSE)+
  ylab("NCD mortality rate (per 100,000)")+
  xlab("Age")+
  theme_bw()

#ggsave("figures/Figure3_alt.jpeg", height=4, width=8)

################################
#Didn't use
################################

####### Figure 4 ################

plot3<-read.csv("figures/Table2.csv", stringsAsFactors = F)%>%
  select(wb2023, Cumulative.incremental.cost, Cumulative.DALYs.averted)%>%
  mutate(Scenario = "All interventions")%>%
  bind_rows(., read.csv("figures/Table5.csv", stringsAsFactors = F)%>%
              select(wb2023, Cumulative.incremental.cost, Cumulative.DALYs.averted)%>%
              mutate(Scenario = "High priority interventions"))

plot3<-plot3%>%
  select(Scenario, wb2023, Cumulative.incremental.cost, Cumulative.DALYs.averted)%>%
  mutate(Cumulative.DALYs.averted = Cumulative.DALYs.averted/1e6,
         Cumulative.incremental.cost = Cumulative.incremental.cost/1e9)%>%
  gather(Measure, value, -wb2023, -Scenario)%>%
  mutate(Measure = ifelse(Measure=="Cumulative.DALYs.averted",
                          "Cumulative DALYs \naverted (millions)",
                          "Cumulative incremental \ncost (billions $USD)"))%>%
  mutate(Measure = factor(Measure,levels=c("Cumulative incremental \ncost (billions $USD)",
                                           "Cumulative DALYs \naverted (millions)")))


library("ggpattern")

ggplot(plot3%>%filter(wb2023!="All countries"), 
       aes(x=wb2023, y=value, alpha=Scenario, fill=Measure, group=Measure))+
  geom_bar(position = "dodge", 
           stat = "identity")+
  xlab("")+
  ylab("")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.5, 0.9))+
  scale_fill_manual(values=c("#4A889A", "#BB667D"))

#ggsave("figures/Figure4.jpeg", height=6, width=9)
