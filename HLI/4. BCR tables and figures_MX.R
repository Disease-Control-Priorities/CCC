rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

#intersectoral results#
load("../for_parallel_processing/output2050_target_intersectoral.Rda")
dadt.all2<-dadt.all
all.pin2<-all.pin
all.dalys2<-all.dalys

#clinical results#
load(paste0("../for_parallel_processing/for_HLI/Mexico_output2050_target.Rda"))

clinical.dadt<-dadt.all
clinical.dalys<-all.dalys
clinical.pin<-all.pin

######## Deaths averted ######## 
deaths.averted<-bind_rows(clinical.dadt, dadt.all2)%>%
  filter(location_name=="Mexico")%>%
  group_by(Code, year_id, location_name)%>%
  summarise(
    #Deaths.Baseline = sum(NCD.Deaths0),
    #Deaths.Adjusted = sum(NCD.Deaths1),
    Deaths.avert = sum(Deaths.Avert))%>%
  mutate(year_id = as.numeric(year_id))

######## DALYs averted ######## 
dalys.averted<-bind_rows(clinical.dalys, all.dalys2)%>%
  filter(location_name=="Mexico")%>%
  group_by(Code, year_id, location_name)%>%
  summarise(DALYS.Adjusted = sum(Adjusted),
            DALYS.Baseline = sum(Baseline),
            DALYS.avert = sum(DALY.ave),
            DALYS.avert =ifelse(DALYS.avert<1, 0, DALYS.avert))%>%
  mutate(year_id = as.numeric(year_id))

######## PIN ######## 
pin<-bind_rows(clinical.pin, all.pin2)%>%
  filter(location_name=="Mexico")%>%
  mutate(unique_id = paste0("C",Code,"_", sub_id))%>%
  select(-sub_id)

unique(pin$Code)

uc<-read.csv("../output/unit_costs/Mexico_adjusted_uc_2020.csv", stringsAsFactors = F)%>%
  mutate(location_name = "Mexico")

cost<-left_join(pin, uc)%>%
  filter(location_name=="Mexico")%>%
  mutate(cost = adjusted_uc*pin)%>%
  group_by(Code, group, year_id, Intervention, location_name)%>%
  summarise(cost = sum(cost))%>%
  spread(group, cost)%>%
  rename(Adjusted.cost = Adjusted,
         Baseline.cost = Baseline)%>%
  mutate(Incremental.cost = Adjusted.cost - Baseline.cost)

##By country##
df<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  group_by(year_id, Code, Intervention, location_name)%>%
  summarise(Baseline.cost = sum(Baseline.cost),
            Adjusted.cost = sum(Adjusted.cost),
            Incremental.cost = sum(Incremental.cost),
            #Deaths.Baseline = sum(Deaths.Baseline),
            #Deaths.Adjusted = sum(Deaths.Adjusted),
            Deaths.avert = sum(Deaths.avert),
            DALYS.Baseline = sum(DALYS.Baseline),
            DALYS.Adjusted = sum(DALYS.Adjusted),
            DALYS.avert = sum(DALYS.avert)
  )%>%
  mutate(discount.rate = ((1-0.05)^(year_id-2022)),
         discount.rate = ifelse(year_id<2023,1,discount.rate),
         Baseline.cost = (Baseline.cost*discount.rate),
         Adjusted.cost = (Adjusted.cost*discount.rate),
         Incremental.cost = (Incremental.cost*discount.rate),
         DALYS.Baseline = (DALYS.Baseline*discount.rate),
         DALYS.Adjusted = (DALYS.Adjusted*discount.rate),
         DALYS.avert = (DALYS.avert*discount.rate)
  )%>%na.omit()%>%
  group_by(location_name, Code, Intervention)%>%
  summarise(Incremental.cost = sum(Incremental.cost),
            Deaths.avert = sum(Deaths.avert),
            DALYS.avert = sum(DALYS.avert)
  )%>%
  mutate(Incremental.cost = ifelse(Deaths.avert==0, 0, Incremental.cost),
         ICER = Incremental.cost/DALYS.avert)%>%
  arrange(ICER)%>%filter(Code<5)%>%
  ungroup()%>%
  mutate(rank = row_number())

any(is.na(df))

#Add baseline coverage
cov<-read.csv("../new_inputs/Coverage0621.csv", stringsAsFactors = F)%>%
  filter(Country=="Mexico")%>%
  select(-WB_region15, -GBD_region, -Country, -iso3, -LocID)%>%
  gather(Code, baseline.coverage)%>%
  mutate(Code = as.numeric(gsub("X", "", Code)))

df<-left_join(df, cov)
  
write.csv(df, "figures/HLI_CEA_MX.csv", row.names = F)

#BCRs for intersectoral policies

df2<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  group_by(year_id, Code, Intervention, location_name)%>%
  summarise(Baseline.cost = sum(Baseline.cost),
            Adjusted.cost = sum(Adjusted.cost),
            Incremental.cost = sum(Incremental.cost),
            #Deaths.Baseline = sum(Deaths.Baseline),
            #Deaths.Adjusted = sum(Deaths.Adjusted),
            Deaths.avert = sum(Deaths.avert),
            DALYS.Baseline = sum(DALYS.Baseline),
            DALYS.Adjusted = sum(DALYS.Adjusted),
            DALYS.avert = sum(DALYS.avert)
  )%>%
  mutate(discount.rate = ((1-0.05)^(year_id-2022)),
         discount.rate = ifelse(year_id<2023,1,discount.rate),
         Baseline.cost = (Baseline.cost*discount.rate),
         Adjusted.cost = (Adjusted.cost*discount.rate),
         Incremental.cost = (Incremental.cost*discount.rate),
         DALYS.Baseline = (DALYS.Baseline*discount.rate),
         DALYS.Adjusted = (DALYS.Adjusted*discount.rate),
         DALYS.avert = (DALYS.avert*discount.rate)
  )%>%na.omit()%>%
  group_by(location_name, Code, Intervention, year_id)%>%
  summarise(Incremental.cost = sum(Incremental.cost),
            Deaths.avert = sum(Deaths.avert),
            DALYS.avert = sum(DALYS.avert)
  )%>%
  mutate(Incremental.cost = ifelse(Deaths.avert==0, 0, Incremental.cost),
         ICER = Incremental.cost/DALYS.avert)


ccc.vsl<-read.csv("../DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)),
         wb2021 = ifelse(wb2021=="UMC", "UMIC", wb2021))%>%
  filter(wb2021=="UMIC")

unique(ccc.vsl$wb2021)

df.bcr<-left_join(df2, ccc.vsl)%>%
  filter(year_id>=2023)%>%
  ungroup()%>%
  select(-wb2021, -location_name, -ICER)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.003), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
  )%>%
  group_by(Code, Intervention)%>%
  summarise(
    Incremental.cost = sum(Incremental.cost, na.rm=T),
    #Deaths.Baseline = sum(Deaths.Baseline),
    #Deaths.Adjusted = sum(Deaths.Adjusted),
    Deaths.avert = sum(Deaths.avert),
    DALYS.avert = sum(DALYS.avert, na.rm=T),
    Gross.benefits = sum(Gross.benefits, na.rm=T),
    Forgone.surplus = sum(Forgone.surplus, na.rm=T))%>%
  ungroup()%>%
  mutate(BCR = Gross.benefits / (Forgone.surplus+Incremental.cost),
         BCR.without.surplus = Gross.benefits/Incremental.cost)%>%
  arrange(-BCR)%>%
  filter(Code>5)

write.csv(df.bcr, "figures/BCR_calcs_MX.csv", row.names = F)


