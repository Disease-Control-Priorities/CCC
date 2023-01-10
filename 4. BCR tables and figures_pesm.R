setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

load("for_parallel_processing/output2023_target_pesm.Rda")
all.locs<-unique(clinical.dadt2$location_name)
#add intersectoral results#
load("for_parallel_processing/output2023_target_intersectoral_pesm.Rda")

deaths.averted<-bind_rows(clinical.dadt2, dadt.all)%>%
  group_by(Code, year_id, location_name)%>%
  summarise(
    #Deaths.Baseline = sum(NCD.Deaths0),
    #Deaths.Adjusted = sum(NCD.Deaths1),
    Deaths.avert = sum(Deaths.Avert))%>%
  mutate(year_id = as.numeric(year_id))

dalys.averted<-bind_rows(clinical.dalys2, all.dalys)%>%
  group_by(Code, year_id, location_name)%>%
  summarise(DALYS.Adjusted = sum(Adjusted),
            DALYS.Baseline = sum(Baseline),
            DALYS.avert = sum(DALY.ave),
            DALYS.avert =ifelse(DALYS.avert<1, 0, DALYS.avert))%>%
  mutate(year_id = as.numeric(year_id))

pin<-bind_rows(clinical.pin2, all.pin)%>%
  mutate(unique_id = paste0("C",Code,"_", sub_id))%>%
  select(-sub_id)

unique(pin$Code)

uc<-read.csv("output/unit_costs/Zimbabwe_adjusted_uc_2020.csv", stringsAsFactors = F)%>%
  mutate(location_name = "Zimbabwe")

for(i in all.locs[2:77]){
  uc<-bind_rows(uc, read.csv(paste0("output/unit_costs/",i,"_adjusted_uc_2020.csv"), stringsAsFactors = F)%>%
                  mutate(location_name = i))
}

cost<-left_join(pin, uc)%>%
  mutate(cost = adjusted_uc*pin)%>%
  group_by(Code, group, year_id, Intervention, location_name)%>%
  summarise(cost = sum(cost))%>%
  spread(group, cost)%>%
  rename(Adjusted.cost = Adjusted,
         Baseline.cost = Baseline)%>%
  mutate(Incremental.cost = Adjusted.cost - Baseline.cost)

WB<-read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  select(iso3, location_gbd, wb2021)%>%
  rename(location_name = location_gbd)

df<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  left_join(.,WB)%>%
  group_by(wb2021, year_id, Code, Intervention)%>%
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
  mutate(discount.rate = ((1-0.08)^(year_id-2022)),
         discount.rate = ifelse(year_id<2023,1,discount.rate),
         Baseline.cost = (Baseline.cost*discount.rate),
         Adjusted.cost = (Adjusted.cost*discount.rate),
         Incremental.cost = (Incremental.cost*discount.rate),
         DALYS.Baseline = (DALYS.Baseline*discount.rate),
         DALYS.Adjusted = (DALYS.Adjusted*discount.rate),
         DALYS.avert = (DALYS.avert*discount.rate)
  )

#Create "LIC/LMC"#
both<-df%>%
  na.omit()%>%
  group_by(year_id, Code, Intervention)%>%
  summarise(discount.rate = mean(discount.rate),
            Baseline.cost = sum(Baseline.cost),
            Adjusted.cost = sum(Adjusted.cost),
            Incremental.cost = sum(Incremental.cost),
            Deaths.avert = sum(Deaths.avert),
            DALYS.Baseline = sum(DALYS.Baseline),
            DALYS.Adjusted = sum(DALYS.Adjusted),
            DALYS.avert = sum(DALYS.avert))%>%
  mutate(wb2021 = "LIC/LMC")

df<-bind_rows(df, both)

ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)))


df.cost<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2022)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.0156), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.0023), Forgone.surplus)
  )

#write.csv(df.cost, "output/cost_outputs.csv", row.names = F)
  
df<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2023)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.0156), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.0023), Forgone.surplus)
  )%>%
  group_by(Code, Intervention, wb2021)%>%
  summarise(
    Baseline.cost = sum(Baseline.cost, na.rm=T),
    Adjusted.cost = sum(Adjusted.cost, na.rm=T),
    Incremental.cost = sum(Incremental.cost, na.rm=T),
    #Deaths.Baseline = sum(Deaths.Baseline),
    #Deaths.Adjusted = sum(Deaths.Adjusted),
    Deaths.avert = sum(Deaths.avert),
    DALYS.Baseline = sum(DALYS.Baseline, na.rm=T),
    DALYS.Adjusted = sum(DALYS.Adjusted, na.rm=T),
    DALYS.avert = sum(DALYS.avert, na.rm=T),
    Gross.benefits = sum(Gross.benefits, na.rm=T),
    Forgone.surplus = sum(Forgone.surplus, na.rm=T))%>%
  ungroup()%>%
  mutate(BCR = Gross.benefits / (Forgone.surplus+Incremental.cost),
         BCR.without.surplus = Gross.benefits/Incremental.cost)%>%
  arrange(wb2021, -BCR)%>%
  mutate(Intervention = gsub("policy", "regulations", Intervention),
         wb2021 = ifelse(wb2021=="LMIC", "LMC", wb2021))%>%
  rename(WB_Region = wb2021)%>%na.omit()


write.csv(df, "Figures/clinical_full_2023_2030_pesm.csv")

###
#5 percent discount rate
###

df<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  left_join(.,WB)%>%
  group_by(wb2021, year_id, Code, Intervention)%>%
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
  )

#Create "LIC/LMC"#
both<-df%>%
  na.omit()%>%
  group_by(year_id, Code, Intervention)%>%
  summarise(discount.rate = mean(discount.rate),
            Baseline.cost = sum(Baseline.cost),
            Adjusted.cost = sum(Adjusted.cost),
            Incremental.cost = sum(Incremental.cost),
            Deaths.avert = sum(Deaths.avert),
            DALYS.Baseline = sum(DALYS.Baseline),
            DALYS.Adjusted = sum(DALYS.Adjusted),
            DALYS.avert = sum(DALYS.avert))%>%
  mutate(wb2021 = "LIC/LMC")

df<-bind_rows(df, both)

df<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2023)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.0156), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.0023), Forgone.surplus)
  )%>%
  group_by(Code, Intervention, wb2021)%>%
  summarise(
    Baseline.cost = sum(Baseline.cost, na.rm=T),
    Adjusted.cost = sum(Adjusted.cost, na.rm=T),
    Incremental.cost = sum(Incremental.cost, na.rm=T),
    #Deaths.Baseline = sum(Deaths.Baseline),
    #Deaths.Adjusted = sum(Deaths.Adjusted),
    Deaths.avert = sum(Deaths.avert),
    DALYS.Baseline = sum(DALYS.Baseline, na.rm=T),
    DALYS.Adjusted = sum(DALYS.Adjusted, na.rm=T),
    DALYS.avert = sum(DALYS.avert, na.rm=T),
    Gross.benefits = sum(Gross.benefits, na.rm=T),
    Forgone.surplus = sum(Forgone.surplus, na.rm=T))%>%
  ungroup()%>%
  mutate(BCR = Gross.benefits / (Forgone.surplus+Incremental.cost),
         BCR.without.surplus = Gross.benefits/Incremental.cost)%>%
  arrange(wb2021, -BCR)%>%
  mutate(Intervention = gsub("policy", "regulations", Intervention),
         wb2021 = ifelse(wb2021=="LMIC", "LMC", wb2021))%>%
  rename(WB_Region = wb2021)%>%na.omit()


write.csv(df, "Figures/clinical_full_2023_2030_pesm_5perc.csv")


###
#14 percent discount rate
###

df<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  left_join(.,WB)%>%
  group_by(wb2021, year_id, Code, Intervention)%>%
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
  mutate(discount.rate = ((1-0.14)^(year_id-2022)),
         discount.rate = ifelse(year_id<2023,1,discount.rate),
         Baseline.cost = (Baseline.cost*discount.rate),
         Adjusted.cost = (Adjusted.cost*discount.rate),
         Incremental.cost = (Incremental.cost*discount.rate),
         DALYS.Baseline = (DALYS.Baseline*discount.rate),
         DALYS.Adjusted = (DALYS.Adjusted*discount.rate),
         DALYS.avert = (DALYS.avert*discount.rate)
  )

#Create "LIC/LMC"#
both<-df%>%
  na.omit()%>%
  group_by(year_id, Code, Intervention)%>%
  summarise(discount.rate = mean(discount.rate),
            Baseline.cost = sum(Baseline.cost),
            Adjusted.cost = sum(Adjusted.cost),
            Incremental.cost = sum(Incremental.cost),
            Deaths.avert = sum(Deaths.avert),
            DALYS.Baseline = sum(DALYS.Baseline),
            DALYS.Adjusted = sum(DALYS.Adjusted),
            DALYS.avert = sum(DALYS.avert))%>%
  mutate(wb2021 = "LIC/LMC")

df<-bind_rows(df, both)

df<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2023)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.0156), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.0023), Forgone.surplus)
  )%>%
  group_by(Code, Intervention, wb2021)%>%
  summarise(
    Baseline.cost = sum(Baseline.cost, na.rm=T),
    Adjusted.cost = sum(Adjusted.cost, na.rm=T),
    Incremental.cost = sum(Incremental.cost, na.rm=T),
    #Deaths.Baseline = sum(Deaths.Baseline),
    #Deaths.Adjusted = sum(Deaths.Adjusted),
    Deaths.avert = sum(Deaths.avert),
    DALYS.Baseline = sum(DALYS.Baseline, na.rm=T),
    DALYS.Adjusted = sum(DALYS.Adjusted, na.rm=T),
    DALYS.avert = sum(DALYS.avert, na.rm=T),
    Gross.benefits = sum(Gross.benefits, na.rm=T),
    Forgone.surplus = sum(Forgone.surplus, na.rm=T))%>%
  ungroup()%>%
  mutate(BCR = Gross.benefits / (Forgone.surplus+Incremental.cost),
         BCR.without.surplus = Gross.benefits/Incremental.cost)%>%
  arrange(wb2021, -BCR)%>%
  mutate(Intervention = gsub("policy", "regulations", Intervention),
         wb2021 = ifelse(wb2021=="LMIC", "LMC", wb2021))%>%
  rename(WB_Region = wb2021)%>%na.omit()


write.csv(df, "Figures/clinical_full_2023_2030_pesm_14perc.csv")
