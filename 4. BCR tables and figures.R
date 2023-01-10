setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

load("for_parallel_processing/output2023_target_base.Rda")
all.locs<-unique(clinical.dadt2$location_name)
#add intersectoral results#
load("for_parallel_processing/output2023_target_intersectoral.Rda")

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
            Baseline.cost = sum(Baseline.cost, na.rm=T),
            Adjusted.cost = sum(Adjusted.cost, na.rm=T),
            Incremental.cost = sum(Incremental.cost, na.rm=T),
            Deaths.avert = sum(Deaths.avert, na.rm=T),
            DALYS.Baseline = sum(DALYS.Baseline, na.rm=T),
            DALYS.Adjusted = sum(DALYS.Adjusted, na.rm=T),
            DALYS.avert = sum(DALYS.avert, na.rm=T))%>%
  mutate(wb2021 = "LIC/LMC")

df<-bind_rows(df, both)

unique(df$wb2021)

ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)))

unique(ccc.vsl$wb2021)

df.cost<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2022)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
  )

unique(df.cost$wb2021)

write.csv(df.cost, "output/cost_outputs.csv", row.names = F)
  
df<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2023)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
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
  rename(WB_Region = wb2021)%>%filter(BCR!=Inf)

write.csv(df, "Figures/clinical_full_2023_2030.csv")

#######################################
## bubble chart ##
#######################################

library(ggplot2)
library(scales)
breaks <- axTicks(side=2)

plot<-df%>%
  filter(WB_Region !="LIC/LMC")%>%
  select(WB_Region, Code, Intervention, BCR, DALYS.avert)%>%
  mutate(`Intervention package` = ifelse(Code %in%c(1.2,2.3,2.4,2.5,2.6,2.7,2.14), "Outpatient cardiometabolic and \nrespiratory disease package",
                                         ifelse(Code %in% c(1.1,2.9,2.10,2.11,2.12,2.13), "Outpatient mental, neurological, and \nsubstance use disorder package",
                                                ifelse(Code %in% c(3.1, 3.2, 3.4, 3.5), "First-level hospital cardiometabolic \nand respiratory disease package",
                                                       ifelse(Code %in% c(2.8,3.3,3.6,3.7,3.8,3.9), "First-level hospital surgical package", 
                                                              ifelse(Code %in% c(4.1,4.2,4.3,4.4), "Referral hospital NCDs package", 
                                                                     "Intersectoral policies"))))))%>%
  mutate(type = ifelse(Code>5, "Intersectoral policies", "Clinical interventions"))

plot<-plot%>%
  mutate(order = ifelse(Code>=5.5, 60, BCR),
         order = ifelse(Intervention=="Alcohol tax", 65, order))


  ggplot(plot, aes(y=reorder(Intervention, order), x=(BCR), size = DALYS.avert/1e6, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  facet_wrap(~WB_Region)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+ 
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_8perc.jpeg", height=6, width=10)


ggplot(plot%>%rename(Region = WB_Region), aes(y=reorder(Intervention, order), x=(BCR), size = DALYS.avert/1e6, 
                                              color=Region)) +
  geom_point(alpha=0.6)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_8perc_alt.jpeg", height=6, width=10)




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
            Deaths.avert = sum(Deaths.avert, na.rm=T),
            DALYS.Baseline = sum(DALYS.Baseline),
            DALYS.Adjusted = sum(DALYS.Adjusted),
            DALYS.avert = sum(DALYS.avert))%>%
  mutate(wb2021 = "LIC/LMC")

df<-bind_rows(df, both)

df.cost<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2022)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
  )

df<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2023)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
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


write.csv(df, "Figures/clinical_full_2023_2030_5perc.csv")


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
            Deaths.avert = sum(Deaths.avert, na.rm=T),
            DALYS.Baseline = sum(DALYS.Baseline),
            DALYS.Adjusted = sum(DALYS.Adjusted),
            DALYS.avert = sum(DALYS.avert))%>%
  mutate(wb2021 = "LIC/LMC")

df<-bind_rows(df, both)

df.cost<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2022)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
  )

df<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2023)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
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


write.csv(df, "Figures/clinical_full_2023_2030_14perc.csv")
