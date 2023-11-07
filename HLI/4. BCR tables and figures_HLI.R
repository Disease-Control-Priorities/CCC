rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

#intersectoral results#
load("../for_parallel_processing/output2050_target_intersectoral_ssb.Rda")
dadt.all2<-dadt.all
all.pin2<-all.pin
all.dalys2<-all.dalys

#exclude Panama and Romania as they are now HICs
#exclude Serbia
#should be 114
all.locs<-unique(all.pin$location_name[all.pin$location_name!="Romania" & all.pin$location_name!="Panama" & all.pin$location_name!="Serbia"])

#clinical results#
load(paste0("../for_parallel_processing/for_HLI/Afghanistan_output2050_target.Rda"))

clinical.dadt<-dadt.all
clinical.dalys<-all.dalys
clinical.pin<-all.pin

for(is in all.locs[2:114]){
  
  load(paste0("../for_parallel_processing/for_HLI/", is,"_output2050_target.Rda"))
  
  clinical.dadt<-bind_rows(clinical.dadt, dadt.all)
  clinical.dalys<-bind_rows(clinical.dalys, all.dalys)
  clinical.pin<-bind_rows(clinical.pin, all.pin)
}

######## Deaths averted ######## 
deaths.averted<-bind_rows(clinical.dadt, dadt.all2)%>%
  filter(location_name!="Serbia")%>%
  group_by(Code, year_id, location_name)%>%
  summarise(
    #Deaths.Baseline = sum(NCD.Deaths0),
    #Deaths.Adjusted = sum(NCD.Deaths1),
    Deaths.avert = sum(Deaths.Avert))%>%
  mutate(year_id = as.numeric(year_id))

######## DALYs averted ######## 
dalys.averted<-bind_rows(clinical.dalys, all.dalys2)%>%
  filter(location_name!="Serbia")%>%
  group_by(Code, year_id, location_name)%>%
  summarise(DALYS.Adjusted = sum(Adjusted),
            DALYS.Baseline = sum(Baseline),
            DALYS.avert = sum(DALY.ave),
            DALYS.avert =ifelse(DALYS.avert<1, 0, DALYS.avert))%>%
  mutate(year_id = as.numeric(year_id))

######## PIN ######## 
pin<-bind_rows(clinical.pin, all.pin2)%>%
  mutate(unique_id = paste0("C",Code,"_", sub_id))%>%
  select(-sub_id)

unique(pin$Code)

uc<-read.csv("../output/unit_costs/Afghanistan_adjusted_uc_2020.csv", stringsAsFactors = F)%>%
  mutate(location_name = "Afghanistan")

for(i in all.locs[2:114]){
  uc<-bind_rows(uc, read.csv(paste0("../output/unit_costs/",i,"_adjusted_uc_2020.csv"), stringsAsFactors = F)%>%
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

WB<-read.csv("../new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  select(iso3, location_gbd, wb2021)%>%
  rename(location_name = location_gbd)%>%
  left_join(., read.csv("../new_inputs/HS_123 revised.csv", stringsAsFactors = F)%>%
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

exclude<-c("Turkey", "Vanuatu", "Maldives", "Moldova")

##By HSP##
df<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  left_join(.,WB)%>%
  filter(location_name %!in% exclude)%>%
  group_by(HSP, year_id, Code, Intervention)%>%
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
  group_by(HSP, Code, Intervention)%>%
  summarise(Incremental.cost = sum(Incremental.cost),
            Deaths.avert = sum(Deaths.avert),
            DALYS.avert = sum(DALYS.avert)
  )%>%
  mutate(Incremental.cost = ifelse(Deaths.avert==0, 0, Incremental.cost),
          ICER = Incremental.cost/DALYS.avert)

##
unique(df$HSP)
write.csv(df, "figures/HLI_CEA_HSP.csv", row.names = F)

##By WB Class##
df<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  left_join(.,WB)%>%
  filter(location_name %!in% exclude)%>%
  group_by(wb2023, year_id, Code, Intervention)%>%
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
  group_by(wb2023, Code, Intervention)%>%
  summarise(Incremental.cost = sum(Incremental.cost),
            Deaths.avert = sum(Deaths.avert),
            DALYS.avert = sum(DALYS.avert)
  )%>%
  mutate(Incremental.cost = ifelse(Deaths.avert==0, 0, Incremental.cost),
         ICER = Incremental.cost/DALYS.avert)


##
unique(df$wb2023)
write.csv(df, "figures/HLI_CEA_WBclass.csv", row.names = F)


#in 2050
df<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  left_join(.,WB)%>%
  filter(location_name %!in% exclude)%>%
  group_by(wb2023, year_id, Code, Intervention)%>%
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
  group_by(wb2023, Code, Intervention, year_id)%>%
  summarise(Incremental.cost = sum(Incremental.cost),
            Deaths.avert = sum(Deaths.avert),
            DALYS.avert = sum(DALYS.avert)
  )%>%
  mutate(Incremental.cost = ifelse(Deaths.avert==0, 0, Incremental.cost),
         ICER = Incremental.cost/DALYS.avert)


write.csv(df%>%filter(year_id==2050), "figures/HLI_CEA_WB_2050.csv", row.names = F)


##By country##
df<-left_join(cost, deaths.averted)%>%
  left_join(., dalys.averted)%>%
  left_join(.,WB)%>%
  filter(location_name %!in% exclude)%>%
  group_by(year_id, Code, Intervention, location_name, HSP)%>%
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

any(is.na(df))
unique(df$location_name)

write.csv(df, "figures/HLI_CEA_country.csv", row.names = F)

##################
#FIGURE 1
##################

gdp<-read.csv("../new_inputs/gdp_pc.csv", stringsAsFactors = F, skip=4)%>% 
  pivot_longer(X1960:X2021) %>% 
  group_by(Country.Code) %>% 
  fill(value, .direction = "down") %>% 
  pivot_wider(Country.Code)%>%
  select(Country.Code, X2021)%>%
  rename(iso3 = Country.Code, gdp_pc = X2021)

library(forcats)

fig1<-left_join(df, WB)%>%
  filter(Code<5)%>%
  left_join(., gdp)%>%
  group_by(HSP, Intervention)%>%
  summarise(gdp_pc = median(gdp_pc, na.rm=T),
            DALYs.averted = sum(DALYS.avert),
            Incremental.cost = sum(Incremental.cost))%>%
  mutate(ICER = Incremental.cost/DALYs.averted)%>%
  arrange(HSP)%>%
  mutate(perc = ICER/gdp_pc,
         color = ifelse(perc<0.1, "<0.1",
                        ifelse(perc<0.5 & perc>=0.1, "0.1-0.5",
                               ifelse(perc>=0.5 & perc<1.0, "0.5-1.0",
                                      ifelse(perc<2.3 & perc>=1.0, "1.0-2.3", ">2.3")))))%>%
  na.omit()%>%
  mutate(color = ifelse(HSP=="conflict" & Intervention=="PCI for ACS", ">2.3", color))%>%
  mutate(color = factor(color, levels=c("<0.1", "0.1-0.5", "0.5-1.0", "1.0-2.3", ">2.3")),
         name = fct_reorder(as.factor(Intervention), desc(ICER)))%>%
  mutate(HSP = ifelse(HSP=="fragile", "Fragile", HSP),
         HSP = ifelse(HSP=="conflict", "Conflict", HSP))%>%
  arrange(ICER)

fig1.all<-left_join(df, WB)%>%
  filter(Code<5)%>%
  left_join(., gdp)%>%
  group_by(Intervention)%>%
  summarise(gdp_pc = median(gdp_pc, na.rm=T),
            DALYs.averted = sum(DALYS.avert),
            Incremental.cost = sum(Incremental.cost))%>%
  mutate(ICER = Incremental.cost/DALYs.averted)%>%
  mutate(perc = ICER/gdp_pc,
         color = ifelse(perc<0.1, "<0.1",
                        ifelse(perc<0.5 & perc>=0.1, "0.1-0.5",
                               ifelse(perc>=0.5 & perc<1.0, "0.5-1.0",
                                      ifelse(perc<2.3 & perc>=1.0, "1.0-2.3", ">2.3")))))%>%
  na.omit()%>%
  mutate(color = factor(color, levels=c("<0.1", "0.1-0.5", "0.5-1.0", "1.0-2.3", ">2.3")),
         name = fct_reorder(as.factor(Intervention), desc(ICER)))%>%
  mutate(HSP = "All countries")

ggplot(fig1, 
       aes(x=HSP, y=reorder(name, -ICER), fill=color))+
  geom_tile()+
  theme_classic()+
  theme(axis.text.x=element_text(size=12, angle=45, hjust=0),    
        axis.text.y=element_text(size=12))+
  scale_fill_manual(values=c("#337599","#99B8BD", "#FFEE9C","#faa175","#f8696b"))+
  scale_x_discrete(position="top")+
  labs(fill="ICER (as a proportion \nof GDP per capita)")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  geom_text(label = paste0( "$", signif(fig1$ICER, digits=2)))

ggsave("figures/Figure1.png", height = 10, width = 12, units = "in")
write.csv(fig1, "figures/figure1_data.csv", row.names = F)

write.csv(cost%>%filter(location_name %in% all.locs), "output/all_costs.csv", row.names = F)


#ALT FIGURE 1
fig1<-left_join(df, WB)%>%
  filter(Code<5)%>%
  left_join(., gdp)%>%
  group_by(wb2023, Intervention)%>%
  summarise(gdp_pc = median(gdp_pc, na.rm=T),
            DALYs.averted = sum(DALYS.avert),
            Incremental.cost = sum(Incremental.cost))%>%
  mutate(ICER = Incremental.cost/DALYs.averted)%>%
  arrange(wb2023)%>%
  mutate(perc = ICER/gdp_pc,
         color = ifelse(perc<0.1, "<0.1",
                        ifelse(perc<0.5 & perc>=0.1, "0.1-0.5",
                               ifelse(perc>=0.5 & perc<1.0, "0.5-1.0",
                                      ifelse(perc<2.3 & perc>=1.0, "1.0-2.3", ">2.3")))))%>%
  na.omit()%>%
  mutate(color = factor(color, levels=c("<0.1", "0.1-0.5", "0.5-1.0", "1.0-2.3", ">2.3")),
         name = fct_reorder(as.factor(Intervention), desc(ICER)))%>%
  arrange(ICER)

fig1.all<-left_join(df, WB)%>%
  filter(Code<5)%>%
  left_join(., gdp)%>%
  group_by(Intervention)%>%
  summarise(gdp_pc = median(gdp_pc, na.rm=T),
            DALYs.averted = sum(DALYS.avert),
            Incremental.cost = sum(Incremental.cost))%>%
  mutate(ICER = Incremental.cost/DALYs.averted)%>%
  mutate(perc = ICER/gdp_pc,
         color = ifelse(perc<0.1, "<0.1",
                        ifelse(perc<0.5 & perc>=0.1, "0.1-0.5",
                               ifelse(perc>=0.5 & perc<1.0, "0.5-1.0",
                                      ifelse(perc<2.3 & perc>=1.0, "1.0-2.3", ">2.3")))))%>%
  na.omit()%>%
  mutate(color = factor(color, levels=c("<0.1", "0.1-0.5", "0.5-1.0", "1.0-2.3", ">2.3")),
         name = fct_reorder(as.factor(Intervention), desc(ICER)))%>%
  mutate(wb2023 = "All countries")

fig1<-bind_rows(fig1, fig1.all)

ggplot(fig1, 
       aes(x=wb2023, y=reorder(name, -ICER), fill=color))+
  geom_tile()+
  theme_classic()+
  theme(axis.text.x=element_text(size=12, angle=45, hjust=0),    
        axis.text.y=element_text(size=12))+
  scale_fill_manual(values=c("#337599","#99B8BD", "#FFEE9C","#faa175","#f8696b"))+
  scale_x_discrete(position="top")+
  labs(fill="ICER (as a proportion \nof GDP per capita)")+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
  geom_text(label = paste0( "$", signif(fig1$ICER, digits=2)))

ggsave("figures/Figure1_WB.png", height = 10, width = 12, units = "in")

##########


#BCRs for intersectoral policies
ccc.vsl<-read.csv("../DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)),
         wb2021 = ifelse(wb2021=="UMC", "UMIC", wb2021))

unique(ccc.vsl$wb2021)

df<-left_join(df, WB)
unique(df$wb2021)

df.bcr<-left_join(df, ccc.vsl)%>%
  filter(year_id>=2023)%>%
  ungroup()%>%
  select(-iso3, -wb2021, -wb2023, -location_name, -ICER)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.003), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6,5.7), (Gross.benefits*0.001), Forgone.surplus)
  )%>%
  group_by(Code, Intervention, HSP)%>%
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
  arrange(HSP, -BCR)%>%
  filter(Code>5)

write.csv(df.bcr, "figures/BCR_calcs.csv", row.names = F)

table3<-df.bcr%>%select(HSP, Intervention, BCR)%>%
  mutate(BCR = signif(BCR, digits=2))%>%
  spread(HSP, BCR)%>%
  arrange(-conflict)

write.csv(table3, "figures/Table3.csv", row.names = F)

## BCRs by year ##
df.bcr2<-left_join(df, ccc.vsl, relationship = "many-to-many")%>%
  filter(Code>5)%>%
  ungroup()%>%
  select(-iso3, -wb2021, -wb2023, -location_name, -ICER)%>%
  filter(year_id>=2023 & year_id<=2050)%>%
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.003), 0), #consumer surplus 75% for tobacco and alcohol
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6, 5.7), (Gross.benefits*0.001), Forgone.surplus))%>% #50% for salt and trans fats
  group_by(Code,Intervention, HSP, year_id)%>%
  summarise(
    Incremental.cost = sum(Incremental.cost, na.rm=T),
    Deaths.avert = sum(Deaths.avert),
    DALYS.avert = sum(DALYS.avert),
    Gross.benefits = sum(Gross.benefits),
    Forgone.surplus = sum(Forgone.surplus))%>%
  ungroup()%>%
  mutate(BCR = Gross.benefits / (Forgone.surplus+Incremental.cost),
         BCR.without.surplus = Gross.benefits/Incremental.cost)%>%
  arrange(HSP, -BCR)%>%
  group_by(Code, Intervention, HSP)%>%
  arrange(year_id)%>%
  mutate(cumulative.benefits = cumsum(Gross.benefits),
         cumulative.surplus = cumsum(Forgone.surplus),
         cumulative.cost = cumsum(Incremental.cost),
         cumulative.deaths.avert = cumsum(Deaths.avert))%>%
  ungroup()%>%
  mutate(cBCR = cumulative.benefits/(cumulative.cost+cumulative.surplus))

ggplot(df.bcr2, aes(x=year_id, y=cBCR, color=Intervention))+
geom_line(size=1)+
facet_wrap(~HSP)

#ggsave("figures/troubleshoot_BCRs.jpeg", height=6, width=6)
