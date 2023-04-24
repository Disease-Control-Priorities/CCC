rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

load("../for_parallel_processing/output2023_target_base.Rda")
clinical.dadt<-clinical.dadt2
clinical.dalys<-clinical.dalys2
clinical.pin<-clinical.pin2

#add UMICs
load("../for_parallel_processing/output2023_target_UMIC.Rda")
clinical.dadt2<-bind_rows(clinical.dadt, clinical.dadt2)
clinical.dalys2<-bind_rows(clinical.dalys, clinical.dalys2)
clinical.pin2<-bind_rows(clinical.pin, clinical.pin2)

all.locs<-unique(clinical.dadt2$location_name)

#add intersectoral results#
load("../for_parallel_processing/output2023_target_intersectoral.Rda")
dadt.all2<-dadt.all
all.pin2<-all.pin
all.dalys2<-all.dalys

load("../for_parallel_processing/output2023_target_intersectoral_UMICs.Rda")
dadt.all<-bind_rows(dadt.all, dadt.all2)
all.dalys<-bind_rows(all.dalys, all.dalys2)
all.pin<-bind_rows(all.pin, all.pin2)

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

uc<-read.csv("../output/unit_costs/Zimbabwe_adjusted_uc_2020.csv", stringsAsFactors = F)%>%
  mutate(location_name = "Zimbabwe")

for(i in all.locs[2:124]){
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
  left_join(., read.csv("../new_inputs/HS123.csv", stringsAsFactors = F)%>%
  select(ISO3, HSP)%>%
  rename(iso3 = ISO3))%>%
  mutate(HSP = gsub(" ", "", HSP))%>%
  filter(HSP!="NA")

exclude<-c("Turkey", "Vanuatu", "Maldives", "Moldova")

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
write.csv(df, "figures/HLI_CEA_region.csv", row.names = F)


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
  group_by(location_name, Code, Intervention, HSP)%>%
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

#fig1<-bind_rows(fig1, fig1.all)

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

all.locs<-unique(df$location_name)
write.csv(cost%>%filter(location_name %in% all.locs), "output/all_costs.csv", row.names = F)


#NEED TO UPDATE#
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
  mutate(Gross.benefits = val*DALYS.avert,
         Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
         Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
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

table2<-df.bcr%>%select(HSP, Intervention, BCR)%>%
  mutate(BCR = signif(BCR, digits=2))%>%
  spread(HSP, BCR)%>%
  arrange(conflict)

write.csv(table2, "figures/TableX.csv", row.names = F)
