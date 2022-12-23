
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("new_inputs/PreppedData2023.Rda")
#countries
hics<-read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  filter(wb2021=="HIC" | wb2021=="UMIC")%>%pull(location_gbd)
lics<-read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  filter(wb2021=="LIC")%>%pull(location_gbd)
lmics<-read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  filter(wb2021=="LMIC")%>%pull(location_gbd)
#problems with data from Palestine and Puerto Rico
all.locs      <- data.frame(loc=c(countries[c(1:118, 120:126, 128:175)]))
all.locs      <-as.character(all.locs%>%filter(loc%!in%hics)%>%pull(loc)) #77 countries

groups<-read.csv("new_inputs/Country_groupings_HLI.csv", stringsAsFactors = F)%>%
  select(location_name, iso3, countryname_WHO)

###########################################

###########################################
load("for_parallel_processing/output2023_base.Rda")

names<-read.csv("new_inputs/PIN_new.csv", stringsAsFactors = F)%>%
  select(NCD, Intervention)%>%
  rename(Code = NCD)%>% distinct()

df<-read.csv("output/bca_costs/All_increment_2030_base.csv", stringsAsFactors = F)%>%
  mutate(Code = as.numeric(gsub("_.*$", "", unique_id)))%>%
  group_by(year_id, Country, Code)%>%
  summarise(sum_increment_all = sum(incremental))%>%
  ungroup()%>%
  rename(location_name = Country)%>%
  left_join(., all.dalys%>%mutate(year_id = as.numeric(year_id))%>%
              select(year_id, Code, DALY.ave, location_name))


WB<-read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  select(iso3, location_gbd, wb2021)%>%
  rename(location_name = location_gbd)

df<-left_join(df, WB)%>%
  mutate(`Intervention package` = ifelse(Code %in%c(1.2,2.3,2.4,2.5,2.6,2.7,2.14), "Outpatient cardiometabolic and \nrespiratory disease package",
                                         ifelse(Code %in% c(1.1,2.9,2.10,2.11,2.12,2.13), "Outpatient mental, neurological, and \nsubstance use disorder package",
                                                ifelse(Code %in% c(3.1, 3.2, 3.4, 3.5), "First-level hospital cardiometabolic \nand respiratory disease package",
                                                       ifelse(Code %in% c(2.8,3.3,3.6,3.7,3.8,3.9), "First-level hospital surgical package", 
                                                              ifelse(Code %in% c(4.1,4.2,4.3,4.4), "Referral hospital NCDs package", 
                                                                     "Intersectoral policies")))))
  )

any(is.na(df$iso3))

#replace with CCC data
#gdp<-read.csv("new_inputs/gdp_pc.csv", stringsAsFactors = F, skip=4)%>% 
#  pivot_longer(X1960:X2021) %>% 
#  group_by(Country.Code) %>% 
#  fill(value, .direction = "down") %>% 
#  pivot_wider(Country.Code)%>%
#  select(Country.Code, X2021)%>%
#  rename(iso3 = Country.Code, gdp_pc = X2021)%>%
#  left_join(., groups%>%select(-countryname_WHO))


newdf<-df%>%
  filter(location_name %in% all.locs)%>%
  mutate(`Intervention package` = ifelse(Code==5.4,"Alcohol regulations",
                                  ifelse(Code==5.2, "Alcohol tax",
                                  ifelse(Code==5.1, "Tobacco tax",
                                  ifelse(Code==5.3, "Tobacco regulations",
                                  ifelse(Code==5.5, "Sodium regulations", 
                                  ifelse(Code==5.6, "Trans fat regulations", `Intervention package`)))))))%>%
  mutate(DALY.benefit = ifelse(Code%in%c(5.1,5.2,5.3,5.4), DALY.ave*0.10, DALY.ave), #consumer surplus
         DALY.benefit = ifelse(Code %in% c(5.5,5.6), DALY.benefit*0.50, DALY.benefit),  #consumer surplus
         DALY.ave = ((1-0.08)^(year_id-2022))*DALY.ave, #discount at 8%
         DALY.benefit = ((1-0.08)^(year_id-2022))*DALY.benefit, #discount at 8%
         sum_increment_all =  ((1-0.08)^(year_id-2022))*sum_increment_all)%>% #discount at 8%
  group_by(wb2021, Code, `Intervention package`, year_id)%>%
  summarise(DALY.ave = sum(DALY.ave, na.rm=T),
            DALY.benefit = sum(DALY.benefit, na.rm=T),
            sum_increment_all = sum(sum_increment_all, na.rm=T))

ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)))

df_code<-left_join(newdf, ccc.vsl)%>%
  mutate(benefit = val*DALY.benefit)%>%
  group_by(Code, `Intervention package`, wb2021)%>%
  summarise(`Incremental cost (billions)` = sum(sum_increment_all, na.rm=T)/1e9,
            `DALY.ave (millions)` = sum(DALY.ave, na.rm=T)/1e6,
            `benefit (billions)`=sum(benefit, na.rm=T)/1e9,
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  left_join(., names)%>%
  arrange(-BCR)%>%
  filter(Intervention!="Heart failure acute treatmentC")%>%
  mutate(Intervention = ifelse(Code>5, `Intervention package`, Intervention))

df_both<-left_join(newdf, ccc.vsl)%>%
  mutate(benefit = val*DALY.benefit)%>%
  group_by(Code, `Intervention package`)%>%
  summarise(`Incremental cost (billions)` = sum(sum_increment_all, na.rm=T)/1e9,
            `DALY.ave (millions)` = sum(DALY.ave, na.rm=T)/1e6,
            `benefit (billions)`=sum(benefit, na.rm=T)/1e9,
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  left_join(., names)%>%
  arrange(-BCR)%>%
  filter(Intervention!="Heart failure acute treatmentC")%>%
  mutate(Intervention = ifelse(Code>5, `Intervention package`, Intervention),
         wb2021 = "LIC+LMIC")

#add intervention names
write.csv(bind_rows(df_code, df_both)%>%arrange(wb2021), "Figures/BCRs_int.csv")

#get country-specific BCRs for optimal benefit package
cdf<-df%>%
  filter(location_name %in% all.locs)%>%
  mutate(DALY.ave = ifelse(DALY.ave<1,0,DALY.ave))%>%
  mutate(`Intervention package` = ifelse(Code==5.4,"Alcohol regulations",
                                         ifelse(Code==5.2, "Alcohol tax",
                                                ifelse(Code==5.1, "Tobacco tax",
                                                       ifelse(Code==5.3, "Tobacco regulations",
                                                              ifelse(Code==5.5, "Sodium regulations", 
                                                                     ifelse(Code==5.6, "Trans fat regulations", `Intervention package`)))))))%>%
  mutate(DALY.benefit = ifelse(Code%in%c(5.1,5.2,5.3,5.4), DALY.ave*0.10, DALY.ave), #consumer surplus
         DALY.benefit = ifelse(Code %in% c(5.5,5.6), DALY.benefit*0.50, DALY.benefit),  #consumer surplus
         DALY.ave = ((1-0.08)^(year_id-2022))*DALY.ave, #discount at 8%
         DALY.benefit = ((1-0.08)^(year_id-2022))*DALY.benefit, #discount at 8%
         sum_increment_all =  ((1-0.08)^(year_id-2022))*sum_increment_all)%>% #discount at 8%
  group_by(location_name, wb2021, Code, `Intervention package`, year_id)%>%
  summarise(DALY.ave = sum(DALY.ave, na.rm=T),
            DALY.benefit = sum(DALY.benefit, na.rm=T),
            sum_increment_all = sum(sum_increment_all, na.rm=T))


cdf_code<-left_join(cdf, ccc.vsl)%>%
  mutate(benefit = val*DALY.benefit)%>%
  group_by(Code, `Intervention package`, location_name)%>%
  summarise(`Incremental cost (billions)` = sum(sum_increment_all, na.rm=T)/1e9,
            `DALY.ave (millions)` = sum(DALY.ave, na.rm=T)/1e6,
            `benefit (billions)`=sum(benefit, na.rm=T)/1e9,
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  left_join(., names)%>%
  arrange(-BCR)%>%
  filter(Intervention!="Heart failure acute treatmentC")%>%
  mutate(Intervention = ifelse(Code>5, `Intervention package`, Intervention))%>%
  mutate(BCR = ifelse(BCR==Inf,0,BCR))

write.csv(cdf_code, "Figures/BCRs_int_bycountry.csv")

#how much from int
DA<-sum(df_code$`DALY.ave (millions)`)
int<-sum(df_code$`DALY.ave (millions)`[df_code$Code>5])
hpp<-sum(df_code%>%filter(Code %in% c(5.1,5.3,2.4,2.6))%>%pull(`DALY.ave (millions)`))

int/DA
hpp/DA

BA<-sum(df_code$`benefit (billions)`)
bint<-sum(df_code$`benefit (billions)`[df_code$Code>5])
bhpp<-sum(df_code%>%filter(Code %in% c(5.1,5.3,2.4,2.6))%>%pull(`benefit (billions)`))

bint/BA
bhpp/BA


##by package##

df_pkg<-bind_rows(df_code, df_both)%>%arrange(wb2021)%>%
  group_by(`Intervention package`, wb2021)%>%
  summarise(`Incremental cost (billions)` = sum(`Incremental cost (billions)`, na.rm=T),
           `DALY.ave (millions)` = sum(`DALY.ave (millions)`, na.rm=T),
           `benefit (billions)`=sum(`benefit (billions)`, na.rm=T),
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  arrange(wb2021, -BCR)

write.csv(df_pkg, "Figures/BCRs_pkg.csv")


### bubble chart ###

library(ggplot2)
library(scales)
breaks <- axTicks(side=2)


df_both%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+ 
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_log.jpeg", height=8, width=10)

df_both%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))

ggsave("Figures/bubbleplot.jpeg", height=8, width=10)


df_code%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  facet_wrap(~wb2021)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+ 
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_log_region.jpeg", height=8, width=12)


###########################################
#run again for 5% discounting 
###########################################

df<-read.csv("output/bca_costs/All_increment_2030_base.csv", stringsAsFactors = F)%>%
  mutate(Code = as.numeric(gsub("_.*$", "", unique_id)))%>%
  group_by(year_id, Country, Code)%>%
  summarise(sum_increment_all = sum(incremental))%>%
  ungroup()%>%
  rename(location_name = Country)%>%
  left_join(., all.dalys%>%mutate(year_id = as.numeric(year_id))%>%
              select(year_id, Code, DALY.ave, location_name))


df<-left_join(df, WB)%>%
  mutate(`Intervention package` = ifelse(Code %in%c(1.2,2.3,2.4,2.5,2.6,2.7,2.14), "Outpatient cardiometabolic and \nrespiratory disease package",
                                         ifelse(Code %in% c(1.1,2.9,2.10,2.11,2.12,2.13), "Outpatient mental, neurological, and \nsubstance use disorder package",
                                                ifelse(Code %in% c(3.1, 3.2, 3.4, 3.5), "First-level hospital cardiometabolic \nand respiratory disease package",
                                                       ifelse(Code %in% c(2.8,3.3,3.6,3.7,3.8,3.9), "First-level hospital surgical package", 
                                                              ifelse(Code %in% c(4.1,4.2,4.3,4.4), "Referral hospital NCDs package", 
                                                                     "Intersectoral policies")))))
  )

any(is.na(df$iso3))

#replace with CCC data
#gdp<-read.csv("new_inputs/gdp_pc.csv", stringsAsFactors = F, skip=4)%>% 
#  pivot_longer(X1960:X2021) %>% 
#  group_by(Country.Code) %>% 
#  fill(value, .direction = "down") %>% 
#  pivot_wider(Country.Code)%>%
#  select(Country.Code, X2021)%>%
#  rename(iso3 = Country.Code, gdp_pc = X2021)%>%
#  left_join(., groups%>%select(-countryname_WHO))


newdf<-df%>%
  filter(location_name %in% all.locs)%>%
  mutate(`Intervention package` = ifelse(Code==5.4,"Alcohol regulations",
                                         ifelse(Code==5.2, "Alcohol tax",
                                                ifelse(Code==5.1, "Tobacco tax",
                                                       ifelse(Code==5.3, "Tobacco regulations",
                                                              ifelse(Code==5.5, "Sodium regulations", 
                                                                     ifelse(Code==5.6, "Trans fat regulations", `Intervention package`)))))))%>%
  mutate(DALY.benefit = ifelse(Code%in%c(5.1,5.2,5.3,5.4), DALY.ave*0.10, DALY.ave), #consumer surplus
         DALY.benefit = ifelse(Code %in% c(5.5,5.6), DALY.benefit*0.50, DALY.benefit),  #consumer surplus
         DALY.ave = ((1-0.05)^(year_id-2022))*DALY.ave, #discount at 5%
         DALY.benefit = ((1-0.05)^(year_id-2022))*DALY.benefit, #discount at 5%
         sum_increment_all =  ((1-0.05)^(year_id-2022))*sum_increment_all)%>% #discount at 5%
  group_by(wb2021, Code, `Intervention package`, year_id)%>%
  summarise(DALY.ave = sum(DALY.ave, na.rm=T),
            DALY.benefit = sum(DALY.benefit, na.rm=T),
            sum_increment_all = sum(sum_increment_all, na.rm=T))

ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)))

df_code<-left_join(newdf, ccc.vsl)%>%
  mutate(benefit = val*DALY.benefit)%>%
  group_by(Code, `Intervention package`, wb2021)%>%
  summarise(`Incremental cost (billions)` = sum(sum_increment_all, na.rm=T)/1e9,
            `DALY.ave (millions)` = sum(DALY.ave, na.rm=T)/1e6,
            `benefit (billions)`=sum(benefit, na.rm=T)/1e9,
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  left_join(., names)%>%
  arrange(-BCR)%>%
  filter(Intervention!="Heart failure acute treatmentC")%>%
  mutate(Intervention = ifelse(Code>5, `Intervention package`, Intervention))

df_both<-left_join(newdf, ccc.vsl)%>%
  mutate(benefit = val*DALY.benefit)%>%
  group_by(Code, `Intervention package`)%>%
  summarise(`Incremental cost (billions)` = sum(sum_increment_all, na.rm=T)/1e9,
            `DALY.ave (millions)` = sum(DALY.ave, na.rm=T)/1e6,
            `benefit (billions)`=sum(benefit, na.rm=T)/1e9,
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  left_join(., names)%>%
  arrange(-BCR)%>%
  filter(Intervention!="Heart failure acute treatmentC")%>%
  mutate(Intervention = ifelse(Code>5, `Intervention package`, Intervention),
         wb2021 = "LIC+LMIC")

#add intervention names
write.csv(bind_rows(df_code, df_both)%>%arrange(wb2021), "Figures/BCRs_int_5perc.csv")

#how much from int
DA<-sum(df_code$`DALY.ave (millions)`)
int<-sum(df_code$`DALY.ave (millions)`[df_code$Code>5])
hpp<-sum(df_code%>%filter(Code %in% c(5.1,5.3,2.4,2.6))%>%pull(`DALY.ave (millions)`))

int/DA
hpp/DA

BA<-sum(df_code$`benefit (billions)`)
bint<-sum(df_code$`benefit (billions)`[df_code$Code>5])
bhpp<-sum(df_code%>%filter(Code %in% c(5.1,5.3,2.4,2.6))%>%pull(`benefit (billions)`))

bint/BA
bhpp/BA


df_pkg<-bind_rows(df_code, df_both)%>%arrange(wb2021)%>%
  group_by(`Intervention package`, wb2021)%>%
  summarise(`Incremental cost (billions)` = sum(`Incremental cost (billions)`, na.rm=T),
            `DALY.ave (millions)` = sum(`DALY.ave (millions)`, na.rm=T),
            `benefit (billions)`=sum(`benefit (billions)`, na.rm=T),
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  arrange(wb2021, -BCR)

write.csv(df_pkg, "Figures/BCRs_pkg_5perc.csv")


### bubble chart ###

library(ggplot2)
library(scales)
breaks <- axTicks(side=2)


df_both%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+ 
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_log_5perc.jpeg", height=8, width=10)

df_both%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))

ggsave("Figures/bubbleplot_5perc.jpeg", height=8, width=10)


df_code%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  facet_wrap(~wb2021)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+ 
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_log_region_5perc.jpeg", height=8, width=12)


###########################################
#run again for 14% discounting 
###########################################

df<-read.csv("output/bca_costs/All_increment_2030_base.csv", stringsAsFactors = F)%>%
  mutate(Code = as.numeric(gsub("_.*$", "", unique_id)))%>%
  group_by(year_id, Country, Code)%>%
  summarise(sum_increment_all = sum(incremental))%>%
  ungroup()%>%
  rename(location_name = Country)%>%
  left_join(., all.dalys%>%mutate(year_id = as.numeric(year_id))%>%
              select(year_id, Code, DALY.ave, location_name))


df<-left_join(df, WB)%>%
  mutate(`Intervention package` = ifelse(Code %in%c(1.2,2.3,2.4,2.5,2.6,2.7,2.14), "Outpatient cardiometabolic and \nrespiratory disease package",
                                         ifelse(Code %in% c(1.1,2.9,2.10,2.11,2.12,2.13), "Outpatient mental, neurological, and \nsubstance use disorder package",
                                                ifelse(Code %in% c(3.1, 3.2, 3.4, 3.5), "First-level hospital cardiometabolic \nand respiratory disease package",
                                                       ifelse(Code %in% c(2.8,3.3,3.6,3.7,3.8,3.9), "First-level hospital surgical package", 
                                                              ifelse(Code %in% c(4.1,4.2,4.3,4.4), "Referral hospital NCDs package", 
                                                                     "Intersectoral policies")))))
  )

any(is.na(df$iso3))

#replace with CCC data
#gdp<-read.csv("new_inputs/gdp_pc.csv", stringsAsFactors = F, skip=4)%>% 
#  pivot_longer(X1960:X2021) %>% 
#  group_by(Country.Code) %>% 
#  fill(value, .direction = "down") %>% 
#  pivot_wider(Country.Code)%>%
#  select(Country.Code, X2021)%>%
#  rename(iso3 = Country.Code, gdp_pc = X2021)%>%
#  left_join(., groups%>%select(-countryname_WHO))


newdf<-df%>%
  filter(location_name %in% all.locs)%>%
  mutate(`Intervention package` = ifelse(Code==5.4,"Alcohol regulations",
                                         ifelse(Code==5.2, "Alcohol tax",
                                                ifelse(Code==5.1, "Tobacco tax",
                                                       ifelse(Code==5.3, "Tobacco regulations",
                                                              ifelse(Code==5.5, "Sodium regulations", 
                                                                     ifelse(Code==5.6, "Trans fat regulations", `Intervention package`)))))))%>%
  mutate(DALY.benefit = ifelse(Code%in%c(5.1,5.2,5.3,5.4), DALY.ave*0.10, DALY.ave), #consumer surplus
         DALY.benefit = ifelse(Code %in% c(5.5,5.6), DALY.benefit*0.50, DALY.benefit),  #consumer surplus
         DALY.ave = ((1-0.14)^(year_id-2022))*DALY.ave, #discount at 14%
         DALY.benefit = ((1-0.14)^(year_id-2022))*DALY.benefit, #discount at 14%
         sum_increment_all =  ((1-0.14)^(year_id-2022))*sum_increment_all)%>% #discount at 14%
  group_by(wb2021, Code, `Intervention package`, year_id)%>%
  summarise(DALY.ave = sum(DALY.ave, na.rm=T),
            DALY.benefit = sum(DALY.benefit, na.rm=T),
            sum_increment_all = sum(sum_increment_all, na.rm=T))

ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)))

df_code<-left_join(newdf, ccc.vsl)%>%
  mutate(benefit = val*DALY.benefit)%>%
  group_by(Code, `Intervention package`, wb2021)%>%
  summarise(`Incremental cost (billions)` = sum(sum_increment_all, na.rm=T)/1e9,
            `DALY.ave (millions)` = sum(DALY.ave, na.rm=T)/1e6,
            `benefit (billions)`=sum(benefit, na.rm=T)/1e9,
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  left_join(., names)%>%
  arrange(-BCR)%>%
  filter(Intervention!="Heart failure acute treatmentC")%>%
  mutate(Intervention = ifelse(Code>5, `Intervention package`, Intervention))

df_both<-left_join(newdf, ccc.vsl)%>%
  mutate(benefit = val*DALY.benefit)%>%
  group_by(Code, `Intervention package`)%>%
  summarise(`Incremental cost (billions)` = sum(sum_increment_all, na.rm=T)/1e9,
            `DALY.ave (millions)` = sum(DALY.ave, na.rm=T)/1e6,
            `benefit (billions)`=sum(benefit, na.rm=T)/1e9,
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  left_join(., names)%>%
  arrange(-BCR)%>%
  filter(Intervention!="Heart failure acute treatmentC")%>%
  mutate(Intervention = ifelse(Code>5, `Intervention package`, Intervention),
         wb2021 = "LIC+LMIC")

#add intervention names
write.csv(bind_rows(df_code, df_both)%>%arrange(wb2021), "Figures/BCRs_int_14perc.csv")

#how much from int
DA<-sum(df_code$`DALY.ave (millions)`)
int<-sum(df_code$`DALY.ave (millions)`[df_code$Code>5])
hpp<-sum(df_code%>%filter(Code %in% c(5.1,5.3,2.4,2.6))%>%pull(`DALY.ave (millions)`))

int/DA
hpp/DA

BA<-sum(df_code$`benefit (billions)`)
bint<-sum(df_code$`benefit (billions)`[df_code$Code>5])
bhpp<-sum(df_code%>%filter(Code %in% c(5.1,5.3,2.4,2.6))%>%pull(`benefit (billions)`))

bint/BA
bhpp/BA


df_pkg<-bind_rows(df_code, df_both)%>%arrange(wb2021)%>%
  group_by(`Intervention package`, wb2021)%>%
  summarise(`Incremental cost (billions)` = sum(`Incremental cost (billions)`, na.rm=T),
            `DALY.ave (millions)` = sum(`DALY.ave (millions)`, na.rm=T),
            `benefit (billions)`=sum(`benefit (billions)`, na.rm=T),
            BCR =  `benefit (billions)`/`Incremental cost (billions)`)%>%
  arrange(wb2021, -BCR)

write.csv(df_pkg, "Figures/BCRs_pkg_14perc.csv")


### bubble chart ###

library(ggplot2)
library(scales)
breaks <- axTicks(side=2)


df_both%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+ 
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_log_14perc.jpeg", height=8, width=10)

df_both%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))

ggsave("Figures/bubbleplot_14perc.jpeg", height=8, width=10)


df_code%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  facet_wrap(~wb2021)+
  scale_size(range=c(1,12), breaks=c(1,5,10,20), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+ 
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_log_region_14perc.jpeg", height=8, width=12)
