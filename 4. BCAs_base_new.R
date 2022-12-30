
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
load("for_parallel_processing/output2023_base_india.Rda")

names<-readxl::read_excel("new_inputs/DCP3_ NCD Data (1).xlsx", sheet='costs')%>%
  select(NCD, Intervention)%>%
  rename(Code = NCD)%>% distinct()

df<-read.csv("output/bca_costs/All_increment_2030_india.csv", stringsAsFactors = F)%>%
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
  mutate(DALY.ave = ifelse(DALY.ave<2, 0, DALY.ave),
         DALY.ave = ((1-0.08)^(year_id-2022))*DALY.ave, #discount at 8%
         sum_increment_all =  ((1-0.08)^(year_id-2022))*sum_increment_all)%>% #discount at 8%
  group_by(wb2021, Code, `Intervention package`, year_id)%>%
  summarise(DALY.ave = sum(DALY.ave, na.rm=T),
            sum_increment_all = sum(sum_increment_all, na.rm=T))

ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
  gather(year_id, val, -wb2021)%>%
  mutate(year_id = as.numeric(gsub("X","",year_id)))

df_code<-left_join(newdf, ccc.vsl)%>%
  mutate(benefit = val*DALY.ave,
         cost.adjusted = ifelse(Code%in%c(5.1,5.2,5.3,5.4), sum_increment_all+(benefit*0.009), sum_increment_all), #consumer surplus
         cost.adjusted = ifelse(Code %in% c(5.5,5.6), cost.adjusted+(benefit*0.001), cost.adjusted))%>%  #consumer surplus)%>%
  group_by(Code, `Intervention package`, wb2021)%>%
  summarise(`Incremental cost (billions)` = sum(sum_increment_all, na.rm=T)/1e9,
            `Adjusted cost (billions)` = sum(cost.adjusted, na.rm=T)/1e9,
            `DALY.ave (millions)` = sum(DALY.ave, na.rm=T)/1e6,
            `benefit (billions)`=sum(benefit, na.rm=T)/1e9,
            BCR =  `benefit (billions)`/`Adjusted cost (billions)`)%>%
  left_join(., names)%>%
  arrange(-BCR)%>%
  filter(Intervention!="Heart failure acute treatmentC")%>%
  mutate(Intervention = ifelse(Code>5, `Intervention package`, Intervention))

#add intervention names
write.csv(df_code, "Figures/BCRs_int_india.csv")

### bubble chart ###

library(ggplot2)
library(scales)
breaks <- axTicks(side=2)

any(is.na(df_code))

df_code%>%
  mutate(BCR = ifelse(is.na(BCR),0,BCR))%>%
  mutate(`Intervention package` = ifelse(Code>5, "Intersectoral polices", `Intervention package`))%>%
  ggplot(aes(y=reorder(Intervention, BCR), x=(BCR), size = `DALY.ave (millions)`, 
             color=`Intervention package`)) +
  geom_point(alpha=0.75)+
  scale_size(range=c(1,12), breaks=c(0.1,0.5,1,5,10), name="DALYs averted (millions)")+
  ylab("")+
  theme_minimal()+
  xlab("Benefit-cost ratio")+ 
  guides(color = guide_legend(override.aes = list(size=3)))+ 
  scale_x_log10(breaks = log_breaks())

ggsave("Figures/bubbleplot_india.jpeg", height=8, width=10)


#detailed results#



