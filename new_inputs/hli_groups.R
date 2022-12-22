setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)

hli<-read.csv("region_groups.csv", stringsAsFactors = F)

new<-left_join(df, hli%>%rename(location_gbd = location_name, HLI.group = Country.group))%>%
  select(Region, Super_region, iso3, LocID, SDI, wb2021, 
         location_wb, location_who, gbd2019, HLI.group, NCD_region, location_gbd)%>%
  rename(countryname_WB = location_wb,
         countryname_WHO = location_who,
         countryname_GBD = gbd2019,
         HLI_group = HLI.group)


new<-new%>%
  mutate(HLI_group = ifelse(countryname_GBD=="Cabo Verde", "Sub-Saharan Africa", HLI_group),
         HLI_group = ifelse(countryname_GBD=="Kyrgyzstan", "Eurasia and the Mediterranean", HLI_group),
         HLI_group = ifelse(countryname_GBD=="Lao People's Democratic Republic", "Eurasia and the Mediterranean", HLI_group),
         HLI_group = ifelse(countryname_GBD=="Palestine", "Eurasia and the Mediterranean", HLI_group),
         HLI_group = ifelse(countryname_GBD=="Taiwan (Province of China)", "High-income", HLI_group),
         HLI_group = ifelse(countryname_GBD=="United States of America", "High-income", HLI_group))


write.csv(new, "country_groupings.csv", row.names = F)
