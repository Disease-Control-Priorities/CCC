rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(tidyr)
library(ggplot2)

#list of countries w/ good VR data: 
#see GBD 2019 appendix table S7 on p1494 
#https://doi.org/10.1016/S0140-6736(20)30925-9
#I would use only countries with 4 or 5 star rating in the “data quality rating” column.

locs<-read.csv("loc_data_quality.csv", skip=1)%>%
  select(location=Country, Data.Quality.Rating)%>%
  filter(Data.Quality.Rating>=4)%>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))%>%
  na.omit()

df<-read.csv("IHME-GBD_2019_DATA-03b2edf0-1.csv", stringsAsFactors = F)%>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))%>%
  filter(iso3 %in% locs$iso3)

unique(df$location)

fig2<-df%>%
  filter(sex=="Both")%>%
  group_by(cause)%>%
  summarise(`0%` = quantile(val, probs=0),
            `25%` = quantile(val, probs=0.25),
            `50%` = quantile(val, probs=0.5),
            `75%` = quantile(val, probs=0.75),
            `100%` = quantile(val, probs=1)
  )%>%
  left_join(., read.csv("IHME-GBD_2019_DATA-03b2edf0-1.csv", stringsAsFactors = F)%>%filter(sex=="Both", location=="Saudi Arabia")%>%select(cause, KSA=val))%>%
  gather(Percentiles, val, -cause)%>%
  mutate(Percentiles = factor(Percentiles, levels = c("0%", "25%", "50%", "75%", "100%", "KSA")))

ggplot(fig2, aes(y=cause, x=val, color=Percentiles))+
  geom_point()+
  ylab("")+
  xlab("Age-standardized mortality rate (Deaths per 100,000)")+
  theme_bw()


#Top preforming countries by cause

out.df<-df%>%group_by(cause)%>%
  filter(val == min(val))%>%
  arrange(cause)

write.csv(out.df, "best_performing_by_cause.csv")

