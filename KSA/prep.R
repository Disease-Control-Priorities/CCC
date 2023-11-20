rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(purrr)

RRs<-read_xlsx("prop_active.xlsx", sheet="RR")%>%
  bind_rows(., merge(read_xlsx("prop_active.xlsx", sheet="RR_all")%>%select(-age), 
                     data.frame(age=c(15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)))
            )

prop<-read_xlsx("prop_active.xlsx", sheet="PA")%>%
  select(age, sex, Low, Moderate, High)%>%
  gather(category, prop, -age, -sex)


targets<-data.frame(scenario = c("Intervention", "Intervention", "Intervention", 
                                 "Ideal", "Ideal", "Ideal"),
                    category = c("Low", "Moderate", "High",
                                 "Low", "Moderate", "High"),
                    prop.target = c(24, 38, 38,
                             5, 0, 95))

#PAFS


#PIFs
df<-left_join(prop, targets)%>%
  mutate(prop.target = ifelse(category=="Low" & prop<prop.target, prop, prop.target),
         prop.target = ifelse(category=="High" & prop>prop.target, prop, prop.target))%>%
  select(-prop)%>%
  spread(category, prop.target)%>%
  mutate(Moderate = 100 - Low - High)%>%
  gather(category, target.prop, -age, -sex, -scenario)%>%
  left_join(., prop%>%rename(base.prop = prop))%>%
  left_join(., RRs)%>%
  mutate(Pi = base.prop*RR/100,
         Pihat = target.prop*RR/100)%>%
  group_by(age, sex, cause, scenario)%>%
  summarise(Pi = sum(Pi),
            Pihat = sum(Pihat))%>%
  mutate(PIF = (Pi - Pihat)/Pi)%>%
  select(age, sex, cause, scenario, PIF)

write.csv(df, "PIFs.csv", row.names = F)  

#interventions
int.df<-read.csv("interventions.csv", stringsAsFactors = F)

out.df<-df%>%mutate(iso3= "SAU",
                Baseline.Coverage = 0,
                sub_id = 1, 
                Code = ifelse(cause == "Breast cancer", 1,
                                ifelse(cause=="Colon and rectum cancer", 2,
                                       ifelse(cause== "Ischemic heart disease", 3,
                                              ifelse(cause=="Ischemic stroke", 4,5)))),
                
                Code = ifelse(scenario=="Intervention", Code+0.1, Code+0.2),
                ageu = age+4,
                metric = "case fatality",
                ghe_cause = ifelse(cause == "Ischemic heart disease", "Ischaemic heart disease", cause),
                ghe_cause = ifelse(cause == "Ischemic stroke", "Ischaemic stroke", ghe_cause),
                ghe_cause = ifelse(cause == "Colon and rectum cancer", "Colon and rectum cancers", ghe_cause),
                ghe_cause = ifelse(cause== "Diabetes mellitus type 2", "Diabetes mellitus", ghe_cause),
                ncd_cause = ghe_cause,
                ncd_cause = ifelse(cause=="Diabetes mellitus type 2", "Diabetes", ncd_cause),
                max.cov=1,
                location_name = "Saudi Arabia"
                )%>%
  rename(cause_name = cause,
         Intervention = scenario,
         agel = age,
         Mortality.reduction = PIF)


#2016 death PAFs (GBD)
pafs<-data.frame(cause_name = c("Breast cancer", "Colon and rectum cancer",
                                "Diabetes mellitus type 2",
                                "Ischemic heart disease", "Ischemic stroke"
                                ),
                 paf = c(0.031, 0.014, 0.027, 0.106, 0.11))

out.df<-left_join(out.df, pafs)%>%
  mutate(Mortality.reduction = Mortality.reduction*paf)%>%
  select(-paf)

write.csv(out.df, "int_formatted.csv", row.names = F)

