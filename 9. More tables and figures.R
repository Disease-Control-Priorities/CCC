setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

WB<-read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  select(iso3, wb2021, location_gbd)%>%rename(location_name = location_gbd)

### Figure 2 ###
load("output/results_q30.Rda")
all.q30<-q30.opt


load("output/results_q30_best.Rda")
best.q30<-all.q30.opt

load("output/goal.q30.covid_0830.Rda")

locs<-unique(all.q30$location_name)

q30<-left_join(all.q30, goal.q30.out%>%select(location_name, Baseline_2015, Target_40q30)%>%unique())%>%
  mutate(reached = ifelse(Adjusted - Target_40q30<=0,1,0))%>%
  filter(year_id==2030)%>%
  left_join(., WB)%>%
  mutate(all = 1)%>%
  group_by(wb2021)%>%
  summarise(prop_reached = sum(reached, na.rm=T)/sum(all, na.rm=T))

lmic.q30<-left_join(all.q30, goal.q30.out%>%select(location_name, Baseline_2015, Target_40q30)%>%unique())%>%
  mutate(reached = ifelse(Adjusted - Target_40q30<=0,1,0))%>%
  filter(year_id==2030)%>%
  left_join(., WB)%>%
  mutate(all = 1)%>%
  summarise(prop_reached = sum(reached, na.rm=T)/sum(all, na.rm=T))%>%
  mutate(wb2021 = "LIC+LMIC")

q30<-bind_rows(q30, lmic.q30)

pop<-read.csv("new_inputs/wb_pop.csv", stringsAsFactors = F, skip=4)%>% 
  pivot_longer(X1960:X2021) %>% 
  group_by(Country.Code) %>% 
  fill(value, .direction = "down") %>% 
  pivot_wider(Country.Code)%>%
  select(Country.Code, X2021)%>%
  rename(iso3 = Country.Code, pop = X2021)%>%
  left_join(., WB)

#Figure 2
#The second figure will be a plot of 40q30-NCD4 over time at the global level, with the business as usual scenario as well as 
## (1) a scenario modeling the impact of the full list of interventions (table 2 above) and 
## (2) a scenario modeling the impact of the list of most cost-beneficial interventions (table 3 above)

baseline<-bind_rows(q30.opt%>%select(location_name, Baseline, year_id)%>%
                      mutate(year_id = as.numeric(year_id)), 
                    goal.q30.out%>%select(location_name, Baseline_2015)%>%unique()%>%
                      mutate(year_id=2015)%>%rename(Baseline = Baseline_2015))%>%
  filter(location_name %in% locs)%>%
  rename(x40q30 = Baseline)%>%
  mutate(Scenario = "Baseline")


full<-all.q30%>%select(location_name, Adjusted, year_id)%>%
  filter(location_name%in%locs)%>%
  mutate(year_id = as.numeric(year_id))%>%
  rename(x40q30 = Adjusted)%>%
  mutate(Scenario = "All interventions and policies")

best<-best.q30%>%select(location_name, Adjusted, year_id)%>%
  filter(location_name%in%locs)%>%
  mutate(year_id = as.numeric(year_id))%>%
  rename(x40q30 = Adjusted)%>%
  mutate(Scenario = "Best investment package (BCR>15)")
  

plot<-bind_rows(baseline, full, best)%>%
  left_join(.,WB)%>%
  left_join(.,pop)%>%
  group_by(year_id, Scenario)%>%
  summarise(x40q30 = weighted.mean(x40q30, pop))%>%
  mutate(Scenario = factor(Scenario, levels= c("Baseline",
                                               "Best investment package (BCR>15)",
                                               "All interventions and policies")))%>%
  filter(Scenario=="Baseline" | year_id>=2022)

plot$x40q30[plot$year_id==2015]*(2/3)

ggplot(plot, aes(x=year_id, y=x40q30, color= Scenario))+
  geom_line(size=1)+
  theme_bw()+
  ylim(0,30)+
  ylab("40q30 (%)")+
  xlab("Year")+  
  geom_line(y=13.65, color="darkgrey", lty=2)

ggsave("Figures/Figure2.png", height = 6, width = 9, units = "in")

library(tidyr)

plot2<-bind_rows(baseline, full, best)%>%
  left_join(.,WB)%>%
  left_join(.,pop)%>%
  group_by(year_id, Scenario, wb2021)%>%
  summarise(x40q30 = weighted.mean(x40q30, pop))%>%
  filter(wb2021!="UMIC")%>%
  bind_rows(., plot%>%mutate(wb2021="LIC+LMIC"))%>%
  mutate(wb2021 = factor(wb2021, levels=c("LIC", "LMIC", "LIC+LMIC")))%>%
  mutate(target = ifelse(year_id==2015, x40q30*(2/3), NA))%>%
  group_by(wb2021)%>%
  fill(target)%>%
  mutate(Scenario = factor(Scenario, levels= c("Baseline",
                                               "Best investment package (BCR>15)",
                                               "All interventions and policies")))%>%
  filter(Scenario=="Baseline" | year_id>=2022)



ggplot(plot2, aes(x=year_id, y=x40q30, color= Scenario))+
  geom_line(size=1)+
  geom_abline()+
  facet_wrap(~wb2021)+
  geom_hline(data = plot2, aes(yintercept = target), color="darkgrey", lty=2)+
  theme_bw()+
  ylim(0,30)+
  ylab("40q30 (%)")+
  xlab("Year")

ggsave("Figures/Figure2_region.png", height = 6, width = 12, units = "in")



# Appendix tables #

# 8% discount rate #

base<-read.csv("Figures/BCRs_int.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg.csv", stringsAsFactors = F)%>%
  mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

pesm<-read.csv("Figures/BCRs_int_pesm.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg_pesm.csv", stringsAsFactors = F)%>%
  mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

optm<-read.csv("Figures/BCRs_int_optm.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg_optm.csv", stringsAsFactors = F)%>%
              mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

lic<-base%>%filter(wb2021=="LIC")%>%
  select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
  gather(metric, Base, -Sub.components, -Intervention.package)%>%
  mutate(Base = signif(Base, digits=2))%>%
  arrange(Intervention.package, Sub.components, metric)%>%
  mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))%>%
  left_join(., 
            pesm%>%filter(wb2021=="LIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Pessimistic, -Sub.components, -Intervention.package)%>%
              mutate(Pessimistic = signif(Pessimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))
            )%>%
  left_join(.,
            optm%>%filter(wb2021=="LIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Optimistic, -Sub.components, -Intervention.package)%>%
              mutate(Optimistic = signif(Optimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components)) 
            )%>%
  mutate(Intervention.package = gsub("\n","",Intervention.package))

write.csv(lic, "Figures/Appendix_tab_LIC.csv", row.names = F)

lmic<-base%>%filter(wb2021=="LMIC")%>%
  select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
  gather(metric, Base, -Sub.components, -Intervention.package)%>%
  mutate(Base = signif(Base, digits=2))%>%
  arrange(Intervention.package, Sub.components, metric)%>%
  mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))%>%
  left_join(., 
            pesm%>%filter(wb2021=="LMIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Pessimistic, -Sub.components, -Intervention.package)%>%
              mutate(Pessimistic = signif(Pessimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))
  )%>%
  left_join(.,
            optm%>%filter(wb2021=="LMIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Optimistic, -Sub.components, -Intervention.package)%>%
              mutate(Optimistic = signif(Optimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components)) 
  )%>%
  mutate(Intervention.package = gsub("\n","",Intervention.package))

write.csv(lmic, "Figures/Appendix_tab_LMIC.csv", row.names = F)

# 5% discount rate #

base<-read.csv("Figures/BCRs_int_5perc.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg_5perc.csv", stringsAsFactors = F)%>%
              mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

pesm<-read.csv("Figures/BCRs_int_pesm_5perc.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg_pesm_5perc.csv", stringsAsFactors = F)%>%
              mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

optm<-read.csv("Figures/BCRs_int_optm_5perc.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg_optm_5perc.csv", stringsAsFactors = F)%>%
              mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

lic<-base%>%filter(wb2021=="LIC")%>%
  select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
  gather(metric, Base, -Sub.components, -Intervention.package)%>%
  mutate(Base = signif(Base, digits=2))%>%
  arrange(Intervention.package, Sub.components, metric)%>%
  mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))%>%
  left_join(., 
            pesm%>%filter(wb2021=="LIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Pessimistic, -Sub.components, -Intervention.package)%>%
              mutate(Pessimistic = signif(Pessimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))
  )%>%
  left_join(.,
            optm%>%filter(wb2021=="LIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Optimistic, -Sub.components, -Intervention.package)%>%
              mutate(Optimistic = signif(Optimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components)) 
  )%>%
  mutate(Intervention.package = gsub("\n","",Intervention.package))

write.csv(lic, "Figures/Appendix_tab_LIC_5perc.csv", row.names = F)

lmic<-base%>%filter(wb2021=="LMIC")%>%
  select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
  gather(metric, Base, -Sub.components, -Intervention.package)%>%
  mutate(Base = signif(Base, digits=2))%>%
  arrange(Intervention.package, Sub.components, metric)%>%
  mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))%>%
  left_join(., 
            pesm%>%filter(wb2021=="LMIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Pessimistic, -Sub.components, -Intervention.package)%>%
              mutate(Pessimistic = signif(Pessimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))
  )%>%
  left_join(.,
            optm%>%filter(wb2021=="LMIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Optimistic, -Sub.components, -Intervention.package)%>%
              mutate(Optimistic = signif(Optimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components)) 
  )%>%
  mutate(Intervention.package = gsub("\n","",Intervention.package))

write.csv(lmic, "Figures/Appendix_tab_LMIC_5perc.csv", row.names = F)


# 14% discount rate #

base<-read.csv("Figures/BCRs_int_14perc.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg_14perc.csv", stringsAsFactors = F)%>%
              mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

pesm<-read.csv("Figures/BCRs_int_pesm_14perc.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg_pesm_14perc.csv", stringsAsFactors = F)%>%
              mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

optm<-read.csv("Figures/BCRs_int_optm_14perc.csv", stringsAsFactors = F)%>%
  filter(Code<5)%>%select(-X, -Code)%>%
  bind_rows(., read.csv("Figures/BCRs_pkg_optm_14perc.csv", stringsAsFactors = F)%>%
              mutate(Intervention = "a")%>%select(-X))%>%arrange(Intervention.package)%>%
  rename(Sub.components = Intervention)%>%
  mutate(DALY.ave..millions. = DALY.ave..millions.)%>%
  rename(`DALYs averted (millions)` = DALY.ave..millions.)

lic<-base%>%filter(wb2021=="LIC")%>%
  select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
  gather(metric, Base, -Sub.components, -Intervention.package)%>%
  mutate(Base = signif(Base, digits=2))%>%
  arrange(Intervention.package, Sub.components, metric)%>%
  mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))%>%
  left_join(., 
            pesm%>%filter(wb2021=="LIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Pessimistic, -Sub.components, -Intervention.package)%>%
              mutate(Pessimistic = signif(Pessimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))
  )%>%
  left_join(.,
            optm%>%filter(wb2021=="LIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Optimistic, -Sub.components, -Intervention.package)%>%
              mutate(Optimistic = signif(Optimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components)) 
  )%>%
  mutate(Intervention.package = gsub("\n","",Intervention.package))

write.csv(lic, "Figures/Appendix_tab_LIC_14perc.csv", row.names = F)

lmic<-base%>%filter(wb2021=="LMIC")%>%
  select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
  gather(metric, Base, -Sub.components, -Intervention.package)%>%
  mutate(Base = signif(Base, digits=2))%>%
  arrange(Intervention.package, Sub.components, metric)%>%
  mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))%>%
  left_join(., 
            pesm%>%filter(wb2021=="LMIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Pessimistic, -Sub.components, -Intervention.package)%>%
              mutate(Pessimistic = signif(Pessimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components))
  )%>%
  left_join(.,
            optm%>%filter(wb2021=="LMIC")%>%
              select(Intervention.package, Sub.components, BCR, `DALYs averted (millions)`)%>%
              gather(metric, Optimistic, -Sub.components, -Intervention.package)%>%
              mutate(Optimistic = signif(Optimistic, digits=2))%>%
              arrange(Intervention.package, Sub.components, metric)%>%
              mutate(Sub.components = ifelse(Sub.components=="a", NA, Sub.components)) 
  )%>%
  mutate(Intervention.package = gsub("\n","",Intervention.package))

write.csv(lmic, "Figures/Appendix_tab_LMIC_14perc.csv", row.names = F)


