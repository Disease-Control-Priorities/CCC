
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load data
load("full_BCR_Saudi Arabia.Rda")
load("full_BCR_int_Saudi Arabia.Rda")

all.pin<-bind_rows(clinical.pin, intersectoral.pin)
all.dalys<-bind_rows(clinical.dalys, intersectoral.dalys)
all.q30<-bind_rows(clinical.q30, intersectoral.q30)
all.dadt<-bind_rows(clinical.dadt, intersectoral.dadt)

uc<-read.csv("Saudi Arabia_adjusted_uc_2020.csv", stringsAsFactors = F)

#which interventions?
ints<-c(2.3, 2.4, 3.3, 4.3, 4.4, 5.1, 5.3, 5.5, 5.6, 5.7, 4.5)
names<-data.frame(Intervention = c("Clinical NCD", "Clinical NCD", "Clinical NCD", "Clinical NCD", "Clinical NCD",
                                   "Tobacco", "Tobacco", "Diet", "Diet", "Diet", "Physical activity"),
                  Code = c(2.3, 2.4, 3.3, 4.3, 4.4, 5.1, 5.3, 5.5, 5.6, 5.7, 4.5))

# 40q30
plot1<-all.q30%>%filter(Code %in% ints)%>%
  group_by(year_id)%>%
  summarise(Baseline = mean(Baseline),
            diff = sum(q30.ave))%>%
  mutate(Adjusted = Baseline - diff)%>%
  select(-diff)%>%
  gather(Scenario, x40q30, -year_id)%>%
  filter(year_id<=2040)%>%
  mutate(year_id = as.numeric(year_id))

ggplot(plot1, 
       aes(x=year_id, y=x40q30, color=Scenario, group=Scenario))+
  geom_line()+
  theme_bw()+
  ylim(0,20)+
  xlab("Year")+
  ylab("NCD Mortality, 40q30 (%)")

ggsave("40q30.jpeg")

# Deaths averted

DA<-all.dadt%>%filter(Code %in% ints)%>%
  left_join(., names)%>%
  group_by(year_id, Intervention)%>%
  summarise(Deaths.averted = sum(Deaths.Avert, na.rm = T))%>%
  ungroup()%>%
  group_by(Intervention)%>%
  mutate(Cumulative.deaths.averted = cumsum(Deaths.averted))%>%
  select(-Deaths.averted)%>%
  filter(year_id %in% c(2025,2030,2035,2040))%>%
  spread(year_id, Cumulative.deaths.averted)

