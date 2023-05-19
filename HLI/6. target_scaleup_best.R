###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData2023c_2050.Rda")
source("../utils/demmod_icer_rankb_2050_all.R")

###############################################################################################################################

all.locs<-read.csv("figures/HLI_CEA_country.csv", stringsAsFactors = F)%>%pull(location_name)%>%unique()

WB<-read.csv("../new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
  select(iso3, location_gbd, HLI_group)%>%
  rename(location_name = location_gbd)%>%
  left_join(., read.csv("../new_inputs/HS123.csv", stringsAsFactors = F)%>%
              select(ISO3, HSP)%>%
              rename(iso3 = ISO3))%>%
  mutate(HSP = gsub(" ", "", HSP))%>%
  filter(HSP!="NA")

interventions <-  read.csv("hpp.csv", stringsAsFactors = F)%>%
  left_join(., WB)%>%
  select(-X)

sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

quality<-0.7 #

#clinical: 2% per year 2023-2050, capped at 80% target
#policies: reach 100% target 2023-2030

time1<-Sys.time()
for (is in all.locs){
  
  #filter high priority interventions
  inter<-interventions%>%filter(location_name==is, Priorty=="High")%>%pull(Code)%>%unique()

    projection <- project_pop(quality, is, inter, 0.02, 0.8, "varying", sel.cse, "no", "yes", "yes", 1)
    
    all.pin    = data.table(projection$pin.est) #population in need
    all.dalys  = data.table(projection$dalys)   #dalys averted
    all.q30    = data.table(projection$q30df)   #40q30
    dadt.all   = data.table(projection$DAdt)    #deaths averted, also has NCD total deaths
    D0cse      = data.table(projection$D0cse)   #ncd deaths baseline
    D1cse      = data.table(projection$D1cse)   #ncd deaths intervention
    
    #########################################################################################
    save(all.pin, all.dalys, all.q30, dadt.all, D0cse, D1cse, file = paste0("output/best/results_", is, ".Rda"))
  }
time2<-Sys.time()
time2-time1
## 90 mins

load("output/best/results_Afghanistan.Rda")

all.pin.opt<-all.pin
dadt.all.opt<-dadt.all
dalys.opt<-all.dalys
q30.opt<-all.q30
d0.opt<-D0cse%>%mutate(location_name="Afghanistan")
d1.opt<-D1cse%>%mutate(location_name="Afghanistan")

for(is in all.locs[c(2:115)]){
    load(paste0("output/best/results_", is, ".Rda"))
    all.pin.opt<-bind_rows(all.pin.opt, all.pin)
    dadt.all.opt<-bind_rows(dadt.all.opt, dadt.all)
    dalys.opt<-bind_rows(dalys.opt, all.dalys)
    q30.opt<-bind_rows(q30.opt, all.q30)
    d0.opt<-bind_rows(d0.opt, D0cse%>%mutate(location_name=is))
    d1.opt<-bind_rows(d1.opt, D1cse%>%mutate(location_name=is))
    
}

save(all.pin.opt, dadt.all.opt, q30.opt, dalys.opt, d0.opt, d1.opt, file = paste0("output/results_target_best.Rda"))

