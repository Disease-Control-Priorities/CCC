###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData2023c.Rda")
source("../utils/demmod_icer_rankb.R")

###############################################################################################################################

all.locs<-read.csv("output/all_costs.csv", stringsAsFactors = F)%>%pull(location_name)%>%unique()
interventions <- int.df %>% filter(Code<5)%>% pull(Code) %>% unique() %>% sort() #run intersectoral policies separately

total         <- length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

time1<-Sys.time()
for (is in all.locs){
    
    projection = project_pop(0.7, is, interventions, 0.80, "no", sel.cse, "varying", "yes", "yes", 1)      
    
    all.pin    = data.table(projection$pin.est)
    all.dalys  = data.table(projection$dalys) 
    all.q30    = data.table(projection$q30df) 
    dadt.all   = data.table(projection$DAdt)

#########################################################################################
save(all.pin, all.dalys, all.q30, dadt.all, file = paste0("output/all/results_", is, ".Rda"))

}

time2<-Sys.time()

time2-time1

## 65 mins

load("output/all/results_Afghanistan.Rda")

all.pin.opt<-all.pin
dadt.all.opt<-dadt.all
dalys.opt<-all.dalys
q30.opt<-all.q30

for(is in all.locs[c(2:117)]){
    load(paste0("output/all/results_", is, ".Rda"))
    all.pin.opt<-bind_rows(all.pin.opt, all.pin)
    dadt.all.opt<-bind_rows(dadt.all.opt, dadt.all)
    dalys.opt<-bind_rows(dalys.opt, all.dalys)
    q30.opt<-bind_rows(q30.opt, all.q30)
    
}

save(all.pin.opt, dadt.all.opt, q30.opt, dalys.opt, file = paste0("output/results_target_all.Rda"))


############
#for 3q0 and all deaths
############
time1<-Sys.time()
for (is in all.locs){
    
    projection = project_pop(0.7, is, interventions, 0.80, "yes", sel.cse, "varying", "yes", "yes", 1)      
    
    all.q30    = data.table(projection$q30df) 
    D0 = data.table(projection$D0)
    D1 = data.table(projection$D1)
    
    #########################################################################################
    save(all.q30, D0, D1, file = paste0("output/for_q30/results_", is, ".Rda"))
    
}

time2<-Sys.time()

time2-time1

## 120 mins

load("output/for_q30/results_Afghanistan.Rda")

q30.opt<-all.q30
D0.opt<-D0%>%mutate(location_name = "Afghanistan")
D1.opt<-D1%>%mutate(location_name = "Afghanistan")

for(is in all.locs[c(2:117)]){
    load(paste0("output/for_q30/results_", is, ".Rda"))
    q30.opt<-bind_rows(q30.opt, all.q30)
    D0.opt<-bind_rows(D0.opt, D0%>%mutate(location_name = is))
    D1.opt<-bind_rows(D1.opt, D1%>%mutate(location_name = is))
    
}

save(q30.opt, D0.opt, D1.opt,  file = paste0("output/results_q30.Rda"))

