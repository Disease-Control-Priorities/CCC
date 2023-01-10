###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("new_inputs/PreppedData2023c.Rda")
source("utils/demmod_icer_rankb.R")

###############################################################################################################################

hics<-read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
    filter(wb2021%in%c("HIC","UMIC"))%>%pull(location_gbd)
all.locs      <- data.frame(loc=c(countries[c(1:118, 120:126, 128:175)]))
# not palestine, not puerto rico
all.locs      <-as.character(all.locs%>%filter(loc%!in%hics)%>%pull(loc)) #77 countries
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

## 40 mins

load("output/all/results_Afghanistan.Rda")

all.pin.opt<-all.pin
dadt.all.opt<-dadt.all
dalys.opt<-all.dalys
q30.opt<-all.q30

for(is in all.locs[c(2:77)]){
    load(paste0("output/all/results_", is, ".Rda"))
    all.pin.opt<-bind_rows(all.pin.opt, all.pin)
    dadt.all.opt<-bind_rows(dadt.all.opt, dadt.all)
    dalys.opt<-bind_rows(dalys.opt, all.dalys)
    q30.opt<-bind_rows(q30.opt, all.q30)
    
}

save(all.pin.opt, dadt.all.opt, q30.opt, dalys.opt, file = paste0("output/results_target_all.Rda"))


############
#for 3q0 and all baseline NCD deaths
############


time1<-Sys.time()
for (is in all.locs){
    
    projection = project_pop(0.7, is, interventions, 0.80, "yes", sel.cse, "varying", "yes", "yes", 1)      
    
    all.q30    = data.table(projection$q30df) 
    cse.deaths = data.table(projection$D0cse)
    
    #########################################################################################
    save(all.q30, cse.deaths, file = paste0("output/for_q30/results_", is, ".Rda"))
    
}

time2<-Sys.time()

time2-time1

## 40 mins

load("output/for_q30/results_Afghanistan.Rda")

q30.opt<-all.q30
ncds.opt<-cse.deaths%>%mutate(location = "Afghanistan")

for(is in all.locs[c(2:77)]){
    load(paste0("output/for_q30/results_", is, ".Rda"))
    q30.opt<-bind_rows(q30.opt, all.q30)
    ncds.opt<-bind_rows(ncds.opt, cse.deaths%>%mutate(location = is))
    
}

save(q30.opt, ncds.opt,  file = paste0("output/results_q30.Rda"))

