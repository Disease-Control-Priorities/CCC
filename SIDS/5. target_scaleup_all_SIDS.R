###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("new_inputs/PreppedData2023_SIDS.Rda")
source("utils/demmod_icer_rankb.R")

###############################################################################################################################

all.locs<-c("Maldives", "Timor-Leste")
interventions <- int.df %>% filter(Code<5)%>% pull(Code) %>% unique() %>% sort() #run intersectoral policies separately

total         <- length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()
interventions
time1<-Sys.time()
for (is in all.locs){
    
    projection = project_pop(0.7, is, interventions, 0.80, "yes", sel.cse, "varying", "yes", "yes", 1)      
    
    all.pin    = data.table(projection$pin.est)
    all.dalys  = data.table(projection$dalys) 
    all.q30    = data.table(projection$q30df) 
    dadt.all   = data.table(projection$DAdt)

#########################################################################################
save(all.pin, all.dalys, all.q30, dadt.all, file = paste0("output/all/results_", is, ".Rda"))

}

time2<-Sys.time()

time2-time1

load("output/all/results_Maldives.Rda")
q30.opt<-all.q30
dalys.opt<-all.dalys
dadt.opt<-dadt.all
pin.opt<-all.pin

load("output/all/results_Timor-Leste.Rda")

q30.opt<-bind_rows(q30.opt, all.q30)
dalys.opt<-bind_rows(dalys.opt, all.dalys)
dadt.opt<-bind_rows(dadt.opt, dadt.all)
pin.opt<-bind_rows(pin.opt, all.pin)

save(pin.opt, dalys.opt, q30.opt, dadt.opt, file = "output/SIDS_all.Rda")
