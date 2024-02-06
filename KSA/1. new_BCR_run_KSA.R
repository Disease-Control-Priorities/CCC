###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("PreppedData2023c_2050_KSA.Rda")
source("../utils/demmod_icer_rankb_2050_KSA.R")

###############################################################################################################################

all.locs      <-"Saudi Arabia"
interventions<-c(2.3, 2.4, 3.3, 4.3, 4.4)
#interventions <- int.df %>% filter(Code<5)%>% pull(Code) %>% unique() %>% sort() #run intersectoral policies separately

total         <- length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()


time1<-Sys.time()

for(is in all.locs){
    
    j = 1
    k = 1
    dadt.all.l<-all.pin.l     <- all.dalys.l <- all.q30.l <- list(total)
    
    for(inter in interventions){

    projection = project_pop(0.8, is, inter, 0.90, "varying", 2022, 2030, sel.cse, "no", "yes", "yes", 1)      

    all.pin.l[[k]]    = data.table(projection$pin.est)
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    all.q30.l[[k]]    = data.table(projection$q30df) %>% mutate(Code = inter)
    dadt.all.l[[k]]   = data.table(projection$DAdt)%>%mutate(Code = inter)
    
    
    j = j + 1; k = k + 1
    
    }
    
    clinical.pin      <- rbindlist(all.pin.l)
    clinical.dalys    <- rbindlist(all.dalys.l)
    clinical.q30      <- rbindlist(all.q30.l)
    clinical.dadt      <- rbindlist(dadt.all.l)
    
    save(clinical.pin, clinical.dalys, clinical.q30, clinical.dadt, file = paste0("../KSA/full_BCR_", is, ".Rda"))
}

time2<-Sys.time()
time2-time1 #3.7 minutes

