###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData2023c_2050.Rda")
source("../utils/demmod_icer_rankb_2050.R")

###############################################################################################################################

hics<-read.csv("../new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
    filter(wb2021%in%c("UMIC"))%>%pull(location_gbd)
all.locs      <- data.frame(loc=c(countries[c(1:118, 120:126, 128:175)]))
# not palestine, not puerto rico
all.locs      <-as.character(all.locs%>%filter(loc%in%hics)%>%pull(loc)) #77 countries
interventions <- int.df %>% filter(Code<5)%>% pull(Code) %>% unique() %>% sort() #run intersectoral policies separately

total         <- length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

time1<-Sys.time()

for(is in all.locs){
    
    j = 1
    k = 1
    dadt.all.l<-all.pin.l     <- all.dalys.l <- all.q30.l <- list(total)
    
    for(inter in interventions){

    projection = project_pop(0.7, is, inter, 0.80, "no", sel.cse, "varying", "yes", "yes", 1)      

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
    
    save(clinical.pin, clinical.dalys, clinical.q30, clinical.dadt, file = paste0("by_country/full_BCR_", is, ".Rda"))
}

time2<-Sys.time()
time2-time1 #


load("by_country/full_BCR_Afghanistan.Rda")
clinical.dadt2<-clinical.dadt
clinical.pin2<-clinical.pin
clinical.dalys2<-clinical.dalys

for(is in all.locs[2:77]){
    load(paste0("by_country/full_BCR_",is,".Rda"))
    clinical.dadt2<-bind_rows(clinical.dadt, clinical.dadt2)
    clinical.pin2<-bind_rows(clinical.pin, clinical.pin2)
    clinical.dalys2<-bind_rows(clinical.dalys, clinical.dalys2)
}

#########################################################################################

save(clinical.dadt2, clinical.pin2, clinical.dalys2, file = paste0("output2023_target_UMIC.Rda"))

