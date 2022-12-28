###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("new_inputs/PreppedData2023b.Rda")
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

is<-all.locs<-"India"

j = 1
k = 1
dadt.all.l<-all.pin.l     <- all.dalys.l <- all.q30.l <- list(total)
time1<-Sys.time()

for(inter in interventions){

    projection = project_pop(is, inter, 0.80, "no", sel.cse, "varying", "yes", "yes", 1)      

    all.pin.l[[k]]    = data.table(projection$pin.est)
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    all.q30.l[[k]]    = data.table(projection$q30df) %>% mutate(Code = inter)
    dadt.all.l[[k]]   = data.table(projection$DAdt)%>%mutate(Code = inter)
    
    
    j = j + 1; k = k + 1
    
}

time2<-Sys.time()

time2-time1 #20 minutes

clinical.pin      <- rbindlist(all.pin.l)
clinical.dalys    <- rbindlist(all.dalys.l)
clinical.q30      <- rbindlist(all.q30.l)
clinical.dadt      <- rbindlist(dadt.all.l)

#########################################################################################

#add intersectoral results#
load("for_parallel_processing/output2023_target_intersectoral.Rda")

deaths.averted<-bind_rows(clinical.dadt, dadt.all)%>%
    filter(location_name=="India")%>%
    group_by(Code)%>%
        summarise(India.Deaths.avert.millions = sum(Deaths.Avert)/1e6)

dalys.averted<-bind_rows(clinical.dalys, all.dalys)%>%
    filter(location_name =="India")%>%
    group_by(Code)%>%
    summarise(India.DALYS.avert.millions = sum(DALY.ave)/1e6)

df<-read.csv("Figures/BCRs_int.csv", stringsAsFactors = F)%>%
    left_join(.,deaths.averted)%>%
    left_join(., dalys.averted)


write.csv(df, "test_results.csv", row.names = F)







