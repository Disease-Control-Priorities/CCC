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
    group_by(Code, year_id)%>%
        summarise(Deaths.avert = sum(Deaths.Avert))%>%
    mutate(year_id = as.numeric(year_id))

dalys.averted<-bind_rows(clinical.dalys, all.dalys)%>%
    filter(location_name =="India")%>%
    group_by(Code, year_id)%>%
    summarise(DALYS.Adjsuted = sum(Adjusted),
              DALYS.Baseline = sum(Baseline),
              DALYS.avert = sum(DALY.ave),
              DALYS.avert =ifelse(DALYS.avert<1, 0, DALYS.avert))%>%
    mutate(year_id = as.numeric(year_id))

pin<-bind_rows(clinical.pin, all.pin)%>%
    filter(location_name=="India")%>%
    mutate(unique_id = paste0("C",Code,"_", sub_id))%>%
    select(-sub_id)

uc<-read.csv("output/unit_costs/India_adjusted_uc_2020.csv")

cost<-left_join(pin, uc)%>%
    mutate(cost = adjusted_uc*pin)%>%
    group_by(Code, group, year_id, Intervention)%>%
    summarise(cost = sum(cost))%>%
    spread(group, cost)%>%
    rename(Adjusted.cost = Adjusted,
           Baseline.cost = Baseline)%>%
    mutate(Incremental.cost = Adjusted.cost - Baseline.cost)


df<-left_join(cost, deaths.averted)%>%
    left_join(., dalys.averted)%>%
    mutate(DALY.ave.discounted = ((1-0.08)^(year_id-2022))*DALYS.avert, #discount at 8% at the end
           Incremental.cost.discounted =  ((1-0.08)^(year_id-2022))*Incremental.cost,
           wb2021="LMIC") 
    
ccc.vsl<-read.csv("DALY_value.csv", stringsAsFactors = F)%>%
    gather(year_id, val, -wb2021)%>%
    mutate(year_id = as.numeric(gsub("X","",year_id)))

df<-left_join(df, ccc.vsl)%>%
    rename(value.DALY = val)%>%
    mutate(benefit = value.DALY*DALYS.avert,
           inc.cost.adjusted = ifelse(Code%in%c(5.1,5.2,5.3,5.4), Incremental.cost.discounted+(benefit*0.009), Incremental.cost.discounted), #consumer surplus
           inc.cost.adjusted = ifelse(Code %in% c(5.5,5.6), inc.cost.adjusted+(benefit*0.001), inc.cost.adjusted)
           )%>%
    group_by(Code, Intervention)%>%
    summarise(
        Adjusted.cost = sum(Adjusted.cost, na.rm=T),
        Baseline.cost = sum(Adjusted.cost, na.rm=T),
        Incremental.cost = sum(Incremental.cost, na.rm=T),
        DALYS.Adjsuted = sum(DALYS.Adjsuted, na.rm=T),
        DALYS.Baseline = sum(DALYS.Baseline, na.rm=T),
        DALYS.averted = sum(DALYS.avert, na.rm=T),
        Incremental.cost.discounted = sum(Incremental.cost.discounted, na.rm=T),
        DALY.ave.discounted = sum(DALY.ave.discounted, na.rm=T),
        benefit = sum(benefit, na.rm=T),
        Cost.surplus.adjusted = sum(inc.cost.adjusted, na.rm=T)
    )%>%
    ungroup()%>%
    mutate(BCR = benefit / Cost.surplus.adjusted)%>%
    arrange(-BCR)

write.csv(df, "india_full.csv")
