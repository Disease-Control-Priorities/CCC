###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData2023c.Rda")
source("../utils/demmod_icer_rankb.R")

###############################################################################################################################

hics<-read.csv("../new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
    filter(wb2021%in%c("HIC","UMIC"))%>%pull(location_gbd)
all.locs      <- data.frame(loc=c(countries[c(1:118, 120:126, 128:175)]))
# not palestine, not puerto rico
all.locs      <-as.character(all.locs%>%filter(loc%!in%hics)%>%pull(loc)) #77 countries
interventions <- int.df %>% filter(Code<5)%>% pull(Code) %>% unique() %>% sort() #run intersectoral policies separately

total         <- length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

time1<-Sys.time()

for(is in all.locs[3:77]){
    
    j = 1
    k = 1
    dadt.all.l<-all.pin.l     <- all.dalys.l <- all.q30.l <- list(total)
    
    for(inter in interventions){

    projection = project_pop(is, inter, 0.80, "no", sel.cse, "varying", "yes", "yes", 1)      

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
#add intersectoral results#
load("output2023_target_intersectoral.Rda")

deaths.averted<-bind_rows(clinical.dadt2, dadt.all)%>%
    group_by(Code, year_id, location_name)%>%
    summarise(
        #Deaths.Baseline = sum(NCD.Deaths0),
        #Deaths.Adjusted = sum(NCD.Deaths1),
        Deaths.avert = sum(Deaths.Avert))%>%
    mutate(year_id = as.numeric(year_id))

dalys.averted<-bind_rows(clinical.dalys2, all.dalys)%>%
    group_by(Code, year_id, location_name)%>%
    summarise(DALYS.Adjusted = sum(Adjusted),
              DALYS.Baseline = sum(Baseline),
              DALYS.avert = sum(DALY.ave),
              DALYS.avert =ifelse(DALYS.avert<1, 0, DALYS.avert))%>%
    mutate(year_id = as.numeric(year_id))

pin<-bind_rows(clinical.pin2, all.pin)%>%
    mutate(unique_id = paste0("C",Code,"_", sub_id))%>%
    select(-sub_id)

uc<-read.csv("../output/unit_costs/Afghanistan_adjusted_uc_2020.csv", stringsAsFactors = F)%>%
    mutate(location_name = "Afghanistan")

for(i in all.locs[2:77]){
    uc<-bind_rows(uc, read.csv(paste0("../output/unit_costs/",i,"_adjusted_uc_2020.csv"), stringsAsFactors = F)%>%
                      mutate(location_name = i))
}

cost<-left_join(pin, uc)%>%
    mutate(cost = adjusted_uc*pin)%>%
    group_by(Code, group, year_id, Intervention, location_name)%>%
    summarise(cost = sum(cost))%>%
    spread(group, cost)%>%
    rename(Adjusted.cost = Adjusted,
           Baseline.cost = Baseline)%>%
    mutate(Incremental.cost = Adjusted.cost - Baseline.cost)

WB<-read.csv("../new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
    select(iso3, location_gbd, wb2021)%>%
    rename(location_name = location_gbd)

df<-left_join(cost, deaths.averted)%>%
    left_join(., dalys.averted)%>%
    left_join(.,WB)%>%
    group_by(wb2021, year_id, Code, Intervention)%>%
    summarise(Baseline.cost = sum(Baseline.cost),
              Adjusted.cost = sum(Adjusted.cost),
              Incremental.cost = sum(Incremental.cost),
              #Deaths.Baseline = sum(Deaths.Baseline),
              #Deaths.Adjusted = sum(Deaths.Adjusted),
              Deaths.avert = sum(Deaths.avert),
              DALYS.Baseline = sum(DALYS.Baseline),
              DALYS.Adjusted = sum(DALYS.Adjusted),
              DALYS.avert = sum(DALYS.avert)
    )%>%
    mutate(discount.rate = ((1-0.08)^(year_id-2022)),
           discount.rate = ifelse(year_id<2023,1,discount.rate),
           Baseline.cost = (Baseline.cost*discount.rate),
           Adjusted.cost = (Adjusted.cost*discount.rate),
           Incremental.cost = (Incremental.cost*discount.rate),
           DALYS.Baseline = (DALYS.Baseline*discount.rate),
           DALYS.Adjusted = (DALYS.Adjusted*discount.rate),
           DALYS.avert = (DALYS.avert*discount.rate)
    )

ccc.vsl<-read.csv("../DALY_value.csv", stringsAsFactors = F)%>%
    gather(year_id, val, -wb2021)%>%
    mutate(year_id = as.numeric(gsub("X","",year_id)))

df<-left_join(df, ccc.vsl)%>%
    filter(year_id>=2023)%>%
    mutate(Gross.benefits = val*DALYS.avert,
           Forgone.surplus = ifelse(Code%in%c(5.1,5.2,5.3,5.4), (Gross.benefits*0.009), 0), #consumer surplus
           Forgone.surplus = ifelse(Code %in% c(5.5,5.6), (Gross.benefits*0.001), Forgone.surplus)
    )%>%
    group_by(Code, Intervention, wb2021)%>%
    summarise(
        Baseline.cost = sum(Baseline.cost, na.rm=T),
        Adjusted.cost = sum(Adjusted.cost, na.rm=T),
        Incremental.cost = sum(Incremental.cost, na.rm=T),
        #Deaths.Baseline = sum(Deaths.Baseline),
        #Deaths.Adjusted = sum(Deaths.Adjusted),
        Deaths.avert = sum(Deaths.avert),
        DALYS.Baseline = sum(DALYS.Baseline, na.rm=T),
        DALYS.Adjusted = sum(DALYS.Adjusted, na.rm=T),
        DALYS.avert = sum(DALYS.avert, na.rm=T),
        Gross.benefits = sum(Gross.benefits, na.rm=T),
        Forgone.surplus = sum(Forgone.surplus, na.rm=T))%>%
    ungroup()%>%
    mutate(BCR = Gross.benefits / (Forgone.surplus+Incremental.cost),
           BCR.without.surplus = Gross.benefits/Incremental.cost)%>%
    arrange(wb2021, -BCR)%>%
    mutate(Intervention = gsub("policy", "regulations", Intervention),
           wb2021 = ifelse(wb2021=="LMIC", "LMC", wb2021))%>%
    rename(WB_Region = wb2021)

both<-df%>%
    group_by(Code, Intervention)%>%
    summarise(
        Baseline.cost = sum(Baseline.cost, na.rm=T),
        Adjusted.cost = sum(Adjusted.cost, na.rm=T),
        Incremental.cost = sum(Incremental.cost, na.rm=T),
        #Deaths.Baseline = sum(Deaths.Baseline),
        #Deaths.Adjusted = sum(Deaths.Adjusted),
        Deaths.avert = sum(Deaths.avert),
        DALYS.Baseline = sum(DALYS.Baseline, na.rm=T),
        DALYS.Adjusted = sum(DALYS.Adjusted, na.rm=T),
        DALYS.avert = sum(DALYS.avert, na.rm=T),
        Gross.benefits = sum(Gross.benefits, na.rm=T),
        Forgone.surplus = sum(Forgone.surplus, na.rm=T)
    )%>%
    ungroup()%>%
    mutate(BCR = Gross.benefits / (Forgone.surplus+Incremental.cost),
           BCR.without.surplus = Gross.benefits/Incremental.cost,
           WB_Region = "LIC/LMC")%>%
    arrange(-BCR)

df<-bind_rows(df, both)%>%
    na.omit()

write.csv(df, "../clinical_full_2023_2030.csv")


## bubble chart ##
