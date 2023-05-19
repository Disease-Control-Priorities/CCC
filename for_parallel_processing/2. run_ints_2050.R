###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
# https://towardsdatascience.com/parallelization-caveats-in-r-1-the-basics-multiprocessing-and-multithreading-performance-eb584b7e850ehttps://towardsdatascience.com/parallelization-caveats-in-r-1-the-basics-multiprocessing-and-multithreading-performance-eb584b7e850e
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

library(foreach)
library(snow)
library(parallel)
library(iterators)
library(doParallel)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData2023c_2050.Rda")
source("../utils/demmod_icer_rankb_2050.R")

###############################################################################################################################
hics<-read.csv("country_groupings.csv", stringsAsFactors = F)%>%
  filter(wb2021=="HIC")%>%pull(location_gbd)
#problems with data from Palestine and Puerto Rico
#all.locs      <- data.frame(loc=c(countries[c(1:118, 120:126, 128:175)]))
#all.locs      <-as.character(all.locs%>%filter(loc%!in%hics)%>%pull(loc))
all.locs<-read.csv("../HLI/output/all_costs.csv", stringsAsFactors = F)%>%pull(location_name)%>%unique()
#should be 117 countries
interventions <-  pin.groups %>% filter(calc_ICER=="yes", Code<5) %>% pull(Code) %>% unique() %>% sort()
total         <- length(interventions) #* ength(all.locs)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

all.pin.l     <- all.dalys.l <- all.q30.l <-dadt.all.l<- list(total)

quality<-0.7 #

#set targets, 2% per year 2023-2050 til 80% target hit
int.df<-int.df%>%
  filter(Code<5)%>%
  mutate(scale_years = ifelse(Code<5, floor((0.8-`Baseline Coverage`)/0.02), floor((1-`Baseline Coverage`)/0.02)),
         scale_years = ifelse(scale_years>28, 28, scale_years),
         scale_years = ifelse(scale_years<0, 0, scale_years),
         target.coverage = `Baseline Coverage`+scale_years*0.02,
         stop_year = 2022+scale_years)%>%
  distinct()


for (is in all.locs){
  k = 1
  dadt.all.l<-all.pin.l     <- all.dalys.l <- all.q30.l <- list(total)
  
  for (inter in interventions){

    base<-int.df%>%filter(location_name==is, inter==Code)%>%pull(`Baseline Coverage`)%>%unique()
    target<-int.df%>%filter(location_name==is, inter==Code)%>%pull(`target.coverage`)%>%unique()
    stop_year<-int.df%>%filter(location_name==is, inter==Code)%>%pull(stop_year)%>%unique()
    
    # scale-up parameters, yes/no intersectoral policies, Calc PIN?, 1x covid shock
    projection <- project_pop(quality, is, inter, base, target, 
                              "varying", 2022, stop_year,
                              sel.cse, "no", "yes", "yes", 1)
    
    all.pin.l[[k]]    = data.table(projection$pin.est) #already includes Code
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    all.q30.l[[k]]    = data.table(projection$q30df) %>% mutate(Code = inter)
    dadt.all.l[[k]]   = data.table(projection$DAdt) %>% mutate(Code = inter)
    
    
    k = k + 1
  }
  
  all.pin       <- rbindlist(all.pin.l)
  all.dalys     <- rbindlist(all.dalys.l)
  all.q30       <- rbindlist(all.q30.l)
  dadt.all      <- rbindlist(dadt.all.l)
  
  save(all.pin, all.dalys, all.q30, dadt.all, file = paste0("for_HLI/", is, "_output2050_target.Rda"))
  
}

#time start: 3:51 am
time.stop<-Sys.time()

