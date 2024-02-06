###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
# https://towardsdatascience.com/parallelization-caveats-in-r-1-the-basics-multiprocessing-and-multithreading-performance-eb584b7e850ehttps://towardsdatascience.com/parallelization-caveats-in-r-1-the-basics-multiprocessing-and-multithreading-performance-eb584b7e850e
rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#grab physical activity
load("PreppedData2023c_2050_KSA.Rda")
PA<-int.df%>%filter(Code==4.5)
cov_pa<-cov.df
pin_pa<-pin.groups

load("../for_parallel_processing/PreppedData2050_ssb.Rda")
source("../for_parallel_processing/fxns_2050.R")

covid.df<-read.csv("covid_df.csv", stringsAsFactors = F)
int.df<-bind_rows(int.df, PA)
cov.df<-cov_pa
pin.groups<-pin_pa

###############################################################################################################################
all.locs<- "Saudi Arabia"
interventions <-  c(4.5, 5.1, 5.3, 5.5, 5.6, 5.7)
total         <- length(all.locs)*length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

all.pin.l     <- all.dalys.l <- all.q30.l <-dadt.all.l<- d0.all.l<-d1.all.l<- list(total)

quality<-0.8 #
k = 1

  for (inter in interventions){

    base<-int.df%>%filter(location_name==all.locs, inter==Code)%>%pull(`Baseline Coverage`)%>%unique()
    
    # scale-up parameters, yes/no intersectoral policies, Calc PIN?, 1x covid shock
    projection <- project_pop(quality, all.locs, inter, 0, 1, 
                              "varying", 2022, 2025,
                              sel.cse, "yes", "yes", 1)
    
    all.pin.l[[k]]    = data.table(projection$pin.est) #already includes Code
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    all.q30.l[[k]]    = data.table(projection$q30df) %>% mutate(Code = inter)
    dadt.all.l[[k]]   = data.table(projection$DAdt) %>% mutate(Code = inter)
    
    k = k + 1
  }


intersectoral.pin      <- rbindlist(all.pin.l)
intersectoral.dalys    <- rbindlist(all.dalys.l)
intersectoral.q30      <- rbindlist(all.q30.l)
intersectoral.dadt      <- rbindlist(dadt.all.l)

save(intersectoral.pin, intersectoral.dalys, intersectoral.q30, intersectoral.dadt, file = paste0("full_BCR_int_Saudi Arabia.Rda"))



