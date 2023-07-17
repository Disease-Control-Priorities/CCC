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
load("PreppedData2050_ssb.Rda")
source("fxns_2050.R")

###############################################################################################################################
all.locs<-read.csv("../HLI/figures/HLI_CEA_country.csv", stringsAsFactors = F)%>%pull(location_name)%>%unique()
interventions <-  pin.groups %>% filter(calc_ICER=="yes", Code>5, Code<5.8) %>% pull(Code) %>% unique() %>% sort()
total         <- length(all.locs)*length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

all.pin.l     <- all.dalys.l <- all.q30.l <-dadt.all.l<- d0.all.l<-d1.all.l<- list(total)

quality<-0.7 #

parallelCluster <- makeCluster(16, type = "SOCK",methods = FALSE,outfile="log.txt") #Can I use more than 16?

setDefaultCluster(parallelCluster)
registerDoParallel(parallelCluster)

clusterEvalQ(cl = parallelCluster, {
  #setMKLthreads(1)    # set each cluster to single thread if worried about thrashing
  pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws)  
  # make sure we load the package on the cluster
  #setDTthreads(1)     # set each cluster to single thread if worried about thrashing
})

rank_fxn <- function(is,interventions,all.pin.l,all.dalys.l,all.q30.l, dadt.all.l, d0.all.l, d1.all.l) {
  k = 1
  for (inter in interventions){

    base<-int.df%>%filter(location_name==is, inter==Code)%>%pull(`Baseline Coverage`)%>%unique()
    
    # scale-up parameters, yes/no intersectoral policies, Calc PIN?, 1x covid shock
    projection <- project_pop(quality, is, inter, base, 1, 
                              "varying", 2022, 2030,
                              sel.cse, "yes", "yes", 1)
    
    all.pin.l[[k]]    = data.table(projection$pin.est) #already includes Code
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    all.q30.l[[k]]    = data.table(projection$q30df) %>% mutate(Code = inter)
    dadt.all.l[[k]]   = data.table(projection$DAdt) %>% mutate(Code = inter)
    d0.all.l[[k]]     = data.table(projection$D0cse)%>% mutate(Code = inter, location_name=is)   #ncd deaths baseline
    d1.all.l[[k]]     = data.table(projection$D1cse)%>% mutate(Code = inter, location_name=is)   #ncd deaths intervention
    
    k = k + 1
  }
  return(list(all.pin.l,all.dalys.l,all.q30.l, dadt.all.l, d0.all.l, d1.all.l))
}

# Run function
time1<-Sys.time()
everything <- foreach (is = all.locs, .combine ='rbind') %dopar% {
  rank_fxn(is,interventions,all.pin.l,all.dalys.l,all.q30.l, dadt.all.l, d0.all.l, d1.all.l)
}
time2<-Sys.time()
#End timer
time2-time1
#about 60 minutes on 16 cores

#Bind results, each of the (114) countries results stored in a list
all.pin <- rbindlist(everything[[1]])

for (i in 2:114){
  temp<-rbindlist(everything[[i]])
  all.pin<-rbind(all.pin, temp)
}

all.dalys <- rbindlist(everything[[115]])

for (i in 116:228){
  temp<-rbindlist(everything[[i]])
  all.dalys<-rbind(all.dalys, temp)
}

all.q30 <- rbindlist(everything[[229]])

for (i in 230:342){
  temp<-rbindlist(everything[[i]])
  all.q30<-rbind(all.q30, temp)
}

dadt.all <- rbindlist(everything[[343]])

for (i in 344:456){
  temp<-rbindlist(everything[[i]])
  dadt.all<-rbind(dadt.all, temp)
}

d0.all <- rbindlist(everything[[457]])

for (i in 458:570){
  temp<-rbindlist(everything[[i]])
  d0.all<-rbind(d0.all, temp)
}

d1.all <- rbindlist(everything[[571]])

for (i in 572:684){
  temp<-rbindlist(everything[[i]])
  d1.all<-rbind(d1.all, temp)
}

#########################################################################################

save(all.pin, all.dalys, all.q30, dadt.all, d0.all, d1.all, file = "output2050_target_intersectoral_ssb.Rda")

stopCluster(parallelCluster)

on.exit({
  try({
    cat("Attempting to stop cluster\n")
    stopImplicitCluster()        # package: `doParallel`
    stopCluster(parallelCluster) # package: `parallel`
  })
})

