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
load("PreppedData2023.Rda")
source("fxns.R")

###############################################################################################################################
hics<-read.csv("country_groupings.csv", stringsAsFactors = F)%>%
  filter(wb2021=="UMIC")%>%pull(location_gbd)
#problems with data from Palestine and Puerto Rico
all.locs      <- data.frame(loc=c(countries[c(1:118, 120:126, 128:175)]))
all.locs      <-as.character(all.locs%>%filter(loc%in%hics)%>%pull(loc)) #47 countries
interventions <-  pin.groups %>% filter(calc_ICER=="yes", Code>5, Code<5.7) %>% pull(Code) %>% unique() %>% sort()
total         <- length(all.locs)*length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

all.pin.l     <- all.dalys.l <- all.q30.l <-dadt.all.l<- list(total)

parallelCluster <- makeCluster(2, type = "SOCK",methods = FALSE,outfile="log.txt") 
quality<-0.7 #

setDefaultCluster(parallelCluster)
registerDoParallel(parallelCluster)

clusterEvalQ(cl = parallelCluster, {
  #setMKLthreads(1)    # set each cluster to single thread if worried about thrashing
  pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws)  
  # make sure we load the package on the cluster
  #setDTthreads(1)     # set each cluster to single thread if worried about thrashing
})

#set targets#
int.df<- int.df%>%
  mutate(target.coverage = 1)%>%
  distinct()

rank_fxn <- function(is,interventions,all.pin.l,all.dalys.l,all.q30.l, dadt.all.l) {
  k = 1
  for (inter in interventions){

    base<-int.df%>%filter(location_name==is, inter==Code)%>%pull(`Baseline Coverage`)%>%unique()
    target<-int.df%>%filter(location_name==is, inter==Code)%>%pull(`target.coverage`)%>%unique()
    stop_year<-2030
    
    # scale-up parameters, yes/no intersectoral policies, Calc PIN?, 1x covid shock
    projection <- project_pop(quality, is, inter, base, target, 
                              "varying", 2022, stop_year,
                              sel.cse, "yes", "yes", 1)
    
    all.pin.l[[k]]    = data.table(projection$pin.est) #already includes Code
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    all.q30.l[[k]]    = data.table(projection$q30df) %>% mutate(Code = inter)
    dadt.all.l[[k]]   = data.table(projection$DAdt) %>% mutate(Code = inter)
    
    
    k = k + 1
  }
  return(list(all.pin.l,all.dalys.l,all.q30.l, dadt.all.l))
}

# Run function
time1<-Sys.time()
everything <- foreach (is = all.locs, .combine ='rbind') %dopar% {
  rank_fxn(is,interventions,all.pin.l,all.dalys.l,all.q30.l, dadt.all.l)
}
time2<-Sys.time()
#End timer
time2-time1

#Bind results, each of the (47) countries results stored in a list
all.pin <- rbindlist(everything[[1]])

for (i in 2:47){
  temp<-rbindlist(everything[[i]])
  all.pin<-rbind(all.pin, temp)
}

all.dalys <- rbindlist(everything[[48]])

for (i in 49:94){
  temp<-rbindlist(everything[[i]])
  all.dalys<-rbind(all.dalys, temp)
}

all.q30 <- rbindlist(everything[[95]])

for (i in 96:141){
  temp<-rbindlist(everything[[i]])
  all.q30<-rbind(all.q30, temp)
}

dadt.all <- rbindlist(everything[[142]])

for (i in 143:188){
  temp<-rbindlist(everything[[i]])
  dadt.all<-rbind(dadt.all, temp)
}

#########################################################################################

save(all.pin, all.dalys, all.q30, dadt.all, file = "output2023_target_intersectoral_UMICs.Rda")

stopCluster(parallelCluster)

on.exit({
  try({
    cat("Attempting to stop cluster\n")
    stopImplicitCluster()        # package: `doParallel`
    stopCluster(parallelCluster) # package: `parallel`
  })
})

