##############################################################################################################
# Create all the base input files to be used in the demographic component of the analyses 
##############################################################################################################

rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws)
#Install Rtools
#library(usethis)
#library(devtools)
#devtools::install_github("mpascariu/MortalityLaws", force=TRUE)
#library(MortalityLaws)
##############################################################################################################
##############################################################################################################
# Functions
##############################################################################################################
##############################################################################################################

# For formatting
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

# If wanting to work in logit space
logit       <- function(x){qlogis(log(x), log.p = TRUE)}
invlogit    <- function(x){exp(plogis(x, log.p = TRUE))}

# Function to convert mortality rates by sex to life-expectancy (Mortality laws)
mxtoex      <- function(y){
  y1=convertFx(x=0:85, data = y[1:86], from = "mx", to = "ex")
  y2=convertFx(x=0:85, data = y[87:172], from = "mx", to = "ex")
  c(y1,y2)
}

# Function to aggregate single ages to 5 year age-groups (by sex)
combine.ages5 <- function(pop){
  get.sum <- function(pops){
    ds      <- rep(NA, 19)
    ds[1]   <- pops[1]
    ds[2]   <- sum(pops[2:5])
    k = 2
    for (i in seq(5,80,5) + 1){
      k = k + 1
      j = i + 4
      ds[k] <- sum(pops[i:j])
    }
    ds[19]  <- pops[86]
    ds
  }
  popf <- get.sum(pop[1:86])
  popm <- get.sum(pop[87:172])
  c(popf,popm)
}

# Function to aggregate 5 year age-grouped males and females to persons
get.pers <- function(pop){
  ds      <- rep(NA, 19)
  for (i in 1:19){
    ds[i] = pop[i] + pop[i + 19]
  }
  ds
}

# Function to estimate 40q30
# Stacked matrices/vectors pop (P), deaths (D) rows 1-86 Female, 87-172 Male
# specify number of years of data (n)

# For persons
get.q30 <- function(mx){
  calc.q <- function(mxs){100-100*prod(1 - (5 * mxs) / (1 + 2.5 * mxs))}
  calc.q(mx[8:15])   
}

get.q15 <- function(mx){
  calc.q <- function(mxs){100-100*prod(1 - (5 * mxs) / (1 + 2.5 * mxs))}
  calc.q(mx[5:13])   
}

q30.est <- function(P, D, n){
  P05 <- apply(P, 2, combine.ages5)[, 1:n]
  D05 <- apply(D, 2, combine.ages5)[, 1:n]
  
  P05p <- apply(P05, 2, get.pers)
  D05p <- apply(D05, 2, get.pers)
  
  mx.p <- D05p/P05p
  mx.p[is.na(mx.p)|mx.p < 0] <- 0
  mx.p[mx.p > 1]    <- 0.999
  
  apply(mx.p, 2, get.q30)
}

q15.est <- function(P, D, n){
  P05 <- apply(P, 2, combine.ages5)[, 1:n]
  D05 <- apply(D, 2, combine.ages5)[, 1:n]
  
  P05p <- apply(P05, 2, get.pers)
  D05p <- apply(D05, 2, get.pers)
  
  mx.p <- D05p/P05p
  mx.p[is.na(mx.p)|mx.p < 0] <- 0
  mx.p[mx.p > 1]    <- 0.999
  
  apply(mx.p, 2, get.q15)
}

# By sex
q30.est.sex <- function(P, D, n){
  P05 <- apply(P, 2, combine.ages5)[, 1:n]
  D05 <- apply(D, 2, combine.ages5)[, 1:n]
  
  Pf  <- P05[1:19,]; Df  <- D05[1:19,] 
  Pm  <- P05[20:38,]; Dm <- D05[20:38,] 
  
  mx.f                       <- Df/Pf
  mx.m                       <- Dm/Pm
  
  get.q30 <- function(mx){
    calc.q <- function(mxs){
      mxs[is.na(mxs)|mxs < 0] <- 0
      mxs[mxs > .999]              <- 0.999
      1-prod(1 - (5 * mxs) / (1 + 2.5 * mxs))
    }
    calc.q(mx[8:15])   
  }
  qf = apply(mx.f, 2, get.q30)
  qm = apply(mx.m, 2, get.q30)
  
  data.table(t(rbind(qf, qm))) %>%
    mutate(year_id=2019:2050) %>% rename(Female = qf, Male = qm) %>%
    gather(sex_name, val, -year_id)
}

##############################################################################################################
# load WPP data
##############################################################################################################

load("new_inputs/WPP_input_data.Rda")

locs          <- wpp.input %>% select(location_name, iso3) %>% distinct()
wpp.in        <- wpp.input %>% filter(year_id > 2009 & year_id < 2052) %>% 
  rename(age_name = age, year = year_id) %>% arrange(iso3, sex_name, year, age_name)


#calibrate to CCC data
pop.adj<-wpp.in%>%
  left_join(., read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
              select(iso3, wb2021))%>%
  group_by(year, wb2021)%>%
  summarise(pop = sum(Nx))%>%
  left_join(., read.csv("ccc_pop.csv", stringsAsFactors = F)%>%
  gather(year, ccc.pop, -region)%>%
  mutate(year = as.numeric(gsub("X", "", year)),
         ccc.pop = 1000000*as.numeric(ccc.pop),
         region = ifelse(region=="LMC", "LMIC", region))%>%
  rename(wb2021 = region))%>%
  filter(wb2021=="LIC"| wb2021=="LMIC")%>%
  mutate(adjustment = ifelse(is.na(ccc.pop),1, ccc.pop/pop))

wpp.in<-wpp.in%>%
  left_join(., read.csv("new_inputs/country_groupings.csv", stringsAsFactors = F)%>%
              select(iso3, wb2021))%>%
  left_join(., pop.adj%>%select(wb2021, adjustment, year))%>%
  mutate(adjustment = ifelse(is.na(adjustment),1, adjustment),
         Nx = Nx*adjustment)%>%
  select(-wb2021, -adjustment)

##############################################################################################################
# Countries included in analysis 
##############################################################################################################

country.lab <- fread("new_inputs/Country_groupings.csv") %>%
  rename(Country = location_gbd)%>%
  filter(!is.na(NCD_region) & Country != "")  %>%
  select(iso3) %>% left_join(locs, by = "iso3") %>% distinct()

countries   <- country.lab %>% pull(location_name) %>% sort() %>% unique()

##############################################################################################################
# GBD, GHE and NCD4 cause groupings mapped
##############################################################################################################

cse_g         <-  readxl::read_excel("new_inputs/GBDtoGHE.xlsx") %>%
  rename(ncd_cause = NCD4, cause_name=gbd, aff_frac = "affected fraction", ghe_cause = ghe) %>%
  select(ncd_cause, cause_name, ghe_cause, aff_frac) %>%
  filter(!is.na(aff_frac)) %>% distinct()

##############################################################################################################
# Import GHE data by cause
##############################################################################################################

format.ghe <- function(df){
  df %>% 
    select(DIM_COUNTRY_CODE, DIM_YEAR_CODE, DIM_AGEGROUP_CODE, DIM_SEX_CODE,
           DIM_GHECAUSE_TITLE, VAL_DEATHS_COUNT_NUMERIC) %>%
    rename(iso3=DIM_COUNTRY_CODE, year=DIM_YEAR_CODE, age_cat=DIM_AGEGROUP_CODE, 
           sex=DIM_SEX_CODE, ghe_cause=DIM_GHECAUSE_TITLE, deaths=VAL_DEATHS_COUNT_NUMERIC) %>%
    mutate(sex_name = ifelse(sex=="MLE", "Male", "Female")) %>%
    filter(ghe_cause %in% unique(cse_g$ghe_cause)) %>%
    right_join(data.table(age_cat = c("YEARS0-1", rep("YEARS1-4", each = 4), 
                                      rep(paste0("YEARS",seq(5,80,5),"-",seq(5,80,5)+4), each=5), "YEARS85PLUS"),
                          age_name = 0:85,
                          sc_obs = c(1, rep(0.25, each = 4), rep(0.2, each=80), 1)), by = "age_cat") %>%
    mutate(deaths = deaths * sc_obs, sc_obs = NULL, age_cat = NULL, sex = NULL)
}

ghe.in           <- rbind(fread("new_inputs/ghe data/data_2010.csv"),
                          fread("new_inputs/ghe data/data_2015.csv"),
                          fread("new_inputs/ghe data/data_2019.csv")) %>% 
  format.ghe() %>%
  mutate(All = ifelse(ghe_cause == "All Causes", deaths, 0)) %>%
  group_by(iso3, year, sex_name, age_name) %>%
  mutate(All = sum(All, na.rm = T)) %>% ungroup() %>%
  mutate(frac = deaths/All, All = NULL, deaths = NULL) %>% 
  left_join(locs, by = "iso3") %>%
  right_join(cse_g %>% select(ghe_cause, cause_name, aff_frac), by = "ghe_cause") %>%
  mutate(cause_frac = frac*aff_frac, aff_frac = NULL, ghe_cause = NULL, frac = NULL)


##############################################################################################################
# Coverage and efficacy data
##############################################################################################################

coverage      <- fread("new_inputs/Coverage0621.csv")%>%
  select(-X2.1)%>%
  rename(X2.1=X2.10)

efficacy      <- readxl::read_excel("new_inputs/DCP3_ NCD Data (1).xlsx", sheet='interventions') %>%
  rename(Code=NCD, agel = `start age`, ageu = `stop age`) %>%
  mutate(cause_name= ifelse(Level4 != "NA", `Level4`, `Level3`),
         ageu = ifelse(ageu=="95+","85",ageu), 
         ageu = as.numeric(ageu), 
         agel = as.numeric(agel),
         ageu = ifelse(ageu>85,85,ageu))

#ncd4          <- efficacy %>% select(Code, NCD4, calc_ICER) %>% distinct()

efficacy2     <- efficacy %>%
  select(Intervention, Code,  sub_id, metric, cause_name, `Mortality reduction`, agel, ageu) %>%
  filter(!is.na(ageu)) %>%
  left_join(cse_g, by = "cause_name") %>% 
  mutate(Code = ifelse(is.na(`Mortality reduction`), "NoCode", Code),
         sub_id = ifelse(is.na(`Mortality reduction`), 1, sub_id),
         `Mortality reduction` = ifelse(is.na(`Mortality reduction`), 0, `Mortality reduction`),
         agel = ifelse(is.na(agel), 0, agel), ageu = ifelse(is.na(ageu), 85, ageu),
         aff_frac = NULL, codex = paste0(Code,"0")) %>%
  mutate(codex = ifelse(codex=="2.140", "2.14", codex))

cov.df        <- coverage %>% 
  select(-c(WB_region15, GBD_region, Country, LocID)) %>% distinct() %>%
  gather(codex, `Baseline (P0)`, -iso3) %>%
  mutate(codex = paste0(substring(codex, 2),"0")) %>%
  mutate(codex = ifelse(codex=="2.140", "2.14", codex))

##############################################################################################################
# Bring the intervention, mortality reduction and coverage data together
##############################################################################################################

 int.df     <- cov.df %>% filter(codex %in% unique(efficacy2$codex)) %>%
  left_join(efficacy2, by = "codex") %>% 
  filter(!is.na(`Mortality reduction`) & cause_name != "NA") %>% distinct() %>% 
  mutate(max.cov = 0.95) %>% 
  rename(`Baseline Coverage` =`Baseline (P0)`) %>% select(-c(codex)) %>%
  arrange(iso3, Code, sub_id) %>% right_join(country.lab, by = "iso3")

##############################################################################################################
# 1) Calibrate the GHE cause fractions to the CCPM
# 2) Get the secular trend for Non-NCDs and NCDs
##############################################################################################################

all.mx     <- ghe.in %>% 
  mutate(cause = ifelse(cause_name=="All Causes", "All", "NCD")) %>%
  group_by(location_name, iso3, sex_name, age_name, year, cause) %>%
  summarise(frac = sum(cause_frac, na.rm = T), .groups = "drop") %>% ungroup() %>%
  filter(year %in% c(2010, 2015, 2019)) %>%
  left_join(wpp.in %>% select(iso3, sex_name, age_name, year, mx), 
            by = c("iso3", "sex_name", "age_name", "year")) %>%
  mutate(mx = frac*mx, frac = NULL) %>%
  spread(cause, mx) %>%
  mutate(nonNCD = All - NCD, All = NULL) %>%
  gather(cause_name, mx, -location_name, -iso3, -sex_name, -age_name, -year) %>%
  spread(year, mx) %>% 
  mutate(bg_trend = ifelse((`2010` == 0 & `2015` == 0) | `2019` == 0, 0, 
                           ifelse(`2010` == 0 & `2015`!= 0 & `2019`!= 0, 
                                  1/4*log(`2019`/`2015`), 
                                  1/9*log(`2019`/`2010`)))) %>%
  select(-c("2010", "2015")) %>% rename(mx = "2019") 

all.mx     <- ghe.in %>% 
  group_by(location_name, iso3, sex_name, age_name, year, cause_name) %>%
  summarise(frac = sum(cause_frac, na.rm = T), .groups = "drop") %>% ungroup() %>%
  filter(year %in% c(2010, 2015, 2019)) %>%
  left_join(wpp.in %>% select(iso3, sex_name, age_name, year, mx), 
            by = c("iso3", "sex_name", "age_name", "year")) %>%
  mutate(mx = frac*mx, frac = NULL) %>%
  spread(year, mx) %>% 
  mutate(bg_trend = ifelse((`2010` == 0 & `2015` == 0) | `2019` == 0, 0, 
                           ifelse(`2010` == 0 & `2015`!= 0 & `2019`!= 0, 
                                  1/4*log(`2019`/`2015`), 
                                  1/9*log(`2019`/`2010`)))) %>%
  select(-c("2010", "2015")) %>% rename(mx = "2019") %>%
  rbind(all.mx)

cause.mx     <- ghe.in %>% 
  filter(cause_name!="All Causes" & year %in% c(2010, 2015, 2019)) %>%
  left_join(wpp.in %>% select(iso3, sex_name, age_name, year, mx), 
            by = c("iso3", "sex_name", "age_name", "year")) %>%
  mutate(mx = cause_frac*mx, cause_frac = NULL) %>%
  arrange(iso3, year, cause_name, sex_name, age_name) 

unique(cause.mx$cause_name)

##############################################################################################################
# Salt Impact
##############################################################################################################

salt_eff  <- fread("new_inputs/salt_policy_effects.csv") %>%
  rename(cause_name=Outcome) %>%
  filter(!is.na(cause_name)) %>%
  select(iso3, cause_name, Mortality.reduction) %>%
  spread(cause_name, Mortality.reduction) %>%
  gather(cause_name, Mortality.reduction, -iso3) %>%
  mutate(Mortality.reduction = ifelse(is.na(Mortality.reduction), 0, Mortality.reduction)) %>%
  #mutate(salt_impact = 1 - Mortality.reduction) %>%
  rename("Mortality reduction" = Mortality.reduction)%>%
  select(iso3, cause_name, `Mortality reduction`) %>%
  left_join(country.lab, by = "iso3")%>%
  mutate(Code = 5.5, `Baseline Coverage` = 0, Intervention='Salt policy',
         agel = 0, ageu = 85, max.cov=1)%>%
  group_by(location_name)%>%mutate(sub_id = row_number())%>%ungroup()%>%
  left_join(., fread("new_inputs/saltcoverage_simplified.csv")%>%select(iso3, mort.redux, target.coverage))%>%
  mutate(`Mortality reduction` = mort.redux*(0.039/0.0566))%>%
  select(-mort.redux)


salt_eff<-na.omit(left_join(salt_eff, cse_g, by="cause_name")%>%mutate(`Mortality reduction` = `Mortality reduction`*aff_frac)%>%
                    select(-c(aff_frac)))


##############################################################################################################
# Trans fat Impact
##############################################################################################################

tf_eff  <- fread("new_inputs/tfa_policy_effects_simplified.csv") %>%
  rename(cause_name=Outcome) %>%
  filter(!is.na(cause_name)) %>%
  select(iso3, cause_name, mort.redux.filled) %>%
  spread(cause_name, mort.redux.filled) %>%
  gather(cause_name, mort.redux.filled, -iso3) %>%
  mutate(mort.redux.filled = ifelse(is.na(mort.redux.filled), 0, mort.redux.filled)) %>%
  #mutate(transfat_impact = 1 - Mortality.reduction) %>%
  rename("Mortality reduction" = mort.redux.filled)%>%
  select(iso3, cause_name, `Mortality reduction`) %>%
  left_join(country.lab, by = "iso3")%>%
  mutate(Code = 5.6, `Baseline Coverage` = 0, Intervention='Tans fat policy',
         agel = 0, ageu = 85, max.cov=1)%>%
  group_by(location_name)%>%mutate(sub_id = row_number())%>%ungroup()

tf_eff<-na.omit(left_join(tf_eff, cse_g, by="cause_name")%>%mutate(`Mortality reduction` = `Mortality reduction`*aff_frac)%>%
                  select(-c(aff_frac)))%>%mutate(target.coverage=1)

##############################################################################################################
# Tobacco and alcohol
##############################################################################################################

ta_eff  <- fread("new_inputs/tobaccoandalcohol_efficacy6_simplified.csv") %>%
  filter(NCD4 == "yes") %>% rename(cause_name = Outcome, risk = Risk) %>%
  mutate(iso3 = countrycode::countrycode(Country, "country.name", "iso3c")) %>%
  select(iso3, cause_name, Mortality.reduction.policy, Mortality.reduction.tax, risk)  %>%
  filter(cause_name %in% c("All Causes", cse_g$cause_name)) %>%
  left_join(country.lab, by = "iso3")

ta_delay<- readxl::read_excel("new_inputs/TobaccoAlcoholDelayedImpact.xlsx") %>% 
  rename(year_id = Year) %>%
  gather(Link, delay, -year_id) %>%
  spread(year_id, delay) %>% 
  mutate(`2031` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2030`) - logit(`2029`))),
         `2032` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2031`) - logit(`2030`))),
         `2033` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2032`) - logit(`2031`))),
         `2034` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2033`) - logit(`2032`))),
         `2035` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2034`) - logit(`2033`))),
         `2036` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2035`) - logit(`2034`))),
         `2037` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2036`) - logit(`2035`))),
         `2038` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2037`) - logit(`2036`))),
         `2039` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2038`) - logit(`2037`))),
         `2040` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2039`) - logit(`2038`))),
         `2041` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2040`) - logit(`2039`))),
         `2042` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2041`) - logit(`2040`))),
         `2043` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2042`) - logit(`2041`))),
         `2044` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2043`) - logit(`2042`))),
         `2045` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2044`) - logit(`2043`))),
         `2046` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2045`) - logit(`2044`))),
         `2047` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2046`) - logit(`2045`))),
         `2048` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2047`) - logit(`2046`))),
         `2049` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2048`) - logit(`2047`))),
         `2050` = ifelse(`2030`== 1, 1, invlogit(2*logit(`2049`) - logit(`2048`)))
         ) %>%
  gather(year_id, delay, -Link) %>% mutate(year_id = as.numeric(year_id)) %>%
  arrange(Link, year_id)

ta_delay<- fread("new_inputs/CauseLinkforTobAlcDelay.csv") %>% 
  rename(cause_name = `GBD Cause`, risk = "Risk factor") %>% 
  left_join(ta_delay, by = "Link") %>% mutate(ic = year_id - 2018) %>%
  select(cause_name, risk, ic, delay) %>% spread(ic, delay) %>% 
  mutate(`1` = 0,`2` = 0,`3` = 0,`4` = 0) %>%
  gather(ic, delay, -cause_name, -risk) %>% mutate(ic = as.numeric(ic)) %>%
  arrange(cause_name, risk, ic) %>%
  spread(risk, delay) %>% rename(Smoking = Tobacco) %>%
  mutate(Alcohol = ifelse(is.na(Alcohol), 0, Alcohol), 
         Smoking = ifelse(is.na(Smoking), 0, Smoking)) %>%
  filter(cause_name %in% c("All Causes", cse_g$cause_name))


tobp_eff <- ta_eff %>% filter(!is.na(location_name), risk=="Smoking") %>%
  select(iso3, cause_name, Mortality.reduction.policy) %>% 
  rename(`Mortality reduction` = Mortality.reduction.policy) %>%
  mutate(`Mortality reduction` = ifelse(is.na(`Mortality reduction`), 0, `Mortality reduction`))%>%
  select(iso3, cause_name, `Mortality reduction`) %>%
  left_join(country.lab, by = "iso3")%>%
  mutate(Code = 5.3, `Baseline Coverage` = 0, Intervention='Tobacco policy',
         agel = 0, ageu = 85, max.cov=1)%>%
  group_by(location_name)%>%mutate(sub_id = row_number())%>%ungroup()%>%
  mutate(target.coverage = 1)


tobp_eff<-na.omit(left_join(tobp_eff, cse_g, by="cause_name")%>%mutate(`Mortality reduction` = `Mortality reduction`*aff_frac)%>%
                    select(-c(aff_frac)))

tobt_eff <- ta_eff %>% filter(!is.na(location_name), risk=="Smoking") %>%
  select(iso3, cause_name, Mortality.reduction.tax) %>% 
  rename(`Mortality reduction` = Mortality.reduction.tax) %>%
  mutate(`Mortality reduction` = ifelse(is.na(`Mortality reduction`), 0, `Mortality reduction`))%>%
  select(iso3, cause_name, `Mortality reduction`) %>%
  left_join(country.lab, by = "iso3")%>%
  mutate(Code = 5.1, `Baseline Coverage` = 0, Intervention='Tobacco tax',
         agel = 0, ageu = 85, max.cov=1)%>%
  group_by(location_name)%>%mutate(sub_id = row_number())%>%ungroup()

tobt_eff<-na.omit(left_join(tobt_eff, cse_g, by="cause_name")%>%mutate(`Mortality reduction` = `Mortality reduction`*aff_frac)%>%
                    select(-c(aff_frac)))%>%mutate(target.coverage = ifelse(`Mortality reduction`==0,0,1))

aclp_eff <- ta_eff %>% filter(!is.na(location_name), risk=="Alcohol") %>%
  select(iso3, cause_name, Mortality.reduction.policy) %>% 
  rename(`Mortality reduction` = Mortality.reduction.policy) %>%
  mutate(`Mortality reduction` = ifelse(is.na(`Mortality reduction`), 0, `Mortality reduction`))%>%
  select(iso3, cause_name, `Mortality reduction`) %>%
  left_join(country.lab, by = "iso3")%>%
  mutate(Code = 5.4, `Baseline Coverage` = 0, Intervention='Alcohol policy',
         agel = 0, ageu = 85, max.cov=1)%>%
  group_by(location_name)%>%mutate(sub_id = row_number())%>%ungroup()

aclp_eff<-na.omit(left_join(aclp_eff, cse_g, by="cause_name")%>%mutate(`Mortality reduction` = `Mortality reduction`*aff_frac)%>%
                    select(-c(aff_frac)))%>%mutate(target.coverage=1)

#add alcohol ban ifelse()

aclt_eff <- ta_eff %>% filter(!is.na(location_name), risk=="Alcohol") %>%
  select(iso3, cause_name, Mortality.reduction.tax) %>% 
  rename(`Mortality reduction` = Mortality.reduction.tax) %>%
  mutate(`Mortality reduction` = ifelse(is.na(`Mortality reduction`), 0, `Mortality reduction`))%>%
  select(iso3, cause_name, `Mortality reduction`) %>%
  left_join(country.lab, by = "iso3")%>%
  mutate(Code = 5.2, `Baseline Coverage` = 0, Intervention='Alcohol tax',
         agel = 0, ageu = 85, max.cov=1)%>%
  group_by(location_name)%>%mutate(sub_id = row_number())%>%ungroup()

aclt_eff<-na.omit(left_join(aclt_eff, cse_g, by="cause_name")%>%mutate(`Mortality reduction` = `Mortality reduction`*aff_frac)%>%
                    select(-c(aff_frac)))%>%mutate(target.coverage=1)

#merge with other interventions#
int.df<-rbind(int.df%>%mutate(target.coverage = 1), salt_eff%>%mutate(metric="case fatality"), 
              tf_eff%>%mutate(metric="case fatality"), tobp_eff%>%mutate(metric="case fatality"), 
              tobt_eff%>%mutate(metric="case fatality"), aclt_eff%>%mutate(metric="case fatality"), 
              aclp_eff%>%mutate(metric="case fatality"))
#update baseline coverage assumptions#
int.df<-left_join(int.df, cov.df%>%rename(Code = codex)%>%mutate(Code=as.numeric(Code)))%>%
  mutate(`Baseline Coverage` = `Baseline (P0)`)%>%select(-`Baseline (P0)`)

##############################################################################################################
# Files for population in need
##############################################################################################################

pin.groups <- readxl::read_excel("new_inputs/DCP3_ NCD Data (1).xlsx", sheet='costs') %>% 
  filter(NCD<=5.6) %>%
  rename(Code = NCD) %>% filter(!is.na(Adjustment1)) %>%
  rename(cause_name = Epi1, metric_name = Measure1, 
         age_l = StartAge1, age_u=StopAge1, sex_name=Sex1, Adjustment = Adjustment1) %>%
  mutate(age_u = ifelse(age_u=="95+","85",age_u), age_u = as.numeric(age_u), age_l = as.numeric(age_l), 
         Adjustment = as.numeric(Adjustment)) %>%
  select(calc_ICER, Code, sub_id, cause_name, metric_name, age_l, age_u, sex_name, Adjustment)

#might need to add some causes...
#https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/7e58e39f315fa0fe393bc75bd3b71be0
library(tidyverse)

add.causes<-fread("new_inputs/new_causes.csv")%>%
  mutate(age = str_remove_all(age, " years"),
         age = gsub("-", " to ", age),
         age = ifelse(age=="85+", "85 plus", age))

pin.data   <- rbind(fread("new_inputs/PIN_heartfailuredata.csv"),
                    fread("new_inputs/pin_data (2).csv") %>%
                      mutate(rei = "None")) %>%
  bind_rows(., add.causes)%>%
  filter(location != "Global" & metric == "Rate" & cause != "All causes" & year == 2019) %>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c")) %>%
  right_join(country.lab, by = "iso3") %>%
  rename(age_cat = age, cause_name = cause, sex_name = sex) %>% 
  right_join(data.table(age_cat = c("<1 year", rep("1 to 4", each = 4), 
                                    rep(paste0(seq(5,80,5)," to ",seq(5,80,5)+4), each=5), "85 plus"),
                        age_name = 0:85), 
             by = "age_cat") %>%
  select(location_name, sex_name, age_name, cause_name, measure, val) %>%
  mutate(val = val*1e-5) %>% spread(measure, val) %>%
  rename(R_inc  = Incidence, R_prev = Prevalence) %>%
  mutate(R_inc  = ifelse(is.na(R_inc), 0, R_inc),
         R_prev = ifelse(is.na(R_prev), 0, R_prev),
         R_pop  = NA) %>%
  rbind(wpp.in %>% 
          filter(year == 2019 & iso3 %in% unique(country.lab$iso3)) %>% 
          select(location_name, sex_name, age_name) %>%
          mutate(cause_name = "All", R_inc = NA, R_prev = NA, R_pop = 1)) %>%
  arrange(location_name, cause_name, sex_name, age_name)

##############################################################################################################
# Age groups for each intervention
##############################################################################################################

age.code <- int.df %>% select(Code, agel, ageu) %>% distinct()

##############################################################################################################
# Ratio of yll to yld to calculate dalys
##############################################################################################################
#https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/db58dc39dd2077abc2d50fac17387ae4
addyld<-fread("new_inputs/new_yld.csv")%>%
  select(val, cause, measure, location)%>%
  spread(measure,val)%>%
  rename(YLD = `YLDs (Years Lived with Disability)`,
         YLL = `YLLs (Years of Life Lost)`)%>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))%>% 
  right_join(country.lab, by = "iso3")%>%
  select(-location)

#update with self-harm YLLs
#https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/84b07312d09f5ee6e119b115739ac210
self.harm<- fread("new_inputs/IHME-GBD_2019_DATA-0a3dc000-1.csv")%>%
  filter(cause=="Self-harm")%>%
  select(location, val)%>%
  rename(sh_yll = val)%>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))%>% 
  right_join(country.lab, by = "iso3")%>%
  select(-location)

addyld<-addyld%>%
  left_join(., self.harm)%>%
  mutate(YLL = ifelse(cause=="Bipolar disorder", sh_yll*0.051 ,YLL),
         YLL = ifelse(cause=="Major depressive disorder", sh_yll*0.461 ,YLL),
         YLL = ifelse(cause=="Schizophrenia", sh_yll*0.047 ,YLL))%>%
  select(-sh_yll)

yldyll <- fread("new_inputs/YLDtoYLLratio.csv") %>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))%>% 
  right_join(country.lab, by = "iso3")%>%
  select(-location)%>%
  bind_rows(., addyld)%>%
  rename(cause_name = cause) %>% 
  mutate(scale = YLD/YLL, 
         scale = ifelse(is.na(scale),1,scale),
         scale = ifelse(cause_name %in% c("Bipolar disorder",
                                          "Major depressive disorder",
                                          "Schizophrenia") & scale>6, 6, scale)) %>%  
  select(iso3, location_name, cause_name, scale) %>%
  filter(cause_name %in% c("All Causes", cse_g$cause_name))

##############################################################################################################
# Background trends by cause group
##############################################################################################################

nonNCD.bg <- all.mx %>% filter(cause_name == "nonNCD") %>% arrange(iso3, sex_name, age_name)  
NCD.bg    <- all.mx %>% filter(cause_name %!in% c("NCD", "nonNCD")) %>% 
  arrange(iso3, cause_name, sex_name, age_name)

##############################################################################################################
# COVID data from WHO
##############################################################################################################
covid.df<-fread("covid_df.csv")
#write.csv(covid.df, "covid_df.csv", row.names = F)
#covid.df <- fread("https://covid19.who.int/WHO-COVID-19-global-data.csv") %>% 
#  rename(Name = Country, iso2 = Country_code) %>%
#  filter(Name %!in% c("Global") & WHO_region != "Other") %>%  
#  mutate(date = as.Date(Date_reported,"%Y-%m-%d"),
#         Name =ifelse(Name == "Curaçao", "Curacao", 
#                      ifelse(Name == "Saint Barthélemy", "Saint Barthelemy", 
#                             ifelse(Name == "Réunion", "Reunion", Name))),
#         iso3 = countrycode::countrycode(Name, "country.name", "iso3c")) %>%
#  mutate(iso3 = ifelse(Name %in% c("Bonaire","Saba","Sint Eustatius"), "BES", 
#                       ifelse(Name == "Kosovo[1]", "XKX", ifelse(Name == "Saint Martin", "MAF", iso3))),
#         Name = ifelse(iso3 == "BES", "Bonaire, Sint Eustatius and Saba", Name),
#         month  = lubridate::month(lubridate::ymd(date)), 
#         year = lubridate::year(lubridate::ymd(date)),
#         half = case_when(month <= 6  ~ 1,
#                          month <= 12  ~ 2), 
#         year.h = paste(year, half)) %>%
#  group_by(iso3, year.h) %>%
#  summarise(deathsn=sum(New_deaths, na.rm=T), .groups = "drop") %>% ungroup() %>%
#  spread(year.h, deathsn) %>% 
#  mutate(t202 = sum(`2020 2`),
#         t211 = sum(`2021 1`), 
#         r1 = t211/t202, 
#         r2 = ifelse(`2020 2` ==0 | `2021 1`/`2020 2` > r1, r1,`2021 1`/`2020 2`),
#         `2021 2` = r2*`2021 1`, 
#         `2020` = `2020 1` + `2020 2`, `2021` = `2021 1` + `2021 2`,
#         `2022` = 0.5*`2021`) %>%
#  select(iso3, "2020", "2021", "2022") %>%
#  right_join(country.lab, by = "iso3") %>%
#  gather(year_id, covid, -iso3, -location_name) %>% mutate(year_id = as.numeric(year_id))

##############################################################################################################
# Files to drop from workspace
##############################################################################################################

drops <- c("ghe.in", "wpp.input", "efficacy", "efficacy2", "tobp_eff", "tobt_eff",
           "loc", "locs","coverage", "all.mx", "obs_wpp", "addyld", "add.causes",
           "self.harm")
rm(list = c(drops,"drops"))

##############################################################################################################

save.image(file = "new_inputs/PreppedData2050.Rda")
save.image(file = "for_parallel_processing/PreppedData2050.Rda")


