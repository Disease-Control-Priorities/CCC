

###############################################################################
# Cohort Component Population Projection Model 
###############################################################################

#' @param is         : Selected location for the projection (location name)
#' @param inter      : Vector or single code for the intervention 
#' @param inc.val    : scale-up in coverage of interventions
#' @param tas        : tobacco, alcohol or salt interventions (yes or no), applied from year 5 (2023)
#' @param sel.cse    : the set of causes to use in estimating 40q30, default is all NCD4 related
#' @param inc.type   : scale-up will be either increasing per year or fixed to a number from 2023
#' @param est.pin    : whether or not to estimate population in need e.g. PIN no included for cause AROC or COVID
#' @param covid      : include a COVID shock for 2020 and 2021 (yes or no)
#' @return A list with various outputs to use for further analyses

project_pop <- function(is, inter, inc.val, tas, sel.cse, inc.type, est.pin, covid.in, sc.covid= NULL){
  # fixed parameters
  n = 12; year0 = 2019; gender_lab = c("Female", "Male"); sex_ratio = 0.5; 
  x = 0:85; z = length(x); w = 2*z; ninters = length(inter) 
  
  ###########################################################################################################
  # Intervention data
  
  in.df.in    <-  int.df %>% 
    filter(location_name == is, metric=="case fatality") %>% 
    arrange(Code, sub_id) %>% 
    select(location_name, Intervention, Code, sub_id, cause_name, `Mortality reduction`,`Baseline Coverage`) %>%
    mutate(`Baseline Coverage`=as.numeric(`Baseline Coverage`),
           inc = ifelse(Code %in% inter, (inc.val-`Baseline Coverage`)/8, 0),
           inc = ifelse(inc<0,0,inc)) %>%
    rename(`Effect (I)` = "Mortality reduction") 
  
  dis.df.in<-  int.df %>% 
    filter(location_name == is,
           Code %in% inter) %>% 
    arrange(Code, sub_id) %>% 
    select(location_name, Intervention, Code, sub_id, cause_name, `Mortality reduction`, metric, `Baseline Coverage`) %>%
    mutate(`Baseline Coverage`=as.numeric(`Baseline Coverage`),
           inc = ifelse(Code %in% inter, (inc.val-`Baseline Coverage`)/8, 0)) %>%
    rename(`Effect (I)` = "Mortality reduction") %>%
    distinct()
  
  # List of causes related to the interventions
  cse.yll     <- in.df.in %>% filter(Code %in% inter) %>% pull(cause_name) %>% unique() %>% sort()
  
  # Demographic data
  wpp.ina     <- wpp.in %>% 
    filter(location_name == is & year %in% 2019:2030) %>% select(-c(location_name, iso3)) %>%
    arrange(sex_name, age_name, year)
  
  Nxa         <- wpp.ina %>%
    select(sex_name, age_name, year, Nx) %>% 
    spread(year, Nx) %>% arrange(sex_name, age_name) %>%
    select(-c(sex_name, age_name)) %>% as.matrix() 
  
  fxa        <- wpp.ina %>%
    select(sex_name, age_name, year, fx) %>% 
    spread(year, fx) %>% arrange(sex_name, age_name) %>%
    select(-c(sex_name, age_name)) %>% as.matrix() 
  
  miga        <- wpp.ina %>%
    select(sex_name, age_name, year, mig) %>% 
    spread(year, mig) %>% arrange(sex_name, age_name) %>%
    select(-c(sex_name, age_name)) %>% as.matrix()  
  
  ####################################################################################################  
  # Now run the projection based on all assumptions
  ####################################################################################################
  
  ta.df   <- ta_eff %>% filter(location_name == is) 
  salt.df <- salt_eff %>% filter(location_name == is)
  tf.df   <- tf_eff %>% filter(location_name == is) 
  
  
  
  if (any(dis.df.in$metric=="disability")){
    #not super robust, assumes only single impact on each cause (as is in the input file for this project)
    #may need updating if using for another paper
    dis.dfb<- dis.df.in %>%
      mutate(year_id=year0)%>%
      mutate(ic = ifelse((2022-year_id)>0, 0, (year_id-2022)),
             Pred = `Baseline Coverage` + ic*inc,
             `Target (P1)`= ifelse(Pred > 1, 1, Pred), #this will hold coverage at 100% after achievement**
             `Target (P1)`= ifelse(`Target (P1)` < `Baseline Coverage`, `Baseline Coverage`, `Target (P1)`),
             `Impact` = 0.7*((`Effect (I)`*(`Target (P1)`-`Baseline Coverage`))/
                                   (1-(`Effect (I)`*`Baseline Coverage`))))
    
    for(i in 2:n){
      dis.dfb<-in.dfb             <- dis.df.in %>%
        mutate(year_id=year0+(i-1))%>%
        mutate(ic = ifelse((2022-year_id)>0, 0, (year_id-2022)),
               Pred = `Baseline Coverage` + ic*inc,
               `Target (P1)`= ifelse(Pred > 1, 1, Pred), #this will hold coverage at 100% after achievement**
               `Target (P1)`= ifelse(`Target (P1)` < `Baseline Coverage`, `Baseline Coverage`, `Target (P1)`),
               `Impact` = 0.7*((`Effect (I)`*(`Target (P1)`-`Baseline Coverage`))/
                                     (1-(`Effect (I)`*`Baseline Coverage`))))%>%mutate(year_id=2019+i-1)%>%
        bind_rows(.,dis.dfb)
      
    }
    
    
    cse.disab<-data.frame(cause_name=cse.yll) %>%
      left_join(yldyll %>% filter(location_name==is), by = "cause_name") %>%
      filter(!is.na(scale))%>%pull(cause_name)%>%unique()
    
    dft.dis  <- dis.dfb%>%
      full_join(., merge(data.frame(cause_name=cse.disab),
                         data.frame(year_id=2019:2030)))%>%
      merge(., data.table(age_name = 0:85))%>%
      filter(year_id>=2020)%>%
      distinct()
    
    disab.df <- dft.dis %>% 
      left_join(age.code, by = "Code") %>% 
      mutate(Imp = (1 - Impact)) %>% 
      mutate(Imp = ifelse(metric=="case fatality", 1, Imp))%>%
      mutate(Imp = ifelse(age_name >= agel & age_name <= ageu, Imp, 1))%>%
      arrange(year_id, age_name)%>%
      select(cause_name, Imp, age_name, year_id)%>%
      mutate(sex="Female")%>%
      group_by(sex, year_id, age_name, cause_name)%>%
      summarise(Imp = prod(Imp))
    
    #stack for two sexes
    disab.df<-disab.df%>%bind_rows(disab.df%>%mutate(sex="Male"))%>%
      arrange(year_id,cause_name,sex)%>%filter(cause_name %in% cse.disab)%>%
      mutate(Imp = ifelse(is.na(Imp),1,Imp))
    
  }
  
  doproj <- function(s.dp){
    
    # Defaults
    P                    <- matrix(0, nrow = w, ncol = n+1)
    mx.mat <- mx.dis.mat <- matrix(0, nrow = w, ncol = n)
    P[,1]                <- Nxa[,1] 
    births               <- array(dim = c(2,n))
    mx.pin.all <- tibble(cause_name=character(), sex_name=character(), 
                         year_id=integer(), age_name=integer(), mx.dis=integer())
    
    for(i in 1:n){
      ########################################################################################################
      # Tobacco, salt and transfats (placed here but not relevant for ICER)
      
      tax.df1 <- tax.df2 <- ta.df %>% filter(ic == i) %>% 
        mutate(Impact = 1 - (1-Alcohol_policy)*(1-Alcohol_tax)*(1-Smoking_policy)*(1-Smoking_tax), 
               Code = "alcohol_smoking", age_name = 0) %>%
        select(cause_name, Code, Impact, age_name)
      for (a in 1:85){
        tax.df1 <- tax.df1 %>% mutate(age_name = a)
        tax.df2 <- rbind(tax.df2, tax.df1)
      }
      tax.df2 <- tax.df2 %>% mutate(Impact = 0.7*ifelse(s.dp == 0, 0, Impact))
      
      salt.df1 <- salt.df2 <- salt.df %>% rename(Impact = salt_impact) %>% 
        mutate(Code = "salt", age_name = 0) %>%
        select(cause_name, Code, Impact, age_name)
      for (a in 1:85){
        salt.df1 <- salt.df1 %>% mutate(age_name = a)
        salt.df2 <- rbind(salt.df2, salt.df1)
      }
      salt.df2 <- salt.df2 %>% mutate(Impact = 0.7*ifelse(i < 5, 0, s.dp*(1 - Impact)))
      
      tf.df1 <- tf.df2 <- tf.df %>% rename(Impact = transfat_impact) %>% 
        mutate(Code = "transfat", age_name = 0) %>%
        select(cause_name, Code, Impact, age_name)
      for (a in 1:85){
        tf.df1 <- tf.df1 %>% mutate(age_name = a)
        tf.df2 <- rbind(tf.df2, tf.df1)
      }
      tf.df2 <- tf.df2 %>% mutate(Impact = 0.7*ifelse(i < 5, 0, s.dp*(1 - Impact)))
      ########################################################################################################
      
      # Calculate the mortality reduction based on baseline coverage, increase and efficacy
      # Starting at i == 3 ie year 2021, assume projection from year 2019
      
      if (inc.type == "varying"){
      in.dfb             <- in.df.in %>%
        mutate(ic = ifelse(i < 5, 0, i - 4),
               Pred = `Baseline Coverage` + ic*inc*s.dp, 
               `Target (P1)`= ifelse(Pred > 0.90, 0.90, Pred),
               `Target (P1)`= ifelse(`Target (P1)` < `Baseline Coverage`, `Baseline Coverage`, `Target (P1)`),
               `Impact` = 0.7*((`Effect (I)`*(`Target (P1)`-`Baseline Coverage`))/
                                 (1-(`Effect (I)`*`Baseline Coverage`)))) 
      } else {
      in.dfb             <- in.df.in %>%
        mutate(ic = ifelse(i < 5, 0, 1),
               Pred = `Baseline Coverage` + ic*inc*s.dp,
               `Target (P1)`= ifelse(Pred > 0.90, 0.90, Pred),
               `Target (P1)`= ifelse(`Target (P1)` < `Baseline Coverage`, `Baseline Coverage`, `Target (P1)`),
               `Impact` = 0.7*((`Effect (I)`*(`Target (P1)`-`Baseline Coverage`))/
                                 (1-(`Effect (I)`*`Baseline Coverage`)))) 
      }
      
      # How many interventions have non zero impact (non zero mortality reduction) 
      
      in.dfa  <- in.dfb %>% filter(Impact != 0)
      ndfs    <- nrow(in.dfa)
      
      # We want to stack all interventions replicated for all age-groups.
      if (ndfs == 0) {
        dft  <- data.table(in.dfb[c(1),], age_name = 0:85)
      } else {
        dft  <- data.table(in.dfa[c(1),], age_name = 0:85)
      }
      
      if (ndfs > 1){
        for (nr in 2:ndfs){
          dftn  <- data.table(in.dfa[c(nr),], age_name = 0:85)
          dft   <- rbind(dft, dftn)
        }
      }
      
      # Now use the age-groups relevant to the intervention to calculate impact
      
      if (tas == "no"){
        in.df <- dft %>% 
          left_join(age.code, by = "Code") %>% filter(age_name >= agel & age_name <= ageu) %>%
          select(cause_name, Code, Impact, age_name) %>% 
          mutate(Imp = 1 - Impact) %>% group_by(cause_name, age_name) %>% 
          summarise(Impact = prod(Imp), .groups = "drop") %>% ungroup() %>% select(cause_name, age_name, Impact)
      } else {
        scal = ifelse(i < 5, 0, (i-4)/8)
        
        in.df <- dft %>% 
          left_join(age.code, by = "Code") %>% filter(age_name >= agel & age_name <= ageu) %>%
          select(cause_name, Code, Impact, age_name) %>% 
          rbind(tax.df2) %>% rbind(salt.df2) %>% rbind(tf.df2) %>%
          mutate(Imp = 1 - scal*Impact) %>% group_by(cause_name, age_name) %>% 
          summarise(Impact = prod(Imp), .groups = "drop") %>% ungroup() %>% select(cause_name, age_name, Impact)
      }
      
      ############################################################################################################################
      
      # Put together global mx using the values from the non-NCD and combined NCDs
      
      ncd.bgs <- NCD.bg %>%
        filter(location_name == is) %>%
        select(sex_name, age_name, cause_name, bg_trend) %>%
        rename(ncd_trend = bg_trend)
 
      # NCDs used to calculate 40q30    
      ncd.mxs <- cause.mx %>%
        filter(location_name == is & year == 2019 & cause_name %in% sel.cse) %>%
        select(cause_name, sex_name, age_name, mx) %>%
        arrange(cause_name, sex_name, age_name) %>%
        left_join(ncd.bgs, by = c("sex_name","age_name", "cause_name"))
      
      mx.dis.df      <- ncd.mxs %>% 
        left_join(in.df, by = c("cause_name", "age_name")) %>% 
        mutate(Impact = ifelse(is.na(Impact), 1, Impact)) %>%
        mutate(t = i - 1, mx.dis = exp(t*ncd_trend)*mx*Impact) 
      
      mx.dis.sum     <- mx.dis.df %>%
        group_by(sex_name, age_name) %>%
        summarise(mx.dis = sum(mx.dis), .groups = "drop") %>% ungroup()
      
      # NCDs not used to calculate 40q30
      ncd.mxs.unused <- cause.mx %>%
        filter(location_name == is & year == 2019 & !(cause_name %in% sel.cse)) %>%
        select(cause_name, sex_name, age_name, mx) %>%
        arrange(cause_name, sex_name, age_name) %>%
        left_join(ncd.bgs, by = c("sex_name","age_name", "cause_name"))
      
      if (nrow(ncd.mxs.unused) > 0){
        ncd.mxs.unused.sum  <- ncd.mxs.unused %>% 
          mutate(t = i - 1, mx.dis = exp(t*ncd_trend)*mx) %>%
          group_by(sex_name, age_name) %>%
          summarise(mx.nondis = sum(mx.dis), .groups = "drop") %>% ungroup()
      }
      
      # Background trend
      mx.nondis.df <- nonNCD.bg %>%
        filter(location_name == is) %>%
        select(sex_name, age_name, mx, bg_trend) %>%
        rename(non_ncd_trend = bg_trend) %>%
        mutate(t = i - 1, mx.nondis = exp(t*non_ncd_trend)*mx, 
               t = NULL, mx = NULL, non_ncd_trend = NULL)
      
      if (nrow(ncd.mxs.unused) > 0){
        mx.nondis.df  <- mx.nondis.df %>% 
          rbind(ncd.mxs.unused.sum) %>%
          group_by(sex_name, age_name) %>%
          summarise(mx.nondis = sum(mx.nondis, na.rm = T), .groups = "drop") %>% ungroup()
      }
      
      mx             <- mx.dis.sum %>%
        right_join(mx.nondis.df, by = c("sex_name","age_name")) %>%
        arrange(sex_name, age_name) %>%
        rowwise() %>%
        mutate(mx = sum(c(mx.dis, mx.nondis), na.rm = T)) %>% ungroup() %>% pull(mx)
      
      mx.all         <- mx
      mx.ncd         <- mx.dis.sum %>% arrange(sex_name, age_name) %>% pull(mx.dis)
      mx.non         <- mx - mx.ncd
      
      ###################################################################################################
      # Incl COVID for 2020 and 2021
      # extend series to 2022
      
      if (covid.in == "yes"){
        if (i %in% c(2:4)){
        covid.adj    <- covid.df %>% filter(location_name == is) %>% arrange(year_id) %>% pull(covid)
        covid.adj    <- sc.covid*covid.adj[i - 1]
        
        dx.all      <- mx.all * P[,i]
        dx.ncd      <- mx.ncd * P[,i]
        dx.non      <- mx.non * P[,i]
        
        dx.ncd[c(1:30,87:117)] <- 0
        dx.non[c(1:30,87:117)] <- 0
        
        cov.ncd        <- (dx.ncd/sum(c(dx.ncd, dx.non))) * (covid.adj/P[,i])
        cov.non        <- (dx.non/sum(c(dx.ncd, dx.non))) * (covid.adj/P[,i])
        mx.non         <- mx.non + cov.non
        mx.ncd         <- mx.ncd + cov.ncd
        }
      }
      
      mx.mat[,i]     <- mx.non + mx.ncd
      mx.dis.mat[,i] <- mx.ncd

      mx[mx<0]       <- 0
      mx[mx>1-1e-5]  <- 1-1e-5
      sx             <- exp(-mx)
      fx             <- fxa[,i]
      mig            <- miga[,i] 
      #######################################################################################################
      
      #######################################################################################################
      P[2:(z-1),i+1]     <- P[1:(z-2),i] * sx[1:(z-2)] * (1 + mig[2:(z-1)])
      P[z,i+1]           <- (P[z-1,i] * sx[z-1] + P[z,i] * sx[z])*(1 + mig[z])
      P[(z+2):(w-1),i+1] <- P[(z+1):(w-2),i] * sx[(z+1):(w-2)] * (1 + mig[(z+2):(w-1)])
      P[w,i+1]           <- (P[w-1,i] * sx[w-1] + P[w,i] * sx[w])*(1 + mig[w])
      B                  <- sum(0.5*(P[10:54,i] + P[11:55,i+1])*fx[10:54]) # fertility rates
      Bf                 <- (1-sex_ratio)  * B  * (1+mig[1])
      Bm                 <- sex_ratio    * B * (1+mig[(z+1)])
      P[1, i + 1]        <- Bf * sx[1]  
      P[(z+1), i + 1]    <- Bm * sx[z+1] 
      #######################################################################################################
  
      # Mortality for DALY
      
      mx.pin <- mx.dis.df %>% filter(cause_name %in% cse.yll) %>%
        select(cause_name, sex_name, age_name, mx.dis) %>% 
        arrange(cause_name, sex_name, age_name) %>%
        mutate(year_id = year0 + i) 
      mx.pin.all <- rbind(mx.pin.all, mx.pin)
      
      births[1,i]  <- Bf
      births[2,i]  <- Bm
    }
    
    list(P = P, mx.mat = mx.mat, mx.dis.mat = mx.dis.mat, mx.pin = mx.pin.all, births = births)
  }
  
  ####################################################################################################
  # Baseline & adjusted

  b0       <- doproj(0)
  P0       <- b0$P[,1:n]
  mx0      <- b0$mx.mat
  mx.dis.0 <- b0$mx.dis.mat
  D0       <- P0 * mx0
  D0cse    <- P0 * mx.dis.0
  Bf0      <- b0$births[1,]
  Bm0      <- b0$births[2,] 
  
  b1       <- doproj(1)
  P1       <- b1$P[,1:n]
  mx1      <- b1$mx.mat
  mx.dis.1 <- b1$mx.dis.mat
  D1       <- P1 * mx1
  D1cse    <- P1 * mx.dis.1
  Bf1      <- b1$births[1,]
  Bm1      <- b1$births[2,] 
  
  q300 <- q30.est(P0, D0cse, n)
  q301 <- q30.est(P1, D1cse, n)
  
  ####################################################################################################
  # Store aggregated NCD related 40q30
  q300d <- data.table(t(q300))
  q301d <- data.table(t(q301))
  
  setnames(q300d, paste0("V",1:12), paste0(2019:2030))
  setnames(q301d, paste0("V",1:12), paste0(2019:2030))
  
  q30df <- left_join(q300d %>% gather(year_id, Baseline),
                     q301d %>% gather(year_id, Adjusted),
                     by = "year_id") %>% 
    mutate(q30.ave = Baseline - Adjusted, location_name = is, 
           n.inters = ninters)
  
  ####################################################################################################
  # Store deaths averted
  D0p = D0[1:86,] + D0[87:172,]
  D0pa = apply(D0p[1:30,],2,sum); D0pb = apply(D0p[31:70,], 2, sum); D0pc = apply(D0p[71:86,], 2, sum)
  
  D1p = D1[1:86,] + D1[87:172,]
  D1pa = apply(D1p[1:30,],2,sum); D1pb = apply(D1p[31:70,], 2, sum); D1pc = apply(D1p[71:86,], 2, sum)
  
  DA1 = D0pa - D1pa; DA2 = D0pb - D1pb; DA3 = D0pc - D1pc  
  DAdt <- data.table(`Under 30`= DA1, `30-69` = DA2, `70+`=DA3,  year_id = 2019:2030) %>%
    gather(age_group, Deaths.Avert, -year_id) %>% filter(year_id > 2019) %>%
    mutate(location_name = is,  n.inters = ninters)
  ####################################################################################################
  
  # 40q30 estimates by sex
  
  ltcause <- q30.est.sex(P0, D0cse, n) %>% mutate(assumption = "Baseline")  
  ltcause <- q30.est.sex(P1, D1cse, n) %>% mutate(assumption = "Adjusted") %>%
    rbind(ltcause) %>% mutate(location_name = is) %>%
    spread(assumption, val) %>% arrange(sex_name, year_id)
  
  #######################################################################################
  # Now get the DALYs
  reps.yll    <- length(cse.yll)
  cse.yll.vec <- data.table(cause_name = rep(cse.yll, each = 172))
  
  ex0      <- apply(mx0, 2, mxtoex)
  ex1      <- apply(mx1, 2, mxtoex)
  
  ex0.yll  <- do.call(rbind, replicate(reps.yll, ex0, simplify=FALSE))[,2:n]
  
  mx.pin.1 <- b1$mx.pin %>% arrange(cause_name, sex_name, age_name) %>%
    spread(year_id, mx.dis) %>% select(paste0(2020:2030)) %>% as.matrix()
  pop.1.yll<- do.call(rbind, replicate(reps.yll, P1, simplify=FALSE))[,2:n]
  yll1     <- ex0.yll*pop.1.yll*mx.pin.1 %>% data.table()  
  
  #if (any(dis.df.in$metric=="disability")){
  #  daly1     <- yll1 %>% 
  #    cbind(cse.yll.vec) %>% gather(year_id, YLL, -cause_name) %>%
  #    left_join(yldyll %>% filter(location_name==is), by = "cause_name") %>%
  #    filter(!is.na(scale)) %>%
  #    mutate(YLD = YLL*scale) %>%
  #    cbind(.,disab.df%>%ungroup()%>%select(Imp))%>%# add disability impacts here
  #    mutate(YLD = YLD*Imp,
  #           DALY = YLD + YLL)%>%
  #    group_by(year_id) %>%
  #    summarise(DALY=sum(DALY), .groups = "drop") %>% ungroup() %>% mutate(group = "Adjusted")
  #}
  #else{
    daly1     <- yll1 %>% 
      cbind(cse.yll.vec) %>% gather(year_id, YLL, -cause_name) %>%
      left_join(yldyll %>% filter(location_name==is), by = "cause_name") %>%
      filter(!is.na(scale)) %>%
      mutate(YLD = YLL*scale,
             DALY = YLD + YLL) %>%
      group_by(year_id) %>%
      summarise(DALY=sum(DALY), .groups = "drop") %>% ungroup() %>% mutate(group = "Adjusted")
  #}
  
  mx.pin.0 <- b0$mx.pin %>% arrange(cause_name, sex_name, age_name) %>%
    spread(year_id, mx.dis) %>% select(paste0(2020:2030)) %>% as.matrix()
  pop.0.yll<- do.call(rbind, replicate(reps.yll, P0, simplify=FALSE))[,2:n]
  yll0     <- ex0.yll*pop.0.yll*mx.pin.0 %>% data.table()  
  daly0     <- yll0 %>% 
    cbind(cse.yll.vec) %>% gather(year_id, YLL, -cause_name) %>%
    left_join(yldyll %>% filter(location_name==is), by = "cause_name") %>%
    filter(!is.na(scale)) %>%
    mutate(DALY = YLL*(1 + scale)) %>% group_by(year_id) %>%
    summarise(DALY=sum(DALY), .groups = "drop") %>% ungroup() %>% mutate(group = "Baseline")
  
  dalys <- rbind(daly0, daly1) %>% 
    spread(group, DALY) %>% 
    mutate(DALY.ave = Baseline - Adjusted, 
           location_name = is,
           n.inters = ninters) 
  
  if (est.pin == "yes"){
  ####################################################################################################
  # Population in need
  causes    <- pin.groups %>% filter(Code %in% inter) %>% pull(cause_name) %>% unique() %>% sort()
  reps      <- length(causes)
  cse.vec   <- rep(causes, each = 172)
  sex.vec   <- rep(rep(c("Female","Male"), each = 86), reps)
  age.vec   <- rep(c(0:85), 2*reps)
  dummy.pin <- tibble(location_name = is, sex_name = sex.vec, 
                      cause_name = cse.vec, age_name = age.vec) %>%
    arrange(cause_name, sex_name, age_name)
  
  pin.parm  <- pin.groups %>% filter(Code %in% inter) %>% mutate(Code.sid = paste(Code, sub_id, sep=".")) 
  years     <- paste0(2020:2030)
  pin.in    <- pin.data %>% filter(location_name == is & cause_name %in% causes) %>% 
    right_join(dummy.pin, by = c("location_name", "sex_name", "age_name", "cause_name")) %>%
    arrange(cause_name, sex_name, age_name)

  
  ######################################################################################################
  # Baseline population in need
  
  pop.0   <- do.call(rbind, replicate(reps, P0, simplify=FALSE))[,2:n]
  Prev0   <- pin.in$R_prev * pop.0
  Inc0    <- pin.in$R_inc * pop.0
  Pop0    <- pin.in$R_pop * pop.0
  
  # Create pop in need matrices to aggregate
  df0     <- rbind(data.table(Prev0) %>% mutate(metric_name = "Prevalence") %>% cbind(dummy.pin %>% select(cause_name, sex_name, age_name)),
                   data.table(Inc0) %>% mutate(metric_name = "Incidence") %>% cbind(dummy.pin %>% select(cause_name, sex_name, age_name)),
                   data.table(Pop0) %>% mutate(metric_name = "Population") %>% cbind(dummy.pin %>% select(cause_name, sex_name, age_name))) 
  
  setnames(df0, paste0("V",1:11), paste0(2020:2030))
  
  df0 <- df0 %>% gather(year_id, val, -sex_name, -cause_name, -age_name, -metric_name)
  
  # Both sexes identified 
  df0     <- df0 %>% group_by(cause_name, age_name, metric_name, year_id) %>% 
    summarise(val = sum(val, na.rm=T), .groups = "drop") %>% ungroup() %>%
    mutate(sex_name = "Both") %>% rbind(df0) %>% mutate(year_id = as.numeric(year_id))
  
  cov.ass0   <- in.df.in %>% filter(Code %in% inter) %>% select(Code, `Baseline Coverage`, inc) %>% distinct() %>% data.table()
  cov.ass0[, (years) := 0L]
  i = 1
  for(x in years) {
    if (inc.type == "varying"){
      ss = ifelse(i < 4, 0, i - 3)
    } else {
      ss = ifelse(i < 4, 0, 1) 
    }
    
    set(cov.ass0, j = x, value = cov.ass0$`Baseline Coverage` + ss*cov.ass0$inc*0)
    i = i + 1
  }
  cov.ass0   <- cov.ass0 %>% select(-c(inc, `Baseline Coverage`)) %>% gather(year_id, cov, -Code) %>% 
    mutate(year_id = as.numeric(year_id)) 
  pin.parm.0 <- left_join(pin.parm, cov.ass0, by = "Code")
  pin.est.0      <- tibble(year_id=integer(), cause_name = character(), metric_name = character(),
                           sex_name = character(), pin= integer(), Code = character(), sub_id = character(), Coverage = integer()) 
  for (Codes in sort(unique(pin.parm.0$Code.sid))){
    pin.est.0 <- pin.est.0 %>%
      rbind(pin.parm.0 %>% filter(Code.sid==Codes) %>%
              left_join(df0, by = c("metric_name", "sex_name", "year_id", "cause_name")) %>% 
              filter(age_name>=age_l & age_name<= age_u) %>% mutate(pin = val*Adjustment*cov, Coverage = cov) %>% 
              group_by(Code, sub_id, cause_name, year_id, metric_name, sex_name, Coverage) %>%
              summarise(pin = sum(pin, na.rm=T), .groups = "drop") %>% ungroup())
  }
  pin.est.0 <- pin.est.0 %>% mutate(group = "Baseline") %>% arrange(Code, sub_id, year_id)
  
  ######################################################################################################
  # Adjusted population in need
  
  pop.1   <- do.call(rbind, replicate(reps, P1, simplify=FALSE))[,2:n]
  Prev1   <- pin.in$R_prev * pop.1
  Inc1    <- pin.in$R_inc * pop.1
  Pop1    <- pin.in$R_pop * pop.1
  
  # Create pop in need matrices to aggregate
  df1     <- rbind(data.table(Prev1) %>% mutate(metric_name = "Prevalence") %>% cbind(dummy.pin %>% select(cause_name, sex_name, age_name)),
                   data.table(Inc1) %>% mutate(metric_name = "Incidence") %>% cbind(dummy.pin %>% select(cause_name, sex_name, age_name)),
                   data.table(Pop1) %>% mutate(metric_name = "Population") %>% cbind(dummy.pin %>% select(cause_name, sex_name, age_name))) 
  
  setnames(df1, paste0("V",1:11), paste0(2020:2030))
  
  df1 <- df1 %>% gather(year_id, val, -sex_name, -cause_name, -age_name, -metric_name)
  
  # Both sexes identified 
  df1     <- df1 %>% group_by(cause_name, age_name, metric_name, year_id) %>% 
    summarise(val = sum(val, na.rm=T), .groups = "drop") %>% ungroup() %>%
    mutate(sex_name = "Both") %>% rbind(df1) %>% mutate(year_id = as.numeric(year_id))
  
  cov.ass1   <- in.df.in %>% filter(Code %in% inter) %>% select(Code, `Baseline Coverage`, inc) %>% distinct() %>% data.table()
  cov.ass1[, (years) := 0L]
  i = 1
  for(x in years) {
    if (inc.type == "varying"){
      ss = ifelse(i < 4, 0, i - 3)
    } else {
      ss = ifelse(i < 4, 0, 1) 
    }
    
    set(cov.ass1, j = x, value = cov.ass1$`Baseline Coverage` + ss*cov.ass1$inc)
    i = i + 1
  }
  cov.ass1   <- cov.ass1 %>% select(-c(inc, `Baseline Coverage`)) %>% gather(year_id, cov, -Code) %>% 
    mutate(year_id = as.numeric(year_id)) 
  pin.parm.1 <- left_join(pin.parm, cov.ass1, by = "Code")
  pin.est.1      <- tibble(year_id=integer(), cause_name = character(), metric_name = character(),
                           sex_name = character(), pin= integer(), Code = character(), sub_id = character(), Coverage = integer()) 
  for (Codes in sort(unique(pin.parm.1$Code.sid))){
    pin.est.1 <- pin.est.1 %>%
      rbind(pin.parm.1 %>% filter(Code.sid==Codes) %>%
              left_join(df1, by = c("metric_name", "sex_name", "year_id", "cause_name")) %>% 
              filter(age_name>=age_l & age_name<= age_u) %>% mutate(pin = val*Adjustment*cov, Coverage = cov) %>% 
              group_by(Code, sub_id, cause_name, year_id, metric_name, sex_name, Coverage) %>%
              summarise(pin = sum(pin, na.rm=T), .groups = "drop") %>% ungroup())
  }
  pin.est.1 <- pin.est.1 %>% mutate(group = "Adjusted") %>% arrange(Code, sub_id, year_id)
  
  #######################################################################################
  pin.est <- rbind(pin.est.0, pin.est.1) %>% mutate(location_name = is, n.inters = ninters)
  
  list(q30df = q30df, DAdt = DAdt, ltcause = ltcause, P1 = P1, D1cse = D1cse, P0 = P0, D0cse = D0cse,
       pin.est = pin.est, dalys = dalys)
  } else {
    
    add_lt <- function(pops, dths, sources) {
      
      get_person <- function(x) {
        x[1:86] + x[-c(1:86)]
      }
      
      lt_est <- function(y) {
        px  = exp(-y)
        lx  = c(1,cumprod(px)) 
        q1  = round(1000*(lx[1] - lx[2])/lx[1], 2)
        q5  = round(1000*(lx[1] - lx[6])/lx[1], 2)
        q15 = round(1000*(lx[16] - lx[61])/lx[16], 2)
        q30 = round(1000*(lx[31] - lx[71])/lx[31], 2)
        ex  = round(sum(head(lx,-1) + tail(lx,-1)) / 2, 3)
        
        c(ex, q1, q5, q15, q30)  
      }
      
      y0 = 2019
      y1 = 2030
      
      pop_tot    <- apply(pops, 2, get_person)
      deaths_tot <- apply(dths, 2, get_person)
      mx_both    <- deaths_tot / pop_tot
      lt_out_both <- apply(mx_both, 2, lt_est)
      
      e0   <- lt_out_both[1, ]
      imr  <- lt_out_both[2, ]
      u5mr <- lt_out_both[3, ]
      q15  <- lt_out_both[4, ]
      q30  <- lt_out_both[5, ]
      
      out_df <- data.table(
        year_id = 2019:2030, group = sources,
        e0 = e0, q1 = imr, q5 = u5mr, q15 = q15, q30 = q30
      )
      
      return(out_df)
    }  
    
 lt_out_df <- bind_rows(add_lt(P0, D0, "Baseline"), add_lt(P1, D1, "Adjusted")) %>% mutate(location_name = is)
 births_df <- data.table(location_name = is, 
                         rbind(data.table(year_id = 2019:2030, group = "Baseline", Female = Bf0, Male = Bm0),
                               data.table(year_id = 2019:2030, group = "Adjusted", Female = Bf1, Male = Bm1)))
    
  list(q30df = q30df, DAdt = DAdt, ltcause = ltcause, P1 = P1, D1cse = D1cse, P0 = P0, D0cse = D0cse, 
       lt_out_df = lt_out_df, D0 = D0, D1 = D1, births_df = births_df, dalys = dalys)  
  }
}