##############################################################################################################
# Create all the base input files to be used in the demographic component of the analyses 
##############################################################################################################

rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws)

load("WPP_input_data.Rda")


# test projection defaults 
test_proj <- function(is, y0, n){
x = 0:85; z = length(x); w = 2*z

# Demographic data
wpp.ina     <- wpp.input %>% rename(year = year_id, age_name = age) %>%
  filter(location_name == is & year %in% y0:(y0+n)) %>% select(-c(location_name, iso3)) %>%
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

mxa         <- wpp.ina %>%
  select(sex_name, age_name, year, mx) %>% 
  spread(year, mx) %>% arrange(sex_name, age_name) %>%
  select(-c(sex_name, age_name)) %>% as.matrix() 

P           <- matrix(0, nrow = w, ncol = n+1)
P[,1]       <- Nxa[,1] 
births      <- array(dim = c(2,n))

for (i in 1:n){
    mx             <- mxa[,i]
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
    Bf                 <- 0.5 * B * (1+mig[1])
    Bm                 <- 0.5 * B * (1+mig[(z+1)])
    P[1, i + 1]        <- Bf * sx[1]  
    P[(z+1), i + 1]    <- Bm * sx[z+1] 
}

wpp <- obs_wpp %>% filter(location_name == is & year %in% y0:(y0+n-1)) %>% pull(deaths_female)
print("WPP")
print(wpp)

print("Projection")
deaths <- mxa[,1:n]*P[,1:n]
projection <- round(apply(deaths[1:86,], 2, sum))
print(projection)

plot(y0:(y0+n-1), wpp, main = is, ylab = "Deaths", xlab = "year")
lines(y0:(y0+n-1), projection)
}

test_proj("South Africa", 2010, 41)
test_proj("Malawi", 2010, 41)
