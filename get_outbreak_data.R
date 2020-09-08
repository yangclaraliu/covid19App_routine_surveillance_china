stages <- c("S", "E", "Ip","Io", "Iu", "Ro_pos", "Ru_pos","R_neg")
nepi <- length(stages) # S E Ip Io Iu Ro_pos Ru_pos R_neg
age_groups <- paste(seq(0, 75, 5),seq(0, 75, 5)+4, sep="-"); age_groups[16] <- "75+"
nage <- length(age_groups) # 16 age groups - 16*16 contact matrices
# parameters
source('code/utils/read_data.R')
params = data.table(dlat = 2.6, # length of latent period
                    dprogress = 2.4, # length of incubation period in days 
                    dinf = 3.6,      # length of clinically infectious period in days,
                    dinf_u = 6, # length of subclinical infectious period in days
                    dneg = 6,
                    R0 = 2)
source("code/utils/get_sen.R")

#load data and functions

paste0("./code/utils/", 
       list.files(path = "./code/utils/",
                  pattern = "*\\.(R|r)")) %>%
  grep(pattern = "(read).*", invert = T, value=T) %>%
  .[-14] %>% 
  map(source)

params[, `:=` (p.latent = 1 - exp(-1/dlat),
               p.progress = 1 - exp(-1/dprogress),
               p.recover = 1 - exp(-1/dinf),
               p.recover_u = 1-exp(-1/dinf_u),
               p.neg = 1 - exp(-1/dneg),
               theta = 0.14, # transmissibility of subclinicals
               rho = list(cfrac_age))] # probability of clinical

t0 <- 1
tmax <- 300

expand.grid(dinf_u = c(5,6,8),
            dincu = c(4,5,6),
            theta = c(0.14, 0.5)) %>% 
  mutate(dprogress = dinf_u*0.4,
         dinf = dinf_u-dprogress,
         dneg = 10-dinf_u,
         R0 = 2,
         dlat = dincu - dprogress,
         p.latent = 1 - exp(-1/dlat),
         p.progress = 1 - exp(-1/dprogress),
         p.recover = 1 - exp(-1/dinf),
         p.recover_u = 1-exp(-1/dinf_u),
         p.neg = 1 - exp(-1/dneg),
         rho = list(cfrac_age)) %>% 
  data.table -> sensitivity_table

R0 <- 2.7
R0sample <- seq(1,5,0.5)# c(1.4, 2, 2.7)
nruns <- length(R0sample)

m <- array(0, dim = c(nepi, nage))
m[1,] <- rep(1, nage)
seed_agegroup <- 6 #sample
print(paste("The infected individual is from the", age_groups[seed_agegroup], "age group."))
#e0 <- 1e-06
m[1,seed_agegroup] <- m[1,seed_agegroup] - 1/age_dist_tmp[seed_agegroup]
m[2,seed_agegroup] <- 1/age_dist_tmp[seed_agegroup]
n0 <- vec(m)
nout <- array(NA_real_, dim = c(length(n0), tmax, nruns))

source("code/utils/run_epi.R")

run_epi(n0     = n0,
        C      = contact$all,
        p      = params,
        runs   = nruns,
        R0post = R0sample,
        tmax   = tmax,
        t0     = t0) -> outbreak_trace

write_rds(outbreak_trace, 
          "covid_surveillance/outbreak_trace.rds")


