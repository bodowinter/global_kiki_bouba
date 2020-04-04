## Bodo Winter
## August 8, 2019
## kiki/bouba analysis

# See "Note on the field experiment" in "bayes_results.Rmd" and "descriptive_statistics.Rmd"

# Load packages:

library(tidyverse)
library(brms)

# Load data:

setwd('/Users/winterb/Research/iconicity/kiki_bouba/data/')
kiki <- read_csv('web_experiment_cleaned.csv')
# field <- read_csv('field_experiment_cleaned.csv')
daakie <- read_csv('field_daakie_only.csv')

# Recode L2 column for web experiment:

kiki <- mutate(kiki, English_MP = ifelse(is.na(English_MP), 'not reported', English_MP))

# There is a German and a French speaker that according to Marcus's recording of the L1/L2 info are wrongly coded as German/French. These arae English natives living in Germany / France who have filled out the German / French version of the survey.

kiki[which(kiki$English_MP == 'L1' & kiki$Language == 'DE'), ]$Language <- 'EN'
kiki[which(kiki$English_MP == 'L1' & kiki$Language == 'FR'), ]$Language <- 'EN'

# OLD ANALYSIS OF FIELD EXPERIMENT (DISCARDED):

# Email from Ola Feb 10, 2020 indicates that the first five participants in the German data of the field experiment have been erroneously coded as mismatches (0), even though they were matches:

# these_ppts <- str_c('DE0', 1:5)
# field[field$ID %in% these_ppts & !is.na(field$Match), ]$Match <- 1

# Since there was such little difference between auditory/written and we can't have
# random slopes for just two trials per participants with such little variation,
# we conservatively count only those participants that show kiki/bouba effect 
# in BOTH tasks as a match:

# First, we need to check those that are NA (missing either auditory or written):

# missing_one_IDs <- field %>% filter(is.na(Match)) %>% pull(ID)
# NAs <- filter(field, ID %in% missing_one_IDs) %>% group_by(ID) %>%
	# summarize(Match = sum(Match, na.rm = TRUE))

# field_all <- filter(field, !(ID %in% missing_one_IDs)) %>% group_by(ID) %>%
	# summarize(Match = sum(Match)) %>%
	# mutate(Match = ifelse(Match == 2, 1, 0))

# Bind these two together:

# field_all <- bind_rows(NAs, field_all)

# Merge the lannguage info back in:

# field_all$Language <- field[match(field_all$ID, field$ID), ]$Language

# As another analysis strategy, we will look at the auditory and written
# version separately:

# ort <- filter(field, Modality == 'Ort', !is.na(Match))
# aud <- filter(field, Modality == 'Aud', !is.na(Match))

# For parallel processing:

options(mc.cores=parallel::detectCores())

# Set regularizing priors on betas and uniform on intercept:

my_priors_with_beta <- c(prior(normal(0, 3), class = b),
                         prior(normal(0, 3), class = 'Intercept'))
my_priors_intercept_only <- c(prior(normal(0, 3), class = 'Intercept'))

## Parameters for all MCMC sampling:

mywarmup <- 4000
myiter <- 6000

## Check the distribution of scripts across families:

table(kiki$Family, kiki$Script)

# Given this, it makes no sense to fit random slopes, but an IE-specific analysis would be useful.

## Field analysis of modality factor:

# kiki_field_mdl <- brm(Match ~ 1 + (1|Language),
                # data = field_all,
                # family = bernoulli,
                # init = 0,
                # cores = 4,
                # seed = 666,
                # iter = myiter,
                # warmup = mywarmup,
                # prior = my_priors_intercept_only,
                # control = list(adapt_delta = 0.995,
                               # max_treedepth = 13))
# save(kiki_field_mdl, file = '../models/kiki_field_mdl.RData')

## Web experiment:

kiki_mdl <- brm(Match ~ 1 + (1|Language) + (1|Family),
                data = kiki,
                family = bernoulli,
                init = 0,
                cores = 4,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_intercept_only,
                control = list(adapt_delta = 0.995,
				               max_treedepth = 13),
                seed = 666)
save(kiki_mdl, file = '../models/kiki_mdl.RData',
     compress="xz", compression_level=9)

## Web experiment with L2 effect:

# Check distribution of L1/L2 across language families to assess viability of slopes:

table(kiki$Family, kiki$English_MP)

# Focus on those that are either L2 English speakers or those that are not: 

kiki_L2 <- filter(kiki, !(English_MP %in% c('not reported', 'L1')))

# Check distribution across families:

table(kiki_L2$Family, kiki_L2$English_MP)

# Fit model with random slope:

kiki_L2_mdl <- brm(Match ~ 1 + English_MP +
                     (1|Language) + (1 + English_MP|Family),
                data = kiki_L2,
                family = bernoulli,
                init = 0,
                cores = 4,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_intercept_only,
                control = list(adapt_delta = 0.995,
				               max_treedepth = 13),
                seed = 666)
save(kiki_L2_mdl, file = '../models/kiki_L2_mdl.RData',
     compress="xz", compression_level=9)

# Without random slope (there are hardly families with both categries anyway):

kiki_L2_noslope_mdl <- brm(Match ~ 1 + English_MP +
                     (1|Language) + (1|Family),
                   data = kiki_L2,
                   family = bernoulli,
                   init = 0,
                   cores = 4,
                   iter = myiter,
                   warmup = mywarmup,
                   prior = my_priors_intercept_only,
                   control = list(adapt_delta = 0.995,
                                  max_treedepth = 13),
                   seed = 666)
save(kiki_L2_noslope_mdl, file = '../models/kiki_L2_noslope_mdl.RData',
     compress="xz", compression_level=9)

## Web experiment with script effect:

kiki_script_mdl <- brm(Match ~ Script + (1|Language) + (1|Family),
                data = kiki,
                family = bernoulli,
                init = 0,
                cores = 4,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_with_beta,
                control = list(adapt_delta = 0.995,
                               max_treedepth = 13),
                seed = 1337)
save(kiki_script_mdl, file = '../models/kiki_script_mdl.RData',
     compress="xz", compression_level=9)

## Web experiment of the script effect with no people who speak English as L2:

kiki_noENG <- filter(kiki, !English_MP %in% c('L1', 'L2', 'not reported'))
nrow(kiki_noENG) # only 19 participants, also no script effect
kiki_script_noENG_mdl <- brm(Match ~ Script + (1|Language) + (1|Family),
                       data = kiki_noENG,
                       family = bernoulli,
                       init = 0,
                       cores = 4,
                       iter = myiter,
                       warmup = mywarmup,
                       prior = my_priors_with_beta,
                       control = list(adapt_delta = 0.995,
                                      max_treedepth = 13),
                       seed = 1337)
save(kiki_script_noENG_mdl, file = '../models/kiki_script_noENG_mdl.RData',
     compress="xz", compression_level=9)

## Web experiment with no English only:

kiki_noENG_mdl <- brm(Match ~ (1|Language) + (1|Family),
                             data = kiki_noENG,
                             family = bernoulli,
                             init = 0,
                             cores = 4,
                             iter = myiter,
                             warmup = mywarmup,
                             prior = my_priors_intercept_only,
                             control = list(adapt_delta = 0.995,
                                            max_treedepth = 13),
                             seed = 1337)
save(kiki_noENG_mdl, file = '../models/kiki_noENG_mdl.RData',
     compress="xz", compression_level=9)

## Auditory analysis only:

# aud_mdl <- brm(Match ~ 1 + (1|Language),
                # data = aud,
                # family = bernoulli,
                # init = 0,
                # cores = 4,
                # seed = 666,
                # iter = myiter,
                # warmup = mywarmup,
                # prior = my_priors_intercept_only,
                # control = list(adapt_delta = 0.995,
                               # max_treedepth = 13))
# save(aud_mdl, file = '../models/field_aud_mdl.RData')

## Written analysis only:

# ort_mdl <- brm(Match ~ 1 + (1|Language),
                # data = ort,
                # family = bernoulli,
                # init = 0,
                # cores = 4,
                # seed = 666,
                # iter = myiter,
                # warmup = mywarmup,
                # prior = my_priors_intercept_only,
                # control = list(adapt_delta = 0.995,
                               # max_treedepth = 13))
# save(ort_mdl, file = '../models/field_ort_mdl.RData')

## Daakie only:

daakie_mdl <- brm(Match ~ 1,
                      data = daakie,
                      family = bernoulli,
                      init = 0,
                      cores = 4,
                      iter = myiter,
                      warmup = mywarmup,
                      prior = my_priors_intercept_only,
                      control = list(adapt_delta = 0.995,
                                     max_treedepth = 13),
                      seed = 1337)
save(daakie_mdl, file = '../models/daakie_mdl.RData',
     compress="xz", compression_level=9)


