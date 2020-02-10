## Bodo Winter
## August 8, 2019
## kiki/bouba analysis

# Load packages:

library(tidyverse)
library(brms)

# Load data:

setwd('/Users/winterb/Research/iconicity/kiki_bouba/data/')
kiki <- read_csv('web_experiment_cleaned.csv')
field <- read_csv('field_experiment_cleaned.csv')

# Factor code relevant columns:

field <- mutate(field,
               Modality = factor(Modality))
kiki <- mutate(kiki,
               Script = factor(Script))

# Since there was such little difference between auditory/written and we can't have
# random slopes for just two trials per participants with such little variation,
# we conservatively count only those participants that show kiki/bouba effect 
# in BOTH tasks as a match:

# First, we need to check those that are NA (missing either auditory or written):

missing_one_IDs <- field %>% filter(is.na(Match)) %>% pull(ID)
NAs <- filter(field, ID %in% missing_one_IDs) %>% group_by(ID) %>%
	summarize(Match = sum(Match, na.rm = TRUE))

field_all <- filter(field, !(ID %in% missing_one_IDs)) %>% group_by(ID) %>%
	summarize(Match = sum(Match)) %>%
	mutate(Match = ifelse(Match == 2, 1, 0))

# Bind these two together:

field_all <- bind_rows(NAs, field_all)

# Merge the lannguage info back in:

field_all$Language <- field[match(field_all$ID, field$ID), ]$Language

# As another analysis strategy, we will look at the auditory and written
# version separately:

ort <- filter(field, Modality == 'Ort', !is.na(Match))
aud <- filter(field, Modality == 'Aud', !is.na(Match))

# For parallel processing:

options(mc.cores=parallel::detectCores())

# Set regularizing priors on betas and uniform on intercept:

my_priors_with_beta <- c(prior(normal(0, 3), class = b),
                         prior(normal(0, 3), class = 'Intercept'))
my_priors_intercept_only <- c(prior(normal(0, 3), class = 'Intercept'))

# Create sum codes for binary fixed effects:

contrasts(kiki$Script) <- contr.sum(2)

## Parameters for all MCMC sampling:

mywarmup <- 4000
myiter <- 6000

## Field analysis of modality factor:

kiki_field_mdl <- brm(Match ~ 1 + (1|Language),
                data = field_all,
                family = bernoulli,
                init = 0,
                cores = 4,
                seed = 666,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_intercept_only,
                control = list(adapt_delta = 0.995,
                               max_treedepth = 13))
save(kiki_field_mdl, file = '../models/kiki_field_mdl.RData')

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
save(kiki_mdl, file = '../models/kiki_mdl.RData')

## Web experiment with script effect:

kiki_script_mdl <- brm(Match ~ Script + (1|Language) + (1 + Script|Family),
                data = kiki,
                family = bernoulli,
                init = 0,
                cores = 4,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_with_beta,
                control = list(adapt_delta = 0.995,
                               max_treedepth = 13),
                seed = 666)
save(kiki_script_mdl, file = '../models/kiki_script_mdl.RData')

## Auditory analysis only:

aud_mdl <- brm(Match ~ 1 + (1|Language),
                data = aud,
                family = bernoulli,
                init = 0,
                cores = 4,
                seed = 666,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_intercept_only,
                control = list(adapt_delta = 0.995,
                               max_treedepth = 13))
save(aud_mdl, file = '../models/field_aud_mdl.RData')

## Written analysis only:

ort_mdl <- brm(Match ~ 1 + (1|Language),
                data = ort,
                family = bernoulli,
                init = 0,
                cores = 4,
                seed = 666,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_intercept_only,
                control = list(adapt_delta = 0.995,
                               max_treedepth = 13))
save(ort_mdl, file = '../models/field_ort_mdl.RData')



