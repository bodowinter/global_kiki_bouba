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
daakie <- read_csv('field_daakie_only.csv')

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
                seed = 42)
save(kiki_mdl, file = '../models/kiki_mdl.RData',
     compress="xz", compression_level=9)

## Check pure non-IE speakers:

noIE <- filter(kiki, Family != 'IE', !IEL2)
table(noIE$Family)
table(noIE$Language)

kiki_noIE_mdl <- brm(Match ~ 1 + (1|Language),
                data = noIE,
                family = bernoulli,
                init = 0,
                cores = 4,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_intercept_only,
                control = list(adapt_delta = 0.995,
                               max_treedepth = 13),
                seed = 42)
save(kiki_noIE_mdl, file = '../models/kiki_noIE_mdl.RData',
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

# Script effect within IE:

kiki_scriptIE_mdl <- brm(Match ~ Script + (1|Language),
                       data = filter(kiki, Family == 'IE'),
                       family = bernoulli,
                       init = 0,
                       cores = 4,
                       iter = myiter,
                       warmup = mywarmup,
                       prior = my_priors_with_beta,
                       control = list(adapt_delta = 0.995,
                                      max_treedepth = 13),
                       seed = 1337)
save(kiki_scriptIE_mdl, file = '../models/kiki_scriptIE_mdl.RData',
     compress="xz", compression_level=9)

# Knows Roman script as L2:

kiki_scriptL2_mdl <- brm(Match ~ factor(RomanScriptL2) +
                               (1|Language) + (1|Family),
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
save(kiki_scriptL2_mdl, file = '../models/kiki_scriptL2_mdl.RData',
     compress="xz", compression_level=9)

# Knows no Roman script:

kiki$KnowsAnyRoman <- 'yes'
kiki[kiki$Script != 'roman' & !kiki$RomanScriptL2, ]$KnowsAnyRoman <- 'no'

# Fit model with whether knowing a language with a Roman script helps:
# No family random intercept here because each family has almost exactly one languge anyway...

kiki_AnyRoman_mdl <- brm(Match ~ 1 + 
                     (1|Language),
                data = filter(kiki, KnowsAnyRoman == 'no'),
                family = bernoulli,
                init = 0,
                cores = 4,
                iter = myiter,
                warmup = mywarmup,
                prior = my_priors_intercept_only,
                control = list(adapt_delta = 0.995,
				               max_treedepth = 13),
                seed = 666)
save(kiki_AnyRoman_mdl, file = '../models/kiki_AnyRoman_mdl.RData',
     compress="xz", compression_level=9)

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


