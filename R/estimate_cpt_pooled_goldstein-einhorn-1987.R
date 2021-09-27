pacman::p_load(tidyverse, R2jags)
source("R/fun_inits_MCMC_pooled.R") # call function creating initial values for MCMC

# read choice data

cols <- list(.default = col_double(),
             strategy = col_factor(),
             boundary = col_factor(),
             gamble = col_factor(),
             rare = col_factor(),
             agent = col_factor(),
             choice = col_factor())
choices <- read_csv("data/choices/choices.csv", col_types = cols)

# prepare data for JAGS

choices_cpt <- choices %>%
  filter(!(is.na(a_ev_exp) | is.na(b_ev_exp))) %>% # remove choices where prospect were not attended
  mutate(choice_A = if_else(choice == "A", 1, 0))


#---- pooled estimates


# group trials of distinct strategy-parameter combinations of the generating model

params_sim <- choices_cpt %>% distinct(strategy, s, boundary, a) # to get distinct strategy-parameter combinations
choices_grouped <- vector("list", nrow(params_sim))
for(set in seq_len(nrow(params_sim))){
  choices_grouped[[set]] <- choices_cpt %>%
    filter(strategy == params_sim[[set, "strategy"]] & s == params_sim[[set, "s"]] & boundary == params_sim[[set, "boundary"]] & a == params_sim[[set, "a"]]) %>%
    mutate(i = row_number()) # assign trial numbers
}

# allocate space for JAGS output

estimates_cpt <- vector("list", nrow(params_sim)) # posterior statistics and MCMC diagnostics
posteriors_cpt <- vector("list", nrow(params_sim)) # posterior distributions

# set up MCMC simulations

params_cpt <- c("alpha", "gamma", "delta", "rho")
n_chains <- 20
n_iter <- 41000
n_burnin <- 1000
n_thin <- 20 # to reduce autocorrelation during sampling process

# MCMC simulation

for(set in seq_len(nrow(params_sim))){

  ## get trials of the respective strategy-parameter combination

  current_trials <- list(choice = choices_grouped[[set]]$choice_A,
                         a_o1 = choices_grouped[[set]]$a_o1,
                         a_o2 = choices_grouped[[set]]$a_o2,
                         b_o1 = choices_grouped[[set]]$b_o1,
                         b_o2 = choices_grouped[[set]]$b_o2,
                         a_p1_exp = choices_grouped[[set]]$a_p1_exp, # use relative frequencies to account for sampling error
                         a_p2_exp = choices_grouped[[set]]$a_p2_exp,
                         b_p1 = choices_grouped[[set]]$b_p1,
                         b_p2 = choices_grouped[[set]]$b_p2,
                         start = min(choices_grouped[[set]]$i),
                         stop = max(choices_grouped[[set]]$i))

  ## sample from posterior distributions using MCMC

  current_sample <- jags.parallel(data = current_trials,
                                  inits = inits_MCMC_pooled,
                                  parameters.to.save = params_cpt,
                                  model.file = "JAGS/cpt_pooled_goldstein-einhorn-87.txt",
                                  n.chains = n_chains,
                                  n.iter = n_iter,
                                  n.burnin = n_burnin,
                                  n.thin = n_thin,
                                  n.cluster = n_chains, # run chains on different cores
                                  DIC = FALSE,
                                  jags.seed = 8362)

  ## get posteriors, credibility intervals, and MCMC diagnostics

  current_summary <- current_sample$BUGSoutput$summary %>% as_tibble(rownames = "parameter")
  estimates_cpt[[set]] <- expand_grid(params_sim[set, ], current_summary)

  current_posteriors <- current_sample$BUGSoutput$sims.matrix %>% as_tibble()
  posteriors_cpt[[set]] <- expand_grid(params_sim[set, ], current_posteriors)
}

# save data

estimates_cpt <- estimates_cpt %>% map_dfr(as.list)
posteriors_cpt <- posteriors_cpt %>% map_dfr(as.list)
write_csv(estimates_cpt, "data/estimates/estimates_cpt_pooled_goldstein-einhorn-87.csv")
write_csv(posteriors_cpt, "data/estimates/posteriors_cpt_pooled_goldstein-einhorn-87.csv")
