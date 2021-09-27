pacman::p_load(tidyverse, R2jags)
source("R/fun_inits_MCMC_hierarchical.R") # call function creating initial values for MCMC

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

gambles <- choices_cpt %>% select(gamble:ev_ratio) %>% distinct()
n_gambles <- nrow(gambles)
n_agents <- choices_cpt %>% distinct(agent) %>% nrow()

## group trials of distinct strategy-parameter combinations of the generating model

params_sim <- choices_cpt %>% distinct(strategy, s, boundary, a) # to get distinct strategy-parameter combinations
params_grouped <- vector("list", nrow(params_sim))

choices_cpt_grouped <- vector("list", nrow(params_sim))
a_p1_exp_grouped <- vector("list", nrow(params_sim))
a_p2_exp_grouped <- vector("list", nrow(params_sim))

for(set in seq_len(nrow(params_sim))){

  params_grouped[[set]] <- choices_cpt %>%
    filter(strategy == params_sim[[set, "strategy"]] & s == params_sim[[set, "s"]] & boundary == params_sim[[set, "boundary"]] & a == params_sim[[set, "a"]])

  choices_cpt_grouped[[set]] <- params_grouped[[set]] %>%
    select(gamble, agent, choice_A) %>%
    pivot_wider(names_from = "agent", values_from = "choice_A", names_prefix = "Ag_") %>%
    select(-gamble)

  a_p1_exp_grouped[[set]] <- params_grouped[[set]] %>%
    select(gamble, agent, a_p1_exp) %>%
    pivot_wider(names_from = "agent", values_from = "a_p1_exp", names_prefix = "Ag_") %>%
    select(-gamble)

  a_p2_exp_grouped[[set]] <- params_grouped[[set]] %>%
    select(gamble, agent, a_p2_exp) %>%
    pivot_wider(names_from = "agent", values_from = "a_p2_exp", names_prefix = "Ag_") %>%
    select(-gamble)
}

# allocate space for JAGS output

estimates_cpt <- vector("list", nrow(params_sim)) # posterior statistics and MCMC diagnostics
posteriors_cpt <- vector("list", nrow(params_sim)) # posterior distributions

# MCMC simulation

params_cpt <- c("mu.alpha", "alpha", "mu.gamma", "gamma", "mu.delta", "delta", "mu.rho", "rho")
n_chains <- 20

for(set in seq_len(nrow(params_sim))){

  ## get trials of the respective strategy-parameter combination

  current_trials <- list(choice = choices_cpt_grouped[[set]],
                         a_o1 = gambles$a_o1,
                         a_o2 = gambles$a_o2,
                         b_o1 = gambles$b_o1,
                         b_o2 = gambles$b_o2,
                         a_p1_exp = a_p1_exp_grouped[[set]], # use relative frequencies to account for sampling error
                         a_p2_exp = a_p2_exp_grouped[[set]],
                         b_p1 = gambles$b_p1,
                         b_p2 = gambles$b_p2,
                         n_agents = n_agents,
                         n_gambles = n_gambles)

  ## sample from posterior distributions using MCMC

  current_sample <- jags.parallel(data = current_trials,
                                  inits = inits_MCMC_hierarchical,
                                  parameters.to.save = params_cpt,
                                  model.file = "JAGS/cpt_hierarchical_prelec-98.txt",
                                  n.chains = n_chains,
                                  n.iter = 21000,
                                  n.burnin = 1000,
                                  n.thin = 20, # to reduce autocorrelation during sampling process
                                  n.cluster = n_chains, # run chains on different cores
                                  jags.seed = 15246)

  ## get posteriors, credibility intervals, and MCMC diagnostics

  current_summary <- current_sample$BUGSoutput$summary %>% as_tibble(rownames = "parameter")
  estimates_cpt[[set]] <- expand_grid(params_sim[set, ], current_summary)
}

# save data

estimates_cpt <- estimates_cpt %>% map_dfr(as.list)
write_csv(estimates_cpt, "data/estimates/estimates_cpt_hierarchical_prelec-98.csv")

