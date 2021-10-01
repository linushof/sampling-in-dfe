pacman::p_load(tidyverse, R2jags)
source("R/fun_inits_MCMC_hierarchical.R") # call function creating initial values for MCMC

# read choice data

cols <- list(.default = col_double(),
             boundary = col_factor(),
             gamble = col_factor(),
             rare = col_factor(),
             agent = col_factor(),
             choice = col_factor())
choices <- read_csv("data/choices/choices.csv", col_types = cols)

# prepare data for JAGS

choices <- choices %>%
  mutate(choice_A = if_else(choice == "A", 1, 0)) # to apply logit choice rule

gambles <- choices %>% select(gamble:ev_ratio) %>% distinct()
n_gambles <- nrow(gambles)
n_agents <- choices %>% distinct(agent) %>% nrow()

## group trials of distinct strategy-parameter combinations of the generating model

params_sim <- choices %>% distinct(s, boundary, a) # to get distinct strategy-parameter combinations
params_grouped <- vector("list", nrow(params_sim))

choices_grouped <- vector("list", nrow(params_sim))
a_p1_exp_grouped <- vector("list", nrow(params_sim))
a_p2_exp_grouped <- vector("list", nrow(params_sim))

for(set in seq_len(nrow(params_sim))){

  params_grouped[[set]] <- choices %>%
    filter(s == params_sim[[set, "s"]] & boundary == params_sim[[set, "boundary"]] & a == params_sim[[set, "a"]])

  choices_grouped[[set]] <- params_grouped[[set]] %>%
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

# MCMC simulation

params_cpt <- c("mu.alpha", "alpha", "mu.gamma", "gamma", "mu.delta", "delta", "mu.rho", "rho")
n_chains <- 20

for(set in seq_len(nrow(params_sim))){

  ## get trials of the respective strategy-parameter combination

  current_trials <- list(choice = choices_grouped[[set]],
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
write_csv(estimates_cpt, "data/estimates/estimates_cpt.csv")

