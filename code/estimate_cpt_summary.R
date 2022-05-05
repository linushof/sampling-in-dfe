pacman::p_load(tidyverse, R2jags)
source("R/fun_inits_MCMC.R") # call function creating initial values for MCMC

# read choice data
cols <- list(.default = col_double(),
             boundary = col_factor(),
             gamble = col_factor(),
             rare = col_factor(),
             agent = col_factor(),
             choice = col_factor())
choices_comprehensive <- read_csv("supplements/comprehensive/data/choices_comprehensive.csv", col_types = cols)

# prepare data for JAGS

choices_comprehensive <- choices_comprehensive %>%
  mutate(choice_A = if_else(choice == "A", 1, 0)) # to apply logit choice rule

# group trials of distinct parameter combinations of the generating model
params_sim <- choices_comprehensive %>% distinct(s, boundary, a) # to get distinct parameter combinations
choices_grouped <- vector("list", nrow(params_sim))
for(set in seq_len(nrow(params_sim))){
  choices_grouped[[set]] <- choices_comprehensive %>%
    filter(s == params_sim[[set, "s"]] & boundary == params_sim[[set, "boundary"]] & a == params_sim[[set, "a"]]) %>%
    mutate(i = row_number()) # assign trial numbers
}

# allocate space for JAGS output

estimates_cpt_comprehensive <- vector("list", nrow(params_sim)) # posterior statistics and MCMC diagnostics

# MCMC simulation

params_cpt <- c("alpha", "gamma", "delta", "rho")
n_chains <- 20

for(set in seq_len(nrow(params_sim))){

  ## get trials of the respective parameter combination

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
                                  inits = inits_MCMC,
                                  parameters.to.save = params_cpt,
                                  model.file = "JAGS/cpt_model.txt",
                                  n.chains = n_chains,
                                  n.iter = 41000,
                                  n.burnin = 1000,
                                  n.thin = 20,
                                  n.cluster = n_chains, # run chains on different cores
                                  DIC = FALSE,
                                  jags.seed = 8362)

  ## get posteriors, credibility intervals, and MCMC diagnostics

  current_summary <- current_sample$BUGSoutput$summary %>% as_tibble(rownames = "parameter")
  estimates_cpt_comprehensive[[set]] <- expand_grid(params_sim[set, ], current_summary)
}

# save data

estimates_cpt_comprehensive <- estimates_cpt_comprehensive %>% map_dfr(as.list)
write_csv(estimates_cpt_comprehensive, "supplements/comprehensive/data/estimates_cpt_comprehensive.csv")
