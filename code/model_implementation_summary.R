pacman::p_load(tidyverse)
source("R/fun_cumulative_stats.R") # call functions for computing cumulative stats

# test set
gambles <- read_csv("data/gambles/sr_subset.csv")
n_agents <- 100

# simulation parameters
theta <- expand_grid(s = seq(-.5, .4, .1), # probability increment added to unbiased sampling probability of p = .5
                     boundary = c("absolute", "relative"), # boundary type
                     a = seq(15, 75, 15)) # boundaries

# simulation
## for each parameter combination (rows of theta), all gambles are played by all agents
## 100 (parameter combinations) x 60 (gambles) x 100 (agents) = 600.000 trials

set.seed(19543)
param_list <- vector("list", length(nrow(theta)))
for (set in seq_len(nrow(theta))) { # loop over parameter combinations
  gamble_list <- vector("list", length(nrow(gambles)))
  for (gamble in seq_len(nrow(gambles))) { # loop over gambles
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){ # loop over agents

      # initiate trials in a state of ignorance

      fd <- tibble() # frequency distribution of sampled outcomes
      p <- .5  # no attention bias
      s <- 0  # no switching at process initiation
      init <- sample(c("a", "b"), size = 1, prob = c(p + s, p - s)) # prospect attended first
      attend <- init
      boundary_reached <- FALSE

      # sampling of outcomes from prospects A and B continues until boundary is reached

      while(boundary_reached == FALSE) {

        # draw single sample from either A or B

        if(attend == "a") {
          single_smpl <- gambles[gamble, ] %>%
            mutate(attended = attend,
                   A = sample(x = c(a_o1, a_o2), size = 1, prob = c(a_p1, 1-a_p1)),
                   B = NA)
          s <- theta[[set, "s"]] # to update the probability of sampling from A again
        } else {
          single_smpl <- gambles[gamble, ] %>%
            mutate(attended = attend,
                   A = NA,
                   B = b_o1)
          s <- -1*theta[[set, "s"]] # to update the probability of sampling from B again
        }

        # add single sample to frequency distribution of sampled outcomes

        fd <- bind_rows(fd, single_smpl) %>%
          mutate(A_sum = cumsum2(A, na.rm = TRUE),
                 B_sum = cumsum2(B, na.rm = TRUE))

        # evaluate accumulated evidence over fd
        # evidence is either compared against the absolute or relative boundary

        if(theta[[set, "boundary"]] == "absolute") {
          fd <- fd %>%
            mutate(choice = case_when(A_sum >= theta[[set, "a"]] ~ "A",
                                      B_sum >= theta[[set, "a"]] ~ "B"))
        } else {
          fd <- fd %>%
            mutate(diff = round(A_sum - B_sum, 2),
                   choice = case_when(diff >= theta[[set, "a"]] ~ "A",
                                      diff <= -1*theta[[set, "a"]] ~ "B"))
        }

        # if boundary is not reached, draw new sample from A (B) according to s

        if(is.na(fd[[nrow(fd), "choice"]]) == FALSE) {
          boundary_reached <- TRUE
        } else {
          attend <- sample(c("a", "b"), size = 1, prob = c(p + s, p - s))
        }
      }
      agents_list[[agent]] <- expand_grid(agent, fd)
    }
    all_agents <- agents_list %>% map_dfr(as.list)
    gamble_list[[gamble]] <- expand_grid(gamble, all_agents)
  }
  all_gambles <- gamble_list %>% map_dfr(as.list)
  param_list[[set]] <- expand_grid(theta[set, ], all_gambles)
}
sim_comprehensive <- param_list %>% map_dfr(as.list)
write_csv(sim_comprehensive, "supplements/comprehensive/data/simulation_comprehensive.csv")
