pacman::p_load(tidyverse)
source("R/fun_compute_cumulative_stats.R") # call functions for computing cumulative stats

# test set
gambles <- read_csv("data/choice_problems.csv")
n_agents <- 100

# simulation parameters
theta <- expand_grid(s = seq(-.5, .4, .1), # probability increment added to unbiased sampling probability of p = .5
                     boundary = c("absolute", "relative"), # boundary type
                     a = seq(1, 5, 1)) # boundaries (number of required round-wins)

# simulation
## for each parameter combination (rows of theta), all gambles are played by all agents
## 100 (parameter combinations) x 60 (gambles) x 100 (agents) = 600.000 trials

set.seed(56221)
param_list <- vector("list", length(nrow(theta)))
for (set in seq_len(nrow(theta))) {  # loop over parameter combinations
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
      round <- 1
      boundary_reached <- FALSE

      # sampling of outcomes from prospects A and B continues until boundary is reached

      while(boundary_reached == FALSE) {

        # prospects are compared round-wise
        ## a round consists of an uninterrupted sequence of sampled outcomes each from A and B
        ## prospects with a higher mean of sampled outcomes within a round earn a round-win

        smpl_round <- tibble()
        while(attend == init) {# sequence of single samples from prospect attended first

          # draw single sample from either A or B

          if(attend == "a") {
            single_smpl <- gambles[gamble, ] %>%
              mutate(round = round,
                     attended = attend,
                     A = sample(x = c(a_o1, a_o2), size = 1, prob = c(a_p1, 1-a_p1)),
                     B = NA)
            s <- theta[[set, "s"]] # to update the probability of sampling from A again
          } else {
            single_smpl <- gambles[gamble, ] %>%
              mutate(round = round,
                     attended = attend,
                     A = NA,
                     B = b_o1)
            s <- -1*theta[[set, "s"]] # to update the probability of of sampling from B again
          }
          smpl_round <- bind_rows(smpl_round, single_smpl)
          attend <- sample(c("a", "b"), size = 1, prob = c(p + s, p - s))
        }

        while(attend != init) { # sequence of single samples from prospect attended second

          # draw single sample from either A or B

          if(attend == "a") {
            single_smpl <- gambles[gamble, ] %>%
              mutate(round = round,
                     attended = attend,
                     A = sample(x = c(a_o1, a_o2), size = 1, prob = c(a_p1, 1-a_p1)),
                     B = NA)
            s <- theta[[set, "s"]]
          } else {
            single_smpl <- gambles[gamble, ] %>%
              mutate(round = round,
                     attended = attend,
                     A = NA,
                     B = b_o1)
            s <- -1*theta[[set, "s"]]
          }
          smpl_round <- bind_rows(smpl_round, single_smpl)
          attend <- sample(c("a", "b"), size = 1, prob = c(p + s, p - s))
        }

        # compare means over sampled outcomes from A and B
        # assign round-win

        smpl_round <- smpl_round %>%
          mutate(A_rmean = cummean2(A, na.rm = TRUE),
                 B_rmean = cummean2(B, na.rm = TRUE),
                 rdiff = A_rmean - B_rmean)
        smpl_round[[nrow(smpl_round), "A_win"]] <- case_when(smpl_round[[nrow(smpl_round), "rdiff"]] > 0 ~ 1,
                                                             smpl_round[[nrow(smpl_round), "rdiff"]] <= 0 ~ 0)
        smpl_round[[nrow(smpl_round), "B_win"]] <-  case_when(smpl_round[[nrow(smpl_round), "rdiff"]] >= 0 ~ 0,
                                                              smpl_round[[nrow(smpl_round), "rdiff"]] < 0 ~ 1)

        # add sampling round to frequency distribution of sampled outcomes

        fd <- bind_rows(fd, smpl_round)
        fd[[nrow(fd), "A_sum"]] <- sum(fd[["A_win"]], na.rm = TRUE)
        fd[[nrow(fd), "B_sum"]] <- sum(fd[["B_win"]], na.rm = TRUE)

        # evaluate accumulated evidence (as round-wins) over fd
        # evidence is either compared against the absolute or relative boundary

        if(theta[[set, "boundary"]] == "absolute") {
          fd <- fd %>%
            mutate(choice = case_when(A_sum >= theta[[set, "a"]] ~ "A",
                                      B_sum >= theta[[set, "a"]] ~ "B"))
        } else {
          fd[[nrow(fd), "diff"]] <- fd[[nrow(fd), "A_sum"]] - fd[[nrow(fd), "B_sum"]]
          fd <- fd %>%
            mutate(choice = case_when(diff >= theta[[set, "a"]] ~ "A",
                                      diff <= -1*theta[[set, "a"]] ~ "B"))
        }

        # if boundary is not reached, start new sampling round

        if(is.na(fd[[nrow(fd), "choice"]]) == FALSE) {
          boundary_reached <- TRUE
        } else {
          round <- round + 1
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
simulation <- param_list %>% map_dfr(as.list)
write_csv(simulation, "data/simulation.csv")
