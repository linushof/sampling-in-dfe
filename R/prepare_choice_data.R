pacman::p_load(tidyverse, digest)

# load simulation data
cols <- list(.default = col_double(),
             boundary = col_factor(),
             gamble = col_factor(),
             agent = col_factor(),
             rare = col_factor(),
             attended = col_factor(),
             choice = col_factor())
sim_piecewise <- read_csv("data/simulation/sim_piecewise.csv", col_types = cols)
sim_comprehensive <- read_csv("data/simulation/sim_comprehensive.csv", col_types = cols)

# piecewise strategy

## use hash to validate simulation data

hash_piecewise <- digest(sim_piecewise, "md5")
if(hash_piecewise != "295e1a7b23402dd50a1dae5a54141f78"){
  warning("Mismatch between original and current data!\nCurrent hash is:\n    '", hash_piecewise, "'")
}

## transform data to obtain trial summaries

choices_piecewise <- sim_piecewise %>%
  group_by(s, boundary, a, gamble, agent) %>% # group by trials
  mutate(n_sample = n(), # total number of single samples
         n_a = n_sample - sum(is.na(A)), # number of single samples drawn from risky option
         a_p2_exp = round(sum(if_else(A == a_o2, 1, 0), na.rm = TRUE)/n_a, 2), # experienced probability of higher risky outcome
         a_p1_exp = round(1 - a_p2_exp, 2), # experienced probability of lower risky outcome
         a_ev_exp = round(mean(A, na.rm = TRUE), 2), # experienced mean A
         b_ev_exp = round(mean(B, na.rm = TRUE), 2)) %>%  # experienced mean B
  ungroup() %>%
  filter(!is.na(choice)) %>% # discard single samples

  ## tidy data

  mutate(strategy = "piecewise",
         s = 1-(s+.5)) %>%  # to interpret parameter s as switching probability
  select(strategy, s:gamble, rare, a_p1:ev_ratio, agent, n_sample, n_a, a_p1_exp, a_p2_exp, a_ev_exp, b_ev_exp, choice, A_sum, B_sum, diff)

# comprehensive strategy

## use hash to validate simulation data

hash_comprehensive <- digest(sim_comprehensive, "md5")
if(hash_comprehensive != "6e62f9cbeebbabcebae86188d72426ab"){
  warning("Mismatch between original and current data!\nCurrent hash is:\n    '", hash_comprehensive, "'")
}

## transform data to obtain trial summaries

choices_comprehensive <- sim_comprehensive %>%
  group_by(s, boundary, a, gamble, agent) %>%
  mutate(n_sample = n(),
         n_a = n_sample - sum(is.na(A)),
         a_p2_exp = round(sum(if_else(A == a_o2, 1, 0), na.rm = TRUE)/n_a, 2),
         a_p1_exp = round(1 - a_p2_exp, 2),
         a_ev_exp = round(mean(A, na.rm = TRUE), 2),
         b_ev_exp = round(mean(B, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  filter(!is.na(choice)) %>%

  ## tidy data

  mutate(strategy = "comprehensive",
         s = 1-(s+.5)) %>%
  select(strategy, s:gamble, rare, a_p1:ev_ratio, agent, n_sample, n_a, a_p1_exp, a_p2_exp, a_ev_exp, b_ev_exp, choice, A_sum, B_sum, diff)

# all choice data
## required: 2 strategies x 60 gambles x 100 subjects x 100 parameter combinations = 1.200.000 choices
choices <- bind_rows(choices_piecewise, choices_comprehensive)
write_csv(choices, "data/choices/choices.csv")
