pacman::p_load(tidyverse, digest, crayon)

# load simulation data
cols <- list(.default = col_double(),
             boundary = col_factor(),
             gamble = col_factor(),
             agent = col_factor(),
             rare = col_factor(),
             attended = col_factor(),
             choice = col_factor())
simulation <- read_csv("data/simulation.csv", col_types = cols)

# use hash to validate simulation data

hash_sim <- digest(simulation, "md5")
if(hash_sim != "295e1a7b23402dd50a1dae5a54141f78"){
  warning("Mismatch between original and current data!\nCurrent hash is:\n    '", hash_sim, "'")
}else{

  cat(green("Data is valid. Continuing preparation of choice data."))


  ## transform data to obtain trial summaries

  choices <- simulation %>%
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

    mutate(s = 1-(s+.5)) %>%  # to interpret parameter s as switching probability
    select(s:gamble, rare, a_p1:ev_ratio, agent, n_sample, n_a, a_p1_exp, a_p2_exp, a_ev_exp, b_ev_exp, choice, A_sum, B_sum, diff)

  # save data
  ## required: 60 gambles x 100 subjects x 100 parameter combinations = 600.000 choices
  write_csv(choices, "data/simulation_summary.csv")

}
