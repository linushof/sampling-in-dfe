pacman::p_load(tidyverse, digest, crayon)

# load simulation data
cols <- list(.default = col_double(),
             boundary = col_factor(),
             gamble = col_factor(),
             agent = col_factor(),
             rare = col_factor(),
             attended = col_factor(),
             choice = col_factor())
simulation_comprehensive <- read_csv("supplements/comprehensive/data/simulation_comprehensive.csv", col_types = cols)

# use hash to validate simulation data

hash_sim_comprehensive <- digest(simulation_comprehensive, "md5")
if(hash_sim_comprehensive != "6e62f9cbeebbabcebae86188d72426ab"){
  warning("Mismatch between original and current data!\nCurrent hash is:\n    '", hash_sim_comprehensive, "'")
}else{

  cat(green("Data is valid. Continuing preparation of choice data."))

  ## transform data to obtain trial summaries

  choices_comprehensive <- simulation_comprehensive %>%
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

    mutate(s = 1-(s+.5)) %>%
    select(s:gamble, rare, a_p1:ev_ratio, agent, n_sample, n_a, a_p1_exp, a_p2_exp, a_ev_exp, b_ev_exp, choice, A_sum, B_sum, diff)

  # save data
  ## required: 60 gambles x 100 subjects x 100 parameter combinations = 600.000 choices
  write_csv(choices_comprehensive, "supplements/comprehensive/data/choices_comprehensive.csv")

}

