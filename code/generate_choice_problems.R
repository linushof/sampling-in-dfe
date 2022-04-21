pacman::p_load(tidyverse)
source("code/fun_generate_choice_problems.R") # call function for generating 2-outcome gambles

# generate 10,000 safe-risky gambles

set.seed(349)
initial_set <- generate_gambles(n = 10000, safe = TRUE, lower = 0, upper = 20)
initial_set <- initial_set %>% mutate(rare = case_when(a_p1 >= .2 & a_p1 <= .8 ~ "none",
                                                     a_p1 < .2 ~ "unattractive", # a_o1 is the smaller outcome
                                                     a_p1 > .8 ~ "attractive"))

# select subset of 60 gambles
# stratified sampling of 20 gambles with no/attractive/unattractive rare event

test_set <- tibble()

set.seed(735)
for(i in unique(initial_set$rare)) {
  type <- initial_set %>% filter(rare == i) # type of rare event
  smpl <- sample(seq_len(nrow(type)), size = 20) # random sample of 20 rows/gambles
  test_set <- bind_rows(test_set, type[smpl, ])
}

write_csv(test_set, "data/choice_problems.csv")
