pacman::p_load(tidyverse)
source("R/fun_generate_gambles.R") # call function for generating 2-outcome gambles

# generate 10,000 safe-risky gambles

set.seed(349)
sr_gambles <- generate_gambles(n = 10000, safe = TRUE, lower = 0, upper = 20)
sr_gambles <- sr_gambles %>% mutate(rare = case_when(a_p1 >= .2 & a_p1 <= .8 ~ "none",
                                                     a_p1 < .2 ~ "unattractive", # a_o1 is the smaller outcome
                                                     a_p1 > .8 ~ "attractive"))
write_csv(sr_gambles, "data/gambles/sr_gambles.csv")


# select subset of 60 gambles
# stratified sampling of 20 gambles with no/attractive/unattractive rare event

sr_subset <- tibble()

set.seed(735)
for(i in unique(sr_gambles$rare)) {
  type <- sr_gambles %>% filter(rare == i) # type of rare event
  smpl <- sample(seq_len(nrow(type)), size = 20) # random sample of 20 rows/gambles
  sr_subset <- bind_rows(sr_subset, type[smpl, ])
}
write_csv(sr_subset, "data/gambles/sr_subset.csv")
