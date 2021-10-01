pacman::p_load(tidyverse)

# load estimates
cols <- list(.default = col_double(),
             boundary = col_factor(),
             parameter = col_guess())
cpt <- read_csv("data/estimates/estimates_hierarchical_cpt.csv", col_types = cols)


# separate parameter type from hierarchy level
cpt <- cpt %>%
  mutate(parameter = str_replace_all(cpt$parameter, c("\\[" = "_",
                                                            "\\]" = "",
                                                            "mu.alpha" = "alpha_mu",
                                                            "mu.gamma" = "gamma_mu",
                                                            "mu.delta" = "delta_mu",
                                                            "mu.rho" = "rho_mu"))) %>% # use uniform separators
  separate(parameter, c("parameter", "level"), sep = "_") # separate variables into two columns
