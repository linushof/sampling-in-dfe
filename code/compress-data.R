library(tidyverse)

# choice data

## read
cols_choices <- list(.default = col_double(),
                     boundary = col_factor(),
                     gamble = col_factor(),
                     rare = col_factor(),
                     agent = col_factor(),
                     choice = col_factor())

choices_roundwise <- read_csv("data/choices_roundwise.csv", col_types = cols_choices)
choices_summary <- read_csv("data/choices_summary.csv", col_types = cols_choices)

## save compressed data
write_rds(choices_roundwise, file = "data/compressed/choices_roundwise.rds.bz2", compress = "bz2")
write_rds(choices_summary, file = "data/compressed/choices_summary.rds.bz2", compress = "bz2")


# simulation data

## read

cols <- list(.default = col_double(),
             boundary = col_factor(),
             gamble = col_factor(),
             agent = col_factor(),
             rare = col_factor(),
             attended = col_factor(),
             choice = col_factor())
simulation_roundwise <- read_csv("data/simulation_roundwise.csv", col_types = cols)
simulation_summary <- read_csv("data/simulation_summary.csv", col_types = cols)

## save compressed data
write_rds(simulation_roundwise, file = "data/compressed/simulation_roundwise.rds.bz2", compress = "bz2")
write_rds(simulation_summary, file = "data/compressed/simulation_summary.rds.bz2", compress = "bz2")

