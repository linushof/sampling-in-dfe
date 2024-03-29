pacman::p_load(tidyverse)

# Function for generating 2-outcome gambles
## n: number of gambles
## safe: gamble typ; TRUE = safe vs. risky option; FALSE = risky options only
## lower, upper: lower and upper boundary of outcome range

generate_gambles <- function(n, safe = TRUE, lower, upper) {

  # define a function for computing expected values

  ev <- function(p1, o1, o2 = 0) {
    round(p1 * o1 + (1-p1) * o2, digits = 2)
  }

  output <- vector("list", n)

  # safe vs. risky gambles

  if(safe == TRUE) {

    # for each gamble, randomly draw outcomes and probabilities

    output %>%
      map(tibble, # create tibble for each gamble
          "names" = c("a_o1", "b_o1", "a_o2", "a_p1"), # randomly generated values
          "values" = c(runif(3, min = lower, max = upper) %>%  # outcomes
                         round(2) %>%
                         sort(),
                       runif(1, min = .01, max = .99) %>% # probabilities
                         round(2))
          ) %>%

      # each gamble should be presented in a single row

      map(pivot_wider, names_from = "names", values_from = "values") %>%
      map_dfr(as.list) %>%

      # add gamble features that are determined by the random samples from above
      # compute and compare expected values

      mutate(a_p2 = round(1-a_p1,2),
             b_o2 = 0,
             b_p1 = 1,
             b_p2 = 0,
             a_ev = ev(a_p1, a_o1, a_o2),
             b_ev = ev(b_p1, b_o1, b_o2),
             ev_diff = round(a_ev - b_ev, 2),
             ev_ratio = round(a_ev/b_ev, 2)
      ) %>%
      select(a_p1, a_o1, a_p2, a_o2, b_p1, b_o1, b_p2, b_o2, a_ev, b_ev, ev_diff, ev_ratio) # sort features

  } else {

    # risky vs. risky gambles

    output %>%
      map(tibble,
          "names" = c("a_o1", "b_o1", "a_o2", "b_o2", "a_p1", "b_p1"),
          "values" = c(runif(3, min = lower, max = upper) %>% round(2) %>%
                         sort(), # prevent dominance: a_o1 < b_o1 < a_o2
                       runif(1, min = lower, max = upper) %>% round(2), # b_o2
                       runif(2, min = .01, max = .99)
          )
      ) %>%
      map(pivot_wider, names_from = "names", values_from = "values") %>%
      map_dfr(as.list) %>%
      mutate(a_p2 = 1-a_p1,
             b_p2 = 1-b_p1,
             a_ev = ev(a_p1, a_o1, a_o2),
             b_ev = ev(b_p1, b_o1, b_o2),
             ev_diff = round(a_ev - b_ev, 2),
             ev_ratio = round(a_ev/b_ev, 2)
      ) %>%
      select(a_p1, a_o1, a_p2, a_o2, b_p1, b_o1, b_p2, b_o2, a_ev, b_ev, ev_diff, ev_ratio)
  }
}
