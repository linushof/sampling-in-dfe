# Functions for generating cumulative sums and and means
## cumsum2() and cummean2() are extensions of the base functions including a na.rm-argument.
## If na.rm = TRUE, the lagging cumulative value is used for the position of the missing element.

# cumsum2()
cumsum2 <- function(x, na.rm = FALSE) {
  output <- vector("double", length(x))
  for (i in seq_along(x)) {
    if(sum(is.na(x[1:i])) == length(x[1:i])) { # return NA if all lagging values are NA
      output[[i]] <- NA
    } else {
      output[[i]] <- sum(x[1:i], na.rm = na.rm)
    }
  }
  output
}
# cummean2()
cummean2 <- function(x, na.rm = FALSE) {
  output <- vector("double", length(x))
  for (i in seq_along(x)) {
    if(sum(is.na(x[1:i])) == length(x[1:i])) {
      output[[i]] <- NA
    } else {
      output[[i]] <- mean(x[1:i], na.rm = na.rm)
    }
  }
  output
}
