source("model.R")

simulation <- function(N) replicate(N, rnorm(100))
summarize <- function(result) replicate(nrow(result), rnorm(100))

