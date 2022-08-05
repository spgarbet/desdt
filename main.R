# Goal:
#  Run  the cartesian product of the following combination
#     replicant = 1:5
#     N = 10^(2:7)
#     model  = "sick_sicker" or "PCE"
#     method = "data.table", "base", "simmer"
#
# Measurements (use system.time)
#     user
#     system
#     elapsed
# of
#     simulation
#     summarization

library(simmer)
library(data.table)
library(dplyr)
library(tidyr)


reps    <- 1:5
Ns      <- 10^(rev(seq(2, 7, by=0.5)))
models  <- c("sick_sicker")
methods <- c("data.table", "base", "simmer")
#methods <- c("data.table", "simmer")
result  <- NA

result <- as.data.frame(t(
  apply(
    expand.grid(reps, 1:length(models), 1:length(methods), Ns),
    1,
    function(x)
    {
      N        <- x[4]
      method   <- methods[x[3]]
      timing   <- c(NA, NA, NA)
      work_env <- new.env()

      cat("Model: ", models[x[2]], " Method: ", method, " N: ", N, "\n")

      source(file.path(models[x[2]], "model.R"), local=work_env)
      source(file.path(models[x[2]], paste0(method, ".R")), chdir=TRUE, local=work_env)

      timing  <- system.time(work_env$result <- work_env$simulation(N))
      timing2 <- system.time(work_env$summarize(work_env$result))
      work_env <- NULL

      gc()

      c(x, timing[1:3], timing2[1:3])
    }
  )
))
names(result) <- c("rep", "model", "method", "N",
                   "run.user", "run.sys", "run.elapsed",
                   "sum.user", "sum.sys", "sum.elapsed")
result$model  <- factor(result$model,  levels=1:length(models),  labels=models)
result$method <- factor(result$method, levels=1:length(methods), labels=methods)


save(result, file="result.RData")
