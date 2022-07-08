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
Ns      <- 10^(2:4)  # For development
models  <- c("sick_sicker")
#methods <- c("data.table", "base", "simmer")
methods <- c("data.table", "simmer")
result  <- NA

result <- as.data.frame(t(
  apply(
    expand.grid(1:length(models), 1:length(methods), Ns, reps),
    1,
    function(x)
    {
      N        <- x[3]
      method   <- methods[x[2]]
      timing   <- c(NA, NA, NA)
      work_env <- new.env()

      source(file.path(models[x[1]], "model.R"), local=work_env)
      source(file.path(models[x[1]], paste0(method, ".R")), chdir=TRUE, local=work_env)

      timing  <- system.time(work_env$result <- work_env$simulation(N))
      timing2 <- system.time(work_env$summarize(work_env$result))
      work_env <- NULL

      c(x, timing[1:3], timing2[4:6])
    }
  )
))
names(result) <- c("model", "method", "N", "rep",
                   "run.user", "run.sys", "run.elapsed",
                   "sum.user", "sum.sys", "sum.elapsed")
result$model  <- factor(result$model,  levels=1:length(models),  labels=models)
result$method <- factor(result$method, levels=1:length(methods), labels=methods)


boxplot(run.user~model+method+N, result)


