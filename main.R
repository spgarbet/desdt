# Goal:
#  Run  the cartesian product of the following combination
#     replicant = 1:5
#     N = 10^(2:7)
#     model = "sick_sicker" or "PCE"
#     method = "data.table", "base", "simmer"
#
# Measurements (use system.time)
#     user
#     system
#     elapsed
#

reps    <- 1:5
Ns      <- 10^(2:3)  # For development
models  <- c("sick_sicker")
methods <- c("data.table", "base", "simmer")

# FIXME: Return type is borked, ugh!
# Considering: https://stackoverflow.com/questions/36982755/r-apply-like-function-that-returns-a-data-frame
result <- as.data.frame(t(
  apply(
    expand.grid(1:length(models), 1:length(methods), Ns, reps),
    1,
    function(x)
    {
      N      <- x[3]
      model  <- models[x[1]]
      method <- methods[x[2]]

      # FIXME: Stub for doing each timing
      timing <- system.time(replicate(N*100, rnorm(100)))


      c(x, timing[1:3])
    }
  )
))
names(result) <- c("model", "method", "N", "rep", "user", "sys", "elapsed")
result$model  <- factor(result$model,  levels=1:length(models),  labels=models)
result$method <- factor(result$method, levels=1:length(methods), labels=methods)

boxplot(user~model+method+N, result)
