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
result <- as.data.frame(do.call(rbind,
  apply(
    expand.grid(reps, Ns, models, methods, stringsAsFactors = FALSE),
    1,
    function(x)
    {
      N      <- as.numeric(x[2])
      model  <- x[3]
      method <- x[4]

      # FIXME: Stub for doing each timing
      timing <- system.time(replicate(N*10, rnorm(10000)))

      list(
         model=x[3],
         method=x[4],
         N=N,
         rep=as.numeric(x[1]),
         user=timing[1],
         system=timing[2],
         elapsed=timing[3])
    }
  )
))
