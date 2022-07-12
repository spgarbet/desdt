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
Ns      <- 10^(5:2)  # For development
models  <- c("sick_sicker")
#methods <- c("data.table", "base", "simmer")
methods <- c("data.table", "simmer")
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
#boxplot(run.user~model+method+N, result)


library(ggplot2)
library(plyr)

# New version of length which can handle NA's: if na.rm==T, don't count them
length2 <- function (x, na.rm=FALSE) if (na.rm) sum(!is.na(x)) else length(x)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE)
{
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
    .fun = function(xx, col) {
      c(N    = length2(xx[[col]], na.rm=na.rm),
        mean = mean   (xx[[col]], na.rm=na.rm),
        sd   = sd     (xx[[col]], na.rm=na.rm)
      )
    },
    measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  datac
}

result$run <- with(result, run.sys + run.user)
result$sum <- with(result, sum.sys + sum.user)
result$log10N <- log10(result$N)

time.run <- summarySE(result, measurevar="run", groupvars=c("log10N", "method"))

ggplot(time.run, aes(x=log10N, y=run, colour=method)) +
  geom_errorbar(aes(ymin=run-ci, ymax=run+ci), width=0.1) +
  geom_line() +
  geom_point() +
  labs(x="log10(N)", y="sys+user CPU time (s)") +
  ggtitle("CPU Usage Sick/Sicker in R")


ggplot(time.run, aes(x=log10N, y=run, colour=method)) +
  geom_errorbar(aes(ymin=run-ci, ymax=run+ci), width=0.1) +
  geom_line() +
  geom_point() +
  scale_y_continuous(trans='log10') +
  labs(x="log10(N)", y="log10(sys+user) CPU time (s)") +
  ggtitle("CPU Usage Sick/Sicker in R")
