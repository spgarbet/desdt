library(ggplot2)
library(plyr)

load("result.RData")

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
