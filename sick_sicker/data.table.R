  ##############################################################################
 ##
## Perform a DES simulation (with no resource constraints) using data.table
##
## Creators: Fernando Alarid Escudero and Carlos Pineda (c) 2022
## Authors: Fernando Alarid Escudero, Carlos Pineda, and Shawn Garbett
##
## MIT License
##
source("model.R")

# Next state vectors
v_when_H  <- c("S1","D")
v_when_S1 <- c("H","S2","D")
v_when_S2 <- c("D")

#'
#' Draw time to next event
#'
#' @param \code{draw_next_state function} draws the next health state for
#' an individual based on the minimum time to event for each state
#' @param dt_current: vector of current state and time when the event occurred;
#' @param t_HD:  The fixed time when the individual dies when is healthy;
#' @param event_no:  number of the next event to estimate
draw_next_state <- function(dt_current, v_t_HD, event_no)
{
  setnames(dt_current, 1, "V1")
  setnames(dt_current, 2, "t_base")

  #===========When healthy
  dt_current[V1 == "H", t_HS1 := ceiling(time_to_healthy_sick(.N, r_HS1)) + t_base] #time to sick
  dt_current$t_HD <- v_t_HD
  dt_current[V1 == "H", nE := v_when_H[max.col(-dt_current[V1 == "H", c("t_HS1", "t_HD")], "last")]]
  dt_current[V1 == "H", ntE := pmin(t_HS1, t_HD)]

  #============When  sick
  dt_current[V1 == "S1", t_S1D  := ceiling(time_to_sick_death(.N, r_S1D))   + t_base]  #random prob of death
  dt_current[V1 == "S1", t_S1H  := ceiling(time_to_sick_healthy(.N, r_S1H)) + t_base]  #random prob of being healthy
  dt_current[V1 == "S1", t_S1S2 := ceiling(time_to_sick_sicker(.N, r_S1S2)) + t_base]  #random prob of being sicker
  dt_current[V1 == "S1", nE := v_when_S1[max.col(-dt_current[V1 == "S1",c("t_S1H", "t_S1S2", "t_S1D", "t_HD")], "last")]]
  dt_current[V1 == "S1", ntE := pmin(t_S1H,t_S1S2, t_S1D, t_HD)]

  #=============When sicker
  dt_current[V1 == "S2", t_S2D := ceiling(time_to_sicker_death(.N, r_S2D)) + t_base ]   #random prob of death when sicker
  dt_current[V1 == "S2", nE := "D"]
  dt_current[V1 == "S2", ntE := pmin(t_S2D, t_HD)]

  #============When Death
  dt_current[V1 == "D", nE := "D"]
  dt_current[V1 == "D", ntE := t_base + 10]   #take ten cycles more to keep the state fixed over the time

  dt_result           <- dt_current[, c("nE", "ntE")]
  new_event_name      <- paste("E_", event_no, sep = "")
  new_time_event_name <- paste("timeE_", event_no, sep = "")

  setnames(dt_result, "nE",  new_event_name)
  setnames(dt_result, "ntE", new_time_event_name)

  dt_result
}


simulation <- function(N)
{
  #return(replicate(N, rnorm(100)))

  attach(params)

  dt_demog <- data.table(
    id      = 1:N,
    sex     = sample(c(0,1), N, prob=c(0.5, 0.5), replace=TRUE),
    E_0     = "H", # All start healthy
    timeE_0 = 0    # at time=0 for simulation start
  )

  ###### Time to secular death
  dt_demog[, t_HD := ceiling(time_to_healthy_death(.N, r_HD))]

  # Initial conditions
  v_t_HD <- data.table(dt_demog$t_HD)   #This time is calculated only once
  setnames(v_t_HD, 1, "t_HD")
  min_lastStateTime <- 0
  i <- 0

  #While to complete in all the individuals the number of cycles required (30 cycles in this exercise)
  while (min_lastStateTime < n_t)
  {
    i <- i + 1
    str_current <- paste("dt_current <- data.table(dt_demog[,c('E_", (i - 1),
                         "','timeE_",(i - 1),"')])", sep = "")
    eval(parse(text = str_current))
    event_no <- i
    dt_newE <- draw_next_state(dt_current = dt_current, v_t_HD = v_t_HD, event_no = event_no)
    dt_demog <- cbind(dt_demog,dt_newE)
    min_lastStateTime <- min(dt_newE[, 2], na.rm = T)
    n_events <- i
  }

  dt_demog
}

summarize <- function(result) replicate(nrow(result), rnorm(100))
