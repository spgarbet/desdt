  ##############################################################################
 ##
## Perform a DES simulation (with no resource constraints) using data.table
##
## Creators: Fernando Alarid Escudero and Carlos Pineda (c) 2022
## Authors: Fernando Alarid Escudero, Carlos Pineda, and Shawn Garbett
##
## MIT License
##

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


# Costs function:
# calculates the cost accrued by an individual this cycle
Costs <- function(M_t) {
  # M_t: current health state
  c_t <- c()
  c_t[M_t == "H"]  <- c_H   # costs accrued by being Healthy this cycle
  c_t[M_t == "S1"] <- c_S1   # costs accrued by being Sick this cycle
  c_t[M_t == "S2"] <- c_S2   # costs at Dead state
  c_t[M_t == "D"]  <- c_D

  return(c_t)  # return costs accrued this cycle
}
# Effs function:
# calculates QALYs accrued by an individual for this cycle
Effs <- function(M_t) {
  # M_t: current health state
  q_t <- c()
  q_t[M_t == "H"]  <- u_H  # QALYs accrued by being Healthy this cycle
  q_t[M_t == "S1"] <- u_S1  # QALYs accrued by being Sick this cycle
  q_t[M_t == "S2"] <- u_S2  # QALYs at Dead state
  q_t[M_t == "D"]  <- u_D  # QALYs at Dead state

  return(q_t)  # return the QALYs accrued this cycle
}


simulation <- function(N)
{
  #return(replicate(N, rnorm(100)))

  dt_demog <- NA

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

summarize <- function(dt_demog)
{
  N <- nrow(dt_demog)

  #Matrix fo States, Costs, and Effs
  m_M_D <- m_C_D <- m_E_D <-  matrix(nrow = N, ncol = n_t + 1,
                               dimnames = list(paste("ind", 1:N, sep = " "),
                                               paste("cycle", 0:n_t, sep = " ")))

  #Change to long format for events
  dt_demog_long_event <- pivot_longer(dt_demog, cols = starts_with("E_"),
                                      names_to = c("State", "event"),
                                      names_pattern =  "(\\w+)\\_(\\d+)") %>%
    select(c("id", "event", "value")) %>% rename(State = value)

  #Change to long format for time to event
  dt_demog_long_time <- pivot_longer(dt_demog, cols = starts_with("timeE_"),
                                     names_to = c("time", "event"),
                                     names_pattern =  "(\\w+)\\_(\\d+)") %>%
    select(c("id","event","value")) %>% rename(t_Event = value)

  #Inner join
  dt_demog_long <- inner_join(dt_demog_long_event,dt_demog_long_time, by = c("id","event") )
  dt_demog_long <- data.table(dt_demog_long)
  dt_demog_long <- dt_demog_long[t_Event <= 30,]    # Keep only events under 30 cycles

  # Difference in cycles between events
  dt_demog_long <- dt_demog_long[ , delta :=  shift(t_Event, type = "lead") - t_Event , by = id]
  dt_demog_long <- dt_demog_long[is.na(delta), delta :=  (n_t + 1) - t_Event , by = id]

  # Expansion of the States vector
  dt_demog_expanded <- dt_demog_long[ ,list(freq=rep(State,delta))]
  dt_demog_expanded <- as.matrix(dt_demog_expanded)

  ## Transforming the expanded vector to State matrix
  for (i in 1:N)
  {
    ini <- 31*(i-1) + 1
    end <- ini + 30
    m_M_D[i,] <- dt_demog_expanded[ini:end,]
  }

  # Drop auxiliar tables
  dt_demog_expanded   <- NULL
  dt_demog_long_event <- NULL
  dt_demog_long_time  <- NULL
  dt_demog_long       <- NULL


  # Initializes the progress bar
  # pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
  #                      max = n_i, # Maximum value of the progress bar
  #                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
  #                      width = 50,   # Progress bar width. Defaults to getOption("width")
  #                      char = "=")   # Character used to create the bar
  #
  # for (ind in 1:n_i) {
  # v_state_i <- c()
  #
  # for (i in 0:(n_events-1)) {
  #
  #   str_state <- paste("State <- as.character(dt_demog[ind,E_",i,"])", sep = "")
  #   eval(parse(text = str_state))
  #
  #   str_tend <- paste("t_end <- as.numeric(dt_demog[ind,timeE_",(i+1),"])", sep = "")
  #   eval(parse(text = str_tend))
  #
  #   str_tini <- paste("t_ini <- as.numeric(dt_demog[ind,timeE_",(i),"])", sep = "")
  #   eval(parse(text = str_tini))
  #
  #   v_state <- rep(State, (t_end-t_ini))
  #
  #   v_state_i <- append(v_state_i,v_state)
  #
  # }
  #
  # #v_state_i
  # m_M_D[ind,] <- v_state_i[1:31]
  # setTxtProgressBar(pb, ind)
  # }

  # Estimation of Costs and effectiveness


  for (t in 1:(n_t+1)) {

    # Costs
    m_C_D[, t] <- Costs(m_M_D[, t])  # costs accrued by each individual during cycle  t+1

    # QALYs
    m_E_D[, t] <- Effs(m_M_D[, t])  # QALYs accrued by each individual during cycle  t+1

  }

  ##### ================= CACLULATE TOTAL COSTS AND QALYS   ================#####

  v_dw <- 1 / (1 + d_c) ^ (0:n_t) # calculate discount weight for each cycle based on discount rate d_r

  tc_DES <- m_C_D %*% v_dw    # total discounted cost per individual
  te_DES <- m_E_D %*% v_dw    # total discounted QALYs per individual

  tc_avg_DES <- mean(tc_DES)    # average discounted cost
  tc_se_DES  <- sd(tc_DES) / sqrt(N)
  te_avg_DES <- mean(te_DES)    # average discounted QALYs
  te_se_DES  <- sd(te_DES) / sqrt(N)

  list(cost=tc_avg_DES, cost_se=tc_se_DES, qaly=te_avg_DES, qaly_se=te_se_DES)
}
