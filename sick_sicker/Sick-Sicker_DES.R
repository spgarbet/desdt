###==========================================================================###
####                DES of sick - sicker model                             ####
###==========================================================================###


rm(list = ls())  # remove any variables in R's memory 

#### 01 Load packages ####

library(data.table)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)

####===================== 02 Load functions ================================####
# no functions required

####===================== 03 Model Structure ============================== ####


# Model input
n_i  <- 100000                # number of simulated individuals
n_t   <- 30                    # time horizon, 30 cycles
v_n   <- c("H","S1","S2","D")  # the model states: Healthy (H), Sick (S1), Sicker (S2), Dead (D)
n_s   <- length(v_n)           # the number of health states
E_0 <- rep("H", n_i)         # everyone begins in the healthy state 
d_c   <- d_e <- 0.03           # equal discounting of costs and QALYs by 3%
v_Trt <- c("No Treatment", "Treatment") # store the strategy names

# Transition rates

r_HS1   <- 0.15       # rate to become sick when healthy

r_S1H   <- 0.5       # rate to become healthy when sick

r_S1S2  <- 0.105      # rate to become sicker when sick

rr_S1   <- 3             	     # rate ratio of death in sick vs healthy
rr_S2   <- 10            	     # rate ratio of death in sicker vs healthy 

r_HD    <- 0.005               # rate of dying when healthy
r_S1D   <- rr_S1 * r_HD  	     # rate of death in sick
r_S2D   <- rr_S2 * r_HD  	     # rate of death in sicker

# Cost and utility inputs 
c_H     <- 2000                # cost of remaining one cycle healthy
c_S1    <- 4000                # cost of remaining one cycle sick 
c_S2    <- 15000               # cost of remaining one cycle sicker
c_Trt   <- 12000               # cost of treatment (per cycle)
c_D     <- 0

u_H     <- 1                   # utility when healthy 
u_S1    <- 0.75                # utility when sick 
u_S2    <- 0.5                 # utility when sicker 
u_Trt   <- 0.95                # utility when being treated
u_D     <- 0

####======================  06  Functions  =================================####

time_to_healthy_sick  <- function(n_samp = 1, r_HS1)  {
  v_t_HS1 <- rexp(n = n_samp, rate = r_HS1)
  return(v_t_HS1)
}

time_to_healthy_death   <- function(n_samp = 1, r_HD)  {
  v_t_HD <- rexp(n = n_samp, rate = r_HD)
  return(v_t_HD)
}

time_to_sick_healthy  <- function(n_samp = 1, r_S1H)  {
  v_t_S1H <- rexp(n = n_samp, rate = r_S1H)
  return(v_t_S1H)
}

time_to_sick_sicker   <- function(n_samp = 1, r_S1S2)  {
  v_t_S1S2 <- rexp(n = n_samp, rate = r_S1S2)
  return(v_t_S1S2)
}

time_to_sick_death <- function(n_samp = 1, r_S1D)  {
  v_t_S1D <- rexp(n = n_samp, rate = r_S1D)
  return(v_t_S1D)
}

time_to_sicker_death <- function(n_samp = 1, r_S2D)  {
  v_t_S2D <- rexp(n = n_samp, rate = r_S2D)
  return(v_t_S2D)
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

# Draw time to next event 
#' @param \code{draw_next_state function} draws the next health state for 
#' an individual based on the minimum time to event for each state
#' @param dt_current: vector of current state and time when the event occurred; 
#' @param t_HD:  The fixed time when the individual dies when is healthy;
#' @param event_no:  number of the next event to estimate

draw_next_state <- function(dt_current, v_t_HD, event_no) {
  
  setnames(dt_current, 1, "V1")          
  setnames(dt_current, 2, "t_base")
  
  # Next state vectors   
  v_when_H  <- c("S1","D")
  v_when_S1 <- c("H","S2","D")
  v_when_S2 <- c("D")
  
  #===========When healthy
  dt_current[V1 == "H", t_HS1 := ceiling(time_to_healthy_sick(n_samp = .N, r_HS1 = r_HS1)) + t_base] #time to sick
  dt_current$t_HD <- v_t_HD 
  
  dt_current[V1 == "H", nE := v_when_H[max.col(-dt_current[V1 == "H", c("t_HS1", "t_HD")], "last")]]
  
  dt_current[V1 == "H", ntE := pmin(t_HS1, t_HD)]
  
  #============When  sick
  dt_current[V1 == "S1", t_S1D := ceiling(time_to_sick_death(n_samp = .N, r_S1D = r_S1D)) + t_base]  #random prob of death
  
  dt_current[V1 == "S1", t_S1H := ceiling(time_to_sick_healthy(n_samp = .N, r_S1H = r_S1H)) + t_base]  #random prob of being healthy
  
  dt_current[V1 == "S1", t_S1S2 := ceiling(time_to_sick_sicker(n_samp = .N, r_S1S2 = r_S1S2)) + t_base]  #random prob of being sicker
  
  dt_current[V1 == "S1", nE := v_when_S1[max.col(-dt_current[V1 == "S1",c("t_S1H", "t_S1S2", "t_S1D", "t_HD")], "last")]]
  dt_current[V1 == "S1", ntE := pmin(t_S1H,t_S1S2, t_S1D, t_HD)]
  
  #=============When sicker
  dt_current[V1 == "S2", t_S2D := ceiling(time_to_sicker_death(n_samp = .N, r_S2D = r_S2D)) + t_base ]   #random prob of death when sicker 
  
  dt_current[V1 == "S2", nE := "D"]
  dt_current[V1 == "S2", ntE := pmin(t_S2D, t_HD)]
  
  #============When Death
  
  dt_current[V1 == "D", nE := "D"]
  dt_current[V1 == "D", ntE := t_base + 10]   #take ten cycles more to keep the state fixed over the time
  
  dt_result <- dt_current[, c("nE", "ntE")]
  new_event_name      <- paste("E_", event_no, sep = "")
  new_time_event_name <- paste("timeE_", event_no, sep = "")
  
  setnames(dt_result, "nE", new_event_name)
  setnames(dt_result, "ntE", new_time_event_name)
  
  return(dt_result)
  
}


####===================== 07 DES Model =====================================####

set.seed(1223)
###### Create data.table ----

dt_demog <- data.table(id = 1:n_i,
                       sex = sample(x = c(0, 1), 
                                    size = n_i,
                                    prob = c(0.5, 0.5),
                                    replace = TRUE))

dt_demog[, E_0 := E_0]

p = Sys.time() 

dt_demog[, timeE_0 := 0]

###### Time to healthy - death  ----
dt_demog[, t_HD := ceiling(time_to_healthy_death(n_samp = .N, r_HD = r_HD))] #time to sick

# Initial conditions 
v_t_HD <- data.table(dt_demog$t_HD)   #This time is calculated only once
setnames(v_t_HD, 1, "t_HD")
min_lastStateTime <- 0
i <- 0

#While to complete in all the individuals the number of cycles required (30 cycles in this exercise)
while (min_lastStateTime < n_t) {     
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

#Matrix fo States, Costs, and Effs
m_M_D <- m_C_D <- m_E_D <-  matrix(nrow = n_i, ncol = n_t + 1, 
                             dimnames = list(paste("ind", 1:n_i, sep = " "), 
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
for (i in 1:n_i)  {
  ini <- 31*(i-1) + 1
  end <- ini + 30
  m_M_D[i,] <- dt_demog_expanded[ini:end,]   
}

comp.time.DES = Sys.time() - p  
comp.time.DES
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
tc_sd_DES  <- sd(tc_DES) / sqrt(n_i)
te_avg_DES <- mean(te_DES)    # average discounted QALYs
te_sd_DES  <- sd(te_DES) / sqrt(n_i)


####======================= 08 Graphics  ===================================####

# Run micro-simulation to get comparative graphics 
source("microsimulation_supp4.R")

m_M_D <- data.table(m_M_D)
m_M <- data.table(sim_no_trt$m.M)

States_micro <- States_DES <- matrix(nrow = n_t + 1, ncol = 4, 
                                     dimnames = list(paste("cycle", 0:n_t, sep = "_"),     # name the rows cycle0, cycle1, cycle2, cycle3, etc.
                                                     paste("State", c("D","H","S1", "S2"), sep = "_")))  # name the columns as the states

States_DES[1,] <- States_micro[1,] <- c(0,1,0,0)

#fix for the second state
v_2nd_DES <- m_M_D[ , (table(m_M_D[,2])/.N)]
v_2nd_DES <- append(v_2nd_DES,0)

v_2nd_mic <- m_M[ , (table(m_M[,2])/.N)]
v_2nd_mic <- append(v_2nd_mic,0)

States_DES[2,] <- v_2nd_DES
States_micro[2,] <- v_2nd_mic

for (t in 2:n_t+1) {
  States_DES[t,]   <- m_M_D[ , (table(m_M_D[,..t])/.N)]
  States_micro[t,] <- m_M[ , (table(m_M[,..t])/.N)]
}

States_DES <- cbind(States_DES,c(0:n_t))
States_micro <- cbind(States_micro,c(0:n_t))

colors <- c("DES Model" = "blue", "Microsimulation model" = "red")

#Comparative for Death
ggplot(data.frame(States_DES)) + geom_line(aes(x = V5, y = State_D, color="DES Model")) +
  geom_line(data = data.frame(States_micro), aes(x = V5, y = State_D, color="Microsimulation model" )) +
  labs(x="Cycle", y="Proportion in Death state", color = "Legend") +
  scale_color_manual(values = colors)


#Comparative for Healthy state
ggplot(data.frame(States_DES)) + geom_line(aes(x = V5, y = State_H, color="DES Model")) +
  geom_line(data = data.frame(States_micro), aes(x = V5, y = State_H, color="Microsimulation model" )) +
  labs(x="Cycle", y="Proportion in Healthy state", color = "Legend") +
  scale_color_manual(values = colors)

#Comparative for Sick state
ggplot(data.frame(States_DES)) + geom_line(aes(x = V5, y = State_S1, color="DES Model")) +
  geom_line(data = data.frame(States_micro), aes(x = V5, y = State_S1, color="Microsimulation model" )) +
  labs(x="Cycle", y="Proportion in Sick state", color = "Legend") +
  scale_color_manual(values = colors)

#Comparative for Sick state
ggplot(data.frame(States_DES)) + geom_line(aes(x = V5, y = State_S2, color="DES Model")) +
  geom_line(data = data.frame(States_micro), aes(x = V5, y = State_S2, color="Microsimulation model" )) +
  labs(x="Cycle", y="Proportion in Sicker state", color = "Legend") +
  scale_color_manual(values = colors)

#=======




