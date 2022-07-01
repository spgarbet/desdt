  ##############################################################################
 ##
## All methods receive this file
##
## Model Parameters
params <- list(
  # Model input
#  n_i     <- 100000,              # number of simulated individuals
  n_t     =  30,                  # time horizon, 30 cycles
  v_n     =  c("H","S1","S2","D"),# the model states:
                                  #  Healthy (H),
                                  #   Sick (S1), Sicker (S2), Dead (D)
  d_c     =  0.03,                # Cost discount rate
  d_e     =  0.03,                # QALY discount rate
  v_Trt   =  c("No Treatment",    # strategy names
               "Treatment"),

  # Transition rates
  r_HS1   =  0.15,                # rate to become sick when healthy
  r_S1H   =  0.5,                 # rate to become healthy when sick

  r_S1S2  =  0.105,               # rate to become sicker when sick

  rr_S1   =  3,                   # rate ratio of death in sick vs healthy
  rr_S2   =  10,            	      # rate ratio of death in sicker vs healthy

  r_HD    =  0.005,               # rate of dying when healthy

  # Cost and utility inputs
  c_H     =  2000,                # cost of remaining one cycle healthy
  c_S1    =  4000,                # cost of remaining one cycle sick
  c_S2    =  15000,               # cost of remaining one cycle sicker
  c_Trt   =  12000,               # cost of treatment (per cycle)
  c_D     =  0,                   # cost of death

  u_H     =  1,                   # utility when healthy
  u_S1    =  0.75,                # utility when sick
  u_S2    =  0.5,                 # utility when sicker
  u_Trt   =  0.95,                # utility when being treated
  u_D     =  0
)
params$r_S1D <- with(params, rr_S1 * r_HD)  	    # rate of death in sick
params$r_S2D <- with(params, rr_S2 * r_HD)  	    # rate of death in sicker
  ##############################################################################
 ##
## Model Transition Functions
##
time_to_healthy_sick    <- rexp
time_to_healthy_death   <- rexp
time_to_sick_healthy    <- rexp
time_to_sick_sicker     <- rexp
time_to_sick_death      <- rexp
time_to_sicker_death    <- rexp

