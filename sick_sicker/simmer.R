env  <- simmer("Simple")

# Setup initial patient state
initialize_patient <- function(traj)
{
  traj %>%
  seize("time_in_model") %>%
  set_attribute("health", 1) # (1=Healthy, 2=Sick, 3=Sicker)
}

# Release resources as needed on termination or death
terminate <- function(traj)
{
  traj %>%
  release("time_in_model") %>%
  branch(
    function() get_attribute(env, "health"),
    continue=rep(FALSE,3),
    trajectory() %>% timeout(0),
    trajectory() %>% release("sick"),
    trajectory() %>% release("sicker")
  )
}

time_to_death <- function()
{
  state <- get_attribute(env,"health")
  if(state == 1) rexp(1, r_HD ) else
  if(state == 2) rexp(1, r_S1D) else
                 rexp(1, r_S2D)
}


# Manage Healthy state
time_to_healthy <- function()
{
  state <- get_attribute(env,"health")
  if(state == 2) rexp(1, r_S1H) else n_t+1
}


time_to_sick <- function()
{
  state <- get_attribute(env,"health")
  if(state == 1) rexp(1, r_HS1) else n_t+1
}


healthy <- function(traj)
{
  traj %>%
  set_attribute("health", 1) %>%
  release("sick") %>%
  set_attribute("tDead", function() time_to_death()+now(env)) %>%
  set_attribute("tSick", function() time_to_sick()+now(env)) %>%
  set_attribute("tSicker", function() n_t+1)
}

# Manage Sick state

sick <- function(traj)
{
  traj %>%
  seize("sick") %>%
  set_attribute("health", 2) %>%
  set_attribute("tHealthy", function() time_to_healthy()+now(env)) %>%
  set_attribute("tDead",    function() time_to_death()+now(env))   %>%
  set_attribute("tSicker",  function() time_to_sicker()+now(env))
}

# Manage Sicker State

time_to_sicker <- function()
{
  state <- get_attribute(env,"health")
  if(state == 2) rexp(1, r_S1S2) else n_t+1
}

sicker <- function(traj)
{
  traj %>%
  release("sick") %>%
  seize("sicker") %>%
  set_attribute("health", 3) %>%
  set_attribute("tDead", function() time_to_death()+now(env)) %>%
  set_attribute("tSick", n_t+1) %>%
  set_attribute("tHealthy", n_t+1)
}

###Event registry
event_registry <- list(
  list(name          = "Death",
       attr          = "tDead",
       time_to_event = time_to_death,
       func          = terminate),
  list(name          = "Healthy",
       attr          = "tHealthy",
       time_to_event = time_to_healthy,
       func          = healthy),
  list(name          = "Sick",
       attr          = "tSick",
       time_to_event = time_to_sick,
       func          = sick),
  list(name          = "Sicker",
       attr          = "tSicker",
       time_to_event = time_to_sicker,
       func          = sicker),
  list(name          = "Terminate Simulation Time Horizon",
       attr          = "tTerminate",
       time_to_event = function() n_t,
       func          = terminate)
)

counters <- c(
  "time_in_model",
  "sick",
  "sicker"
)

# Create the counters, takes a list or vector
create_counters <- function(env, counters)
{
  sapply(counters, FUN=function(counter)
  {
    env <- add_resource(env, counter, Inf, 0)
  })

  env
}


# Mark a counter
mark <- function(traj, counter)
{
  traj               %>%
  seize(counter,1)   %>%
  timeout(0)         %>%
  release(counter,1)
}

  ##############################################
 ##
## Helper functions for managing events
##
## Hopefully, no modification required.
##
assign_events <- function(traj)
{
  sapply(event_registry, FUN=function(event)
  {
    traj <- set_attribute(traj, event$attr, function()
    {
      event$time_to_event()
    })
  })
  traj
}

# Find the next event based on time
next_event <- function()
{
  event_time <- Inf
  event      <- NA
  id         <- 0
  for(i in 1:length(event_registry))
  {
    e <- event_registry[[i]]
    tmp_time   <- get_attribute(env,e$attr)
    if(tmp_time < event_time)
    {
      event      <- e
      event_time <- tmp_time
      id         <- i
    }
  }

  return(list(event=event, event_time=event_time, id=id))
}

# Process events in main loop
process_events <- function(traj, env)
{
  # Find the next event from possible events, and timeout (wait) till that moment
  traj <- timeout(traj, function()
  {
    # Determine next up
    ne <- next_event()
    event      <- ne[['event']]
    event_time <- ne[['event_time']]

    #cat(" Next up => ",event$name,"\n")
    #cat("            waiting", event_time-now(env), '\n')

    # Wait the clock time for the nearest event, minus now()
    event_time - now(env)
  })

  # Create a handler for every possible event, using their
  # list position as the branch number
  # This will determine the id of the next event
  # Call it's modification function
  # and then update it's next time to event
  args <- lapply(event_registry,FUN=function(e) {
    #print(e$name)   # Good for debugging event loading
    trajectory(e$name) %>%
      e$func() %>%
      set_attribute(e$attr, function() {now(env)+e$time_to_event()})
  })
  args$".trj"    <- traj
  args$option    <- function() next_event()$id
  args$continue  <- rep(TRUE,length(event_registry))

  do.call(branch, args)
}

  ##############################################
 ##
## MAIN LOOP
##
## This should not require modification
## This creates a patient simulation (trajectory)
##
## It uses a branch in a manner to prevent the
## rollback from looking further up the stack
## of the event loop.
patient.traj <- function(env)
{
  trajectory("Patient")      %>%
    initialize_patient()     %>%
    assign_events()          %>%
    branch( # Used branch, to prevent rollback from looking inside event loop function
      function() 1,
      continue=TRUE,
      trajectory("main_loop") %>% process_events(env)
    ) %>%
    rollback(amount=1, times=30) # Process up to 30 events per person
}

simulation <- function(N)
{
   env  <<- simmer("Simple")
   traj <- patient.traj(env)

   env %>%
   create_counters(counters) %>%
   add_generator("patient", traj, at(rep(0, N)), mon=2) %>%
   run(n_t+1) %>% # Simulate just past horizon
   wrap()

   get_mon_arrivals(env, per_resource = T)
}

discount_value = function(value, discount, t_A, t_B)
{
  r <- (1 + discount)^(1/365.25)-1
  (value/r)*(exp(-r*t_A)-exp(-r*t_B))
}

summarize <- function(result)
{
#  replicate(nrow(result), rnorm(100))
  result$dQALY <-
    discount_value(
      c(u_H, u_S1-u_H, u_S2-u_H)[match(result$resource, c('time_in_model','sick','sicker'))],
      d_e,
      result$start_time,
      result$end_time)
  result$dCOST <-
    discount_value(
      c(c_H, c_S1-c_H, c_S2-c_H)[match(result$resource, c('time_in_model','sick','sicker'))],
      d_e,
      result$start_time,
      result$end_time)

  # Aggregate per patient for final expected value
  x   <- aggregate(cbind(dQALY, dCOST) ~ name, result, sum)
  srN <- sqrt(length(x))

  list(cost    = mean(x$dCOST),
       cost_se = sd(x$dCOST)/srN,
       qaly    = mean(x$dQALY),
       qaly_se = sd(x$dQALY)/srN)
}


