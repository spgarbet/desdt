#source("model.R")

# Next state vectors
v_when_H  <- c("S1","D")
v_when_S1 <- c("H","S2","D",'D') # what should [4] be?
v_when_S2 <- c("D")

opt_reshapeLong <- function (data, varying = NULL, v.names = NULL, timevar = "time", 
    idvar = "id", ids = 1L:NROW(data), times = seq_along(varying[[1L]]), drop = NULL, sep = "_") {

    if (!is.character(sep) || length(sep) != 1L) 
        stop("'sep' must be a character string")
    ix2names <- function(ix) if (is.character(ix)) 
        ix
    else names(data)[ix]
    guess <- function(nms, re = sep, drop = TRUE, fixed = TRUE) {
        if (drop) 
            nn <- do.call("rbind", strsplit(nms, re, fixed = fixed))
        else nn <- cbind(substr(nms, 1L, regexpr(re, nms)), substr(nms, 
            regexpr(re, nms) + 1L, 10000L))
        if (ncol(nn) != 2L) 
            stop("failed to guess time-varying variables from their names")
        vn <- unique(nn[, 1])
        v.names <- split(nms, factor(nn[, 1L], levels = vn))
        times <- unique(nn[, 2L])
        attr(v.names, "v.names") <- vn
        tt <- tryCatch(as.numeric(times), warning = function(w) times)
        attr(v.names, "times") <- tt
        v.names
    }
    reshapeLong <- function(data, varying, v.names = NULL, timevar, 
        idvar, ids = 1L:NROW(data), times, drop = NULL) {
        ll <- unlist(lapply(varying, length))
        if (any(ll != ll[1L])) 
            stop("'varying' arguments must be the same length")
        if (ll[1L] != length(times)) 
            stop("'lengths(varying)' must all match 'length(times)'")
        if (!is.null(drop)) {
            if (is.character(drop)) 
                drop <- names(data) %in% drop
            data <- data[, if (is.logical(drop)) 
                !drop
            else -drop, drop = FALSE]
        }
        undoInfo <- list(varying = varying, v.names = v.names, idvar = idvar, timevar = timevar)
        d <- data
        all.varying <- unlist(varying)
        d <- d[, !(names(data) %in% all.varying), drop = FALSE]
        if (is.null(v.names)) 
            v.names <- vapply(varying, `[`, 1L, FUN.VALUE = character(1L))
        lvals <- lapply(seq_along(times), function(i) {
            d[, timevar] <- times[i]
            varying.i <- vapply(varying, `[`, i, FUN.VALUE = character(1L))
            d[, v.names] <- data[, varying.i]
            d
        })
        rval <- do.call(rbind, c(lvals, make.row.names = FALSE))
        if (length(idvar) == 1L && !(idvar %in% names(data))) {
            rval[, idvar] <- ids
        }
        attr(rval, "reshapeLong") <- undoInfo
        return(rval)
    }
    if (is.atomic(varying)) {
        varying <- ix2names(varying)
        if (missing(v.names)) varying <- guess(varying) else {
            if (length(varying)%%length(v.names)) stop("length of 'v.names' does not evenly divide length of 'varying'")
            ntimes <- length(varying)%/%length(v.names)
            if (missing(times)) times <- seq_len(ntimes) else if (length(times) != 
              ntimes) stop("length of 'varying' must be the product of length of 'v.names' and length of 'times'")
            varying <- split(varying, rep(v.names, ntimes))
            attr(varying, "v.names") <- v.names
            attr(varying, "times") <- times
        }
    } else varying <- lapply(varying, ix2names)
    if (missing(v.names) && !is.null(attr(varying, "v.names"))) {
        v.names <- attr(varying, "v.names")
        times <- attr(varying, "times")
    }
    reshapeLong(data, idvar = idvar, timevar = timevar, varying = varying, v.names = v.names, drop = drop, times = times, ids = ids)
}

from_H <- function(t_base, t_HD) {
  nr <- length(t_base)
  if(nr == 0) return(NULL)
  t_HS1 <- ceiling(time_to_healthy_sick(nr, r_HS1)) + t_base
  nE <- v_when_H[1 + !(t_HS1 < t_HD)]
  ntE <- pmin(t_HS1, t_HD)
  data.frame(nE, ntE)
}

from_S1 <- function(t_base, t_HD) {
  nr <- length(t_base)
  if(nr == 0) return(NULL)
  t_S1D <- ceiling(time_to_sick_death(nr, r_S1D)) + t_base
  t_S1H <- ceiling(time_to_sick_healthy(nr, r_S1H)) + t_base
  t_S1S2 <- ceiling(time_to_sick_sicker(nr, r_S1S2)) + t_base
  nE <- v_when_S1[max.col(-cbind(t_S1H, t_S1S2, t_S1D, t_HD), 'last')]
  ntE <- pmin(t_S1H, t_S1S2, t_S1D, t_HD)
  data.frame(nE, ntE)
}

from_S2 <- function(t_base, t_HD) {
  nr <- length(t_base)
  if(nr == 0) return(NULL)
  t_S2D <- ceiling(time_to_sicker_death(nr, r_S2D)) + t_base
  nE <- 'D'
  ntE <- pmin(t_S2D, t_HD)
  data.frame(nE, ntE)
}

from_D <- function(t_base, t_HD) {
  nr <- length(t_base)
  if(nr == 0) return(NULL)
  data.frame(nE = 'D', ntE = t_base + 10)
}

draw_next_state <- function(dt_current, v_t_HD) {
  f <- factor(dt_current[,1], levels = c('H','S1','S2','D'))
  l_ix <- tapply(seq(nrow(dt_current)), f, I)
  l1 <- from_H(dt_current[l_ix$H,2], v_t_HD[l_ix$H])
  l2 <- from_S1(dt_current[l_ix$S1,2], v_t_HD[l_ix$S1])
  l3 <- from_S2(dt_current[l_ix$S2,2], v_t_HD[l_ix$S2])
  l4 <- from_D(dt_current[l_ix$D,2], v_t_HD[l_ix$D])
  do.call(rbind, list(l1, l2, l3, l4))[order(unlist(l_ix)),]
}

# this is "faster" until 100,000
simulation <- function(N) {
  dt_demog <- data.frame(
    id      = 1:N,
    sex     = sample(c(0,1), N, prob=c(0.5, 0.5), replace=TRUE),
    t_HD = ceiling(time_to_healthy_death(N, r_HD))
  )

  # Initial conditions
  min_lastStateTime <- 0
  i <- 1
  li <- vector('list', n_t + 1)
  li[[1]] <- data.frame(E_0 = rep('H', N), timeE_0 = 0)

  #While to complete in all the individuals the number of cycles required (30 cycles in this exercise)
  while (min_lastStateTime < n_t) {
    i <- i + 1
    dt_newE <- draw_next_state(dt_current = li[[i-1]], v_t_HD = dt_demog$t_HD)
    names(dt_newE) <- paste0(c('E_', 'timeE_'), i-1)
    min_lastStateTime <- min(dt_newE[, 2], na.rm = TRUE)
    li[[i]] <- dt_newE
  }
  cbind(dt_demog, do.call(cbind, li[lengths(li) > 0]))
}

summarize <- function(dt_demog) {
  N <- nrow(dt_demog)

  vnames <- grep('E_', names(dt_demog), value = TRUE)
  dt_demog_long <- opt_reshapeLong(dt_demog[, c('id', vnames)], varying = vnames, sep = '_', timevar = 'event')
#   dt_demog_long <- reshape(dt_demog[, c('id', vnames)], varying = vnames, sep = '_', direction = 'long', timevar = 'event', new.row.names = seq(newsize))
  names(dt_demog_long)[3] <- 'State'
  names(dt_demog_long)[4] <- 't_Event'
  # Keep only events under 30 (n_t) cycles
  dt_demog_long <- dt_demog_long[dt_demog_long['t_Event'] <= n_t,]
  dt_demog_long <- dt_demog_long[order(dt_demog_long[,'id'], dt_demog_long[,'event']),]

  # Difference in cycles between events
  delta <- c(diff(dt_demog_long[,'t_Event']), NA)
  lastix <- which(dt_demog_long[,'id'] != c(dt_demog_long[-1,'id'], -999))
  delta[lastix] <- n_t - dt_demog_long[lastix,'t_Event'] + 1
  state <- dt_demog_long[,'State']
  # Expansion of the States vector
  states <- rep(state, times = delta)
  rm(delta, state, dt_demog_long)

  #Matrix for Costs, and Effs
#   dim_names <- list(paste("ind", 1:N, sep = " "), paste("cycle", 0:n_t, sep = " "))
  num_states <- match(states, c('H','S1','S2','D'))
  rm(states)

  ##### ================= CACLULATE TOTAL COSTS AND QALYS   ================#####

  v_dw <- 1 / (1 + d_c) ^ (0:n_t) # calculate discount weight for each cycle based on discount rate d_r

  # costs accrued by each individual during cycle  t+1
  m_C_D <- matrix(c(c_H, c_S1, c_S2, c_D)[num_states], nrow = N, byrow = TRUE)
  tc_DES <- m_C_D %*% v_dw    # total discounted cost per individual
  rm(m_C_D)
  # QALYs accrued by each individual during cycle  t+1
  m_E_D <- matrix(c(u_H, u_S1, u_S2, u_D)[num_states], nrow = N, byrow = TRUE)
  te_DES <- m_E_D %*% v_dw    # total discounted QALYs per individual
  rm(m_E_D)

  tc_avg_DES <- mean(tc_DES)    # average discounted cost
  tc_se_DES  <- sd(tc_DES) / sqrt(N)
  te_avg_DES <- mean(te_DES)    # average discounted QALYs
  te_se_DES  <- sd(te_DES) / sqrt(N)
  list(cost=tc_avg_DES, cost_se=tc_se_DES, qaly=te_avg_DES, qaly_se=te_se_DES)
}
