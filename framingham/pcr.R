# Betas from Ref 2. Table A.
beta_f_w <- c(-29.799, 4.884, 13.540, -3.114, -13.578, 3.149,  2.019,  0,      1.957,  0,     7.574, -1.665, 0.661)
beta_f_a <- c( 17.114, 0,      0.940,  0,     -18.920, 4.475, 29.291, -6.432, 27.820, -6.087, 0.691,  0,     0.874)
beta_m_w <- c( 12.344, 0,     11.853, -2.664,  -7.990, 1.769, 1.797,   0,      1.764,  0,     7.837, -1.795, 0.658)
beta_m_a <- c( 2.469,  0,      0.302,  0,      -0.307, 0,     1.916,   0,      1.809,  0,     0.549,  0,     0.645)


  ############################################################################
 ##
#' The Framingham model has been replaced by the 'Pooled Cohort Risk Model'
#' in clinical practice. 
#'
#' This code implements in R the Pooled Cohort Equations to predict
#' 10-year risk for first hard ASCVD (atherosclerotic cardiovascular disease)
#' event.
#'
#' References:
#' 1. https://clincalc.com/cardiology/ascvd/pooledcohort.aspx
#' 2. 2013 ACC/AHA Guideline on the Assessment of Cardiovascular Risk.
#'    doi: 10.1161/01.cir.0000437741.48606.98.
#' 3. 2013 ACC/AHA Guideline on the Treatment of Blood Cholesterol to Reduce
#'    Atherosclerotic Cardiovascular Risk in Adults.
#'    doi: 10.1161/01.cir.0000437738.63853.7a.
#' 4. Expert Panel on Detection, Evaluation, and Treatment of High Blood
#'    Cholesterol in Adults. Executive Summary of The Third Report of The
#'    National Cholesterol Education Program (NCEP) Expert Panel on Detection,
#'    Evaluation, And Treatment of High Blood Cholesterol In Adults (Adult
#'    Treatment Panel III). JAMA. 2001 May 16;285(19):2486-97. PMID 11368702.
#' 5. Lloyd-Jones DM, Leip EP, Larson MG, et al. Prediction of lifetime risk
#'    for cardiovascular disease by risk factor burden at 50 years of age.
#'    Circulation. 2006 Feb 14;113(6):791-8. PMID 16461820.
#'
#' @param gender       Character; "M" or "F"
#' @param age          Numeric; Age in years [40, 79]
#' @param race         Character; "White", "African American", "Hispanic", "Other"
#' @param tot_chol     Total Cholesterol, mg/dL [30, 500]
#' @param hdl_chol     HDL Cholesterol, mg/dL [5, 200]
#' @param systolic_bp  Systolic BP, mm Hg [60, 200]
#' @param bp_treatment Boolean; TRUE is subject is receiving BP treatment
#' @param smoker       Boolean; TRUE if subject is a smoker
#' @param diabetic     Boolean; TRUE if subject is diabetic
#' @param prs_z        Numeric; Polygenic risk score Z-stat [-5, 5]
#' 
#' @return Vector of probabilities for 10 year ASCVD event
#' @export
#' @examples 
#'  pcr("M", 45, "Hispanic", 190, 30, 130, FALSE, TRUE, FALSE, 1)
#'  pcr("F", 55, "European", 213, 50, 120, FALSE, FALSE, FALSE, 1)
#'  pcr("F", 55, "African American", 213, 50, 120, FALSE, FALSE, FALSE, 1)
#'  pcr("M", 55, "European", 213, 50, 120, FALSE, FALSE, FALSE, 1)
#'  pcr("M", 55, "African American", 213, 50, 120, FALSE, FALSE, FALSE, 1)
pcr <- Vectorize(function(gender, age, race, tot_chol, hdl_chol,
                          systolic_bp, bp_treatment, smoker, diabetic,
                          prs_z)
{
  if(missing(gender)      || any(!gender %in% c("M", "F")))
    stop("Need to specify gender as 'M' or 'F'")
  if(missing(age)         || any(age < 40 | age > 79))
    stop("Need to specify age in the range [40, 79]")
  if(missing(race)        || any(!race %in%  c("African American", "Hispanic", "European", "Other")))
    stop('Need to specify race as "African American", "Hispanic", "European", or "Other"')
  if(missing(tot_chol)    || any(tot_chol < 30 | tot_chol > 500))
    stop("Need to specify tot_chol in the range [30, 500]")
  if(missing(hdl_chol)    || any(hdl_chol < 5  | hdl_chol > 200))
    stop("Need to specify hdl_chol in the range [5, 200]")
  if(missing(systolic_bp) || any(systolic_bp < 60 | systolic_bp > 200))
    stop("Need to specify systolic_bp in the range [60,200]")
  if(missing(bp_treatment)|| !is.logical(bp_treatment))
    stop("Need to specify bp_treatment as a logical")
  if(missing(diabetic)    || !is.logical(diabetic))
    stop("Need to specify diabetic as logical")
  if(missing(smoker)      || !is.logical(smoker))
    stop("Need to specify smoker as logical")
  if(missing(prs_z)       || any(prs_z < -5 | prs_z > 5))
    stop("Polygenic risk score must be in the range [-5, 5]")
  
  # Compute individual values for X
  X <- c(log(age),
         log(age)^2,
         log(tot_chol),
         log(age)*log(tot_chol),
         log(hdl_chol),
         log(age)*log(hdl_chol),
         ifelse(bp_treatment, log(systolic_bp), 0), 
         log(age)*ifelse(bp_treatment, log(systolic_bp), 0),
         ifelse(!bp_treatment, log(systolic_bp), 0),
         log(age)*ifelse(!bp_treatment, log(systolic_bp), 0),
         smoker,
         log(age)*smoker,
         diabetic)

  beta <- if(gender == "F")
          {
            if(race == "African American") beta_f_a else beta_f_w
          } else { 
            if(race == "African American") beta_m_a else beta_m_w
          }
  
  expected <- ifelse(gender == "F",
                ifelse(race == "African American", 86.61, -29.18),
                ifelse(race == "African American", 19.54,  61.18)
              )
  
  baseline <- ifelse(gender == "F",
                ifelse(race == "African American", 0.9533, 0.9665),
                ifelse(race == "African American", 0.8954, 0.9144)
              )
  
  # FIXME
  # Hazard Ratios for polygenic risk scores. Request made for use
  # of values from Mayo Clinic research in private correspondence.
  # Values set to 1 until permission / attribution granted
  HR <- c(              1, 1, 1, 1)[match(race,
        c("African American", "Hispanic", "European", "Other"))]
    

  100*c(
    "Pooled Cohort Equation (10y)" = 1 - baseline^exp(sum(beta*X) - expected),
    "PCE+PRS (10y)"                = 1 - baseline^exp(sum(beta*X) - expected + prs_z*log(HR))
  )
},USE.NAMES=FALSE)