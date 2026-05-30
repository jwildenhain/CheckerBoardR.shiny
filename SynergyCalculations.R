#
# SynergyCalculations.R
# Dedicated, mathematically rigorous computation engine for drug synergy calculations.
# Implements HSA, Bliss, Loewe, and ZIP models with robust curve fitting and fallbacks.
#

extract_concentration <- function(label) {
  # Remove non-numeric characters except decimals, negative signs, and numbers
  val <- as.numeric(gsub("[^0-9.-]", "", label))
  # Replace NA with 0 if it was a text label like 'control' or empty
  val[is.na(val)] <- 0
  return(val)
}

normalize_data <- function(xx, data_type = "viability", control_row = 1, control_col = 1) {
  # Perform robust data normalization
  #
  # Args:
  #   xx: raw data matrix / data frame
  #   data_type: "viability" (or OD) or "inhibition"
  #   control_row: 1-indexed row coordinate of the control well
  #   control_col: 1-indexed column coordinate of the control well
  #
  # Returns:
  #   A list containing:
  #     1) normalized viability matrix [0, 1]
  #     2) normalized inhibition matrix [0, 1]
  
  z <- as.matrix(xx)
  
  # Ensure all values are numeric
  class(z) <- "numeric"
  
  if (data_type == "viability") {
    # Divide by the control well value to get viability fraction
    ctrl_val <- z[control_row, control_col]
    if (is.na(ctrl_val) || ctrl_val <= 0) {
      ctrl_val <- max(z, na.rm = TRUE) # fallback
    }
    viability <- z / ctrl_val
    # Cap viability at [0, 1.2] to handle slight experimental noise
    viability[viability < 0] <- 0
    viability[viability > 1.2] <- 1.2
    
    inhibition <- 1 - viability
    inhibition[inhibition < -0.2] <- -0.2
    inhibition[inhibition > 1.0] <- 1.0
  } else {
    # Input is already inhibition (e.g. cell death percentage or fraction)
    # Check if values are in percent (max > 1.5)
    max_val <- max(z, na.rm = TRUE)
    if (max_val > 1.5) {
      inhibition <- z / 100
    } else {
      inhibition <- z
    }
    
    inhibition[inhibition < -0.2] <- -0.2
    inhibition[inhibition > 1.0] <- 1.0
    
    viability <- 1 - inhibition
    viability[viability < 0] <- 0
    viability[viability > 1.2] <- 1.2
  }
  
  return(list(viability = viability, inhibition = inhibition))
}

# 4-Parameter Logistic (4PL) Hill curve fitting functions
fit_4pl <- function(conc, resp) {
  # Sort inputs by concentration
  ord <- order(conc)
  conc <- conc[ord]
  resp <- resp[ord]
  
  non_zero <- conc > 0
  if (sum(non_zero) < 2) {
    return(NULL) # Too few non-zero points to fit a curve
  }
  
  # Loss function (Sum of Squared Residuals)
  loss <- function(par) {
    Emin <- par[1]
    Emax <- par[2]
    EC50 <- par[3]
    Hill <- par[4]
    
    if (EC50 <= 0) return(Inf)
    
    # Predict response
    pred <- Emin + (Emax - Emin) / (1 + (conc / EC50)^(-Hill))
    sum((resp - pred)^2, na.rm = TRUE)
  }
  
  # Sensible starting parameters
  init_par <- c(
    Emin = min(resp, na.rm = TRUE),
    Emax = max(resp, na.rm = TRUE),
    EC50 = median(conc[non_zero], na.rm = TRUE),
    Hill = 1.0
  )
  
  fit <- tryCatch({
    optim(init_par, loss, method = "Nelder-Mead", control = list(maxit = 2000))
  }, error = function(e) NULL)
  
  if (is.null(fit) || fit$convergence != 0) {
    return(NULL)
  }
  
  return(fit$par)
}

predict_4pl <- function(conc, par) {
  Emin <- par[1]
  Emax <- par[2]
  EC50 <- par[3]
  Hill <- par[4]
  Emin + (Emax - Emin) / (1 + (conc / EC50)^(-Hill))
}

inverse_4pl <- function(y, par) {
  Emin <- par[1]
  Emax <- par[2]
  EC50 <- par[3]
  Hill <- par[4]
  
  # Avoid division by zero and log/power issues
  diff_y_emin <- y - Emin
  diff_emax_emin <- Emax - Emin
  
  if (abs(diff_y_emin) < 1e-6) diff_y_emin <- 1e-6 * sign(diff_y_emin)
  if (abs(diff_emax_emin) < 1e-6) diff_emax_emin <- 1e-6 * sign(diff_emax_emin)
  
  ratio <- diff_emax_emin / diff_y_emin - 1
  ratio[ratio <= 0] <- 1e-10
  
  d <- EC50 * ratio^(-1 / Hill)
  # Handle potential NaNs or infinite values
  d[is.nan(d) | is.infinite(d)] <- 0
  return(d)
}

# General single-agent modeling function with linear fallback
get_single_agent_curve <- function(conc, resp, use_fit = TRUE) {
  ord <- order(conc)
  conc <- conc[ord]
  resp <- resp[ord]
  
  par <- NULL
  if (use_fit) {
    par <- fit_4pl(conc, resp)
  }
  
  predict_fn <- function(x) {
    if (!is.null(par)) {
      predict_4pl(x, par)
    } else {
      # Monotonic linear interpolation fallback
      approx(x = conc, y = resp, xout = x, rule = 2)$y
    }
  }
  
  inverse_fn <- function(y) {
    if (!is.null(par)) {
      inverse_4pl(y, par)
    } else {
      # Monotonic linear interpolation fallback
      approx(x = resp, y = conc, xout = y, rule = 2)$y
    }
  }
  
  list(predict = predict_fn, inverse = inverse_fn, par = par)
}

# Complete Synergy Calculator Function
calculate_synergy <- function(xx, data_type = "viability", use_fit = TRUE, control_row = 1, control_col = 1) {
  # Normalize data matrix to [0, 1] viability and inhibition
  norm <- normalize_data(xx, data_type, control_row, control_col)
  I_matrix <- norm$inhibition
  V_matrix <- norm$viability
  
  # Parse concentrations from row/column labels
  conc_A <- extract_concentration(colnames(xx))
  conc_B <- extract_concentration(rownames(xx))
  
  nr <- nrow(xx)
  nc <- ncol(xx)
  
  # Single-agent responses (Drug A on columns, Drug B on rows)
  # In a standard checkerboard, row 1 is Drug B = 0, column 1 is Drug A = 0
  resp_A <- I_matrix[1, ]
  resp_B <- I_matrix[, 1]
  
  # Fit single-agent curves
  model_A <- get_single_agent_curve(conc_A, resp_A, use_fit)
  model_B <- get_single_agent_curve(conc_B, resp_B, use_fit)
  
  # Initialize output matrices
  hsa_scores <- matrix(0, nr, nc, dimnames = dimnames(xx))
  bliss_scores <- matrix(0, nr, nc, dimnames = dimnames(xx))
  loewe_scores <- matrix(0, nr, nc, dimnames = dimnames(xx))
  zip_scores <- matrix(0, nr, nc, dimnames = dimnames(xx))
  
  hsa_expected <- matrix(0, nr, nc, dimnames = dimnames(xx))
  bliss_expected <- matrix(0, nr, nc, dimnames = dimnames(xx))
  loewe_expected <- matrix(0, nr, nc, dimnames = dimnames(xx))
  zip_expected <- matrix(0, nr, nc, dimnames = dimnames(xx))
  
  for (i in 1:nc) {      # Drug A (Columns)
    for (j in 1:nr) {    # Drug B (Rows)
      d_A <- conc_A[i]
      d_B <- conc_B[j]
      obs <- I_matrix[j, i]
      
      # Raw single-agent effects
      Ia <- resp_A[i]
      Ib <- resp_B[j]
      
      # 1. HSA Model
      E_hsa <- max(Ia, Ib)
      hsa_expected[j, i] <- E_hsa
      hsa_scores[j, i] <- if (i == 1 || j == 1) 0 else obs - E_hsa
      
      # 2. Bliss Model
      E_bliss <- Ia + Ib - (Ia * Ib)
      bliss_expected[j, i] <- E_bliss
      bliss_scores[j, i] <- if (i == 1 || j == 1) 0 else obs - E_bliss
      
      # 3. ZIP Model (using fitted values)
      Ia_fit <- model_A$predict(d_A)
      Ib_fit <- model_B$predict(d_B)
      E_zip <- Ia_fit + Ib_fit - (Ia_fit * Ib_fit)
      zip_expected[j, i] <- E_zip
      zip_scores[j, i] <- if (i == 1 || j == 1) 0 else obs - E_zip
      
      # 4. Loewe Model (using Combination Index)
      if (i == 1 && j == 1) {
        loewe_scores[j, i] <- 0
        loewe_expected[j, i] <- 0
      } else if (i == 1) {
        loewe_scores[j, i] <- 0
        loewe_expected[j, i] <- Ib
      } else if (j == 1) {
        loewe_scores[j, i] <- 0
        loewe_expected[j, i] <- Ia
      } else {
        # Determine the single-agent doses required to achieve the observed effect 'obs'
        D_A <- model_A$inverse(obs)
        D_B <- model_B$inverse(obs)
        
        # Avoid division by zero if single agent cannot achieve 'obs'
        if (D_A <= 0) D_A <- 1e10
        if (D_B <= 0) D_B <- 1e10
        
        ci <- d_A / D_A + d_B / D_B
        
        # Loewe Score is defined as 1 - CI, representing synergy/antagonism
        loewe_scores[j, i] <- 1 - ci
        
        # Determine expected effect (where CI = 1)
        # Root finding for expected effect under Loewe additivity
        loewe_root_fn <- function(y) {
          da_Da <- d_A / model_A$inverse(y)
          db_Db <- d_B / model_B$inverse(y)
          # Treat boundary and divide-by-zero safely
          da_Da[is.na(da_Da) | is.infinite(da_Da)] <- 1e10
          db_Db[is.na(db_Db) | is.infinite(db_Db)] <- 1e10
          da_Da + db_Db - 1
        }
        
        expected_y <- tryCatch({
          uniroot(loewe_root_fn, interval = c(-0.2, 1.0), extendInt = "yes")$root
        }, error = function(e) {
          # Fallback expectation to average single-agent curves
          0.5 * (Ia + Ib)
        })
        loewe_expected[j, i] <- expected_y
      }
    }
  }
  
  # Format scores to standard scale (e.g. 0 to 100 for easy interpretation in heatmaps,
  # or keep as fractional. Let's keep as fractional but round for clarity)
  list(
    raw_inhibition = round(I_matrix, 3),
    raw_viability = round(V_matrix, 3),
    conc_A = conc_A,
    conc_B = conc_B,
    single_fit_A = model_A$par,
    single_fit_B = model_B$par,
    
    HSA = list(scores = round(hsa_scores, 3), expected = round(hsa_expected, 3)),
    Bliss = list(scores = round(bliss_scores, 3), expected = round(bliss_expected, 3)),
    Loewe = list(scores = round(loewe_scores, 3), expected = round(loewe_expected, 3)),
    ZIP = list(scores = round(zip_scores, 3), expected = round(zip_expected, 3))
  )
}
