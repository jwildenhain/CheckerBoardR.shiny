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
  
  z <- suppressWarnings(matrix(as.numeric(as.matrix(xx)), nrow = nrow(xx),
                               ncol = ncol(xx), dimnames = dimnames(xx)))
  if (!data_type %in% c("viability", "inhibition")) stop("Invalid data type.")
  if (!length(z) || any(!is.finite(z))) stop("Checkerboard values must all be finite and numeric.")
  if (control_row < 1 || control_row > nrow(z) || control_col < 1 || control_col > ncol(z))
    stop("Control well coordinates are outside the checkerboard.")
  
  if (data_type == "viability") {
    # Divide by the control well value to get viability fraction
    ctrl_val <- z[control_row, control_col]
    if (!is.finite(ctrl_val) || ctrl_val <= 0)
      stop("The selected viability control must be a positive finite value.")
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
  
  valid <- is.finite(conc) & is.finite(resp)
  conc <- conc[valid]; resp <- resp[valid]
  non_zero <- conc > 0
  if (sum(non_zero) < 3 || length(unique(conc[non_zero])) < 3) {
    return(NULL) # Too few non-zero points to fit a curve
  }
  
  # Loss function (Sum of Squared Residuals)
  loss <- function(par) {
    Emin <- par[1]
    Emax <- par[2]
    EC50 <- par[3]
    Hill <- par[4]
    
    # Predict response
    pred <- Emin + (Emax - Emin) / (1 + (conc / EC50)^(-Hill))
    if (any(!is.finite(pred))) return(.Machine$double.xmax)
    sum((resp - pred)^2)
  }
  
  # Sensible starting parameters
  init_par <- c(
    Emin = min(resp, na.rm = TRUE),
    Emax = max(resp, na.rm = TRUE),
    EC50 = median(conc[non_zero], na.rm = TRUE),
    Hill = 1.0
  )
  
  lower <- c(-0.2, 0, min(conc[non_zero]) / 100, 0.05)
  upper <- c(0.5, 1.2, max(conc[non_zero]) * 100, 10)
  init_par <- pmin(pmax(init_par, lower), upper)
  fit <- tryCatch({
    optim(init_par, loss, method = "L-BFGS-B", lower = lower, upper = upper, control = list(maxit = 2000))
  }, error = function(e) NULL)
  
  if (is.null(fit) || fit$convergence != 0) {
    return(NULL)
  }
  
  if (!all(is.finite(fit$par)) || fit$par[2] <= fit$par[1]) return(NULL)
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
  
  # Pre-filter out NAs and aggregate coordinates to avoid formulas in loops
  valid_idx <- !is.na(conc) & !is.na(resp)
  c_valid <- conc[valid_idx]
  r_valid <- resp[valid_idx]
  
  agg_pred_x <- numeric(0)
  agg_pred_y <- numeric(0)
  agg_inv_x <- numeric(0)
  agg_inv_y <- numeric(0)
  
  if (length(c_valid) >= 2) {
    # Predict aggregation: x = conc (c_valid), y = resp (r_valid)
    agg_pred <- aggregate(r_valid, list(c_valid), FUN = mean)
    agg_pred_x <- agg_pred$Group.1
    agg_pred_y <- pmin(1, pmax(-0.2, stats::isoreg(agg_pred_x, agg_pred$x)$yf))
    
    # Build the inverse from the same monotonic forward curve.
    agg_inv <- aggregate(agg_pred_x, list(agg_pred_y), FUN = mean)
    agg_inv_x <- agg_inv$Group.1
    agg_inv_y <- agg_inv$x
  }
  
  predict_fn <- function(x) {
    if (!is.null(par)) {
      predict_4pl(x, par)
    } else {
      # Monotonic linear interpolation fallback
      if (length(c_valid) >= 2 && length(agg_pred_x) >= 2) {
        approx(x = agg_pred_x, y = agg_pred_y, xout = x, rule = 2)$y
      } else if (length(c_valid) >= 1) {
        rep(r_valid[1], length(x))
      } else {
        rep(0, length(x))
      }
    }
  }
  
  inverse_fn <- function(y) {
    if (!is.null(par)) {
      inverse_4pl(y, par)
    } else {
      # Monotonic linear interpolation fallback
      if (length(c_valid) >= 2 && length(agg_inv_x) >= 2) {
        approx(x = agg_inv_x, y = agg_inv_y, xout = y, rule = 2)$y
      } else if (length(c_valid) >= 1) {
        rep(c_valid[1], length(y))
      } else {
        rep(0, length(y))
      }
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
  
  # Parse concentrations from labels; unlabeled matrices use ordinal doses from zero.
  x_names <- colnames(xx); y_names <- rownames(xx)
  if (is.null(x_names) || identical(x_names, paste0("V", seq_len(ncol(xx))))) x_names <- as.character(seq(0, length.out = ncol(xx)))
  if (is.null(y_names) || identical(y_names, as.character(seq_len(nrow(xx))))) y_names <- as.character(seq(0, length.out = nrow(xx)))
  colnames(I_matrix) <- colnames(V_matrix) <- x_names
  rownames(I_matrix) <- rownames(V_matrix) <- y_names
  conc_A <- extract_concentration(x_names)
  conc_B <- extract_concentration(y_names)
  zero_A <- which(abs(conc_A) < sqrt(.Machine$double.eps))
  zero_B <- which(abs(conc_B) < sqrt(.Machine$double.eps))
  if (length(zero_A) != 1 || length(zero_B) != 1)
    stop("Exactly one zero-concentration row and column are required.")
  zero_col <- zero_A[[1]]
  zero_row <- zero_B[[1]]
  
  nr <- nrow(xx)
  nc <- ncol(xx)
  
  # Single-agent responses (Drug A on columns, Drug B on rows)
  # In a standard checkerboard, row 1 is Drug B = 0, column 1 is Drug A = 0
  resp_A <- I_matrix[zero_row, ]
  resp_B <- I_matrix[, zero_col]
  
  # Fit single-agent curves
  model_A <- get_single_agent_curve(conc_A, resp_A, use_fit)
  model_B <- get_single_agent_curve(conc_B, resp_B, use_fit)
  row_models <- lapply(seq_len(nr), function(j) get_single_agent_curve(conc_A, I_matrix[j, ], use_fit))
  col_models <- lapply(seq_len(nc), function(i) get_single_agent_curve(conc_B, I_matrix[, i], use_fit))
  
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
      is_single_agent <- i == zero_col || j == zero_row
      hsa_scores[j, i] <- if (is_single_agent) 0 else obs - E_hsa
      
      # 2. Bliss Model
      E_bliss <- Ia + Ib - (Ia * Ib)
      bliss_expected[j, i] <- E_bliss
      bliss_scores[j, i] <- if (is_single_agent) 0 else obs - E_bliss
      
      # 3. ZIP delta from bidirectionally fitted combination responses
      Ia_fit <- model_A$predict(d_A)
      Ib_fit <- model_B$predict(d_B)
      E_zip <- Ia_fit + Ib_fit - (Ia_fit * Ib_fit)
      zip_expected[j, i] <- E_zip
      combination_fit <- mean(c(row_models[[j]]$predict(d_A), col_models[[i]]$predict(d_B)))
      zip_scores[j, i] <- if (is_single_agent) 0 else combination_fit - E_zip
      
      # 4. Loewe Model (using Combination Index)
      if (i == zero_col && j == zero_row) {
        loewe_scores[j, i] <- 0
        loewe_expected[j, i] <- 0
      } else if (i == zero_col) {
        loewe_scores[j, i] <- 0
        loewe_expected[j, i] <- Ib
      } else if (j == zero_row) {
        loewe_scores[j, i] <- 0
        loewe_expected[j, i] <- Ia
      } else {
        # Determine the single-agent doses required to achieve the observed effect 'obs'
        D_A <- model_A$inverse(obs)
        D_B <- model_B$inverse(obs)
        
        # Avoid division by zero if single agent cannot achieve 'obs'
        if (D_A <= 0) D_A <- 1e10
        if (D_B <= 0) D_B <- 1e10
        
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
        loewe_scores[j, i] <- obs - expected_y
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
    zero_row = zero_row,
    zero_col = zero_col,
    single_fit_A = model_A$par,
    single_fit_B = model_B$par,
    
    HSA = list(scores = round(hsa_scores, 3), expected = round(hsa_expected, 3)),
    Bliss = list(scores = round(bliss_scores, 3), expected = round(bliss_expected, 3)),
    Loewe = list(scores = round(loewe_scores, 3), expected = round(loewe_expected, 3)),
    ZIP = list(scores = round(zip_scores, 3), expected = round(zip_expected, 3))
  )
}
