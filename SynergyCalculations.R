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

# Restore readable concentration labels after R has made numeric-leading names
# syntactically valid (for example, X0uM or X.25uM).
format_concentration_labels <- function(labels) {
  if (is.null(labels)) return(labels)
  formatted <- trimws(as.character(labels))
  formatted <- sub("^X(?=(?:[0-9]|\\.[0-9]))", "", formatted, perl = TRUE)
  formatted <- sub("^\\.([0-9])", "0.\\1", formatted, perl = TRUE)
  formatted <- sub("^-\\.([0-9])", "-0.\\1", formatted, perl = TRUE)
  formatted
}

# Convert a filename such as paclitaxel_carboplatin.json into readable
# condition names. Extra underscore-separated suffixes (for example replicate
# identifiers) are ignored after the first two non-empty tokens.
condition_names_from_filename <- function(filename, default_a = "Drug A", default_b = "Drug B") {
  if (is.null(filename) || length(filename) == 0 || !nzchar(filename[[1]])) {
    return(list(a = default_a, b = default_b, derived = FALSE))
  }
  stem <- tools::file_path_sans_ext(basename(as.character(filename[[1]])))
  tokens <- Filter(nzchar, strsplit(stem, "_", fixed = TRUE)[[1]])
  if (length(tokens) < 2) return(list(a = default_a, b = default_b, derived = FALSE))

  humanize <- function(token) {
    cleaned <- trimws(gsub("[-.]+", " ", token))
    if (identical(cleaned, tolower(cleaned))) {
      tools::toTitleCase(cleaned)
    } else {
      paste0(toupper(substr(cleaned, 1, 1)), substr(cleaned, 2, nchar(cleaned)))
    }
  }
  list(a = humanize(tokens[[1]]), b = humanize(tokens[[2]]), derived = TRUE)
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

correct_inhibition_baseline <- function(inhibition, conc_A, conc_B, zero_row, zero_col,
                                        method = c("none", "negative", "all"), use_fit = TRUE) {
  method <- match.arg(method)
  adjusted <- inhibition
  response_A <- inhibition[zero_row, ]
  response_B <- inhibition[, zero_col]
  curve_A <- get_single_agent_curve(conc_A, response_A, use_fit)
  curve_B <- get_single_agent_curve(conc_B, response_B, use_fit)
  fitted_baseline <- min(c(curve_A$predict(conc_A), curve_B$predict(conc_B)), na.rm = TRUE)

  if (!is.finite(fitted_baseline)) fitted_baseline <- 0
  applied <- method != "none" && abs(fitted_baseline) > sqrt(.Machine$double.eps)
  if (applied) {
    if (method == "negative") {
      selected <- adjusted < 0
      adjusted[selected] <- adjusted[selected] - (1 - adjusted[selected]) * fitted_baseline
    } else {
      adjusted <- adjusted - (1 - adjusted) * fitted_baseline
    }
    adjusted[] <- pmin(1, pmax(-0.2, adjusted))
  }

  list(
    inhibition = adjusted,
    method = method,
    fitted_baseline = fitted_baseline,
    applied = applied
  )
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
calculate_synergy <- function(xx, data_type = "viability", use_fit = TRUE, control_row = 1,
                              control_col = 1, baseline_method = "none") {
  # Normalize data matrix to [0, 1] viability and inhibition
  norm <- normalize_data(xx, data_type, control_row, control_col)
  original_I_matrix <- norm$inhibition
  I_matrix <- original_I_matrix
  V_matrix <- norm$viability
  
  # Parse concentrations from labels; unlabeled matrices use ordinal doses from zero.
  x_names <- colnames(xx); y_names <- rownames(xx)
  if (is.null(x_names) || identical(x_names, paste0("V", seq_len(ncol(xx))))) x_names <- as.character(seq(0, length.out = ncol(xx)))
  if (is.null(y_names) || identical(y_names, as.character(seq_len(nrow(xx))))) y_names <- as.character(seq(0, length.out = nrow(xx)))
  x_names <- format_concentration_labels(x_names)
  y_names <- format_concentration_labels(y_names)
  colnames(original_I_matrix) <- colnames(I_matrix) <- colnames(V_matrix) <- x_names
  rownames(original_I_matrix) <- rownames(I_matrix) <- rownames(V_matrix) <- y_names
  conc_A <- extract_concentration(x_names)
  conc_B <- extract_concentration(y_names)
  zero_A <- which(abs(conc_A) < sqrt(.Machine$double.eps))
  zero_B <- which(abs(conc_B) < sqrt(.Machine$double.eps))
  if (length(zero_A) != 1 || length(zero_B) != 1)
    stop("Exactly one zero-concentration row and column are required.")
  zero_col <- zero_A[[1]]
  zero_row <- zero_B[[1]]

  baseline <- correct_inhibition_baseline(
    I_matrix, conc_A, conc_B, zero_row, zero_col,
    method = baseline_method, use_fit = use_fit
  )
  I_matrix <- baseline$inhibition
  
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
  hsa_scores <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  bliss_scores <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  loewe_scores <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  zip_scores <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  consensus_scores <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  
  hsa_expected <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  bliss_expected <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  loewe_expected <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  zip_expected <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  zip_fitted <- matrix(0, nr, nc, dimnames = dimnames(I_matrix))
  
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
      zip_fitted[j, i] <- combination_fit
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

  consensus_expected <- pmax(hsa_expected, bliss_expected, loewe_expected)
  consensus_scores <- I_matrix - consensus_expected
  consensus_scores[zero_row, ] <- 0
  consensus_scores[, zero_col] <- 0
  
  # Keep full precision in the engine. Formatting and rounding belong in the UI/export layer.
  list(
    raw_inhibition = I_matrix,
    adjusted_inhibition = I_matrix,
    original_inhibition = original_I_matrix,
    raw_viability = V_matrix,
    conc_A = conc_A,
    conc_B = conc_B,
    zero_row = zero_row,
    zero_col = zero_col,
    single_fit_A = model_A$par,
    single_fit_B = model_B$par,
    baseline_method = baseline$method,
    baseline_value = baseline$fitted_baseline,
    baseline_applied = baseline$applied,
    replicate_count = 1L,
    bootstrap_iterations = 0L,
    score_scale = "fraction",
    
    HSA = list(scores = hsa_scores, expected = hsa_expected),
    Bliss = list(scores = bliss_scores, expected = bliss_expected),
    Loewe = list(scores = loewe_scores, expected = loewe_expected),
    ZIP = list(scores = zip_scores, expected = zip_expected, fitted = zip_fitted),
    Consensus = list(scores = consensus_scores, expected = consensus_expected),
    replicate_statistics = NULL
  )
}

validate_replicate_matrices <- function(matrices) {
  if (!is.list(matrices) || length(matrices) < 2) {
    stop("At least two replicate matrices are required for uncertainty analysis.")
  }
  matrices <- lapply(matrices, function(x) as.matrix(x))
  reference_dim <- dim(matrices[[1]])
  reference_names <- dimnames(matrices[[1]])
  for (i in seq_along(matrices)) {
    if (!identical(dim(matrices[[i]]), reference_dim)) {
      stop("Replicate matrices must have identical dimensions.")
    }
    if (!identical(dimnames(matrices[[i]]), reference_names)) {
      stop("Replicate matrices must have identical row and column concentration labels.")
    }
    values <- suppressWarnings(as.numeric(matrices[[i]]))
    if (length(values) != length(matrices[[i]]) || any(!is.finite(values))) {
      stop("Replicate matrices must contain only finite numeric values.")
    }
    matrices[[i]] <- matrix(values, nrow = nrow(matrices[[i]]),
                            ncol = ncol(matrices[[i]]), dimnames = dimnames(matrices[[i]]))
  }
  matrices
}

extract_result_matrices <- function(result) {
  list(
    Data = list(observed = result$adjusted_inhibition),
    HSA = list(scores = result$HSA$scores, expected = result$HSA$expected),
    Bliss = list(scores = result$Bliss$scores, expected = result$Bliss$expected),
    Loewe = list(scores = result$Loewe$scores, expected = result$Loewe$expected),
    ZIP = list(scores = result$ZIP$scores, expected = result$ZIP$expected,
               fitted = result$ZIP$fitted),
    Consensus = list(scores = result$Consensus$scores,
                     expected = result$Consensus$expected)
  )
}

summarize_replicate_metric <- function(matrices, bootstrap_indices) {
  replicate_array <- simplify2array(matrices)
  replicate_count <- length(matrices)
  metric_mean <- apply(replicate_array, c(1, 2), mean)
  metric_sd <- apply(replicate_array, c(1, 2), stats::sd)
  metric_sem <- metric_sd / sqrt(replicate_count)

  bootstrap_array <- array(NA_real_, dim = c(dim(matrices[[1]]), nrow(bootstrap_indices)))
  for (i in seq_len(nrow(bootstrap_indices))) {
    selected <- matrices[bootstrap_indices[i, ]]
    bootstrap_array[, , i] <- Reduce(`+`, selected) / replicate_count
  }
  ci_lower <- apply(bootstrap_array, c(1, 2), stats::quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(bootstrap_array, c(1, 2), stats::quantile, probs = 0.975, na.rm = TRUE)

  for (mat in list(metric_mean, metric_sd, metric_sem, ci_lower, ci_upper)) {
    dimnames(mat) <- dimnames(matrices[[1]])
  }
  dimnames(metric_mean) <- dimnames(metric_sd) <- dimnames(metric_sem) <-
    dimnames(ci_lower) <- dimnames(ci_upper) <- dimnames(matrices[[1]])

  list(mean = metric_mean, sd = metric_sd, sem = metric_sem,
       ci_lower = ci_lower, ci_upper = ci_upper)
}

calculate_replicate_synergy <- function(matrices, data_type = "viability", use_fit = TRUE,
                                        control_row = 1, control_col = 1,
                                        baseline_method = "none", iterations = 100,
                                        seed = 123) {
  matrices <- validate_replicate_matrices(matrices)
  iterations <- as.integer(iterations)
  if (!is.finite(iterations) || iterations < 20) {
    stop("Bootstrap iterations must be at least 20.")
  }

  replicate_results <- lapply(matrices, function(mat) {
    calculate_synergy(
      mat, data_type = data_type, use_fit = use_fit,
      control_row = control_row, control_col = control_col,
      baseline_method = baseline_method
    )
  })
  adjusted_matrices <- lapply(replicate_results, `[[`, "adjusted_inhibition")
  original_matrices <- lapply(replicate_results, `[[`, "original_inhibition")
  mean_adjusted <- Reduce(`+`, adjusted_matrices) / length(adjusted_matrices)

  result <- calculate_synergy(
    mean_adjusted, data_type = "inhibition", use_fit = use_fit,
    control_row = control_row, control_col = control_col,
    baseline_method = "none"
  )
  result$original_inhibition <- Reduce(`+`, original_matrices) / length(original_matrices)
  result$baseline_method <- baseline_method
  result$baseline_value <- mean(vapply(replicate_results, `[[`, numeric(1), "baseline_value"))
  result$baseline_applied <- any(vapply(replicate_results, `[[`, logical(1), "baseline_applied"))
  result$replicate_count <- length(matrices)
  result$bootstrap_iterations <- iterations

  set.seed(seed)
  bootstrap_indices <- matrix(
    sample(seq_along(matrices), iterations * length(matrices), replace = TRUE),
    nrow = iterations, ncol = length(matrices)
  )
  extracted <- lapply(replicate_results, extract_result_matrices)
  statistics <- list()
  for (model in names(extracted[[1]])) {
    statistics[[model]] <- list()
    for (metric in names(extracted[[1]][[model]])) {
      metric_matrices <- lapply(extracted, function(x) x[[model]][[metric]])
      statistics[[model]][[metric]] <- summarize_replicate_metric(
        metric_matrices, bootstrap_indices
      )
    }
  }
  # Keep the displayed/exported centre aligned with the uncertainty estimate:
  # every model matrix is the mean of independently calculated replicates.
  result$adjusted_inhibition <- result$raw_inhibition <- statistics$Data$observed$mean
  for (model in setdiff(names(statistics), "Data")) {
    for (metric in names(statistics[[model]])) {
      result[[model]][[metric]] <- statistics[[model]][[metric]]$mean
    }
  }
  result$replicate_statistics <- statistics
  result
}

# Shared matrix-selection contract for plots, summaries, and downloads.
select_analysis_matrix <- function(result, model = "Bliss", value_type = "score") {
  model <- match.arg(model, c("Data", "HSA", "Bliss", "Loewe", "ZIP", "Consensus"))
  allowed <- if (model == "Data") {
    c("observed", "original")
  } else if (model == "ZIP") {
    c("score", "reference", "fitted", "observed")
  } else {
    c("score", "reference", "observed")
  }
  # Shiny updates model and matrix-type inputs in separate messages. During
  # that brief transition, use the model's primary view instead of throwing.
  if (length(value_type) != 1 || !value_type %in% allowed) value_type <- allowed[[1]]

  if (value_type == "observed") {
    return(list(matrix = result$adjusted_inhibition, title = "Adjusted Inhibition",
                divergent = FALSE, statistics = result$replicate_statistics$Data$observed))
  }
  if (value_type == "original") {
    return(list(matrix = result$original_inhibition, title = "Original Inhibition",
                divergent = FALSE, statistics = NULL))
  }

  metric <- switch(value_type, score = "scores", reference = "expected", fitted = "fitted")
  label <- switch(value_type, score = "Score", reference = "Reference Effect", fitted = "Fitted Response")
  list(
    matrix = result[[model]][[metric]],
    title = paste(model, label),
    divergent = value_type == "score",
    statistics = result$replicate_statistics[[model]][[metric]]
  )
}

# Tidy export: one row per dose pair, with observed, reference, fitted, score,
# and replicate uncertainty columns kept together for auditability.
build_matrix_export <- function(result) {
  nr <- nrow(result$adjusted_inhibition)
  nc <- ncol(result$adjusted_inhibition)
  grid <- expand.grid(row_index = seq_len(nr), column_index = seq_len(nc))
  at_cells <- function(mat) mat[cbind(grid$row_index, grid$column_index)]

  exported <- data.frame(
    condition_a = if (!is.null(result$condition_A)) result$condition_A else "Drug A",
    condition_b = if (!is.null(result$condition_B)) result$condition_B else "Drug B",
    drug_a_concentration = colnames(result$adjusted_inhibition)[grid$column_index],
    drug_b_concentration = rownames(result$adjusted_inhibition)[grid$row_index],
    original_inhibition = at_cells(result$original_inhibition),
    adjusted_inhibition = at_cells(result$adjusted_inhibition),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  paths <- list(
    HSA_reference = c("HSA", "expected"), HSA_score = c("HSA", "scores"),
    Bliss_reference = c("Bliss", "expected"), Bliss_score = c("Bliss", "scores"),
    Loewe_reference = c("Loewe", "expected"), Loewe_score = c("Loewe", "scores"),
    ZIP_reference = c("ZIP", "expected"), ZIP_fitted = c("ZIP", "fitted"),
    ZIP_score = c("ZIP", "scores"),
    Consensus_reference = c("Consensus", "expected"),
    Consensus_score = c("Consensus", "scores")
  )
  for (column_name in names(paths)) {
    path <- paths[[column_name]]
    exported[[column_name]] <- at_cells(result[[path[[1]]]][[path[[2]]]])
  }

  if (!is.null(result$replicate_statistics)) {
    stat_names <- c("sd", "sem", "ci_lower", "ci_upper")
    for (column_name in names(paths)) {
      path <- paths[[column_name]]
      stats <- result$replicate_statistics[[path[[1]]]][[path[[2]]]]
      for (stat_name in stat_names) {
        exported[[paste0(column_name, "_", stat_name)]] <- at_cells(stats[[stat_name]])
      }
    }
    observed_stats <- result$replicate_statistics$Data$observed
    for (stat_name in stat_names) {
      exported[[paste0("adjusted_inhibition_", stat_name)]] <- at_cells(observed_stats[[stat_name]])
    }
  }
  exported
}
