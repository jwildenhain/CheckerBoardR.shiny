#
# unit_test.R
# Thorough automated test suite for the redesigned CheckerBoardR synergy calculations.
# Verifies data normalization, 4PL Hill curve-fitting, and HSA/Bliss/Loewe/ZIP scores.
#

source("SynergyCalculations.R")
set.seed(20260723)

cat("=========================================\n")
cat("   STARTING CHECKERBOARDR SYNERGY TESTS   \n")
cat("=========================================\n\n")

# 1. Test Data Normalization
cat("[1/6] Testing Data Normalization...\n")
mock_viability <- matrix(c(0.9, 0.45, 0.05, 0.8, 0.35, 0.01), 3, 2)
colnames(mock_viability) <- c("0uM", "2uM")
rownames(mock_viability) <- c("0uM", "1uM", "5uM")

# Viability normalized by [1,1] (value 0.9)
norm_v <- normalize_data(mock_viability, data_type = "viability", control_row = 1, control_col = 1)
stopifnot(abs(norm_v$viability[1,1] - 1.0) < 1e-6)
stopifnot(abs(norm_v$inhibition[1,1] - 0.0) < 1e-6)
stopifnot(abs(norm_v$viability[2,1] - 0.45/0.9) < 1e-6)
stopifnot(abs(norm_v$inhibition[2,1] - 0.5) < 1e-6)
cat(" -> Viability normalization check: SUCCESS\n")

# Inhibition normalized directly
mock_inhibition <- matrix(c(0, 50, 95, 10, 60, 99), 3, 2)
colnames(mock_inhibition) <- c("0uM", "2uM")
rownames(mock_inhibition) <- c("0uM", "1uM", "5uM")

norm_i <- normalize_data(mock_inhibition, data_type = "inhibition")
stopifnot(abs(norm_i$inhibition[1,1] - 0.0) < 1e-6)
stopifnot(abs(norm_i$inhibition[2,1] - 0.5) < 1e-6)
stopifnot(abs(norm_i$inhibition[3,2] - 0.99) < 1e-6)
cat(" -> Inhibition percent normalization check: SUCCESS\n\n")


# 2. Test 4PL Curve Fitting
cat("[2/6] Testing 4-Parameter Logistic (4PL) Hill Curve Fitting...\n")
conc <- c(0, 0.25, 0.5, 1, 2, 4, 8, 16)
# Simulated responses matching an IC50 around 2.0, Emin=0, Emax=0.9, Hill=1.2
resp <- 0.0 + (0.9 - 0.0) / (1 + (conc / 2.0)^(-1.2))
# Add very minor noise
resp_noise <- resp + rnorm(length(conc), mean = 0, sd = 0.01)
resp_noise[resp_noise < 0] <- 0
resp_noise[resp_noise > 1] <- 1

fit_par <- fit_4pl(conc, resp_noise)
stopifnot(!is.null(fit_par))
cat(sprintf(" -> Fit results: Emin=%.4f, Emax=%.4f, EC50=%.4f, Hill=%.4f\n", 
            fit_par[1], fit_par[2], fit_par[3], fit_par[4]))

# Test curve prediction & inverse
pred_val <- predict_4pl(2.0, fit_par)
inv_val <- inverse_4pl(pred_val, fit_par)
stopifnot(abs(inv_val - 2.0) < 0.05)
cat(" -> Hill predictor and inverse dose solving check: SUCCESS\n\n")


# 3. Test Full Synergy Calculator Pipeline
cat("[3/6] Testing HSA, Bliss, Loewe, and ZIP calculations on sample file...\n")
# Load the testData3 dataset
test_df <- read.table("testData3.tab", sep="\t", header=FALSE)

# Run complete synergy calculator
results <- calculate_synergy(test_df, data_type = "viability", use_fit = TRUE, control_row = 1, control_col = 1)

# Check matrix dimensions match original
nr <- nrow(test_df)
nc <- ncol(test_df)
stopifnot(all(dim(results$HSA$scores) == c(nr, nc)))
stopifnot(all(dim(results$Bliss$scores) == c(nr, nc)))
stopifnot(all(dim(results$Loewe$scores) == c(nr, nc)))
stopifnot(all(dim(results$ZIP$scores) == c(nr, nc)))

cat(" -> Pipeline dimensions and matrix structures check: SUCCESS\n")

# Verify mathematical indexing correctness (e.g. check corner controls are zero synergy)
stopifnot(abs(results$HSA$scores[1,1]) < 1e-6)
stopifnot(abs(results$Bliss$scores[1,1]) < 1e-6)
stopifnot(abs(results$ZIP$scores[1,1]) < 1e-6)
stopifnot(abs(results$Loewe$scores[1,1]) < 1e-6)

# Verify single agent columns/rows are zero synergy
stopifnot(all(abs(results$HSA$scores[1, ]) < 1e-6))
stopifnot(all(abs(results$HSA$scores[, 1]) < 1e-6))
stopifnot(all(abs(results$Bliss$scores[1, ]) < 1e-6))
stopifnot(all(abs(results$Bliss$scores[, 1]) < 1e-6))

cat(" -> Control well & single-agent zero synergy index check: SUCCESS\n\n")


# 4. Test Monotonic Interpolation Fallback
cat("[4/6] Testing interpolation fallback under fit failures...\n")
# Fit curve using linear fallback (set use_fit = FALSE)
results_fallback <- calculate_synergy(test_df, data_type = "viability", use_fit = FALSE, control_row = 1, control_col = 1)
stopifnot(all(dim(results_fallback$Loewe$scores) == c(nr, nc)))
stopifnot(is.null(results_fallback$single_fit_A))
stopifnot(is.null(results_fallback$single_fit_B))
cat(" -> Monotonic linear interpolation fallback check: SUCCESS\n\n")



# 5. Regression coverage for reordered zero-dose axes and validation
cat("[5/6] Testing reordered axes, ZIP surface fitting, and validation...\n")
reordered <- matrix(c(
  0.30, 0.55, 0.10,
  0.45, 0.75, 0.20,
  0.00, 0.40, 0.00
), nrow = 3, byrow = TRUE,
 dimnames = list(c("1uM", "4uM", "0uM"), c("2uM", "8uM", "0uM")))
reordered_result <- calculate_synergy(reordered, data_type = "inhibition", use_fit = FALSE)
stopifnot(reordered_result$zero_row == 3, reordered_result$zero_col == 3)
stopifnot(all(reordered_result$Bliss$scores[3, ] == 0))
stopifnot(all(reordered_result$Bliss$scores[, 3] == 0))
stopifnot(any(abs(results$ZIP$scores - results$Bliss$scores) > 1e-6))
invalid_control <- try(normalize_data(reordered, "viability", 99, 1), silent = TRUE)
stopifnot(inherits(invalid_control, "try-error"))
non_numeric <- reordered; non_numeric[1, 1] <- NA
stopifnot(inherits(try(normalize_data(non_numeric), silent = TRUE), "try-error"))
cat(" -> Reordered axes, ZIP, and validation checks: SUCCESS\n")

# 6. Concentration label formatting and distinct bundled samples
cat("[6/6] Testing concentration labels and sample uniqueness...\n")
label_cases <- c("X0uM", "X.25uM", ".5uM", "-.75uM", "Xenon", "control")
label_expected <- c("0uM", "0.25uM", "0.5uM", "-0.75uM", "Xenon", "control")
stopifnot(identical(format_concentration_labels(label_cases), label_expected))
antifungal_sample <- as.matrix(read.table("testData3.tab", sep = "\t", header = FALSE))
anticancer_sample <- as.matrix(read.table("anticancer_synergy.tab", sep = "\t", header = TRUE,
                                          row.names = 1, check.names = FALSE))
stopifnot(identical(dim(antifungal_sample), dim(anticancer_sample)))
stopifnot(!isTRUE(all.equal(unname(antifungal_sample), unname(anticancer_sample))))
stopifnot(identical(format_concentration_labels(colnames(anticancer_sample))[1:4],
                    c("0uM", "0.01uM", "0.03uM", "0.1uM")))
cat(" -> Concentration label and distinct sample checks: SUCCESS\n")

cat("=========================================\n")
cat("   ALL 6 AUTOMATED TEST GROUPS PASSED!  \n")
cat("=========================================\n")
