#!/usr/bin/env Rscript

source("SynergyCalculations.R")

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_matrix_finite <- function(value, expected_dim, message) {
  assert_true(is.matrix(value) && identical(dim(value), expected_dim) && all(is.finite(value)), message)
}

cat("CheckerboardR capability regression tests\n")

filename_labels <- condition_names_from_filename("paclitaxel_carboplatin.json")
assert_true(filename_labels$derived && identical(filename_labels$a, "Paclitaxel") &&
              identical(filename_labels$b, "Carboplatin"),
            "Two-token sample filename did not derive the expected condition names.")
replicate_labels <- condition_names_from_filename("DrugA_DrugB_replicate03.csv")
assert_true(replicate_labels$derived && identical(replicate_labels$a, "DrugA") &&
              identical(replicate_labels$b, "DrugB"),
            "Filename condition parsing did not ignore a replicate suffix.")
fallback_labels <- condition_names_from_filename("testData.xlsx")
assert_true(!fallback_labels$derived && identical(fallback_labels$a, "Drug A") &&
              identical(fallback_labels$b, "Drug B"),
            "A filename without an underscore should retain generic condition labels.")

if (requireNamespace("readxl", quietly = TRUE)) {
  sheets <- readxl::excel_sheets("testData.xlsx")
  assert_true(identical(sheets, c("Sheet1", "Conditions")),
              "testData.xlsx must contain its matrix followed by a Conditions sheet.")
  condition_sheet <- readxl::read_excel("testData.xlsx", sheet = "Conditions")
  assert_true(identical(as.character(condition_sheet$Field), c("Condition A", "Condition B")) &&
                identical(as.character(condition_sheet$Value), c("Condition A", "Condition B")),
              "The Conditions sheet must expose editable Condition A and Condition B values.")
}

base <- matrix(c(
  -0.05000, 0.12345, 0.28765, 0.50123,
  -0.02000, 0.24111, 0.40234, 0.63456,
   0.08000, 0.39876, 0.61234, 0.78123,
   0.22000, 0.55789, 0.74321, 0.89234
), nrow = 4, byrow = TRUE,
dimnames = list(c("0uM", ".25uM", "1uM", "4uM"),
                c("0uM", ".5uM", "2uM", "8uM")))

none <- calculate_synergy(base, data_type = "inhibition", use_fit = FALSE, baseline_method = "none")
negative <- calculate_synergy(base, data_type = "inhibition", use_fit = FALSE, baseline_method = "negative")
all_cells <- calculate_synergy(base, data_type = "inhibition", use_fit = FALSE, baseline_method = "all")

assert_true(identical(none$original_inhibition, none$adjusted_inhibition),
            "No baseline correction must preserve the original inhibition matrix.")
assert_true(identical(negative$adjusted_inhibition[base >= 0], negative$original_inhibition[base >= 0]),
            "Negative-only baseline correction changed a non-negative cell.")
assert_true(any(abs(all_cells$adjusted_inhibition - all_cells$original_inhibition) > 1e-10),
            "Full-matrix baseline correction did not change any cell.")
assert_true(all_cells$baseline_applied && all_cells$baseline_method == "all",
            "Baseline correction audit metadata is incorrect.")

expected_consensus <- pmax(none$HSA$expected, none$Bliss$expected, none$Loewe$expected)
assert_true(isTRUE(all.equal(none$Consensus$expected, expected_consensus, tolerance = 1e-12)),
            "Consensus reference must be the conservative maximum of HSA, Bliss, and Loewe.")
assert_true(isTRUE(all.equal(none$Consensus$scores,
                             none$adjusted_inhibition - expected_consensus,
                             tolerance = 1e-12, check.attributes = FALSE)) ||
              all(none$Consensus$scores[-none$zero_row, -none$zero_col] ==
                  (none$adjusted_inhibition - expected_consensus)[-none$zero_row, -none$zero_col]),
            "Consensus scores do not match observed minus reference.")
assert_true(all(none$Consensus$scores[none$zero_row, ] == 0) &&
              all(none$Consensus$scores[, none$zero_col] == 0),
            "Consensus single-agent cells must have zero synergy score.")
assert_matrix_finite(none$ZIP$fitted, dim(base), "ZIP fitted response matrix is missing or invalid.")
assert_true(any(abs(none$Bliss$scores - round(none$Bliss$scores, 3)) > 1e-8),
            "Calculation matrices appear to be rounded internally.")

replicates <- list(base, base + matrix(c(
  0, .01, -.01, .02, -.01, .02, .01, -.02,
  .01, -.02, .02, .01, 0, .01, -.02, .02
), 4, byrow = TRUE), base + matrix(c(
  0, -.015, .012, -.01, .012, -.01, .02, .01,
  -.01, .015, -.01, .02, .01, -.02, .01, -.015
), 4, byrow = TRUE))
replicate_result <- calculate_replicate_synergy(
  replicates, data_type = "inhibition", use_fit = FALSE,
  baseline_method = "none", iterations = 40, seed = 17
)
assert_true(replicate_result$replicate_count == 3L && replicate_result$bootstrap_iterations == 40L,
            "Replicate count or bootstrap iteration metadata is incorrect.")
for (model in c("HSA", "Bliss", "Loewe", "ZIP", "Consensus")) {
  stats <- replicate_result$replicate_statistics[[model]]$scores
  for (metric in c("mean", "sd", "sem", "ci_lower", "ci_upper")) {
    assert_matrix_finite(stats[[metric]], dim(base), paste(model, metric, "uncertainty matrix is invalid."))
  }
  assert_true(all(stats$ci_lower <= stats$ci_upper), paste(model, "confidence interval bounds are inverted."))
}
assert_true(any(replicate_result$replicate_statistics$Bliss$scores$sem > 0),
            "Replicate SEM is unexpectedly zero for perturbed matrices.")

zip_fitted <- select_analysis_matrix(replicate_result, "ZIP", "fitted")
assert_true(identical(zip_fitted$matrix, replicate_result$ZIP$fitted) && !zip_fitted$divergent,
            "ZIP fitted-response selector returned the wrong matrix or scale type.")
consensus_reference <- select_analysis_matrix(replicate_result, "Consensus", "reference")
assert_true(identical(consensus_reference$matrix, replicate_result$Consensus$expected),
            "Consensus reference selector returned the wrong matrix.")
transitional_data <- select_analysis_matrix(replicate_result, "Data", "score")
assert_true(identical(transitional_data$matrix, replicate_result$adjusted_inhibition),
            "A transitional invalid Data view did not fall back to observed inhibition.")
assert_true(isTRUE(all.equal(replicate_result$Bliss$scores,
                             replicate_result$replicate_statistics$Bliss$scores$mean)),
            "Displayed replicate scores do not use the replicate-level mean.")

exported <- build_matrix_export(replicate_result)
required_columns <- c(
  "condition_a", "condition_b",
  "drug_a_concentration", "drug_b_concentration", "original_inhibition", "adjusted_inhibition",
  "HSA_reference", "HSA_score", "Bliss_reference", "Bliss_score",
  "Loewe_reference", "Loewe_score", "ZIP_reference", "ZIP_fitted", "ZIP_score",
  "Consensus_reference", "Consensus_score", "Bliss_score_sem", "ZIP_fitted_ci_upper"
)
assert_true(nrow(exported) == length(base), "Matrix export must have one row per dose pair.")
assert_true(all(required_columns %in% names(exported)), "Matrix export is missing required score/reference columns.")
assert_true(all(is.finite(exported$Consensus_score)), "Matrix export contains non-finite consensus scores.")

mismatched <- replicates
colnames(mismatched[[3]])[2] <- "wrong-dose"
error_message <- tryCatch({
  calculate_replicate_synergy(mismatched, data_type = "inhibition", use_fit = FALSE, iterations = 20)
  ""
}, error = function(e) e$message)
assert_true(grepl("identical row and column", error_message),
            "Mismatched replicate concentration labels were not rejected.")

cat("PASS: baseline, consensus, fitted response, replicate uncertainty, selectors, and export\n")
