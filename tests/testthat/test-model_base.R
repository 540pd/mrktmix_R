# # Mock data for independent_var_info
# independent_var_info <- data.frame(
#   variable = c("wt", "qsec", "(Intercept)"),
#   type = c("fixed", "flexible", "intercept")
# )
#
# # Define positive and negative variables
# pos_vars <- c("var1", "var3")
# neg_vars <- c("var2")
#
# # Mock lm_model object
# lm_model <- lm(mpg ~ ., mtcars[,c("mpg" , "wt" , "qsec")])
#
# # coefficients for testing
# coefficients <- summary(lm_model)$coefficients
#
# # Mock VIF values - normally these would be calculated based on the model
# # calculate_vif <- function(model) {
# #   return(c(1.5, 2.5, 1.2))
# # }
#
# # Sample VIF threshold and p-value thresholds
# vif_threshold <- 5
# pvalue_thresholds <- c(intercept = 0.05, fixed = 0.05, flexible = 0.10)
#
# library(testthat)
# library(dplyr)
# library(stringr)
# library(tibble)
library(testthat)
library(mrktmix)

# Defining p-value thresholds for the test cases
pvalue_thresholds <- c(intercept = 0.05, fixed = 0.05, flexible = 0.1)

# Test 1: P-value below threshold for fixed predictor
test_that("P-value below threshold for fixed predictor", {
  expect_false(determine_pvalue_flag("fixed", 0.04, pvalue_thresholds))
})

# Test 2: P-value above threshold for flexible predictor
test_that("P-value above threshold for flexible predictor", {
  expect_true(determine_pvalue_flag("flexible", 0.12, pvalue_thresholds))
})

# Test 3: P-value equal to threshold for intercept
test_that("P-value equal to threshold for intercept", {
  expect_false(determine_pvalue_flag("intercept", 0.05, pvalue_thresholds))
})

# Test 4: P-value below threshold for intercept
test_that("P-value below threshold for intercept", {
  expect_false(determine_pvalue_flag("intercept", 0.03, pvalue_thresholds))
})

# Test 5: P-value above threshold for fixed predictor
test_that("P-value above threshold for fixed predictor", {
  expect_true(determine_pvalue_flag("fixed", 0.07, pvalue_thresholds))
})

library(testthat)

# Define the positive and negative variables for testing
pos_vars <- c("sales", "marketing")
neg_vars <- c("costs", "returns")

# Test 1: Positive sign for a positive variable
test_that("Positive sign for a positive variable", {
  expect_true(determine_expected_sign("sales", pos_vars, neg_vars, "|"))
})

# Test 2: Negative sign for a negative variable
test_that("Negative sign for a negative variable", {
  expect_false(determine_expected_sign("costs", pos_vars, neg_vars, "|"))
})

# Test 3: NA for a variable not in either list
test_that("NA for a variable not in either list", {
  expect_type(determine_expected_sign("other", pos_vars, neg_vars, "|"), "logical")
  expect_true(is.na(determine_expected_sign("other", pos_vars, neg_vars, "|")))
})

# Test 4: Aggregated variable with all positive segments
test_that("Aggregated variable with all positive segments", {
  expect_true(determine_expected_sign("sales|marketing", pos_vars, neg_vars, "|"))
})

# Test 5: Aggregated variable with mixed segments
test_that("Aggregated variable with mixed segments", {
  expect_true(is.na(determine_expected_sign("sales|costs", pos_vars, neg_vars, "|")))
})

# Test 6: Aggregated variable with all negative segments
test_that("Aggregated variable with all negative segments", {
  expect_false(determine_expected_sign("costs|returns", pos_vars, neg_vars, "|"))
})

library(tibble)

# Prepare a sample linear model and its summary for testing
sample_lm <- lm(mpg ~ wt + qsec, data = mtcars)
sample_lm_summary <- summary(sample_lm)

# Test 1: Basic functionality check
test_that("Basic functionality check", {
  result <- summarize_model(sample_lm_summary, loop_id = 1)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("loop_id", "sigma", "r_squared", "adj_r_squared", "residuals_all") %in% names(result)))
})

# Test 2: Check values in the summary
test_that("Check values in the summary", {
  result <- summarize_model(sample_lm_summary, loop_id = 1)
  expect_equal(result$loop_id, 1)
  expect_equal(result$sigma, sample_lm_summary$sigma)
  expect_equal(result$r_squared, sample_lm_summary$r.squared)
  expect_equal(result$adj_r_squared, sample_lm_summary$adj.r.squared)
  expect_equal(length(result$residuals_all[[1]]), length(residuals(sample_lm)))
})

# Test 3: Check handling of NA input
test_that("Check handling of NA input", {
  expect_error(summarize_model(NA, loop_id = 1))
})

# Test 1: Identifying variable to drop based on highest p-value
test_that("Identifying variable to drop based on highest p-value", {
  coef_df <- data.frame(
    variable = c("Var1", "Var2", "Var3"),
    Estimate = c(1.5, -0.3, 2.0),
    `Pr(>|t|)` = c(0.04, 0.06, 0.01),
    type = c("flexible", "flexible", "flexible"),
    flag_pvalue = c(TRUE, FALSE, TRUE),
    flag_sign = c(FALSE, TRUE, FALSE),
    flag_vif = c(FALSE, TRUE, FALSE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 2, FALSE, FALSE, 1)
  expect_equal(result, "Var2")
})

# Test 2: No variable to drop when all are below threshold
test_that("No variable to drop when all are below threshold", {
  coef_df <- data.frame(
    variable = c("Var1", "Var2", "Var3"),
    Estimate = c(1.5, -0.3, 2.0),
    `Pr(>|t|)` = c(0.02, 0.03, 0.01),
    type = c("fixed", "fixed", "flexible"),
    flag_pvalue = c(FALSE, FALSE, FALSE),
    flag_sign = c(FALSE, FALSE, FALSE),
    flag_vif = c(FALSE, FALSE, FALSE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 2, FALSE, FALSE, 1)
  expect_true(is.na(result)==T)
})

# Test 3: Considering estimate magnitude in decision
test_that("Considering estimate magnitude in decision", {
  coef_df <- data.frame(
    variable = c("Var1", "Var2", "Var3"),
    Estimate = c(1.5, -0.3, 2.0),
    `Pr(>|t|)` = c(0.041, 0.042, 0.043),
    type = c("flexible", "flexible", "flexible"),
    flag_pvalue = c(TRUE, TRUE, TRUE),
    flag_sign = c(FALSE, FALSE, FALSE),
    flag_vif = c(FALSE, FALSE, FALSE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 2, FALSE, TRUE, 1)
  expect_equal(result, "Var3")
})

# Test 4: Handling only flexible variables
test_that("Handling only flexible variables", {
  coef_df <- data.frame(
    variable = c("Flex1", "Flex2", "Flex3"),
    Estimate = c(0.5, -0.7, 0.2),
    `Pr(>|t|)` = c(0.2, 0.3, 0.1),
    type = c("flexible", "flexible", "flexible"),
    flag_pvalue = c(TRUE, FALSE, TRUE),
    flag_sign = c(FALSE, TRUE, FALSE),
    flag_vif = c(FALSE, TRUE, FALSE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 2, TRUE, FALSE, 2)
  expect_equal(result, "Flex2")
})

# Test 5: Multiple fixed variables, identifying based on p-value
test_that("Multiple fixed variables, identifying based on p-value", {
  coef_df <- data.frame(
    variable = c("flexible1", "flexible2", "flexible3"),
    Estimate = c(-1.5, 1.2, -0.9),
    `Pr(>|t|)` = c(0.06, 0.08, 0.07),
    type = c("flexible", "flexible", "flexible"),
    flag_pvalue = c(FALSE, TRUE, FALSE),
    flag_sign = c(FALSE, FALSE, TRUE),
    flag_vif = c(FALSE, FALSE, TRUE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 1, FALSE, FALSE, 1)
  expect_equal(result, "flexible3")
})

# Test 5: Multiple fixed variables, identifying based on p-value
test_that("Multiple fixed variables, identifying based on p-value", {
  coef_df <- data.frame(
    variable = c("flexible1", "flexible2", "flexible3"),
    Estimate = c(-1.5, 1.2, -0.9),
    `Pr(>|t|)` = c(0.06, 0.08, 0.07),
    type = c("flexible", "flexible", "flexible"),
    flag_pvalue = c(FALSE, FALSE, FALSE),
    flag_sign = c(FALSE, FALSE, TRUE),
    flag_vif = c(FALSE, FALSE, TRUE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 1, FALSE, FALSE, 1)
  expect_equal(result, "flexible3")
})

# Test 5: Multiple fixed variables, identifying based on p-value
test_that("Multiple fixed variables, identifying based on p-value", {
  coef_df <- data.frame(
    variable = c("flexible1", "flexible2", "flexible3"),
    Estimate = c(-1.5, 1.2, -0.9),
    `Pr(>|t|)` = c(0.06, 0.08, 0.07),
    type = c("flexible", "flexible", "flexible"),
    flag_pvalue = c(FALSE, FALSE, FALSE),
    flag_sign = c(FALSE, FALSE, FALSE),
    flag_vif = c(FALSE, FALSE, FALSE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 1, FALSE, FALSE, 3)
  expect_true(is.na(result)==TRUE)
})

# Test 5: Multiple fixed variables, identifying based on p-value
test_that("Multiple fixed variables, identifying based on p-value", {
  coef_df <- data.frame(
    variable = c("flexible1", "flexible2", "flexible3"),
    Estimate = c(-1.5, 1.2, -0.9),
    `Pr(>|t|)` = c(0.06, 0.08, 0.07),
    type = c("flexible", "flexible", "flexible"),
    flag_pvalue = c(FALSE, TRUE, FALSE),
    flag_sign = c(FALSE, FALSE, TRUE),
    flag_vif = c(FALSE, FALSE, TRUE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 2, FALSE, FALSE, 1)
  expect_equal(result, "flexible2")
})

# Test 6: Combination of fixed and flexible variables
test_that("Combination of fixed and flexible variables", {
  coef_df <- data.frame(
    variable = c("Fixed1", "Flex1", "Fixed2"),
    Estimate = c(2.0, -1.2, 0.8),
    `Pr(>|t|)` = c(0.04, 0.07, 0.05),
    type = c("fixed", "flexible", "fixed"),
    flag_pvalue = c(TRUE, FALSE, TRUE),
    flag_sign = c(FALSE, TRUE, FALSE),
    flag_vif = c(FALSE, TRUE, FALSE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 2, FALSE, FALSE, 1)
  expect_equal(result, "Flex1")
})

# Test 6: Combination of fixed and flexible variables
test_that("Combination of fixed and flexible variables", {
  coef_df <- data.frame(
    variable = c("Fixed1", "Flex1", "Flex2"),
    Estimate = c(2.0, -1.2, 0.8),
    `Pr(>|t|)` = c(0.08, 0.07, 0.06),
    type = c("fixed", "flexible", "flexible"),
    flag_pvalue = c(TRUE, FALSE, TRUE),
    flag_sign = c(FALSE, TRUE, FALSE),
    flag_vif = c(FALSE, TRUE, FALSE),
    check.names = FALSE
  )
  result <- identify_drop_variable(coef_df, 1, FALSE, TRUE, 1)
  expect_equal(result, "Flex2")
})

# Prepare a sample dataset and an initial model for testing
data(mtcars)
initial_model <- lm(mpg ~ wt + cyl + gear, data = mtcars)

# Test 1: Removing a standard variable
test_that("Removing a standard variable", {
  updated_model <- update_model(initial_model, mtcars, "gear")
  updated_formula <- formula(updated_model)
  expect_false("gear" %in% all.vars(updated_formula))
})

# Test 2: Removing the intercept
test_that("Removing the intercept", {
  updated_model_no_intercept <- update_model(initial_model, mtcars, "(Intercept)")
  updated_formula_no_intercept <- formula(updated_model_no_intercept)
  expect_false(attr(terms(updated_formula_no_intercept), "intercept") == 1)
})

# Test 3: Model class remains the same after update
test_that("Model class remains the same after update", {
  updated_model <- update_model(initial_model, mtcars, "wt")
  expect_s3_class(updated_model, class(initial_model))
})

# Prepare a sample dataset and an initial model for testing
data(mtcars)
initial_model <- lm(mpg ~ wt + hp + drat, data = mtcars)

# Test 1: Basic functionality with no singular variables
test_that("Basic functionality with no singular variables", {
  cleansed_model <- cleanse_model_singularity(initial_model, mtcars, names(mtcars), round_digits = 2, verbose = FALSE)
  expect_s3_class(cleansed_model, "lm")
  expect_true("drat" %in% names(coef(cleansed_model))) # Assuming 'drat' is not singular
})

# Test 2: Handling model with singular variables
test_that("Handling model with singular variables", {
  # Create a model with an artificially introduced singularity
  mtcars$wt_dup = mtcars$wt
  singular_model <- lm(mpg ~ wt + wt_dup + hp, data = mtcars)
  cleansed_model <- cleanse_model_singularity(singular_model, mtcars, c("wt", "wt_dup", "hp"), round_digits = 2, verbose = FALSE)
  expect_false("wt_dup" %in% names(coef(cleansed_model))) # 'wt_dup' should be removed
})

# Test 3: Verbose output
test_that("Verbose output", {
  mtcars$wt_dup = mtcars$wt
  singular_model <- lm(mpg ~ wt + wt_dup + hp, data = mtcars)
  expect_message(cleanse_model_singularity(singular_model, mtcars, c("wt", "wt_dup", "hp"), round_digits = 2, verbose = TRUE), "Dropping variable due to singularity")
})

# Test 4: No flexible variables present
test_that("No flexible variables present", {
  cleansed_model <- cleanse_model_singularity(initial_model, mtcars, c("nonexistent_var"), round_digits = 2, verbose = FALSE)
  expect_s3_class(cleansed_model, "lm")
})

# Prepare a sample dataset and an initial model for testing
data(mtcars)
initial_model <- lm(mpg ~ wt + hp + drat, data = mtcars)

# Test 1: Basic functionality with no perfect fit
test_that("Basic functionality with no perfect fit", {
  cleansed_model <- cleanse_model_perfect_fit(initial_model, mtcars, names(mtcars), drop_highest_estimate = TRUE, ignore_estimate_sign = TRUE)
  expect_s3_class(cleansed_model, "lm")
  expect_true("drat" %in% names(coef(cleansed_model))) # Assuming 'drat' does not cause perfect fit
})

# Prepare a sample dataset and an initial model for testing
data(mtcars)
initial_model <- lm(mpg ~ wt + hp + drat, data = mtcars)

# Test 1: Basic VIF calculation
test_that("Basic VIF calculation", {
  vif_values <- calculate_vif(initial_model)
  expect_type(vif_values, "double")
  expect_length(vif_values, length(coef(initial_model)))
  expect_true(all(names(vif_values) == names(coef(initial_model))))
  expect_true(all(!is.na(vif_values[-1])))
})

# Test 2: Handling model with multicollinearity
test_that("Handling model with multicollinearity", {
  # Creating a model with high multicollinearity
  multicollinear_model <- lm(mpg ~ wt + I(2 * wt) + hp, data = mtcars)
  vif_values <- calculate_vif(multicollinear_model)
  expect_type(vif_values, "double")
  expect_true(all(vif_values[-1] == Inf))  # Non-intercept VIFs should be Inf
})

# Test 3: Model without an intercept
test_that("Model without an intercept", {
  no_intercept_model <- lm(mpg ~ wt + hp + 0, data = mtcars)
  vif_values <- calculate_vif(no_intercept_model)
  expect_type(vif_values, "double")
  expect_false(any(is.na(vif_values)))  # No NA values as there's no intercept
})



