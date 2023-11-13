# Load necessary packages and your functions
library(testthat)
library(mrktmix)  # Replace with the actual name of your package

library(testthat)

# Sample Data for Testing
test_df <- data.frame(
  Variable1 = rnorm(10),
  Variable2 = rnorm(10)
)

# Test 1: Basic Functionality
test_that("Basic functionality works", {
  result <- compute_apl_values(test_df, adstock = 0.5, power = 0.2, lag = 1)
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) == ncol(test_df))
  expect_true(nrow(result) == nrow(test_df))
})

# Test 3: Extreme Parameter Values
test_that("Function handles extreme parameter values", {
  result <- compute_apl_values(test_df, adstock = 1, power = 1, lag = 0)
  expect_s3_class(result, "data.frame")
})

# Test 4: Invalid Adstock Rate
test_that("Function handles invalid adstock rate", {
  expect_error(compute_apl_values(test_df, adstock = -0.5, power = 2, lag = 1))
  expect_error(compute_apl_values(test_df, adstock = 1.5, power = 2, lag = 1))
})

# Test 5: Invalid Power Parameter
test_that("Function handles invalid power parameter", {
  expect_error(compute_apl_values(test_df, adstock = 0.5, power = -1, lag = 1))
})

# Test 6: Invalid Lag Parameter
test_that("Function handles invalid lag parameter", {
  expect_error(compute_apl_values(test_df, adstock = 0.5, power = 0.2, lag = -1))
})

# Sample Data for Testing
test_df <- data.frame(
  Variable1 = 1:10,
  Variable2 = (1:10) * 2
)

# Expected Results for Specific Parameters
expected_adstock_rate <- 0.5
expected_power <- .2
expected_lag <- 1

# Manually calculate expected results for comparison
expected_df <- test_df
expected_df$Variable1 <- stats::filter(expected_df$Variable1, expected_adstock_rate, method = "recursive")^expected_power
expected_df$Variable2 <- stats::filter(expected_df$Variable2, expected_adstock_rate, method = "recursive")^expected_power
expected_df <- dplyr::lag(expected_df, expected_lag)

# Test 7: Check Correctness of Output Values
test_that("Function generates correct values", {
  result <- compute_apl_values(test_df, adstock = expected_adstock_rate, power = expected_power, lag = expected_lag)
  expect_true(all.equal(result, expected_df, check.attributes = FALSE))
})


# Test 1: Basic Functionality
test_that("Basic functionality works", {
  result <- generate_apl_dataframe(test_df, adstock = c(0.5), power = c(0.5), lag = c(1))
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= ncol(test_df)) # Expecting more columns due to transformations
})

# Test 2: Power Validation
test_that("Power less than 1 validation", {
  expect_error(generate_apl_dataframe(test_df, adstock = c(0.5), power = c(1.1), lag = c(1)))
  expect_error(generate_apl_dataframe(test_df, adstock = c(0.5), power = c(1.5), lag = c(1)))
})

# Define a small, predictable dataset
test_df <- data.frame(
  TV = 1:5,
  Radio = 6:10
)

# Define transformation parameters for each variable
candidate_variables <- list(
  TV = setNames(c(0, 1, 0), c("adstock", "power", "lag")),
  Radio = setNames(c(0.0, 1.0, 0), c("adstock", "power", "lag"))
)

# Test 1: Check Correctness of Output Values
test_that("Function generates correct values for each variable", {
  result <- apply_apl(test_df, candidate_variables)
  # Replace the following line with actual comparison to `expected_output`
  expect_true(all.equal(result, test_df, check.attributes = FALSE))
})

# Test 3: Invalid Candidate Variables
test_that("Function handles invalid candidate variables", {
  invalid_candidates <- list(
    TV = setNames(c(-0.5, 0.4, 1), c("adstock", "power", "lag")) # Invalid adstock
  )
  expect_error(apply_apl(test_df, invalid_candidates))
})

# Test 1: Basic Functionality with Specified Ranges
test_that("Basic functionality with specified ranges", {
  adstock_params <- list(adstock = setNames(c(.1, .3, .1), c("start", "end", "step")))
  power_params <- list(power = setNames(c(.2, .5, .1), c("start", "end", "step")))
  lag_params <- list(lag = setNames(c(0, 2, 1), c("start", "end", "step")))

  combinations <- generate_apl_combinations(adstock_params, power_params, lag_params, NA)
  expect_type(combinations, "list")
  expect_true(length(combinations) > 0)
})

# Test 2: Functionality with Constraints
test_that("Functionality with constraints", {
  adstock_params <- list(adstock = setNames(c(.1, .3, .1), c("start", "end", "step")))
  power_params <- list(power = setNames(c(.2, .5, .1), c("start", "end", "step")))
  lag_params <- list(lag = setNames(c(0, 2, 1), c("start", "end", "step")))
  constraints <- list(constraints="adstock <= power")

  combinations <- generate_apl_combinations(adstock_params, power_params, lag_params, constraints)
  expect_type(combinations, "list")
  # Check if constraints are applied correctly
  expect_true(all(sapply(combinations, function(x) x["adstock"] <= x["power"])))
})

# Test 3: Edge Case with Empty Ranges
test_that("Edge case with empty ranges", {
  adstock_params <- list(adstock = setNames(c(.1, .1, .1), c("start", "end", "step")))
  power_params <- list(power = setNames(c(.2, .2, .1), c("start", "end", "step")))
  lag_params <- list(lag = setNames(c(0, 0, 1), c("start", "end", "step")))

  combinations <- generate_apl_combinations(adstock_params, power_params, lag_params, NA)
  expect_equal(class(combinations), "array")
  expect_equal(length(combinations), 1) # Expecting a single combination
})

# Test 4: Check Specific Combinations
test_that("Function generates specific combinations", {
  adstock_params <- list(adstock = setNames(c(.1, .2, .1), c("start", "end", "step")))
  power_params <- list(power = setNames(c(.2, .3, .1), c("start", "end", "step")))
  lag_params <- list(lag = setNames(c(1, 2, 1), c("start", "end", "step")))
  constraints <- NA

  combinations <- generate_apl_combinations(adstock_params, power_params, lag_params, constraints)

  # Manually create the expected combinations
  expected_combinations <- list(
    c(adstock = 0.1, power = 0.2, lag = 1),
    c(adstock = 0.2, power = 0.2, lag = 1),
    c(adstock = 0.1, power = 0.3, lag = 1),
    c(adstock = 0.2, power = 0.3, lag = 1),
    c(adstock = 0.1, power = 0.2, lag = 2),
    c(adstock = 0.2, power = 0.2, lag = 2),
    c(adstock = 0.1, power = 0.3, lag = 2),
    c(adstock = 0.2, power = 0.3, lag = 2)
  )

  expect_equal(length(combinations), length(expected_combinations))
  expect_true(all(sapply(1:length(combinations), function(i) {
    all(combinations[[i]] == expected_combinations[[i]])
  })))
})

# Test 1: Basic Functionality with Multiple Variables
test_that("Basic functionality with multiple variables", {
  variables_wt_apl_constraints <- list(
    TV = list(
      adstock = setNames(c(.1, .2, .1), c("start", "end", "step")),
      power = setNames(c(.2, .3, .1), c("start", "end", "step")),
      lag = setNames(c(1, 2, 1), c("start", "end", "step")),
      constraints = NA
    ),
    Radio = list(
      adstock = setNames(c(.2, .3, .1), c("start", "end", "step")),
      power = setNames(c(.3, .4, .1), c("start", "end", "step")),
      lag = setNames(c(1, 1, 1), c("start", "end", "step")),
      constraints = NA
    )
  )

  combinations <- generate_variable_combination(variables_wt_apl_constraints)
  expect_type(combinations, "list")
  expect_true(length(combinations) > 0)
  # Check if the list contains combinations for both TV and Radio
  expect_true(all(c("TV", "Radio") %in% names(unlist(combinations,recursive=F))))
})

# Test 2: Check Specific Combinations
test_that("Function generates specific combinations", {
  # Define a simpler case with expected outputs
  variables_wt_apl_constraints <- list(
    TV = list(
      adstock = setNames(c(.1, .1, .1), c("start", "end", "step")),
      power = setNames(c(.2, .2, .1), c("start", "end", "step")),
      lag = setNames(c(1, 1, 1), c("start", "end", "step")),
      constraints = NA
    )
  )

  combinations <- generate_variable_combination(variables_wt_apl_constraints)
  expect_type(combinations, "list")
  # Manually create the expected combination for TV
  expected_combinations <- list(
     list(TV=
      setNames(c(0.1,0.2,1),c("adstock", "power" , "lag"))
    )
  )

  expect_equal(unlist(combinations,recursive = T), unlist(expected_combinations,recursive = T))
})

