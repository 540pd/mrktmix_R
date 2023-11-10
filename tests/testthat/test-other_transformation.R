library(testthat)
library(stringr)

# Test that Aggregate model type is identified correctly and processed
test_that("Aggregate model type is identified and processed correctly", {
  model_variable <- "A + B + C"
  result <- get_dep_indep_vars(model_variable, print_model_type = FALSE)

  expect_equal(result$dependent_var, c("A", "B", "C"))
  expect_equal(result$independent_var, "A|B|C")
})

test_that("Aggregate model type is identified and processed correctly", {
  model_variable <- "A + B + C"
  result <- get_dep_indep_vars(model_variable, trim = F, print_model_type = FALSE)

  expect_equal(result$dependent_var, c("A ", " B ", " C"))
  expect_equal(result$independent_var, "A | B | C")
})

# Test that Segregate model type is identified correctly and processed
test_that("Segregate model type is identified and processed correctly", {
  model_variable <- "A - B - C"
  result <- get_dep_indep_vars(model_variable, print_model_type = FALSE)

  expect_equal(result$dependent_var, "A|B|C")
  expect_equal(result$independent_var, c("A", "B", "C"))
})

# Test that Aggregate & Segregate model type is identified correctly and processed
test_that("Aggregate & Segregate model type is identified and processed correctly", {
  model_variable <- "A + B - C"
  result <- get_dep_indep_vars(model_variable, print_model_type = FALSE)

  # Assuming the function is intended to split "+" and "-" equally, which may need review.
  expect_equal(result$dependent_var, c("A","B|C"))
  expect_equal(result$independent_var, c("A|B","C"))
})

# Test that Remodel type is identified correctly and processed
test_that("Remodel type is identified and processed correctly", {
  model_variable <- "A"
  result <- get_dep_indep_vars(model_variable, print_model_type = FALSE)

  expect_equal(result$dependent_var, "A")
  expect_equal(result$independent_var, "A")
})

# Test that function handles custom aggregation delimiter
test_that("Function handles custom aggregation delimiter", {
  model_variable <- "A + B + C"
  custom_delimiter <- "&"
  result <- get_dep_indep_vars(model_variable, var_agg_delimiter = custom_delimiter, print_model_type = FALSE)

  expect_equal(result$dependent_var, c("A", "B", "C"))
  expect_equal(result$independent_var, "A&B&C")
})

# # Run the tests
# test_dir("tests")
