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

# Prepare a sample dataset for testing
advertising <- data.frame(
  Sales = runif(10),
  TV = runif(10),
  Radio = runif(10),
  Newspaper = runif(10)
)

# Test 1: Basic aggregation functionality
test_that("Basic aggregation functionality", {
  aggregated_df <- aggregate_columns(advertising, c("TV|Radio|Newspaper"), delimiter = "|")
  expect_true("TV|Radio|Newspaper" %in% names(aggregated_df))
  expect_equal(ncol(aggregated_df), 1)
  expect_equal(nrow(aggregated_df), nrow(advertising))
})

# Test 2: Correctness of aggregation
test_that("Correctness of aggregation", {
  aggregated_df <- aggregate_columns(advertising, c("TV|Radio|Newspaper"), delimiter = "|")
  calculated_aggregation <- rowSums(advertising[, c("TV", "Radio", "Newspaper")])
  expect_equal(aggregated_df$"TV|Radio|Newspaper", calculated_aggregation)
})

# Test 4: Multiple aggregation groups
test_that("Multiple aggregation groups", {
  aggregated_df <- aggregate_columns(advertising, c("TV|Radio", "Newspaper|Sales"), delimiter = "|")
  expect_equal(ncol(aggregated_df), 2)
  expect_equal(nrow(aggregated_df), nrow(advertising))
  expect_true(all(c("TV|Radio", "Newspaper|Sales") %in% names(aggregated_df)))
})

# Prepare a sample dataset and a weights vector for testing
advertising <- data.frame(TV = rnorm(10), Radio = rnorm(10))
event <- data.frame(Date2021_01_08 = sample(0:1, 10, replace = TRUE),
                    Date2021_01_01 = sample(0:1, 10, replace = TRUE))
model_df <- cbind(advertising, event)
variables_wt_weights <- setNames(1:5, c("TV|0.8_0.22_0", "Radio|0.5_0.15_1",
                                        "Intercept", "Date2021_01_08", "Date2021_01_01"))

# Test 1: Basic functionality with valid inputs
test_that("Basic functionality with valid inputs", {
  transformed_df <- decompose_model_component(variables_wt_weights, model_df,
                                              is_weight_coefficient = FALSE,
                                              apl_delimiter = "_",
                                              delimiter = "|")
  expect_s3_class(transformed_df, "data.frame")
  expect_true(all(c("TV", "Radio", "Intercept", "Date2021_01_08", "Date2021_01_01") %in% names(transformed_df)))
})

# Test 1: Basic functionality with valid inputs
test_that("Basic functionality with valid inputs", {
  variables_wt_named_apl <- list(
    "variable1" = c("adstock" = 0.5, "power" = 2, "lag" = 1),
    "variable2" = c("adstock" = 0.3, "power" = 1, "lag" = 0)
  )
  composed_names <- compose_variable_apl(variables_wt_named_apl, apl_delimiter = "_", delimiter = "|")
  expected_names <- c("variable1|0.5_2_1", "variable2|0.3_1_0")
  expect_equal(composed_names, expected_names)
})

# Test 2: Handling empty inputs
test_that("Handling empty inputs", {
  empty_variables <- list()
  composed_names <- compose_variable_apl(empty_variables, apl_delimiter = "_", delimiter = "|")
  expect_length(composed_names, 0)
})

# Test 3: Custom delimiters
test_that("Custom delimiters", {
  variables_wt_named_apl <- list(
    "var1" = c("adstock" = 0.6, "power" = 3, "lag" = 2),
    "var2" = c("adstock" = 0.4, "power" = 2, "lag" = 1)
  )
  composed_names <- compose_variable_apl(variables_wt_named_apl, apl_delimiter = "-", delimiter = ":")
  expected_names <- c("var1:0.6-3-2", "var2:0.4-2-1")
  expect_equal(composed_names, expected_names)
})

# Test 4: Handling NAs in APL attributes
test_that("Handling NAs in APL attributes", {
  variables_wt_named_apl <- list(
    "variable1" = c("adstock" = NA, "power" = 2, "lag" = 1),
    "variable2" = c("adstock" = 0.3, "power" = NA, "lag" = 0)
  )
  composed_names <- compose_variable_apl(variables_wt_named_apl, apl_delimiter = "_", delimiter = "|")
  expected_names <- c("variable1|NA_2_1", "variable2|0.3_NA_0")
  expect_equal(composed_names, expected_names)
})

# # Test 1: Correct Parsing of Variables with APL Attributes
# test_that("Correct parsing of variables with APL attributes", {
#   input_variables <- c("TV_Smart|0.8_0.22_0.11", "Radio|0.5_0.15")
#   parsed_data <- parse_variable_wt_apl(input_variables, apl_delimiter = "_", delimiter = "|")
#   expected_data <- data.frame(
#     variable = c("TV_Smart", "Radio"),
#     adstock = c(0.8, 0.5),
#     power = c(0.22, 0.15),
#     lag = c(0.11, NA)
#   )
#   expect_equal(parsed_data, expected_data)
# })

# Test 1: Correct Parsing of Variables with APL Attributes
test_that("Correct parsing of variables with APL attributes", {
  input_variables <- c("TV_Smart|0.8_0.22_0.11", "Radio|0.5_0.15_0")
  parsed_data <- parse_variable_wt_apl(input_variables, apl_delimiter = "_", delimiter = "|")
  expected_data <- data.frame(
    variable = c("TV_Smart", "Radio"),
    adstock = c(0.8, 0.5),
    power = c(0.22, 0.15),
    lag = c(0.11, 0)
  )
  expect_equal(parsed_data, expected_data)
})

# # Test 2: Handling Incomplete APL Information
# test_that("Handling incomplete APL information", {
#   input_variables <- c("TV_Smart|0.8", "Radio")
#   parsed_data <- parse_variable_wt_apl(input_variables, apl_delimiter = "_", delimiter = "|")
#   expected_data <- data.frame(
#     variable = c("TV_Smart", "Radio"),
#     adstock = c(0.8, NA),
#     power = c(NA, NA),
#     lag = c(NA, NA)
#   )
#   expect_equal(parsed_data, expected_data)
# })

# Test 2: Handling Incomplete APL Information
test_that("Handling incomplete APL information", {
  input_variables <- c("TV_Smart|0.8_1_2", "Radio")
  parsed_data <- parse_variable_wt_apl(input_variables, apl_delimiter = "_", delimiter = "|")
  expected_data <- data.frame(
    variable = c("TV_Smart", "Radio"),
    adstock = c(0.8, 0),
    power = c(1, 1),
    lag = c(2, 0)
  )
  expect_equal(parsed_data, expected_data)
})

# # Test 4: Custom Delimiters
# test_that("Custom delimiters", {
#   input_variables <- c("TV_Smart:0.8-0.22-0.11", "Radio:0.5-0.15")
#   parsed_data <- parse_variable_wt_apl(input_variables, apl_delimiter = "-", delimiter = ":")
#   expected_data <- data.frame(
#     variable = c("TV_Smart", "Radio"),
#     adstock = c(0.8, 0.5),
#     power = c(0.22, 0.15),
#     lag = c(0.11, NA)
#   )
#   expect_equal(parsed_data, expected_data)
# })

# # Test 4: Custom Delimiters
# test_that("Custom delimiters", {
#   input_variables <- c("TV_Smart:0.8-0.22-0.11", "Radio:0.5-0.15_0")
#   parsed_data <- parse_variable_wt_apl(input_variables, apl_delimiter = "-", delimiter = ":")
#   expected_data <- data.frame(
#     variable = c("TV_Smart", "Radio"),
#     adstock = c(0.8, 0.5),
#     power = c(0.22, 0.15),
#     lag = c(0.11, NA)
#   )
#   expect_equal(parsed_data, expected_data)
# })

# Prepare a sample dataset for testing
model_df <- data.frame(
  TV = rnorm(10),
  Radio = rnorm(10),
  Date2021_01_08 = sample(0:1, 10, replace = TRUE),
  Date2021_01_01 = sample(0:1, 10, replace = TRUE)
)

# Test 1: Basic functionality with named numeric vector
test_that("Basic functionality with named numeric vector", {
  var_info_vec <- setNames(5, "TV|0.8_0.22_0")
  result_vec <- generate_model_dependent(var_info_vec, model_df)
  expect_type(result_vec, "list")
  expect_true("TV" %in% names(result_vec[[2]][[1]]))
  expect_type(result_vec[[2]], "list")
})

# Test 2: Functionality with list input
test_that("Functionality with list input", {
  var_info_list <- list(
    TV = list(
      adstock = setNames(c(.1, .2, .1), c("start", "end", "step")),
      power = setNames(c(.2, .3, .1), c("start", "end", "step")),
      lag = setNames(c(0, 1, 1), c("start", "end", "step")),
      constraints = "adstock <= power"
    )
  )
  result_list <- generate_model_dependent(var_info_list, model_df)
  expect_type(result_list, "list")
  expect_type(result_list[[2]], "list")
  expect_true("TV" %in% names(result_list[[2]][[1]]))
})

# Test 4: Custom Delimiters
test_that("Custom delimiters", {
  var_info_vec <- setNames(5, "TV|0.8|0.22|0")
  result_vec <- generate_model_dependent(var_info_vec, model_df, apl_delimiter = "|", var_apl_delimiter = "|", var_agg_delimiter = "|")
  expect_type(result_vec, "list")
})

# Test 1: Aggregation
test_that("Aggregation", {
  model_variable <- "TV_0_1_0+Sales_0_1_0"
  result <- get_dep_indep_vars(model_variable, trim = FALSE, print_model_type = FALSE)
  expect_equal(result$dependent_var, c("TV_0_1_0","Sales_0_1_0"))
  expect_equal(result$independent_var, "TV_0_1_0|Sales_0_1_0")
})

# Test 2: Remodel
test_that("Remodel", {
  model_variable <- "TV_0_1_0"
  result <- get_dep_indep_vars(model_variable, trim = FALSE, print_model_type = FALSE)
  expect_equal(result$dependent_var, "TV_0_1_0")
  expect_equal(result$independent_var, "TV_0_1_0")
})

# Test 3: Segregation
test_that("Segregation", {
  model_variable <- "TV-Sales_0_1_0"
  result <- get_dep_indep_vars(model_variable, trim = FALSE, print_model_type = FALSE)
  expect_equal(result$dependent_var, "TV|Sales_0_1_0")
  expect_equal(result$independent_var, c("TV", "Sales_0_1_0"))
})

# Test 4: Aggregation & Segregation
test_that("Aggregation & Segregation", {
  model_variable <- "TV_0_1_0+-Sales_0_1_0"
  result <- get_dep_indep_vars(model_variable, trim = FALSE, print_model_type = FALSE)
  expect_equal(result$dependent_var, c("TV_0_1_0","Sales_0_1_0"))
  expect_equal(result$independent_var, c("TV_0_1_0", "Sales_0_1_0"))
})

# Test 5: Aggregation & Segregation with Different Delimiters
test_that("Aggregation & Segregation with different delimiters", {
  model_variable <- "TV_0_1_0+Sales|Radio_0_1_0"
  result <- get_dep_indep_vars(model_variable, var_agg_delimiter = "|", trim = FALSE, print_model_type = FALSE)
  expect_equal(result$dependent_var, c("TV_0_1_0","Sales|Radio_0_1_0"))
  expect_equal(result$independent_var, "TV_0_1_0|Sales|Radio_0_1_0")
})
