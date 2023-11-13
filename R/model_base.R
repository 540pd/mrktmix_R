#' Determine VIF Flag for Predictors
#'
#' This function evaluates the Variance Inflation Factor (VIF) for predictors and
#' determines if it exceeds a specified threshold. It accounts for special cases
#' based on the type of the variable.
#'
#' @param type Character string indicating the type of the variable.
#'             Options are 'fixed' or 'flexible'. The function applies special
#'             logic if there is only one 'fixed' variable.
#' @param vif Numeric VIF value for the predictor. This value is used to assess
#'            whether the VIF exceeds the threshold.
#' @param vif_threshold Numeric value specifying the threshold above which the
#'                      VIF flag should be triggered.
#'
#' @return A logical value; TRUE if the VIF is greater than the threshold,
#'         except when there is only one 'fixed' variable and the VIF is
#'         infinite, in which case it returns FALSE.
#'
#' @details
#' The function uses the following logic:
#' - If the `type` is 'fixed' and there is only one such variable with an
#'   infinite VIF, it returns FALSE. This accounts for the scenario where a
#'   single fixed variable should not trigger a flag.
#' - In all other cases, it checks if the `vif` exceeds the `vif_threshold`.
#'   If so, it returns TRUE, indicating that the VIF flag should be triggered.
#'
#' @importFrom stringr str_count
#'
#' @examples
#' \dontrun{
#'   determine_vif_flag("fixed", Inf, 5)    # Returns FALSE
#'   determine_vif_flag("flexible", 10, 5)  # Returns TRUE
#'   determine_vif_flag("fixed", 4, 5)      # Returns FALSE
#' }
#'
determine_vif_flag <- function(type, vif, vif_threshold) {
  dplyr::if_else(sum(str_count(type, "fixed")) == 1 & is.infinite(vif), FALSE, vif > vif_threshold)
}

#' Flag P-Values Based on Predefined Thresholds
#'
#' This function evaluates p-values of predictors and flags them based on
#' predefined thresholds specific to their types (intercept, fixed, or flexible).
#'
#' @param type A character string indicating the type of the predictor.
#'             Valid options are 'intercept', 'fixed', or 'flexible'.
#' @param pvalue A numeric value representing the p-value of the predictor.
#' @param pvalue_thresholds A named numeric vector of thresholds for each
#'                          predictor type. The names of the vector should
#'                          be 'intercept', 'fixed', and 'flexible'.
#'
#' @return A logical value; TRUE if the p-value is above the threshold for its
#'         respective type. This function helps in identifying statistically
#'         significant predictors based on their p-values and predefined criteria.
#'
#' @examples
#' \dontrun{
#'   pvalue_thresholds <- c(intercept = 0.05, fixed = 0.05, flexible = 0.1)
#'   determine_pvalue_flag("fixed", 0.04, pvalue_thresholds)     # Returns FALSE
#'   determine_pvalue_flag("flexible", 0.08, pvalue_thresholds)  # Returns TRUE
#'   determine_pvalue_flag("intercept", 0.06, pvalue_thresholds) # Returns TRUE
#' }
#'
determine_pvalue_flag <- function(type, pvalue, pvalue_thresholds) {
  dplyr::if_else(
    type == "intercept", pvalue > pvalue_thresholds["intercept"],
    dplyr::if_else(
      type == "fixed", pvalue > pvalue_thresholds["fixed"],
      pvalue > pvalue_thresholds["flexible"]
    )
  )
}

#' Determine the Expected Sign of Model Coefficients
#'
#' This function assesses whether the coefficients of given variables are
#' expected to be positive or negative. It is particularly useful for variables
#' that may have aggregated names, determining the expected sign based on the
#' segregation of each part of the variable name.
#'
#' @param variable A character string representing the name of the variable
#'                 whose coefficient sign is to be determined. For aggregated
#'                 variable names, the function segregates the parts based on
#'                 the provided delimiter.
#' @param pos_vars A character vector of variable names that are expected to
#'                 have a positive impact. Coefficients of these variables
#'                 are expected to be positive.
#' @param neg_vars A character vector of variable names that are expected to
#'                 have a negative impact. Coefficients of these variables
#'                 are expected to be negative.
#' @param var_agg_delimiter A character string delimiter used in the variable
#'                          names for aggregation purposes. This delimiter
#'                          is used to split the variable names if they are
#'                          aggregated.
#'
#' @return A logical value; returns TRUE if all segregated parts of the variable
#'         are expected to have a positive sign, FALSE if all are expected to
#'         have a negative sign. If the segregated parts of the variable do not
#'         uniformly align with either positive or negative expectations, the
#'         function returns NA.
#'
#' @details
#' In the case of an aggregated variable (a variable name composed of multiple
#' parts separated by the delimiter), the function determines the expected sign
#' based on the segregation of each part. The output will be TRUE only if all
#' segregated parts have an expected positive sign; similarly, it will be FALSE
#' only if all parts have an expected negative sign. If there is any inconsistency
#' among the segregated parts, the expected sign will be NA.
#'
#' @examples
#' \dontrun{
#'   pos_vars <- c("sales", "marketing")
#'   neg_vars <- c("costs", "returns")
#'   determine_expected_sign("sales|Q1", pos_vars, neg_vars, "|") # Returns TRUE
#'   determine_expected_sign("costs|Q1", pos_vars, neg_vars, "|") # Returns FALSE
#'   determine_expected_sign("sales|other", pos_vars, neg_vars, "|") # Returns NA
#' }
#'
determine_expected_sign <- function(variable, pos_vars, neg_vars, var_agg_delimiter) {
  expected_pos <- lapply(stringr::str_split(variable, stringr::fixed(var_agg_delimiter)), `%in%`, pos_vars)
  expected_neg <- lapply(stringr::str_split(variable, stringr::fixed(var_agg_delimiter)), `%in%`, neg_vars)

  dplyr::if_else(
    unlist(lapply(expected_pos, all)), TRUE,
    dplyr::if_else(
      unlist(lapply(expected_pos, any)), NA,
      dplyr::if_else(unlist(lapply(expected_neg, all)), FALSE, NA)
    )
  )
}

#' Summarize Key Statistics from a Linear Model
#'
#' This function extracts and summarizes important statistics from a linear
#' model summary object. It is useful for consolidating key model metrics
#' into a concise format.
#'
#' @param model_summary A summary object from a linear model, typically
#'                      obtained using `summary(lm_model)` where `lm_model`
#'                      is a linear model object.
#' @param loop_id An identifier for the model iteration, allowing for tracking
#'                and comparison of model statistics across different model runs.
#'
#' @return A `tibble` containing key model summary statistics including:
#'         `loop_id` (model iteration identifier), `sigma` (residual standard error),
#'         `r_squared` (R-squared value), `adj_r_squared` (adjusted R-squared value),
#'         and `residuals_all` (list of all residuals from the model).
#'
#' @importFrom stats residuals
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#'   lm_model <- lm(mpg ~ wt + qsec, data = mtcars)
#'   model_sum <- summary(lm_model)
#'   summarize_model(model_sum, loop_id = 1)
#' }
#'
summarize_model <- function(model_summary, loop_id) {
  tibble::tibble(
    loop_id = loop_id,
    sigma = model_summary$sigma,
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared,
    residuals_all = list(residuals(model_summary))
  )
}

#' Identify the Variable to Drop from a Linear Model
#'
#' This function identifies which variable should be dropped from a linear model based
#' on criteria involving the type of variable, p-values, and estimates of the model's
#' coefficients. It is useful in model simplification and refinement processes.
#'
#' @param coef_df A data frame of model coefficients, typically containing columns
#'                like 'Estimate', 'Pr(>|t|)', and 'type', along with flag information.
#' @param pvalue_precision An integer specifying the number of decimal places to
#'                         round the p-values for comparison purposes.
#' @param discard_sign A logical value; if TRUE, the sign of the estimates is
#'                     ignored in the decision-making process.
#' @param highest_estimate A logical value; if TRUE, the variable with the highest
#'                         absolute estimate is considered for dropping.
#' @param run_up_to_flexi_vars An integer indicating the maximum number of flexible
#'                             variables to consider for retention in the final model.
#'
#' @return A character string indicating the name of the variable suggested to be
#'         dropped from the model. The decision is based on p-value magnitude, and
#'         optionally, the estimate magnitude.
#'
#' @details
#' The function performs the following steps:
#' - Filters out variables of type 'flexible' and 'intercept'.
#' - Arranges the remaining variables in descending order of their p-values.
#' - Rounds the p-values to the specified precision.
#' - Adjusts estimates based on the `discard_sign` parameter.
#' - Identifies the variable to be dropped, considering both the p-value and
#'   possibly the estimate magnitude, in accordance with `highest_estimate` and
#'   `run_up_to_flexi_vars` criteria.
#'
#' @importFrom dplyr filter arrange desc mutate row_number n
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   coef_df <- data.frame(
#'     variable = c("Var1", "Var2", "Var3"),
#'     Estimate = c(1.5, -0.3, 2.0),
#'     `Pr(>|t|)` = c(0.04, 0.06, 0.01),
#'     type = c("fixed", "fixed", "flexible"),
#'     flag_pvalue = c(TRUE, FALSE, TRUE),
#'     flag_sign = c(FALSE, TRUE, FALSE),
#'     flag_vif = c(FALSE, TRUE, FALSE)
#'   )
#'   identify_drop_variable(coef_df, 2, TRUE, FALSE, 1)
#' }
#'
identify_drop_variable <- function(coef_df, pvalue_precision, discard_sign, highest_estimate, run_up_to_flexi_vars) {
  # Using .data pronoun for non-standard evaluation inside dplyr verbs
  drop_var <- coef_df %>%
    dplyr::filter(.data[["type"]] %in% c("flexible", "intercept")) %>%
    dplyr::mutate(
      `Pr(>|t|)` = round(dplyr::if_else(.data[["type"]]=="intercept",.data[["Pr(>|t|)"]]-1,.data[["Pr(>|t|)"]]), pvalue_precision),
      Estimate = if(discard_sign) abs(.data[["Estimate"]]) else .data[["Estimate"]],
      flag_flag = rowSums(dplyr::across(c("flag_pvalue", "flag_sign", "flag_vif")), na.rm = T) != 0) %>%
    dplyr::arrange(dplyr::desc(.data[["flag_flag"]]),dplyr::desc(.data[["Pr(>|t|)"]]),if(highest_estimate) dplyr::desc(.data[["Estimate"]]) else .data[["Estimate"]]) %>%
    dplyr::mutate(to_drop = .data[["flag_flag"]] | (dplyr::n() > run_up_to_flexi_vars)) %>%
    dplyr::filter(.data[["to_drop"]]) %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data[["variable"]])

  drop_var
}

#' Update Linear Model by Dropping a Variable
#'
#' This function updates an existing linear model by removing a specified
#' variable. It is particularly useful for model refinement and exploration
#' of model variants. The function can also remove the model's intercept if
#' "(Intercept)" is specified as the variable to drop.
#'
#' @param model An object of class \code{\link[stats]{lm}} or similar,
#'              representing the fitted model to be updated.
#' @param model_data A dataframe containing the data that the model was
#'                   originally fitted with. This dataframe must contain all
#'                   the columns used in the model.
#' @param variable_to_drop A character string indicating the name of the
#'                         variable to be dropped from the model. Passing
#'                         "(Intercept)" will update the model to fit without
#'                         an intercept.
#'
#' @return An object of the same class as \code{model}, representing the
#'         updated model without the specified variable or intercept.
#'
#' @importFrom stats formula
#'
#' @examples
#' \dontrun{
#'   data(mtcars)
#'   initial_model <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#'   updated_model <- update_model(initial_model, mtcars, "gear")
#'
#'   # To remove the intercept:
#'   updated_model_no_intercept <- update_model(initial_model, mtcars, "(Intercept)")
#' }
#'
update_model <- function(model, model_data, variable_to_drop) {
  formula_update <- if (variable_to_drop == "(Intercept)") ". ~ . - 1" else paste(". ~ . -", variable_to_drop)
  stats::update(model, formula(formula_update), data = model_data)
}

#' Cleanse Model Singularity
#'
#' Removes singular variables from a linear regression model to enhance model
#' stability. This is particularly useful when dealing with multicollinearity
#' or redundant predictors in a model.
#'
#' @param lm_model The linear regression model (of class \code{\link[stats]{lm}})
#'                 to be cleansed.
#' @param model_data The data frame used to fit \code{lm_model}. It must
#'                   contain all variables used in the model.
#' @param flexible_variables A character vector specifying the names of
#'                           flexible variables in the model. Only these
#'                           variables are considered for removal.
#' @param round_digits Integer value indicating the number of digits for
#'                     rounding in the process of identifying order of
#'                     variables to be dropped due to singular dependencies.
#' @param verbose Logical; if TRUE, the function prints the name of the
#'                variable being dropped in each iteration.
#'
#' @return An updated linear regression model object of the same class as
#'         \code{lm_model}, with singular variables removed.
#'
#' @examples
#' \dontrun{
#'   advertising <- data.frame(tv = runif(10), radio = runif(10), newspaper = runif(10))
#'   event <- data.frame(event1 = runif(10), event2 = runif(10))
#'   Sales <- runif(10)
#'
#'   lm_model <- lm(Sales ~ ., data = cbind(advertising, event))
#'   cleansed_model <- cleanse_model_singularity(lm_model, cbind(advertising, event),
#'    names(event), round_digits = 2, verbose = TRUE)
#' }
#'
cleanse_model_singularity <- function(lm_model, model_data, flexible_variables, round_digits = 2, verbose = FALSE) {
  model_vars <- names(stats::coef(lm_model))

  # Filter flexible variables present in the model
  flexible_variables <- flexible_variables[flexible_variables %in% model_vars]

  repeat {
    aliased_info <- summary(lm_model)$aliased
    singular_vars <- names(aliased_info)[aliased_info]
    singular_vars <- intersect(singular_vars, flexible_variables)

    # Break if no singular variables
    if (length(singular_vars) == 0) break

    # Calculate linear dependency
    linear_dependency <- apply(stats::alias(lm_model)[[2]], 1, function(x) sum(abs(x[round(abs(x), 1) == 1])))
    singular_vars_dependency <- linear_dependency[names(linear_dependency) %in% singular_vars]
    drop_var <- names(sort(singular_vars_dependency, decreasing = TRUE))[1]

    # Verbose output
    if (verbose) {
      message("Dropping variable due to singularity: ", drop_var)
    }

    # Update model by removing the most dependent variable
    lm_model <- stats::update(lm_model, formula = stats::as.formula(paste(". ~ . -", drop_var)), data = model_data)
  }

  lm_model
}

#' Cleanse Model Perfect Fit
#'
#' Removes variables causing a perfect fit in a linear regression model to prevent
#' issues like overfitting. This is particularly useful in scenarios where a model
#' might be overly complex due to too many variables or multicollinearity.
#'
#' @param lm_model The linear regression model (of class \code{\link[stats]{lm}})
#'                 to be cleansed from variables causing a perfect fit.
#' @param model_data The data frame used to fit \code{lm_model}. It must
#'                   contain all the variables used in the model.
#' @param flexible_variables A character vector specifying the names of
#'                           flexible variables in the model. Only these
#'                           variables are considered for removal.
#' @param drop_highest_estimate Logical; if TRUE, the variable with the highest
#'                              (or lowest, if `ignore_estimate_sign` is TRUE)
#'                              estimate is removed from the model.
#' @param ignore_estimate_sign Logical; if TRUE, the absolute value of the
#'                             estimates is considered in determining which
#'                             variable to drop.
#'
#' @return An updated linear regression model object of the same class as
#'         \code{lm_model}, with variables causing a perfect fit removed.
#'
#' @examples
#' \dontrun{
#'   advertising <- data.frame(tv = runif(10), radio = runif(10),
#'    newspaper = runif(10))
#'   event <- data.frame(event1 = runif(10), event2 = runif(10))
#'   Sales <- runif(10)
#'
#'   lm_model <- lm(Sales ~ ., data = cbind(advertising, event))
#'   cleansed_model <- cleanse_model_perfect_fit(lm_model, model_data = cbind(advertising, event),
#'     flexible_variables = names(event), drop_highest_estimate = TRUE,
#'     ignore_estimate_sign = TRUE)
#' }
#'
cleanse_model_perfect_fit <- function(lm_model, model_data, flexible_variables,
                                      drop_highest_estimate = TRUE,
                                      ignore_estimate_sign = TRUE) {
  model_summary <- summary(lm_model)

  # Check for no residual degree of freedom
  if (model_summary$df[2] != 0) {
    return(lm_model)
  }

  # Extract and optionally modify estimates for flexible variables
  model_estimate <- stats::coef(lm_model)[flexible_variables]
  if (ignore_estimate_sign) {
    model_estimate <- abs(model_estimate)
  }

  # Determine variable to drop based on estimates
  drop_var <- if (drop_highest_estimate) {
    names(which.min(model_estimate))
  } else {
    names(which.max(model_estimate))
  }

  # Update the model formula by removing the identified variable
  lm_model <- stats::update(lm_model,
                            formula = stats::as.formula(paste(". ~ . -", drop_var)), data = model_data)

  lm_model
}

#' Calculate Variance Inflation Factor (VIF)
#'
#' Computes the Variance Inflation Factor (VIF) for each predictor variable in a
#' linear regression model. This function is useful for assessing multicollinearity.
#' In cases where VIF calculation fails (e.g., due to severe multicollinearity),
#' it returns `Inf` for the affected predictor variables.
#'
#' @param model A `lm` or `glm` object representing the fitted linear regression
#'              model. The model must be of class 'lm' or 'glm'.
#'
#' @return A numeric vector containing the VIF values for each predictor variable
#'         in the model. If the model includes an intercept, its VIF is represented
#'         as `NA`. In case of calculation errors, `Inf` is returned for all
#'         predictor variables.
#'
#' @examples
#' \dontrun{
#'   advertising <- data.frame(tv = runif(10), radio = runif(10), newspaper = runif(10),
#'    sales = runif(10))
#'   lm_model <- lm(sales ~ tv + radio + newspaper, data = advertising)
#'   vif_values <- calculate_vif(lm_model)
#'   print(vif_values)
#' }
#'
#' @importFrom stats coef
#' @importFrom car vif
#'
calculate_vif <- function(model) {
  # Checking if the model is a linear model
  if (!inherits(model, c("lm", "glm"))) {
    stop("Input must be a linear model object of class 'lm' or 'glm'.")
  }

  # Check if the model includes an intercept
  coef_model <- stats::coef(model)
  has_intercept <- !is.na(coef_model["(Intercept)"])

  # Calculate VIF, handling potential errors
  vif_values <- tryCatch({
    # Suppress warnings to handle multicollinearity gracefully
    suppressWarnings(car::vif(model))
  }, error = function(e) {
    # Return Inf if error (typically due to multicollinearity)
    warning("An error occurred in calculating VIF. Returning Inf for all coefficients. Error: ", e$message)
    rep(Inf, length(coef_model) - if (has_intercept) 1 else 0)
  })

  # Add intercept with NA if model has an intercept, otherwise use VIF values directly
  if (has_intercept) {
    vif_result <- c(NA, vif_values)
  } else {
    vif_result <- vif_values
  }
  names(vif_result) <- names(coef_model)

  return(vif_result)
}

#' Get Base Model After Iteratively Dropping Variables
#'
#' Iteratively updates a linear model by dropping variables based on criteria such as
#' high p-values, high VIF, and expected sign of coefficients. The process continues
#' until no more variables meet the criteria for exclusion, refining the model.
#'
#' @param lm_model A linear model object (of class \code{\link[stats]{lm}}) to be processed.
#' @param model_data Data used for fitting \code{lm_model}.
#' @param independent_var_info A data frame with information about independent
#'   variables in the model. It should contain columns for variable names, adstock,
#'   power, lag, and the type of variable (e.g., "flexible", "fixed").
#' @param pos_vars Character vector of variables expected to have a positive
#'   relationship with the response.
#' @param neg_vars Character vector of variables expected to have a negative
#'   relationship with the response.
#' @param var_agg_delimiter Delimiter used in variable names for aggregation
#'   (default is "\\|").
#' @param run_up_to_flexi_vars Integer indicating the number of "flexible"
#'   variables to consider for retention (default is 10).
#' @param vif_threshold Threshold for VIF above which variables are considered
#'   for removal (default is 10).
#' @param pvalue_thresholds Named numeric vector of p-value thresholds for
#'   different types of variables (default is c(intercept = 0.15, fixed = 0.15, flexible = 0.15)).
#' @param drop_pvalue_precision Integer for rounding p-values in the decision
#'   process (default is 2).
#' @param discard_estimate_sign Logical; if TRUE, the sign of estimates is
#'   disregarded in the decision process (default is TRUE).
#' @param drop_highest_estimate Logical; if TRUE, always drop the variable
#'   with the highest estimate (default is FALSE).
#' @param get_model_object Logical; if TRUE, returns the final model object
#'   (default is FALSE).
#'
#' @return A list containing the data frame of model coefficients, the data frame
#'   of model summary statistics, and optionally the final model object if
#'   \code{get_model_object} is TRUE.
#'
#' @examples
#' \dontrun{
#'   advertising <- data.frame(tv = runif(10), radio = runif(10), newspaper = runif(10),
#'    sales = runif(10))
#'   event <- data.frame(event1 = runif(10), event2 = runif(10))
#'   lm_model <- lm(sales ~ tv + radio + newspaper + event1 + event2, data = cbind(advertising, event))
#'   independent_var_info <- data.frame(variable = c("tv", "radio", "newspaper", "event1", "event2"),
#'                                      type = c("fixed", "fixed", "fixed", "flexible", "flexible"),
#'                                      adstock = rep(NA, 5), power = rep(NA, 5), lag = rep(NA, 5))
#'   base_model <- get_base_model(lm_model, cbind(advertising, event), independent_var_info,
#'                                pos_vars = c("tv", "radio"), neg_vars = c("newspaper"),
#'                                get_model_object = TRUE)
#' }
#'
#' @importFrom dplyr tibble right_join mutate bind_rows
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
#'
get_base_model <- function(lm_model, model_data, independent_var_info, pos_vars, neg_vars,
                           var_agg_delimiter = "\\|", run_up_to_flexi_vars = 10,
                           vif_threshold = 10, pvalue_thresholds = c(intercept = 0.15, fixed = 0.15, flexible = 0.15),
                           drop_pvalue_precision = 2, discard_estimate_sign = TRUE, drop_highest_estimate = FALSE,
                           get_model_object = FALSE) {
  # Cleanse model for singularity and perfect fit
  lm_model <- cleanse_model_singularity(
    lm_model, model_data,
    independent_var_info$variable[independent_var_info$type == "flexible"],
    round_digits = 2
  )
  lm_model <- cleanse_model_perfect_fit(
    lm_model, model_data,
    independent_var_info$variable[independent_var_info$type == "flexible"],
    drop_highest_estimate = FALSE,
    ignore_estimate_sign = TRUE
  )

  # Initialize loop and data frames to store results
  loop_id <- 1
  model_coef_all <- tibble::tibble()
  model_smry_all <- tibble::tibble()

  # Begin variable dropping loop
  repeat {
    # Get summary of the current model
    lm_model_smry <- summary(lm_model)
    # Determine coefficients and flags
    model_coef <- independent_var_info %>%
      dplyr::right_join(
        as.data.frame(lm_model_smry$coefficients) %>%
          tibble::rownames_to_column('variable') %>%
          dplyr::mutate(variable = stringr::str_replace_all(.data$variable, "`", ""),
                        vif = calculate_vif(lm_model)) %>%
          tibble::tibble(), by = "variable"
      ) %>%
      dplyr::mutate(
        loop_id = loop_id,
        flag_pvalue = determine_pvalue_flag(.data$type, .data$`Pr(>|t|)`, pvalue_thresholds),
        expected_sign = determine_expected_sign(.data$variable, pos_vars, neg_vars, var_agg_delimiter),
        flag_sign = (.data$Estimate > 0) != .data$expected_sign,
        flag_vif = determine_vif_flag(.data$type, .data$vif, vif_threshold)
      ) %>%
      dplyr::select(-"expected_sign")

    # Accumulate results
    model_coef_all <- dplyr::bind_rows(model_coef_all, model_coef)
    model_smry_all <- dplyr::bind_rows(model_smry_all, summarize_model(lm_model_smry, loop_id))

    # Identify variable to drop based on the current model
    variable_to_drop <- identify_drop_variable(
      model_coef,
      drop_pvalue_precision,
      discard_estimate_sign,
      drop_highest_estimate,
      run_up_to_flexi_vars
    )

    # Break loop if no variable is identified to drop
    if (is.na(variable_to_drop) || !length(variable_to_drop)) {
      break
    }

    # Update loop counter and model
    loop_id <- loop_id + 1
    lm_model <- update_model(lm_model, model_data, variable_to_drop)
  }

  # Return results
  if (get_model_object) {
    coef_smry_lm <- list(model_coef_all, model_smry_all, lm_model)
  } else {
    coef_smry_lm <- list(model_coef_all, model_smry_all, NA)
  }

  return(coef_smry_lm)
}
