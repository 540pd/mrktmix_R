#' Determine VIF Flag
#'
#' This function determines the VIF flag for predictors based on their type and given VIF value.
#'
#' @param type Character string indicating the type of the variable (either 'fixed' or 'flexible').
#' @param vif Numeric VIF value for the predictor.
#' @param vif_threshold Threshold above which the VIF flag should be triggered.
#'
#' @return Logical; TRUE if the VIF is greater than the threshold. If there is only one 'fixed' variable and the VIF is infinite, then it will return FALSE.
#' @importFrom stringr str_count
#' @export
determine_vif_flag <- function(type, vif, vif_threshold) {
  dplyr::if_else(sum(str_count(type, "fixed")) == 1 & is.infinite(vif), FALSE, vif > vif_threshold)
}

#' Determine P-Value Flag
#'
#' Flags p-values based on predefined thresholds for different types of predictors.
#'
#' @param type Character string indicating the type of the predictor (can be 'intercept', 'fixed', or 'flexible').
#' @param pvalue Numeric p-value of the predictor.
#' @param pvalue_thresholds Named numeric vector of thresholds for each predictor type. Names can be 'intercept', 'fixed', and 'flexible'.
#'
#' @return Logical; TRUE if the p-value is above the threshold for its type.
determine_pvalue_flag <- function(type, pvalue, pvalue_thresholds) {
  dplyr::if_else(
    type == "intercept", pvalue >= pvalue_thresholds["intercept"],
    dplyr::if_else(
      type == "fixed", pvalue >= pvalue_thresholds["fixed"],
      pvalue >= pvalue_thresholds["flexible"]
    )
  )
}

#' Determine Expected Sign of Coefficients
#'
#' Determines the expected sign (positive or negative) of model coefficients.
#'
#' @param variable Character string of variable names.
#' @param pos_vars Character vector of variables expected to have a positive sign.
#' @param neg_vars Character vector of variables expected to have a negative sign.
#' @param var_agg_delimiter Character string delimiter used in variable names for aggregation.
#'
#' @return Logical; TRUE if the variable is expected to have a positive sign, FALSE if expected to have a negative sign, NA otherwise.
determine_expected_sign <- function(variable, pos_vars, neg_vars, var_agg_delimiter) {
  expected_pos <- lapply(stringr::str_split(variable, var_agg_delimiter), `%in%`, pos_vars)
  expected_neg <- lapply(stringr::str_split(variable, var_agg_delimiter), `%in%`, neg_vars)

  dplyr::if_else(
    unlist(lapply(expected_pos, all)), TRUE,
    dplyr::if_else(
      unlist(lapply(expected_pos, any)), NA,
      dplyr::if_else(unlist(lapply(expected_neg, all)), FALSE, NA)
    )
  )
}

#' Summarize Model Statistics
#'
#' Summarizes important statistics from a linear model.
#'
#' @param model_summary A summary object from a linear model.
#' @param loop_id Identifier for the model iteration.
#' @importFrom stats residuals
#'
#' @return A tibble with model summary statistics including loop_id, sigma, r_squared, adj_r_squared, and residuals.
summarize_model <- function(model_summary, loop_id) {
  tibble::tibble(
    loop_id = loop_id,
    sigma = model_summary$sigma,
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared,
    residuals_all = list(residuals(model_summary))
  )
}

#' Identify Variable to Drop from the Model
#'
#' Identifies which variable should be dropped from the model based on p-value and estimation criteria.
#'
#' @param coef_df Data frame of model coefficients.
#' @param pvalue_precision Integer; number of decimal places to round p-values to.
#' @param discard_sign Logical; whether to discard the sign of estimates in the decision process.
#' @param highest_estimate Logical; whether to consider the highest estimate for dropping.
#' @param run_up_to_flexi_vars Integer; up to how many flexible variables to consider for keeping in the final model.
#'
#' @return Character; the name of the variable suggested to be dropped.
#'
#' @importFrom dplyr filter arrange desc mutate row_number n
#' @importFrom magrittr %>%
#' @importFrom rlang .data
identify_drop_variable <- function(coef_df, pvalue_precision, discard_sign, highest_estimate, run_up_to_flexi_vars) {
  # Using .data pronoun for non-standard evaluation inside dplyr verbs
  drop_var <- coef_df %>%
    dplyr::filter(.data[["type"]] %in% c("flexible", "intercept")) %>%
    dplyr::arrange(dplyr::desc(.data[["Pr(>|t|)"]])) %>%
    dplyr::mutate(
      `Pr(>|t|)` = round(.data[["Pr(>|t|)"]], pvalue_precision),
      Estimate = if(discard_sign) abs(.data[["Estimate"]]) else .data[["Estimate"]],
      to_drop = dplyr::row_number() == 1 | dplyr::n() > run_up_to_flexi_vars
    ) %>%
    dplyr::filter(.data[["to_drop"]]) %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data[["variable"]])

  drop_var
}


#' Update Linear Model by Dropping a Variable
#'
#' This function updates a given linear model by removing a specified variable.
#' The model is re-fitted without the specified variable or the intercept if
#' "(Intercept)" is passed as the variable to drop.
#'
#' @param model An object of class \code{\link[stats]{lm}} or similar, representing
#'   the fitted model to be updated.
#' @param model_data A dataframe containing the data that the model was originally
#'   fitted with. It must contain all the necessary columns used by the model.
#' @param variable_to_drop A character string indicating the name of the variable
#'   to be dropped from the model. If "(Intercept)" is provided, the model will be
#'   updated without an intercept.
#'
#' @return An object of the same class as \code{model}, representing the updated
#'   model without the specified variable or intercept.
#' @importFrom stats formula
#' @export
#' @examples
#' \dontrun{
#'   data(mtcars)
#'   initial_model <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#'   updated_model <- update_model(initial_model, mtcars, "gear")
#'
#'   # To remove the intercept:
#'   updated_model_no_intercept <- update_model(initial_model, mtcars, "(Intercept)")
#' }
update_model <- function(model, model_data, variable_to_drop) {
  formula_update <- if (variable_to_drop == "(Intercept)") ". ~ . - 1" else paste(". ~ . -", variable_to_drop)
  stats::update(model, formula(formula_update), data = model_data)
}

#' Cleanse Model Singularity
#'
#' This function removes singular variables from a linear regression model to improve model stability.
#'
#' @param lm_model The linear regression model to be cleansed.
#' @param model_data Data used for modeling i.e. lm_model
#' @param flexible_variables A character vector specifying the flexible variables in the model.
#' @param round_digits Number of digits to round to identify order of variables dropped with singular dependencies.
#' @param verbose Logical; if TRUE, prints the variable being dropped in each iteration.
#'
#' @return A linear regression model with singular variables removed.
#'
#' @examples
#' \dontrun{
#'   lm_model <- lm(Sales ~ ., data = cbind(advertising, event))
#'   cleanse_model_singularity(lm_model, names(event), round_digits = 2, verbose = TRUE)
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
#' This function removes variables causing a perfect fit in a linear regression
#' model, which can lead to issues like over-fitting.
#'
#' @param lm_model The linear regression model to be cleansed.
#' @param model_data Data used for modeling i.e. lm_model.
#' @param flexible_variables A character vector specifying the flexible variables
#'   in the model.
#' @param drop_highest_estimate Logical indicating whether to drop the variable
#'   with the highest estimate.
#' @param ignore_estimate_sign Logical indicating whether to ignore the sign of
#'   the estimate while dropping variables.
#'
#' @return A linear regression model with variables causing a perfect fit
#'   removed.
#'
#' @examples
#' \dontrun{
#'   lm_model <- lm(Sales ~ ., data = cbind(advertising, event))
#'   cleanse_model_perfect_fit(lm_model, model_data = cbind(advertising, event),
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
#' This function calculates the VIF for a given linear regression model. It returns a numeric vector containing the VIF values for each predictor variable. If the calculation fails (e.g., due to multicollinearity issues), it returns `Inf` for all predictor variables.
#'
#' @param model A `lm` or `glm` object representing the fitted linear regression model.
#'
#' @return A numeric vector containing the VIF values for each predictor variable in the model.
#'
#' @examples
#' \dontrun{
#'   lm_model <- lm(Sales ~ ., data = advertising)
#'   calculate_vif(lm_model)
#' }
#'
calculate_vif <- function(model) {
  # Checking if the model is a linear model
  if (!inherits(model, c("lm", "glm"))) {
    stop("Input must be a linear model object of class 'lm' or 'glm'.")
  }

  # Check if the model includes an intercept
  coef_model<-stats::coef(model)
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
#' This function iteratively updates a linear model by dropping variables based on
#' several criteria such as high p-values, high variance inflation factors (VIF),
#' and the expected sign of the coefficients until no more variables meet the
#' criteria for exclusion.
#'
#' @param lm_model The linear model object to be processed.
#' @param model_data Data used for modeling i.e. lm_model
#' @param independent_var_info A data frame containing information about the
#' independent variables in the model. It should contain columns for the variable,
#' adstock, power, lag, and the type of variable being used in the model.
#' @param pos_vars Character vector specifying which variables are expected to have
#' a positive relationship with the response.
#' @param neg_vars Character vector specifying which variables are expected to have
#' a negative relationship with the response.
#' @param var_agg_delimiter Delimiter used in variable names for aggregation
#' (default is "\\|").
#' @param run_up_to_flexi_vars Integer indicating how many "flexible" variables to
#' consider for retention in the final model (default is 10). This condition is
#' checked only after a successful model is generated.
#' @param vif_threshold The threshold for the variance inflation factor (VIF) above
#' which variables will be considered for removal (default is 10).
#' @param pvalue_thresholds Named numeric vector of p-value thresholds for
#' "intercept", "fixed", and "flexible" variables (default is
#' c(intercept = 0.15, fixed = 0.15, flexible = 0.15)).
#' @param drop_pvalue_precision Integer indicating the number of decimal places to
#' round p-values to for the decision process (default is 2). This dilutes the
#' p-values' weightage in choosing which variable to be dropped.
#' @param discard_estimate_sign Logical indicating whether to disregard the sign of
#' estimates in the decision process (default is TRUE). This guides if the sign of
#' the Estimate should be considered while dropping a variable.
#' @param drop_highest_estimate Logical indicating whether to always drop the
#' variable with the highest estimate (default is FALSE).
#' @param get_model_object Logical indicating whether to return the final model
#' object (default is FALSE).
#' @return A list containing the data frame of model coefficients, the data frame of
#' model summary statistics, and optionally the final model object if
#' get_model_object is TRUE.
#' @export
#' @examples
#' \dontrun{
#' # lm_model is an lm object that has been fitted previously.
#' # independent_var_info is a data frame with variables and types.
#' # Example usage:
#' get_base_model(lm_model, independent_var_info, pos_vars = c("var1", "var2"),
#'                neg_vars = c("var3", "var4"), get_model_object = TRUE)
#' }
#' @importFrom dplyr tibble
#' @importFrom dplyr right_join
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
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
          dplyr::mutate(vif = calculate_vif(lm_model)) %>%
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
