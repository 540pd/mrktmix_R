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
summarize_model_ <- function(model_summary) {
  tibble::tibble(
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
identify_drop_variable <-
  function(coef_df,
           pvalue_precision,
           discard_sign,
           highest_estimate,
           run_up_to_flexi_vars) {
    coef_df <-
      coef_df[coef_df$type != "fixed", c("type",
                                         "variable",
                                         "Pr(>|t|)",
                                         "Estimate",
                                         "flag_pvalue",
                                         "flag_sign",
                                         "flag_vif")]
    coef_df$flag_sum = rowSums(coef_df[, c("flag_pvalue", "flag_sign", "flag_vif")], na.rm = TRUE) != 0
    coef_df$"Pr(>|t|)"[coef_df$type == "intercept"] <-
      coef_df$"Pr(>|t|)"[coef_df$type == "intercept"] - 1
    coef_df$"Pr(>|t|)" <-
      round(coef_df$"Pr(>|t|)", pvalue_precision)
    if (discard_sign) {
      coef_df$Estimate <- abs(coef_df$Estimate)
    }
    if (highest_estimate) {
      coef_order <- with(coef_df, order(-flag_sum, -`Pr(>|t|)`, -Estimate))
    } else {
      coef_order <- with(coef_df, order(-flag_sum, -`Pr(>|t|)`, Estimate))
    }
    coef_df <- coef_df[coef_order,]

    if (sum(coef_df$flag_sum, na.rm = T) ||
        (nrow(coef_df) > run_up_to_flexi_vars)) {
      drop_var <- coef_df$variable[1]
    } else {
      drop_var <- NA
    }
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
  formula_update <-
    if (variable_to_drop == "(Intercept)")
      ". ~ . - 1"
  else
    paste(". ~ . -", variable_to_drop)
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
cleanse_model_singularity <-
  function(lm_model,
           model_data,
           flexible_variables,
           round_digits = 2,
           verbose = FALSE) {
    model_vars <- names(stats::coef(lm_model))

    # Filter flexible variables present in the model
    flexible_variables <-
      flexible_variables[flexible_variables %in% model_vars]

    repeat {
      aliased_info <- summary(lm_model)$aliased
      singular_vars <- names(aliased_info)[aliased_info]
      singular_vars <- intersect(singular_vars, flexible_variables)

      # Break if no singular variables
      if (length(singular_vars) == 0)
        break

      # Calculate linear dependency
      linear_dependency <-
        apply(stats::alias(lm_model)[[2]], 1, function(x)
          sum(abs(x[round(abs(x), 1) == 1])))
      singular_vars_dependency <-
        linear_dependency[names(linear_dependency) %in% singular_vars]
      drop_var <-
        names(sort(singular_vars_dependency, decreasing = TRUE))[1]

      # Verbose output
      if (verbose) {
        message("Dropping variable due to singularity: ", drop_var)
      }

      # Update model by removing the most dependent variable
      lm_model <-
        stats::update(lm_model,
                      formula = stats::as.formula(paste(". ~ . -", drop_var)),
                      data = model_data)
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
cleanse_model_perfect_fit <-
  function(lm_model,
           model_data,
           flexible_variables,
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
                              formula = stats::as.formula(paste(". ~ . -", drop_var)),
                              data = model_data)

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
    warning(
      "An error occurred in calculating VIF. Returning Inf for all coefficients. Error: ",
      e$message
    )
    rep(Inf, length(coef_model) - if (has_intercept)
      1
      else
        0)
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

#' Iteratively Refine Linear Model by Dropping Variables
#'
#' This function iteratively updates a linear model by dropping variables based on several
#' criteria such as expected sign of coefficients, critical p-values, and VIF (Variance
#' Inflation Factor). Variables are dropped until no more meet the criteria for exclusion,
#' refining the model at each step.
#'
#' @param lm_model A linear model object (of class \code{\link[stats]{lm}}).
#' @param model_data Dataset used for fitting the \code{lm_model}.
#' @param expected_pos_sign Named numeric vector indicating the expected sign for each variable.
#' @param critical_pvalue Named numeric vector indicating critical p-value threshold for determining variable significance.
#' @param critical_vif Named numeric vector indicating critical VIF threshold for assessing multicollinearity.
#' @param flexi_vars Character vector of variable names considered as flexible in the model.
#' @param run_up_to_flexi_vars Integer; number of flexible variables to consider for retention.
#' @param drop_pvalue_precision Integer; number of decimal places to round the p-values to.
#' @param discard_estimate_sign Logical; if TRUE, the sign of the estimates is disregarded.
#' @param drop_highest_estimate Logical; if TRUE, the variable with the highest estimate is dropped.
#' @param defer_intercept_test Logical; if TRUE, the intercept test is deferred.
#' @param always_check_vif Logical; if TRUE, VIF is always checked.
#'
#' @return A list containing two elements: a list of linear model objects after each iteration
#'         and a list of VIF values for each iteration.
#'
#' @examples
#' \dontrun{
#'   # Assuming lm_model and model_data are predefined
#'   expected_pos_sign <- c(var1 = 1, var2 = -1) # Example
#'   result <- get_base_model(lm_model, model_data, expected_pos_sign, 0.05, 5,
#'                            c("var1", "var2"), 10, 2, TRUE, FALSE, FALSE, TRUE)
#' }
#' @export
#' @importFrom stats lm
#' @importFrom dplyr mutate
#' @importFrom purrr map
get_base_model <- function(lm_model,
                            model_data,
                            expected_pos_sign,
                            critical_pvalue,
                            critical_vif,
                            flexi_vars,
                            run_up_to_flexi_vars,
                            drop_pvalue_precision,
                            discard_estimate_sign,
                            drop_highest_estimate,
                            defer_intercept_test,
                            always_check_vif) {
  lm_accumulator <- list()
  vif_accumulator <- list()
  # Begin variable dropping loop
  repeat {
    lm_accumulator <- c(lm_accumulator, list(lm_model))

    # Get summary of the current model
    lm_model_smry <- summary(lm_model)

    model_coef <- lm_model_smry$coefficients
    rownames(model_coef) <- gsub("`", "", rownames(model_coef))

    model_coef <-
      cbind(
        model_coef,
        sign_flag = (model_coef[, 1] > 0) != compare_named_vectors(model_coef[, 1], expected_pos_sign)
      )
    model_coef <-
      cbind(
        model_coef,
        pvalue_flag = model_coef[, 4] >= compare_named_vectors(model_coef[, 4], critical_pvalue)
      )
    if (always_check_vif ||
        sum(c(model_coef[,"pvalue_flag"], model_coef[,"sign_flag"]),na.rm=T)==0) {
      mdl_vif <- calculate_vif(lm_model)
    } else {
      mdl_vif <-
        setNames(replicate(nrow(model_coef), NA), rownames(model_coef))
    }
    vif_accumulator <- c(vif_accumulator, list(mdl_vif))
    model_coef <- cbind(model_coef, vif_flag = mdl_vif >= compare_named_vectors(mdl_vif, critical_vif))

    model_coef <-
      cbind(model_coef, flag_sum = rowSums(model_coef[, 5:7], na.rm = T))

    # If sign flag is T, reverse pvalue value flag
    model_coef[, 6] <- (model_coef[, 5] + model_coef[, 6]) %% 2
    # If sign flag is T, pvalue = 1- pvalue
    model_coef[, 4] <-
      abs(ifelse(is.na(model_coef[, 5]), 0, model_coef[, 5]) - model_coef[, 4])
    if (defer_intercept_test) {
      model_coef[rownames(model_coef) %in% "(Intercept)", 4] <-
        model_coef[rownames(model_coef) %in% "(Intercept)", 4] * (-1)
    }

    model_coef <-
      model_coef[rownames(model_coef) %in% flexi_vars, , drop = FALSE]

    model_coef <-
      model_coef[order(
        -model_coef[, "flag_sum"],
        -model_coef[, "sign_flag"],-round(model_coef[, 4], drop_pvalue_precision),
        if (discard_estimate_sign &&
            drop_highest_estimate)
          - abs(model_coef[, 1])
        else
          if (discard_estimate_sign &&
              !drop_highest_estimate)
            abs(model_coef[, 1])
        else
          if (!discard_estimate_sign &&
              drop_highest_estimate)
            - (model_coef[, 1])
        else
          if (!discard_estimate_sign &&
              !drop_highest_estimate)
            (model_coef[, 1])
      ), , drop = FALSE]

    variable_to_drop <-
      rownames(model_coef[model_coef[, "flag_sum"] != 0, , drop = FALSE])[1]
    if (is.null(variable_to_drop)) {
      model_coef_flexi_flag <- !is.na(model_coef[, "pvalue_flag", drop = FALSE])
      if (run_up_to_flexi_vars < sum(model_coef_flexi_flag)) {
        variable_to_drop <-
          rownames(model_coef[model_coef_flexi_flag, , drop = FALSE])[1]
      }
    }

    # Break loop if no variable is identified to drop
    if (is.null(variable_to_drop)) {
      break
    }

    # Update loop counter and model
    lm_model <-
      update_model(lm_model, model_data, variable_to_drop)
  }

  return(list(lm_accumulator, vif_accumulator))
}
