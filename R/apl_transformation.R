#' Apply Adstock, Power, and Lag Transformation to a Data Frame
#'
#' This function applies the Adstock, Power, and Lag transformation to a given data frame.
#'
#' @param modeling_df A data frame containing the input variables.
#' @param adstock The adstock rate for the transformation.
#' @param power The power transformation to capture the non-linear effectiveness of marketing activity.
#' @param lag The lag parameter for the time lag in the transformation.
#'
#' @return A data frame with Adstock, Power and Lag transformed values.
#'
#' @importFrom dplyr lag
#'
#' @details
#' The function applies the Adstock transformation using a recursive filtering
#' method and raises the filtered values to the power specified. It then lags
#' the result by the specified time lag.
#'
#' @examples
#' \dontrun{
#'   compute_apl_values(advertising, adstock = 0.8, power = 2, lag = 1)
#'   compute_apl_values(advertising[,"Sales",drop=F], adstock = 0.8, power = 2, lag = 1)
#' }
#'
#' @export
compute_apl_values <- function(modeling_df, adstock, power, lag) {
  transformed_df <- apply(stats::filter(modeling_df, adstock, method = "recursive")^power, 2, dplyr::lag, lag)
  dimnames(transformed_df) <- list(row.names(modeling_df), names(modeling_df))
  return(as.data.frame(transformed_df))
}

#' Apply Adstock, Power, and Lag Vectorized Transformation to a Data Frame
#'
#' This function applies vectorized Adstock, Power, and Lag transformations to a given data frame.
#'
#' @param modeling_df A data frame containing the input variables.
#' @param adstock A vector of adstock rates for the transformations.
#' @param power A vector of power transformations to capture the non-linear effectiveness of marketing activity.
#' @param lag A vector of lag parameters for the time lag in the transformations.
#' @param apl_delimeter Delimiter used to separate Adstock, Power, and Lag values in resulting column names.
#' @param delimeter Delimiter used to separate original column name and apl in output data frame.
#'
#' @return A data frame with vectorized Adstock, Power, and Lag-transformed values.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#'
#' @examples
#' \dontrun{
#'   generate_apl_dataframe(advertising, adstock = c(0.8, 0.9), power = c(2, 3), lag = c(1, 2))
#' }
#'
#' @export
generate_apl_dataframe <- function(modeling_df, adstock, power, lag, apl_delimeter = "_", delimeter = "|") {
  transformed_df_list <- purrr::map(adstock, function(adstock_) {
    purrr::map(power, function(power_) {
      purrr::map(lag, function(lag_) {
        stats::setNames(
          list(compute_apl_values(modeling_df, adstock_, power_, lag_)),
          paste(adstock_, power_, lag_, sep = apl_delimeter)
        )
      })
    })
  })

  transformed_df_list <- unlist(unlist(transformed_df_list, recursive = FALSE), recursive = FALSE)

  transformed_df_list <- lapply(transformed_df_list, function(transformed_df) {
    names(transformed_df[[1]]) <- paste(names(transformed_df[[1]]), names(transformed_df), sep = delimeter)
    return(transformed_df)
  })
  return(dplyr::bind_cols(transformed_df_list))
}

#' Apply Adstock, Power, and Lag Transformation to Multiple Variables
#'
#' This function applies the Adstock, Power, and Lag transformation to multiple variables in a given data frame.
#'
#' @param modeling_df A data frame containing the input variables.
#' @param candidate_variables A list specifying adstock, power, and lag for each variable.
#'
#' Each element in `candidate_variables` should be a named vector with components:
#' \describe{
#'   \item{adstock}{Adstock rate for the transformation.}
#'   \item{power}{Power transformation to capture non-linear effectiveness of marketing activity.}
#'   \item{lag}{Lag parameter for the time lag in the transformation.}
#' }
#'
#' @return A data frame with Adstock, Power, and Lag transformed values for each variable.
#'
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#'   model_df <- cbind(advertising, event)
#'   candidate_variables <- list(
#'     TV = setNames(c(.1, .3, 1), c("adstock", "power", "lag")),
#'     Radio = setNames(c(.2, .4, 0), c("adstock", "power", "lag"))
#'   )
#'   apply_apl(model_df, candidate_variables)
#' }
#'
#' @export
apply_apl <- function(modeling_df, candidate_variables) {
  dplyr::bind_cols(
    purrr::map(names(candidate_variables), function(x) {
      compute_apl_values(
        modeling_df[, x, drop = FALSE],
        candidate_variables[[x]]["adstock"],
        candidate_variables[[x]]["power"],
        candidate_variables[[x]]["lag"]
      )
    })
  )
}

#' Generate Adstock, Power, and Lag Combinations
#'
#' This function generates combinations of adstock, power, and lag based on specified constraints.
#'
#' @param adstock_start_end_step A named numeric vector specifying the start, end, and step for adstock.
#' @param power_start_end_step A named numeric vector specifying the start, end, and step for power.
#' @param lag_start_end_step A named numeric vector specifying the start, end, and step for lag.
#' @param apl_constraints A list containing additional constraints for adstock, power, and lag.
#'
#' @return A list of combinations for adstock, power, and lag.
#'
#' @examples
#' \dontrun{
#'   variable_apl_constraints <- list(
#'     TV = list(
#'       adstock = setNames(c(.1, .3, .1), c("start", "end", "step")),
#'       power = setNames(c(.2, .5, .1), c("start", "end", "step")),
#'       lag = setNames(c(0, 2, 1), c("start", "end", "step")),
#'       constraints = "adstock <= power"
#'     )
#'   )
#'   apl_combinations <- generate_apl_combinations(
#'     variable_apl_constraints$TV["adstock"],
#'     variable_apl_constraints$TV["power"],
#'     variable_apl_constraints$TV["lag"],
#'     variable_apl_constraints$TV["constraints"]
#'   )
#'   print(apl_combinations)
#' }
#'
generate_apl_combinations <- function(adstock_start_end_step, power_start_end_step, lag_start_end_step, apl_constraints = NA) {
  # Extract relevant constraints
  apl <- lapply(
    c(adstock_start_end_step, power_start_end_step, lag_start_end_step),
    function(x) seq(x["start"], x["end"], x["step"])
  )

  # Generate all combinations
  combination <- expand.grid(adstock = apl$adstock, power = apl$power, lag = apl$lag)

  # Apply additional constraints if specified
  if (!is.na(apl_constraints)) {
    combination <- subset(combination, eval(parse(text = apl_constraints$constraints)))
  }

  # Return the combinations as a list
  return(unname(asplit(combination, 1)))
}

#' Generate Variable Combinations with Adstock, Power, and Lag
#'
#' This function generates combinations of adstock, power, and lag for multiple variables based on specified constraints.
#'
#' @param variables_wt_apl_constraints A list containing named vectors for each variable with adstock, power, lag, and constraints.
#'
#' Each named vector should include the following components:
#' \describe{
#'   \item{adstock}{A named numeric vector specifying the start, end, and step for adstock.}
#'   \item{power}{A named numeric vector specifying the start, end, and step for power.}
#'   \item{lag}{A named numeric vector specifying the start, end, and step for lag.}
#'   \item{constraints}{An optional character string specifying additional constraints for adstock and power.}
#' }
#'
#' @return A list of combinations for adstock, power, and lag for each variable.
#'
#' @examples
#' \dontrun{
#'   variables_wt_apl_constraints <- list(
#'     TV = list(
#'       adstock = setNames(c(.1, .3, .1), c("start", "end", "step")),
#'       power = setNames(c(.2, .5, .1), c("start", "end", "step")),
#'       lag = setNames(c(0, 2, 1), c("start", "end", "step")),
#'       constraints = "adstock <= power"
#'     ),
#'     Radio = list(
#'       adstock = setNames(c(.2, .5, .1), c("start", "end", "step")),
#'       power = setNames(c(.4, .7, .1), c("start", "end", "step")),
#'       lag = setNames(c(0, 0, 1), c("start", "end", "step")),
#'       constraints = "adstock <= power"
#'     )
#'   )
#'   generate_variable_combination(variables_wt_apl_constraints)
#' }
#'
#' @export
generate_variable_combination <- function(variables_wt_apl_constraints) {
  variables_apl_combination <- lapply(variables_wt_apl_constraints, function(x)
    generate_apl_combinations(
      x["adstock"],
      x["power"],
      x["lag"],
      x["constraints"]
    )
  )

  if (length(variables_apl_combination) == 1) {
    variables_apl_combination <- list(variables_apl_combination)
  }

  return(apply(do.call(expand.grid, variables_apl_combination), 1, as.list))
}

#' Parse Variable with Adstock, Power, and Lag
#'
#' This function parses a vector of strings containing variable information along with adstock, power, and lag, separated by specified delimiters.
#' Each string should contain variable names and up to three numeric values (adstock, power, and lag) in a consistent order, separated by delimiters.
#'
#' @param variables_wt_apl A character vector with each element containing variable names and up to three numeric values separated by `delimeter` and `apl_delimeter`.
#' @param apl_delimeter A string representing the delimiter between adstock, power, and lag values.
#' @param delimeter A string representing the main delimiter between variable and apl in the variables_wt_apl strings.
#' @return A data frame containing variables and their corresponding adstock, power, and lag values.
#' @examples
#' \dontrun{
#' parse_variable_wt_apl(c("TV_Smart|0.8_0.22_0.11", "Radio|0.5_0.15"), "_", "\\|")
#' }
#' @importFrom stats na.omit
#' @export
parse_variable_wt_apl <- function(variables_wt_apl, apl_delimeter = "_", delimeter = "\\|") {

  # check acceptable pattern
  if(sum(!stringr::str_detect(variables_wt_apl, paste0(delimeter,"\\d+(\\.\\d+)?",apl_delimeter,"\\d+(\\.\\d+)?",apl_delimeter,"\\d+(\\.\\d+)?$")))){
    stop(paste("Please ensure that all variable with apl should be followed by",delimeter,"then followed by adstock, power and lag, each separated by an underscore",apl_delimeter))
  }

  regex_pattern <- paste0("(.+?)", delimeter, "([0-9.]+", apl_delimeter, "?[0-9.]*", apl_delimeter, "?[0-9.]*)$")

  matches <- stringr::str_match(variables_wt_apl, regex_pattern)

  if (any(is.na(matches))) {
    warning("Some inputs did not match the expected format and will be omitted.")
    matches <- na.omit(matches)
  }

  data.frame(
    variable = matches[, 2],
    adstock = as.numeric(stringr::str_extract(matches[, 3], "^[0-9.]+")),
    power = as.numeric(stringr::str_extract(stringr::str_replace(matches[, 3], "^[0-9.]+_", ""), "^[0-9.]+")),
    lag = as.numeric(stringr::str_extract(matches[, 3], "[0-9.]+$"))
  )
}
