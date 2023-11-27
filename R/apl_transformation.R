#' Apply Adstock, Power, and Lag Transformation to a Data Frame
#'
#' This function applies Adstock, Power, and Lag transformations to a data frame.
#' It is designed to process input variables for market mix modeling, enhancing
#' their utility for subsequent analysis.
#'
#' @param modeling_df A data frame containing the input variables. This should
#'                    be structured with columns representing different marketing
#'                    variables and rows representing time periods.
#' @param adstock     The adstock rate for the transformation, which controls
#'                    the degree of weight given to past values.
#' @param power       The power transformation parameter, used to capture the
#'                    non-linear effectiveness of marketing activities.
#' @param lag         The lag parameter, specifying the time lag in the
#'                    transformation to account for delayed effects.
#'
#' @return            A transformed data frame with Adstock, Power, and Lag
#'                    applied to the input variables.
#'
#' @importFrom dplyr  lag
#'
#' @details
#' The function first applies the Adstock transformation using a recursive
#' filtering method. It then raises the filtered values to the specified power,
#' capturing non-linearities in the effect of marketing activities. Finally,
#' it introduces a lag, shifting the transformed values by the specified
#' time periods to account for delayed effects.
#'
#' @examples
#' \dontrun{
#'   # Basic usage with a sample data frame 'advertising'
#'   compute_apl_values(advertising, adstock = 0.8, power = 2, lag = 1)
#'
#'   # Applying to a single column 'Sales' in 'advertising'
#'   compute_apl_values(advertising[,"Sales",drop=F], adstock = 0.8, power = 2, lag = 1)
#' }
#'
compute_apl_values <- function(modeling_df, adstock, power, lag) {
  stopifnot(adstock >= 0 & power >= 0 & adstock <= 1 & power <= 1)
  transformed_df <-
    apply(stats::filter(modeling_df, adstock, method = "recursive") ^ power,
          2,
          dplyr::lag,
          lag)
  dimnames(transformed_df) <-
    list(row.names(modeling_df), names(modeling_df))
  return(as.data.frame(transformed_df))
}

#' Apply Vectorized Adstock, Power, and Lag Transformations to a Data Frame
#'
#' This function extends `compute_apl_values` to apply vectorized Adstock, Power,
#' and Lag transformations to a data frame. It allows for the application of multiple
#' transformation parameters simultaneously, creating a comprehensive transformed output.
#'
#' @inheritParams compute_apl_values
#' @param apl_delimiter Delimiter used to separate Adstock, Power, and Lag values
#'                      in resulting column names.
#' @param delimiter Delimiter used to separate original column name and apl
#'                  suffix in output data frame.
#'
#' @return A data frame with each column transformed by all combinations of
#'         Adstock, Power, and Lag parameters. The column names in the output
#'         indicate the specific transformations applied.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#'
#' @details
#' The function iteratively applies combinations of Adstock, Power, and Lag
#' transformations to the input data frame. Each column in the resulting data frame
#' corresponds to one unique combination of these parameters. This function is
#' particularly useful for scenarios where different sets of transformation
#' parameters need to be evaluated simultaneously.
#'
#' @examples
#' \dontrun{
#'   # Applying vectorized transformations to 'advertising' data frame
#'   generate_apl_dataframe(advertising, adstock = c(0.8, 0.9),
#'                          power = c(2, 3), lag = c(1, 2))
#' }
#'
generate_apl_dataframe <-
  function(modeling_df,
           adstock,
           power,
           lag,
           apl_delimiter = "_",
           delimiter = "|") {
    transformed_df_list <- purrr::map(adstock, function(adstock_) {
      purrr::map(power, function(power_) {
        purrr::map(lag, function(lag_) {
          stats::setNames(
            list(
              compute_apl_values(modeling_df, adstock_, power_, lag_)
            ),
            paste(adstock_, power_, lag_, sep = apl_delimiter)
          )
        })
      })
    })

    transformed_df_list <-
      unlist(unlist(transformed_df_list, recursive = FALSE), recursive = FALSE)

    transformed_df_list <-
      lapply(transformed_df_list, function(transformed_df) {
        names(transformed_df[[1]]) <-
          paste(names(transformed_df[[1]]), names(transformed_df), sep = delimiter)
        return(transformed_df)
      })
    return(dplyr::bind_cols(transformed_df_list))
  }

#' Apply Adstock, Power, and Lag Transformation to Multiple Variables
#'
#' This function extends the functionality of `compute_apl_values` to apply Adstock,
#' Power, and Lag transformations to multiple variables in a data frame. It allows
#' for different transformation parameters to be specified for each variable.
#'
#' @param modeling_df A data frame containing the input variables.
#' @param candidate_variables A list specifying adstock, power, and lag for each variable.
#'        Each element in `candidate_variables` should be a named vector with components:
#'        \describe{
#'          \item{adstock}{Adstock rate for the transformation.}
#'          \item{power}{Power transformation to capture non-linear effectiveness of marketing activity.}
#'          \item{lag}{Lag parameter for the time lag in the transformation.}
#'        }
#'
#' @return A data frame with Adstock, Power, and Lag transformed values for each variable.
#'
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#'
#' @details
#' The function iteratively applies the specified Adstock, Power, and Lag transformations
#' to each variable in the `modeling_df` data frame based on the parameters defined in
#' `candidate_variables`. This is useful for market mix modeling where different marketing
#' channels may require different transformation parameters.
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
#'
apply_apl <- function(modeling_df, candidate_variables) {
  dplyr::bind_cols(purrr::map(names(candidate_variables), function(x) {
    compute_apl_values(
      modeling_df[, x, drop = FALSE],
      candidate_variables[[x]]["adstock"],
      candidate_variables[[x]]["power"],
      candidate_variables[[x]]["lag"]
    )
  }))
}

#' Generate Combinations of Adstock, Power, and Lag
#'
#' This function is designed to:
#' - Generate a range of adstock, power, and lag values based on specified start, end, and step values.
#' - Create combinations of these values.
#' - Apply additional logical constraints to these combinations if specified.
#'
#' @param adstock_start_end_step A named list 'adstock' containing a named numeric
#'                               vector with 'start', 'end', and 'step' values for
#'                               the adstock rate.
#' @param power_start_end_step A named list 'power' containing a named numeric
#'                             vector with 'start', 'end', and 'step' values for
#'                             the power transformation.
#' @param lag_start_end_step A named list 'lag' containing a named numeric
#'                           vector with 'start', 'end', and 'step' values for
#'                           the lag.
#' @param apl_constraints A list containing additional constraints for the
#'                        combinations of adstock, power, and lag. Constraints
#'                        should be specified as character strings representing
#'                        logical conditions.
#'
#' @return A list of all valid combinations of adstock, power, and lag that
#'         meet the specified ranges and constraints.
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
generate_apl_combinations <-
  function(adstock_start_end_step,
           power_start_end_step,
           lag_start_end_step,
           apl_constraints = NA) {
    # Extract relevant constraints
    apl <- lapply(c(
      adstock_start_end_step,
      power_start_end_step,
      lag_start_end_step
    ),
    function(x)
      seq(x["start"], x["end"], x["step"]))

    # Generate all combinations
    combination <-
      expand.grid(
        adstock = apl$adstock,
        power = apl$power,
        lag = apl$lag
      )

    # Apply additional constraints if specified
    if (!is.na(apl_constraints)) {
      combination <-
        subset(combination, eval(parse(text = apl_constraints$constraints)))
    }

    # Return the combinations as a list
    return(unname(asplit(combination, 1)))
  }

#' Generate Variable Combinations with Adstock, Power, and Lag
#'
#' This function generates combinations of adstock, power, and lag for multiple
#' variables based on specified constraints. It is designed to handle a variety
#' of marketing variables, each with its own set of transformation parameters and constraints.
#'
#' @param variables_wt_apl_constraints A list containing named lists for each variable.
#' Each named list for a variable should include the following components:
#' \describe{
#'   \item{adstock}{A named numeric vector specifying the start, end, and step
#'                  for adstock, with names "start", "end", "step".}
#'   \item{power}{A named numeric vector specifying the start, end, and step for
#'                power, with names "start", "end", "step".}
#'   \item{lag}{A named numeric vector specifying the start, end, and step for
#'              lag, with names "start", "end", "step".}
#'   \item{constraints}{An optional character string specifying additional
#'                      constraints between adstock and power.}
#' }
#'
#' @return A list where each element represents a combination of adstock, power,
#'         and lag parameters for a given variable. If multiple variables are
#'         provided, the function returns a list of lists, with each inner list
#'         containing combinations for a specific variable.
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
#'   variable_combinations <- generate_variable_combination(variables_wt_apl_constraints)
#'   print(variable_combinations)
#' }
#'
#'
generate_variable_combination <-
  function(variables_wt_apl_constraints) {
    variables_apl_combination <-
      lapply(variables_wt_apl_constraints, function(x)
        generate_apl_combinations(x["adstock"],
                                  x["power"],
                                  x["lag"],
                                  x["constraints"]))

    if (length(variables_apl_combination) == 1) {
      variables_apl_combination <- list(variables_apl_combination)
    }

    return(apply(do.call(
      expand.grid, variables_apl_combination
    ), 1, as.list))
  }
