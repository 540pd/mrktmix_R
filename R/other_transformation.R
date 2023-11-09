#' Aggregate Columns in a Data Frame
#'
#' This function aggregates specified columns in a data frame and returns a new data frame.
#'
#' @param modeling_df A data frame containing the input variables.
#' @param aggregated_variables A character vector specifying the columns to aggregate.
#' @param delimeter Delimiter used to find individual variables for aggregated_variables.
#'
#' @return A data frame with aggregated values.
#'
#' @examples
#' \dontrun{
#'   aggregate_columns(advertising, c("Sales", "TV|Radio|Newspaper"), delimeter = "|")
#' }
#'
#' @export
aggregate_columns <- function(modeling_df, aggregated_variables, delimeter = "|") {
  individual_variables <- stringr::str_split(aggregated_variables, fixed(delimeter))
  aggregated_vals <- lapply(individual_variables, function(x) apply(modeling_df[, x, drop = FALSE], 1, sum, na.rm = TRUE))
  aggregated_df <- as.data.frame(do.call(cbind, stats::setNames(aggregated_vals, aggregated_variables)))
  return(aggregated_df)
}
                            
#' Decompose Model Component with Adstock, Power, and Lag (APL) Transformations
#'
#' This function takes a set of variables with weights and applies Adstock, Power,
#' and Lag (APL) transformations to the variables within a model dataframe.
#' It handles the decomposition of model components based on provided coefficients
#' or contributions.
#'
#' @param variables_wt_weights A named vector of weights for the variables, where names include APL info.
#' @param model_df Data frame containing the variables to be transformed.
#' @param is_weight_coefficient Logical indicating if the weights should be treated as coefficients.
#' @param apl_delimiter The delimiter used in variable names to separate APL components.
#' @param delimiter The delimiter used in variable names to denote different variables.
#'
#' @return A transformed data frame with variables weighted and APL transformations applied.
#'
#' @examples
#' \dontrun{
#'   advertising <- data.frame(TV = rnorm(100), Radio = rnorm(100))
#'   event <- data.frame(Date2021_01_08 = sample(0:1, 100, replace = TRUE),
#'                       Date2021_01_01 = sample(0:1, 100, replace = TRUE))
#'   model_df <- cbind(advertising, event)
#'
#'   variables_wt_weights <- setNames(1:5, c("TV|0.8_0.22_0", "Radio|0.5_0.15_1",
#'                                           "Intercept", "Date2021_01_08", "Date2021_01_01"))
#'
#'   transformed_df <- decompose_model_component(variables_wt_weights, model_df,
#'                                               is_weight_coefficient = FALSE,
#'                                               apl_delimiter = "_",
#'                                               delimiter = "\\|")
#' }
#' @importFrom dplyr mutate across any_of bind_cols select if_else cur_column
#' @importFrom purrr pmap
#' @importFrom magrittr "%>%"
#' @importFrom stats setNames
#' @importFrom stringr str_replace str_split
#' @importFrom tidyselect everything
#' @export
decompose_model_component <- function(variables_wt_weights, model_df,
                                      is_weight_coefficient = TRUE,
                                      apl_delimiter = "_",
                                      delimiter = "\\|") {
  # Treat weights as coefficients if is_weight_coefficient is TRUE, otherwise as contributions
  # Select and weight the variables in the model dataframe
  model_df_selected <- model_df %>%
    dplyr::mutate(dplyr::across(any_of(names(variables_wt_weights)),
                                ~ .x * variables_wt_weights[dplyr::cur_column()]),
                  .keep = "used")
  # Remaining variables after selection
  variables_wt_weights_left <- variables_wt_weights[!names(variables_wt_weights) %in% names(model_df_selected)]

  # Check for the presence of an intercept term and include it if present
  intercept_key <- c("Intercept","intercept","(Intercept)")
  intercept_exists <- names(variables_wt_weights_left) %in% intercept_key
  if (any(intercept_exists)) {
    model_df_selected[names(variables_wt_weights_left[intercept_exists])] <- variables_wt_weights_left[intercept_exists]
    variables_wt_weights_left <- variables_wt_weights_left[!intercept_exists]
  }

  # Parse variables for APL information
  variable_info <- parse_variable_wt_apl(names(variables_wt_weights_left), apl_delimiter, delimiter)
  variables_wt_weights_left <- setNames(variables_wt_weights_left, variable_info$variable)

  # Create APL information list using pmap
  apl_info <- purrr::pmap(variable_info, function(variable, adstock, power, lag) {
    setNames(c(adstock, power, lag), names(variable_info)[-1])
  }) %>%
    setNames(variable_info$variable)

  # Clean variable names by removing APL-related patterns
  pattern_to_remove <- paste0(delimiter, "([0-9]+(?:\\.[0-9]*)?",
                              apl_delimiter, "[0-9]+(?:\\.[0-9]*)?",
                              apl_delimiter, "[0-9]+(?:\\.[0-9]*)?)$")
  names(variables_wt_weights) <- str_replace(names(variables_wt_weights), pattern_to_remove, "")

  # Apply APL transformations and recombine with the selected data
  model_df_transformed <- apply_apl(model_df, apl_info)
  model_df_selected <- model_df_selected %>%
    dplyr::bind_cols(model_df_transformed) %>%
    dplyr::select(any_of(names(variables_wt_weights)))

  # Adjust for weight coefficients if not treating as coefficients
  if (!is_weight_coefficient) {
    model_df_selected <- model_df_selected %>%
      dplyr::mutate(dplyr::across(everything(), ~ .x / sum(.x, na.rm = TRUE) * {{variables_wt_weights}}[dplyr::cur_column()]))
  }

  # Return the final model dataframe with applied transformations
  return(model_df_selected)
}

#' Compose Variable Names with Adstock, Power, and Lag (APL) Attributes
#'
#' This function takes a list of variables with their corresponding Adstock, Power, and Lag (APL)
#' attributes and composes a standardized naming convention for each variable incorporating
#' its APL attributes.
#'
#' @param variables_wt_named_apl A list where each element is named after a variable and contains a
#'        named vector of APL attributes: adstock, power, and lag.
#' @param apl_delimiter The delimiter to use between the variable name and APL values.
#' @param delimiter The delimiter to use between APL attributes.
#'
#' @return A character vector of variable names composed with their APL attributes.
#'
#' @examples
#' \dontrun{
#'   variables_wt_named_apl <- list(
#'     "variable1" = c("adstock" = 0.5, "power" = 2, "lag" = 1),
#'     "variable2" = c("adstock" = 0.3, "power" = 1, "lag" = 0)
#'   )
#'   composed_names <- compose_variable_apl(variables_wt_named_apl)
#' }
#'
#' @export
compose_variable_apl <- function(variables_wt_named_apl, apl_delimiter = "_", delimiter = "|") {
  # Compose variable names with their APL attributes into a standardized format
  result <- mapply(function(name, values) {
    # Concatenate each value with the variable name using the provided delimiters
    paste(name, paste(unlist(values), collapse = apl_delimiter), sep = delimiter)
  }, names(variables_wt_named_apl), variables_wt_named_apl, SIMPLIFY = FALSE)

  # Combine the result into a single character vector without names
  unname(unlist(result))
}

#' Generate Model-Dependent Data Frames with APL Transformations
#'
#' Applies Adstock-Power-Lag (APL) transformations to variable information within
#' a model data frame. The function returns a list containing APL information
#' and a list of data frames with applied transformations. The `apl_delimiter`
#' and `delimiter` are used to interpret and construct variable names when
#' `var_info` is a named numeric vector. When `var_info` is a list, the function
#' generates variable combinations and applies APL to `model_df`.
#'
#' @param var_info A named numeric vector or a list detailing the variables and
#'   their respective APL transformations.
#' @param model_df A data frame containing model variables to which the APL
#'   transformations are to be applied.
#' @param apl_delimiter A string delimiter for concatenating variable names with
#'   their APL attributes when `var_info` is a named numeric vector. Default "_".
#' @param delimiter A string delimiter for separating variable names from their
#'   APL attributes when `var_info` is a named numeric vector. Default "\\|".
#'
#' @return A list containing a tibble of APL information and a list of data
#'   frames with APL transformations applied.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Named vector input
#'   var_info_vec <- setNames(5, "TV|0.8_0.22_0")
#'   result_vec <- generate_model_dependent(var_info_vec, model_df)
#'
#'   # List input with APL specifications
#'   var_info_list <- list(
#'     TV = list(
#'       adstock = setNames(c(.1, .2, .1), c("start", "end", "step")),
#'       power = setNames(c(.2, .3, .1), c("start", "end", "step")),
#'       lag = setNames(c(0, 1, 1), c("start", "end", "step")),
#'       constraints = "adstock <= power"
#'     )
#'   )
#'   result_list <- generate_model_dependent(var_info_list, model_df)
#' }
#' @importFrom purrr flatten
#' @importFrom tibble enframe as_tibble
#' @importFrom tidyr unnest_wider
#'
generate_model_dependent <- function(var_info, model_df,
                                     apl_delimiter = "_",
                                     delimiter = "\\|") {
  # Ensure that var_info is a vector or a list with one element
  stopifnot(length(var_info) == 1)

  if (is.vector(var_info) && is.numeric(var_info) && all(!is.na(names(var_info)))) {
    # Process named vector
    apl_df_list <- list(
      decompose_model_component(var_info, model_df,
                                is_weight_coefficient = FALSE,
                                apl_delimiter = apl_delimiter,
                                delimiter = delimiter
      ) %>%
        dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(.x, 0)))
    )
    var_apl_info <- parse_variable_wt_apl(names(var_info), apl_delimiter, delimiter)

  } else {
    # Process list
    var_wt_apl <- generate_variable_combination(var_info)
    apl_df_list <- map(var_wt_apl, ~apply_apl(model_df, .x) %>%
                         mutate(dplyr::across(everything(), ~replace_na(.x, 0)))
    )
    var_apl_info <- var_wt_apl %>%
      flatten() %>%
      enframe(name = "variable", value = "named_vector") %>%
      unnest_wider("named_vector") %>%
      as_tibble()
  }

  list(var_apl_info, apl_df_list)
}
