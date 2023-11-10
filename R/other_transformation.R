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
  individual_variables <- stringr::str_split(aggregated_variables, stringr::fixed(delimeter))
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
#'                                               delimiter = "|")
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
                                      delimiter = "|") {
  # Treat weights as coefficients if is_weight_coefficient is TRUE, otherwise as contributions

  # Select and weight the variables in the model dataframe. If variable is already present in data, apl won't be applied.
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

if(length(variables_wt_weights_left)){
  # Parse variables for APL information
  variable_info <- parse_variable_wt_apl(names(variables_wt_weights_left), apl_delimiter, delimiter)
  # update variables_wt_weights names 
  variables_wt_weights_captured <- variables_wt_weights[!names(variables_wt_weights) %in% names(variables_wt_weights_left)]
  variables_wt_weights_apl <- setNames(variables_wt_weights_left, variable_info$variable)
  names(variables_wt_weights)[names(variables_wt_weights) %in% names(variables_wt_weights_left)]<-names(variables_wt_weights_apl)

  # Create APL information list using pmap
  apl_info <- purrr::pmap(variable_info, function(variable, adstock, power, lag) {
    setNames(c(adstock, power, lag), names(variable_info)[-1])
  }) %>%
    setNames(variable_info$variable)

  # Apply APL transformations and recombine with the selected data
  model_df_transformed <- apply_apl(model_df, apl_info)
  model_df_combined <- model_df_selected %>%
    dplyr::bind_cols(model_df_transformed) %>%
    dplyr::select(tidyr::all_of(names(variables_wt_weights)))
} else {
  model_df_combined <- model_df_selected %>%
    dplyr::select(tidyr::all_of(names(variables_wt_weights)))
}

  # Adjust for weight coefficients if not treating as coefficients
  if (!is_weight_coefficient) {
    model_df_combined <- model_df_combined %>%
      dplyr::mutate(dplyr::across(everything(), ~ .x / sum(.x, na.rm = TRUE) * {{variables_wt_weights}}[dplyr::cur_column()]))
  }

  # Return the final model dataframe with applied transformations
  return(model_df_combined)
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

#' Parse Variable with Adstock, Power, and Lag
#'
#' This function parses a vector of strings containing variable information along with adstock, power, and lag, separated by specified delimiters.
#' Each string should contain variable names and up to three numeric values (adstock, power, and lag) in a consistent order, separated by delimiters.
#'
#' @param variables_wt_apl A character vector with each element containing variable names and up to three numeric values separated by `delimeter` and `apl_delimeter`.
#' @param apl_delimiter A string representing the delimiter between adstock, power, and lag values.
#' @param delimiter A string representing the main delimiter between variable and apl in the variables_wt_apl strings.
#' @return A data frame containing variables and their corresponding adstock, power, and lag values.
#' @examples
#' \dontrun{
#' parse_variable_wt_apl(c("TV_Smart|0.8_0.22_0.11", "Radio|0.5_0.15"), "_", "|")
#' }
#' @importFrom stats na.omit
#' @export
parse_variable_wt_apl <- function(variables_wt_apl, apl_delimiter = "_", delimiter = "|") {
  # check acceptable pattern
  # if(sum(!stringr::str_detect(variables_wt_apl, paste0(stringr::fixed(delimiter),"\\d+(\\.\\d+)?",stringr::fixed(apl_delimiter),"\\d+(\\.\\d+)?",stringr::fixed(apl_delimiter),"\\d+(\\.\\d+)?$")))){
  #   stop(paste("Please ensure that all variable with apl should be followed by",delimiter,"then followed by adstock, power and lag, each separated by an underscore",apl_delimiter))
  # }

  # Escape the special characters in regular expression
  escaped_delimiter <- gsub("([.|()\\[^$?*+])", "\\\\\\1", delimiter)
  escaped_apl_delimiter <- gsub("([.|()\\[^$?*+])", "\\\\\\1", apl_delimiter)

  regex_pattern <- paste0("(.+?)", escaped_delimiter, "([0-9.]+", escaped_apl_delimiter, "?[0-9.]*", escaped_apl_delimiter, "?[0-9.]*)$")

  matches <- stringr::str_match(variables_wt_apl, regex_pattern)

  if(any(!complete.cases(matches))){
    matches[!complete.cases(matches),]<-c(variables_wt_apl[!complete.cases(matches)], variables_wt_apl[!complete.cases(matches)], paste(0,1,0,sep=apl_delimiter))
  }
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
#' @param var_apl_delimiter A string delimiter for separating variable names from their
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
generate_model_dependent <- function(var_info, model_df,
                                     apl_delimiter = "_",
                                     var_apl_delimiter = "|", var_agg_delimiter = "|") {

  if (is.vector(var_info) && is.numeric(var_info) && all(!is.na(names(var_info)))) {

    # Dependent Data - create aggregation if required
    vars_in_model_df_logical<-names(var_info) %in%  names(model_df)
    vars_expected_model_df<-names(var_info)[!vars_in_model_df_logical] %>%
        parse_variable_wt_apl(apl_delimiter, var_apl_delimiter) %>%
        dplyr::pull(variable) %>%
        c(names(var_info)[vars_in_model_df_logical])
    model_df_rel<-aggregate_columns(model_df, vars_expected_model_df, delimeter = var_agg_delimiter)

    # Process named vector
    apl_df_list <- list(
      decompose_model_component(var_info, model_df_rel,
                                is_weight_coefficient = FALSE,
                                apl_delimiter = apl_delimiter,
                                delimiter = var_apl_delimiter
      ) %>%
        dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(.x, 0)))
    )
    var_apl_info <- parse_variable_wt_apl(names(var_info), apl_delimiter, var_apl_delimiter)

  } else {
    # Dependent Data - create aggregation if required
    #vars_in_model_df_logical<-names(var_info) %in%  names(model_df)
    #vars_expected_model_df<- c(names(var_info)[!vars_in_model_df_logical], c(names(var_info)[vars_in_model_df_logical]))
    model_df_rel<-aggregate_columns(model_df, names(var_info), delimeter = var_agg_delimiter)

    # Process list
    var_wt_apl <- generate_variable_combination(var_info)
    apl_df_list <- purrr::map(var_wt_apl, ~apply_apl(model_df_rel, .x) %>%
                         dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(.x, 0))))
    var_apl_info <- var_wt_apl %>%
      purrr::flatten() %>%
      tibble::enframe(name = "variable", value = "named_vector") %>%
      tidyr::unnest_wider("named_vector") %>%
      tibble::as_tibble()
  }
  
  list(var_apl_info, apl_df_list)
}

#' Get Dependent and Independent Variables
#'
#' This function determines the type of a model based on the presence of characters + and -,
#' and processes the model variable accordingly to extract dependent and independent variables.
#'
#' @param model_variable A character string representing the model variable.
#' @param var_agg_delimiter A character string representing the delimiter for aggregated variables. Default is "|".
#' @param trim A logical indicating whether to trim output or not. Default is TRUE.
#' @param print_model_type A logical indicating whether to print the determined model type. Default is TRUE.
#'
#' @return A list containing the processed dependent and independent variables.
#' @examples
#' \dontrun{
#' get_dep_indep_vars("A + B - C")
#' }
#' @export
get_dep_indep_vars <- function(model_variable, var_agg_delimiter = "|", trim = TRUE, print_model_type = TRUE) {
  # Determine model type based on the presence of characters + and -
  model_type <- if (!stringr::str_detect(model_variable, "[+-]")) {
    "Remodel"
  } else if (stringr::str_detect(model_variable, "\\+") & stringr::str_detect(model_variable, "-")) {
    "Aggregate & Segregate"
  } else if (stringr::str_detect(model_variable, "-")) {
    "Segregate"
  } else if (stringr::str_detect(model_variable, "\\+")) {
    "Aggregate"
  }

  # Print the determined model type if requested
  if (print_model_type) {
    cat("Model Type:", model_type, "\n")
  }

  # Initialize variables for dependent and independent variables
  dep_var_rel <- indep_vars <- character()

  # Based on the model type, process the model variable
  if (model_type == "Remodel") {
    dep_var_rel <- model_variable
    indep_vars <- model_variable
  } else if (model_type == "Aggregate") {
    dep_var_rel <- unlist(str_split(model_variable, "\\+"))
    indep_vars <- stringr::str_replace_all(model_variable, "\\+", var_agg_delimiter)
  } else if (model_type == "Aggregate & Segregate") {
    dep_var_rel <- stringr::str_replace_all(str_replace(unlist(str_split(model_variable, "\\+")), paste0("^-",var_agg_delimiter,"-$"), ""), "-", var_agg_delimiter)
    indep_vars <- stringr::str_replace_all(str_replace(unlist(str_split(model_variable, "-")), paste0("^\\+",var_agg_delimiter,"\\+$"), ""), "\\+", var_agg_delimiter)
  } else if (model_type == "Segregate") {
    dep_var_rel <- stringr::str_replace_all(model_variable, "-", var_agg_delimiter)
    indep_vars <- unlist(stringr::str_split(model_variable, "-"))
  }

  if(trim){
    dep_var_rel<-unlist(lapply(stringr::str_split(dep_var_rel,stringr::fixed(var_agg_delimiter)), function(x) paste0(stringr::str_trim(x, side = c("both")),collapse = var_agg_delimiter)))
    indep_vars<-unlist(lapply(stringr::str_split(indep_vars,stringr::fixed(var_agg_delimiter)), function(x) paste0(stringr::str_trim(x, side = c("both")),collapse = var_agg_delimiter)))
  }

  # Return a list containing the processed dependent and independent variables
  return(list(dependent_var = dep_var_rel, independent_var = indep_vars))
}
