#' Compose Variable Names with Adstock, Power, and Lag (APL) Attributes
#'
#' Constructs standardized variable names by incorporating Adstock, Power, and Lag
#' (APL) attributes into each variable's name. This function is useful for creating
#' clear and descriptive variable names in models where APL transformations are applied.
#'
#' @param variables_wt_named_apl A list where each element is a named vector representing
#'        a variable. The names of the elements are the variable names, and each named
#'        vector contains the APL attributes: adstock, power, and lag.
#' @param apl_delimiter The delimiter to use between the variable name and its APL values
#'        (default is "_").
#' @param delimiter The delimiter to use between different APL attributes
#'        (default is "|").
#'
#' @return A character vector where each element is a variable name composed with
#'         its APL attributes. These names follow the format:
#'         `variable_name[delimiter]adstock_value[apl_delimiter]power_value[apl_delimiter]lag_value`.
#'
#' @examples
#' \dontrun{
#'   variables_wt_named_apl <- list(
#'     "variable1" = c("adstock" = 0.5, "power" = 2, "lag" = 1),
#'     "variable2" = c("adstock" = 0.3, "power" = 1, "lag" = 0)
#'   )
#'   composed_names <- compose_variable_apl(variables_wt_named_apl,
#'                                          apl_delimiter = "_",
#'                                          delimiter = "|")
#'   print(composed_names)
#' }
#'
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
#' Parses a vector of strings containing variable information along with Adstock,
#' Power, and Lag (APL) attributes, separated by specified delimiters. Each string
#' in the vector should contain a variable name followed by up to three numeric
#' values representing the APL attributes, in a consistent order and separated
#' by the delimiters.
#'
#' @param variables_wt_apl A character vector where each element contains a
#'   variable name and up to three numeric values for adstock, power, and lag,
#'   separated by `delimiter` and `apl_delimiter`.
#' @param apl_delimiter A string representing the delimiter used between the
#'   adstock, power, and lag values in each string. It cann't be period (.)
#' @param delimiter A string representing the main delimiter used between the
#'   variable name and the APL attributes in each string. It cann't be period (.)
#'
#' @return A data frame where each row corresponds to an element in
#'   `variables_wt_apl`, with columns for the variable name, adstock value,
#'   power value, and lag value.
#'
#' @examples
#' \dontrun{
#'   parsed_data <- parse_variable_wt_apl(c("TV_Smart|0.8_0.22_0.11", "Radio|0.5_0.15"),
#'                                        apl_delimiter = "_",
#'                                        delimiter = "|")
#'   print(parsed_data)
#' }
#'
#' @importFrom stats na.omit
#'
parse_variable_wt_apl <- function(variables_wt_apl, apl_delimiter = "_", delimiter = "|") {
  
  # Escape the special characters in regular expression
  escaped_delimiter <- gsub("([.|()\\[^$?*+])", "\\\\\\1", delimiter)
  escaped_apl_delimiter <- gsub("([.|()\\[^$?*+])", "\\\\\\1", apl_delimiter)
  
  regex_pattern <- paste0("(.+?)", escaped_delimiter, "([0-9.]+", escaped_apl_delimiter, "?[0-9.]*", escaped_apl_delimiter, "?[0-9.]*)$")
  
  matches <- stringr::str_match(variables_wt_apl, regex_pattern)
  
  if(any(!stats::complete.cases(matches))){
    matches[!stats::complete.cases(matches),]<-c(variables_wt_apl[!stats::complete.cases(matches)], variables_wt_apl[!stats::complete.cases(matches)], paste(0,1,0,sep=apl_delimiter))
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

#' Aggregate Columns in a Data Frame
#'
#' Aggregates specified columns in a data frame and returns a new data frame with
#' these aggregated values. It takes a character vector of column names,
#' possibly representing aggregated variables separated by a delimiter, and
#' computes the sum of these variables row-wise.
#'
#' @param modeling_df A data frame containing the input variables.
#' @param aggregated_variables A character vector specifying the columns to
#'   aggregate. The columns are expected to be delimited strings representing
#'   the individual variables to aggregate.
#' @param delimiter A string used as a delimiter to split the names in
#'   `aggregated_variables`. Default is "|".
#'
#' @return A data frame with columns specified in `aggregated_variables` aggregated.
#'
#' @examples
#' \dontrun{
#'   advertising <- data.frame(tv = runif(10), radio = runif(10), newspaper = runif(10),
#'    sales = runif(10))
#'   aggregate_columns(advertising, c("Sales", "TV|Radio|Newspaper"), delimiter = "|")
#' }
#'
aggregate_columns <- function(modeling_df, aggregated_variables, delimiter = "|") {
  individual_variables <- stringr::str_split(aggregated_variables, stringr::fixed(delimiter))
  aggregated_vals <- lapply(individual_variables, function(x) apply(modeling_df[, x, drop = FALSE], 1, sum, na.rm = TRUE))
  aggregated_df <- as.data.frame(do.call(cbind, stats::setNames(aggregated_vals, aggregated_variables)))
  return(aggregated_df)
}

#' Decompose Model Component with Adstock, Power, and Lag (APL) Transformations
#'
#' Applies Adstock, Power, and Lag (APL) transformations to variables within a
#' model data frame. It handles the decomposition of model components based on
#' provided weights, which can be treated either as coefficients or contributions.
#'
#' @param variables_wt_weights A named vector of weights for the variables, where
#'   names include APL information.
#' @param model_df A data frame containing the variables to be transformed.
#' @param is_weight_coefficient Logical; if TRUE, weights are treated as
#'   coefficients. If FALSE, they are treated as contributions.
#' @param apl_delimiter Delimiter used in variable names to separate APL components.
#' @param delimiter Delimiter used in variable names to denote different variables.
#'
#' @return A transformed data frame with variables weighted and APL transformations
#'   applied.
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
#'
#' @importFrom dplyr mutate across any_of bind_cols select if_else cur_column
#' @importFrom purrr pmap
#' @importFrom magrittr "%>%"
#' @importFrom stats setNames
#' @importFrom stringr str_replace str_split
#' @importFrom tidyselect everything
#' @export
#'
decompose_model_component <- function(variables_wt_weights, model_df,
                                      is_weight_coefficient = TRUE,
                                      apl_delimiter = "_",
                                      delimiter = "|",
                                      var_agg_delimiter="|") {
  # Treat weights as coefficients if is_weight_coefficient is TRUE, otherwise as contributions
  
  model_df_selected <- model_df %>%
    dplyr::select(any_of(names(variables_wt_weights)))
  
  # Remaining variables after selection
  variables_wt_weights_left <- variables_wt_weights[!names(variables_wt_weights) %in% names(model_df_selected)]
  
  # Check for the presence of an intercept term and include it if present
  intercept_key <- c("Intercept","intercept","(Intercept)")
  intercept_exists <- names(variables_wt_weights_left) %in% intercept_key
  if (any(intercept_exists)) {
    # model_df_selected[,names(variables_wt_weights_left[intercept_exists])] <- variables_wt_weights_left[intercept_exists]
    model_df_selected[,names(variables_wt_weights_left[intercept_exists])] <- 1
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
    
    # Dependent Data - create aggregation if required
    vars_in_model_df_logical<-names(apl_info) %in%  names(model_df)
    vars_expected_model_df<-names(apl_info)[!vars_in_model_df_logical]
    model_df_rel<-aggregate_columns(model_df, c(vars_expected_model_df, names(apl_info)[vars_in_model_df_logical]) ,  var_agg_delimiter)
    
    
    # Apply APL transformations and recombine with the selected data
    model_df_transformed <- apply_apl(model_df_rel, apl_info)
    model_df_combined <- model_df_selected %>%
      dplyr::bind_cols(model_df_transformed) %>%
      dplyr::select(tidyr::all_of(names(variables_wt_weights)))
  } else {
    model_df_combined <- model_df_selected %>%
      dplyr::select(tidyr::all_of(names(variables_wt_weights)))
  }
  
  model_df_combined_est <- model_df_combined %>%
    dplyr::mutate(dplyr::across(any_of(names(variables_wt_weights)),
                                ~ .x * variables_wt_weights[dplyr::cur_column()]),
                  .keep = "used")
  
  
  # Adjust for weight coefficients if not treating as coefficients
  if (!is_weight_coefficient) {
    model_df_combined_est <- model_df_combined_est %>%
      dplyr::mutate(dplyr::across(everything(), ~ .x / sum(.x, na.rm = TRUE) * {{variables_wt_weights}}[dplyr::cur_column()]))
  }
  
  # Return the final model dataframe with applied transformations
  return(model_df_combined_est)
}

#' Generate Model-Dependent Data Frames with APL Transformations
#'
#' Applies Adstock-Power-Lag (APL) transformations to variable information within
#' a model data frame. The function returns a list containing APL information
#' and a list of data frames with applied transformations. The `apl_delimiter`
#' and `var_apl_delimiter` are used to interpret and construct variable names when
#' `var_info` is a named numeric vector. When `var_info` is a list, the function
#' generates variable combinations and applies APL transformations to `model_df`.
#'
#' @param var_info A named numeric vector or a list detailing the variables and
#'   their respective APL transformations. The named numeric vector format uses
#'   `var_apl_delimiter` to separate variable names and APL attributes, while the
#'   list format provides detailed specifications for each variable.
#' @param model_df A data frame containing model variables to which the APL
#'   transformations are to be applied.
#' @param apl_delimiter Delimiter used for concatenating variable names with
#'   their APL attributes when `var_info` is a named numeric vector (default "_").
#' @param var_apl_delimiter Delimiter used for separating variable names from their
#'   APL attributes when `var_info` is a named numeric vector (default "|").
#' @param var_agg_delimiter Delimiter used for aggregating variables (default "|").
#'
#' @return A list containing a tibble of APL information and a list of data
#'   frames with APL transformations applied. The tibble includes variable names
#'   and their APL attributes, while each data frame in the list represents a
#'   model-dependent transformation.
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
#'
#' @importFrom purrr map flatten
#' @importFrom tibble enframe as_tibble
#' @importFrom tidyr unnest_wider
#'
generate_model_dependent <- function(var_info, model_df,
                                     apl_delimiter = "_",
                                     var_apl_delimiter = "|", var_agg_delimiter = "|") {
  if(is.vector(var_info) && is.character(var_info)){
    var_info <- setNames(sapply(model_df[,var_info, drop=FALSE],sum, na.rm=T),paste(var_info, var_apl_delimiter,0,apl_delimiter,1,apl_delimiter,0, sep =""))
  }
  
  if (is.vector(var_info) && is.numeric(var_info) && all(!is.na(names(var_info)))) {
    
    # # Dependent Data - create aggregation if required
    # vars_in_model_df_logical<-names(var_info) %in%  names(model_df)
    # vars_expected_model_df<-names(var_info)[!vars_in_model_df_logical] %>%
    #   parse_variable_wt_apl(apl_delimiter, var_apl_delimiter) %>%
    #   dplyr::pull("variable") %>%
    #   c(names(var_info)[vars_in_model_df_logical])
    # model_df_rel<-aggregate_columns(model_df, vars_expected_model_df,  var_agg_delimiter)
    
    # Process named vector
    apl_df_list <- list(
      decompose_model_component(var_info, model_df,
                                is_weight_coefficient = FALSE,
                                apl_delimiter = apl_delimiter,
                                delimiter = var_apl_delimiter,
                                var_agg_delimiter = var_agg_delimiter
      ) %>%
        dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(.x, 0)))
    )
    var_apl_info <- parse_variable_wt_apl(names(var_info), apl_delimiter, var_apl_delimiter)
    
  } else {
    # Dependent Data - create aggregation if required
    model_df_rel<-aggregate_columns(model_df, names(var_info), var_agg_delimiter)
    
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
#' Determines the type of a model based on the presence of characters '+' and '-',
#' and processes the model variable to extract dependent and independent variables.
#' It classifies models into types like "Remodel", "Aggregate", "Segregate",
#' or "Aggregate & Segregate" based on the syntax of the model variable.
#'
#' @param model_variable A character string representing the model variable,
#'   typically a formula or expression where variables may be aggregated or
#'   segregated using '+' and '-'.
#' @param var_agg_delimiter A character string representing the delimiter for
#'   aggregated variables. Default is "|".
#' @param trim A logical indicating whether to trim whitespace from the output.
#'   Default is TRUE.
#' @param print_model_type A logical indicating whether to print the determined
#'   model type to the console. Default is TRUE.
#'
#' @return A list with two elements: 'dependent_var' containing the processed
#'   dependent variable(s) and 'independent_var' containing the processed
#'   independent variable(s).
#'
#' @examples
#' \dontrun{
#'   # Aggregation
#'   get_dep_indep_vars("TV_0_1_0+Sales_0_1_0")
#'   # Remodel
#'   get_dep_indep_vars("TV_0_1_0")
#'   # Segregation
#'   get_dep_indep_vars("TV-Sales_0_1_0")
#'   # Aggregation & Segregation
#'   get_dep_indep_vars("TV_0_1_0+-Sales_0_1_0")
#'   # Aggregation & Segregation, segregation is evaluated first and then aggregation
#'   get_dep_indep_vars("TV_0_1_0+Sales-Radio_0_1_0")
#'   # Aggregation with alternate delimiter
#'   get_dep_indep_vars("TV_0_1_0+Sales|Radio_0_1_0", var_agg_delimiter = "|")
#' }
#'
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
