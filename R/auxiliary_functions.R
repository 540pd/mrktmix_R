#' Convert String to Named Sequence
#'
#' This function takes a character vector, splits each string by a specified delimiter,
#' converts the split parts into numeric values, and assigns user-defined names
#' to these numeric values.
#'
#' @param char_vector A character vector with strings to be split and converted.
#' @param delimiter A character string specifying the delimiter used for splitting.
#' @param names_vector A character vector specifying the names to be assigned to the numeric values.
#' @return A list of named numeric vectors.
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' char_vector <- c("1|3|2", "4|6|2")
#' delimiter <- "|"
#' names_vector <- c("start", "end", "step")
#' convert_to_named_seq(char_vector, delimiter, names_vector)
#' }
convert_to_named_seq <-
  function(char_vector, delimiter, names_vector) {
    char_vector %>%
      stringr::str_split(stringr::fixed(delimiter)) %>%
      purrr::map(~ setNames(as.numeric(.x), names_vector))
  }

#' Name and Truncate List
#'
#' Takes a list and a vector of variable names, truncates the list
#' to the length of the variable names (if necessary), and sets the names
#' of the list elements to those variable names. If the list is shorter
#' than the names vector, the function truncates the names vector to the
#' length of the list.
#'
#' @param list_to_name A list to be named.
#' @param var_names A vector of names to be assigned to the list elements.
#' @return A named list.
#' @examples
#' \dontrun{
#' list_to_name <- list(1, 2, 3, 4)
#' var_names <- c("var1", "var2")
#' name_and_truncate_list(list_to_name, var_names)
#' }
name_and_truncate_list <- function(list_to_name, var_names) {
  if (length(var_names) > length(list_to_name)) {
    var_names <- var_names[1:length(list_to_name)]
  }
  rel_list <- list_to_name[1:length(var_names)]
  setNames(rel_list, var_names)
}

#' Split String and Convert to Numeric
#'
#' Splits each string in a character vector by a specified delimiter and converts
#' the resulting parts to numeric vectors. Returns a list of these numeric vectors.
#'
#' @param char_vector A character vector with strings to be split and converted to numeric.
#' @param delimiter A character string specifying the delimiter used for splitting.
#' @return A list of numeric vectors.
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' char_vector <- c("1,2,3", "4,5,6")
#' delimiter <- ","
#' split_and_convert_to_numeric(char_vector, delimiter)
#' }
split_and_convert_to_numeric <- function(char_vector, delimiter) {
  char_vector %>%
    stringr::str_split(delimiter) %>%
    map(~ as.numeric(.x))
}

#' Create Named Lists from Variables with Predefined Pairs
#'
#' Creates a list where each element is a copy of the predefined pairs list.
#' The output list will have the same names as the input `var_names` vector.
#' Note that each element in the output list is a direct copy of `predefined_pairs`,
#' and not a list of individual key-value pairs.
#'
#' @param var_names A named vector of variable names.
#' @param predefined_pairs A list of predefined key-value pairs to be replicated.
#' @return A named list where each element is a copy of the predefined pairs list.
#' @examples
#' \dontrun{
#' var_names <- c("var1" = "Variable 1", "var2" = "Variable 2")
#' predefined_pairs <- list(setNames(1:3, c("adstock", "power", "lag")))
#' create_named_lists_from_vars(var_names, predefined_pairs)
#' }
create_named_lists_from_vars <-
  function(var_names, predefined_pairs) {
    if (is.null(var_names) || length(var_names) == 0) {
      stop("var_names cannot be NULL or empty")
    }

    if (is.null(predefined_pairs) || !is.list(predefined_pairs)) {
      stop("predefined_pairs must be a non-empty list")
    }

    named_lists <-
      replicate(length(var_names), predefined_pairs, simplify = TRUE)
    setNames(named_lists, names(var_names))
  }

#' Replace Values Pairwise
#'
#' Replaces each value in an input vector that matches a condition from a list
#' of conditions with the corresponding value from a list of replacement values.
#' Error if lengths of conditions and replacements are not equal.
#'
#' @param input_vector An input vector with values to be replaced.
#' @param conditions_replacements A vector of conditions to be checked.
#' @param replacement_values A vector of replacement values.
#' @return A vector with replaced values.
#'
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' input_vector <- c(1, 2, 3, 4, 5)
#' conditions_replacements <- c(2, 4)
#' replacement_values <- c(20, 40)
#' replace_values_pairwise(input_vector, conditions_replacements, replacement_values)
#' }
replace_values_pairwise <-
  function(input_vector,
           conditions_replacements,
           replacement_values) {
    if (length(conditions_replacements) != length(replacement_values)) {
      stop("Length of conditions_replacements and replacement_values should be the same.")
    }
    for (i in seq_along(conditions_replacements)) {
      condition <- conditions_replacements[i]
      replacement <- replacement_values[i]
      input_vector[input_vector == condition] <- replacement
    }
    return(input_vector)
  }

#' Scope for Dependent Variable
#'
#' Scope for Dependent Variable
#'
#' Determines the feasible dependent variables based on previous and current variable names.
#'
#' @description
#' This function takes the previous named variable as a base and tries to find a new feasible named vector
#' based on whether it involves aggregation, aggregation and segregation, update in the variable,
#' or segregation. It assumes the current variable with APL is based on these cases in the named vector.
#' The names of these vectors will be variables with APL, and their values represent the contribution of the model.
#'
#'
#'
#' @param previous_var_apl Named numeric vector of previous variable names with application suffix.
#'                        The names must contain variables with APL as per the specified delimiter.
#'                        The values represent the contribution.
#' @param current_var_apl Named numeric vector of current variable names with application suffix.
#'                       The names must contain variables with APL as per the specified delimiter.
#'                       The values represent the contribution.
#' @param var_agg_delimiter Delimiter used for variable aggregation (default is "|").
#' @param delimiter Delimiter used for variable parts (default is "_").
#' @return Feasible dependent variables based on the given conditions.
#' @details
#' The function aims to identify feasible dependent variables by comparing the previous and current variable names.
#' It considers cases such as aggregation, aggregation and segregation, variable updates, or segregation in the process.
#' The named vectors generated represent the contribution of the model, with variable names containing APL information.
#' @examples
#' \dontrun{
#' previous_var_apl <- c("var1|A_0.2_2_0.5" = 0.5, "var2|B_0.5_3_0.3" = 0.3)
#' current_var_apl <- c("var1|A_0.2_2_0.5" = 0.6, "var3|B_0.4_1_0.2" = 0.2)
#' scope_for_dependent_variable(previous_var_apl, current_var_apl)
#' }
#' @export
scope_for_dependent_variable <-
  function(previous_var_apl,
           current_var_apl,
           var_agg_delimiter = "|",
           delimiter = "_") {
    if (var_agg_delimiter == delimiter) {
      stop("var_agg_delimiter and delimiter cannot be the same.")
    }

    previous_var_wo_apl <-
      sapply(str_split(names(previous_var_apl), stringr::fixed(delimiter)), function(x)
        x[1])
    current_var_wo_apl <-
      sapply(str_split(names(current_var_apl), stringr::fixed(delimiter)), function(x)
        x[1])

    previous_vars_wo_apl <-
      str_split(previous_var_wo_apl, stringr::fixed(var_agg_delimiter))
    current_vars_wo_apl <-
      str_split(current_var_wo_apl, stringr::fixed(var_agg_delimiter))

    previous_var_in_current_vars <-
      sapply(previous_vars_wo_apl, function(x)
        all(x %in% unlist(current_vars_wo_apl)))

    # remodel and regroup & split
    feasible_dependent_variables <-
      if (all(sapply(current_var_wo_apl, function(x)
        x %in% previous_var_wo_apl))) {
        previous_var_wo_apl
      } else
        # split
        if (length(current_vars_wo_apl) > 1 &&
            sum(unlist(previous_var_in_current_vars)) == 1) {
          c(previous_var_wo_apl[!previous_var_in_current_vars], current_var_wo_apl)
        } else
          # aggregation
          if (length(current_vars_wo_apl) == 1 &&
              length(previous_vars_wo_apl) > 1 &&
              length(purrr::reduce(
                unlist(current_vars_wo_apl),
                unlist(previous_vars_wo_apl),
                setdiff
              )) == 0) {
            c(previous_var_wo_apl[!previous_var_in_current_vars], current_var_wo_apl)
          } else {
            previous_var_wo_apl
          }
    return(c(current_var_apl[current_var_wo_apl %in% feasible_dependent_variables],
             previous_var_apl[previous_var_wo_apl %in% feasible_dependent_variables[!feasible_dependent_variables %in% current_var_wo_apl]]))
  }

#' Create File Path
#'
#' Create a file path based on the specified folder, file name, and relative directory.
#'
#' @param folder The folder in which the file is located.
#' @param file_name The name of the file.
#' @param relative_directory The relative directory path (default is ".").
#' @return A character vector representing the absolute file path.
#' @examples
#' \dontrun{
#' create_file_path("data", "example.csv")
#' create_file_path("output", "result.txt", relative_directory = "project")
#' }
create_file_path <- function(folder, file_name, relative_directory = ".") {
  # Check if the suffix indicates a CSV file
  path.expand(
    file.path(
      normalizePath(relative_directory),
      folder,
      file_name
    )
  )
}

#' Slowly Changing Dimension Type 2 Update Function
#'
#' This function updates a base dataset with changes from a current dataset using the
#' Slowly Changing Dimension Type 2 (SCD2) methodology. It handles additions of new
#' records and updates to existing records based on specified identification columns
#' and comparison columns. It assumes that the base data should not contain records
#' ending with '_base', and new data should not contain 'start_date' and 'end_date'.
#' Additionally, it only compares numeric values in `compare_cols` and excludes any
#' columns starting with 'source_' in `current_data`.
#'
#' @param base_data A dataframe representing the base data to be updated.
#' @param current_data A dataframe representing the current data with potential updates.
#' @param id_cols A vector of column names used for identifying records.
#' @param compare_cols A vector of column names used for comparing record values.
#' @param create_date The name of the column representing creation date in the base data.
#' @param update_date The name of the column representing update date in the base data.
#' @return A dataframe that is the result of applying SCD2 methodology on the base data
#'         with the updates from the current data.
#' @importFrom dplyr full_join group_by mutate filter
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' base_data <- data.frame(
#'   model_id = c(1, 1, 2, 3),
#'   version_id = c(1, 2, 3, 4),
#'   name = c("Alice", "Bob", "Charlie", "David"),
#'   value = c(100, 200, 300, 400),
#'   start_date = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-05", "2023-01-05")),
#'   end_date = as.POSIXct(NA)
#' )
#' id_cols <- c("model_id", "version_id")
#' compare_cols <- "value"
#' scd_type_2_update(base_data, data.frame(), id_cols, compare_cols)
#' }
#'
scd_type_2_update <-
  function(base_data,
           current_data,
           id_cols,
           compare_cols,
           create_date = "start_date",
           update_date = "end_date") {
    # Initialize start_date and end_date in base_data if not present
    if (!create_date %in% names(base_data)) {
      base_data[[create_date]] <- Sys.time()
    }
    if (!update_date %in% names(base_data)) {
      base_data[[update_date]] <- as.POSIXct(NA)
    }

    if(nrow(current_data)!=0){
      current_data[[create_date]] <- Sys.time()
      current_data[[update_date]] <- as.POSIXct(NA)
      current_data[["source_current"]] = TRUE

      current_base_data <- current_data %>%
        full_join(
          base_data %>% filter(is.na(get(update_date)))  %>% mutate(source_base = TRUE),
          by = id_cols,
          suffix = c("", "_base")
        )

      # data present in current but not in base, new data
      new_data_added<-current_base_data[is.na(current_base_data$source_base) & !is.na(current_base_data$source_current),names(current_data)]

      # common ids data
      common_data<-current_base_data[!is.na(current_base_data$source_base) & !is.na(current_base_data$source_current),]
      common_data$is_difference <- rowSums(abs(common_data[,compare_cols, drop = F] - common_data[,paste(compare_cols,"base",sep="_"), drop= F])!=0)
      common_data <- common_data %>%
        group_by(across(tidyselect::all_of(id_cols))) %>%
        mutate(id_cols_diff = sum(.data$is_difference))
      common_data[common_data$id_cols_diff!=0,paste0(update_date,"_base")]<-common_data[common_data$id_cols_diff!=0,create_date]

      old_data_updated<-rbind(
        current_base_data[!is.na(current_base_data$source_base) & is.na(current_base_data$source_current),c(id_cols,names(current_base_data)[names(current_base_data)  %in%   paste(names(base_data),"base",sep = "_")])],
        common_data[,c(id_cols,names(current_base_data)[names(current_base_data)  %in%   paste(names(base_data),"base",sep = "_")])])
      names(old_data_updated)<-c(id_cols,sub("_base","",names(old_data_updated)[!names(old_data_updated) %in% id_cols]))

      new_data_added<- rbind(common_data[common_data$id_cols_diff!=0,names(current_data)],
                             current_base_data[is.na(current_base_data$source_base) & !is.na(current_base_data$source_current),names(current_data)])

      rbind(old_data_updated, new_data_added[, !names(new_data_added) %in% "source_current"])
    }
    else {
      base_data
    }
  }

