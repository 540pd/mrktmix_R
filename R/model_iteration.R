#' Assemble Base Models
#'
#' This function assembles base models for linear regression, applying various
#' statistical checks and thresholds.
#'
#' @param candidate_predictors A list containing fixed and flexible candidate
#'   predictors for the model. Each list must contain both a fixed and a flexible
#'   list of variables.
#' @param model_indep_df A dataframe of independent variables.
#' @param model_dep_df A dataframe of the dependent variable.
#' @param with_intercept Logical; if TRUE, an intercept is included in the model.
#' @param pos_vars A vector of variable names expected to have a positive
#'   sign in the model.
#' @param neg_vars A vector of variable names expected to have a negative
#'   sign in the model.
#' @param var_agg_delimiter A character string used as the delimiter to aggregate
#'   variables.
#' @param run_up_to_flexi_vars An integer representing the number of flexible
#'   variables to consider up to.
#' @param vif_threshold A numeric threshold for the Variance Inflation Factor (VIF)
#'   above which variables are dropped.
#' @param pvalue_thresholds A named list containing p-value thresholds for
#'   intercept, fixed, and flexible variables.
#' @param drop_pvalue_precision The precision for the p-value to drop variables.
#' @param discard_estimate_sign Logical; if TRUE, the sign of the estimate is
#'   ignored when dropping variables.
#' @param drop_highest_estimate Logical; if TRUE, the variable with the highest
#'   estimate is dropped.
#' @param get_model_object Logical; if TRUE, the function returns the linear model
#'   object instead of the default list.
#'
#' @return A list containing the base model and its statistics, or the model
#'   object if `get_model_object` is TRUE.
#' @importFrom stats lm as.formula
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#'   candidate_predictors <- list(
#'     fixed = c("var1", "var2"),
#'     flexible = c("var3", "var4")
#'   )
#'   independent_df <- data.frame(var1 = rnorm(100), var2 = rnorm(100))
#'   dependent_df <- data.frame(dep_var = rnorm(100))
#'
#'   base_models <- assemble_base_models(
#'     candidate_predictors = candidate_predictors,
#'     model_indep_df = independent_df,
#'     model_dep_df = dependent_df,
#'     c("TV", "Radio"), c("Date2021_01_01","Date2021_01_08","Date2021_01_15")
#'     # Additional parameters can be set as needed
#'   )
#' }
assemble_base_models <- function(candidate_predictors, model_indep_df, model_dep_df,
                                 pos_vars, neg_vars,
                                 with_intercept = TRUE,
                                 var_agg_delimiter = "\\|", run_up_to_flexi_vars = 0,
                                 vif_threshold = 10, pvalue_thresholds = c(intercept = 0.15, fixed = 0.15, flexible = 0.15),
                                 drop_pvalue_precision = 2, discard_estimate_sign = TRUE, drop_highest_estimate = FALSE,
                                 get_model_object = FALSE) {
  # Apply the 'apl' function on independent variables and replace NA with zero
  # model_df_apl <- apply_apl(model_indep_df, c(candidate_predictors[[1]], candidate_predictors[[2]]))
  model_df_apl <- apply_apl(model_indep_df, unlist(unname(candidate_predictors), recursive = F))
  model_df_apl[is.na(model_df_apl)] <- 0

  # Combine predictors info and calculate sum ignoring NA
  # independent_variable_info <- dplyr::bind_rows(
  #   dplyr::bind_rows(candidate_predictors[[1]], .id = "variable") %>%
  #     dplyr::mutate(type = names(candidate_predictors[1])),
  #   dplyr::bind_rows(candidate_predictors[[2]], .id = "variable") %>%
  #     dplyr::mutate(type = names(candidate_predictors[2]))
  # ) %>%
  #   dplyr::bind_cols(sum = sapply(model_df_apl, sum, na.rm = TRUE))
  candidate_predictors_unlist<-unlist(candidate_predictors,recursive=F)
  independent_variable_info <- dplyr::bind_rows(candidate_predictors_unlist) %>%
    dplyr::mutate(var=names(candidate_predictors_unlist))%>%
    tidyr::separate("var", c("type", "variable"), sep = "\\.", extra = "merge") %>%
    dplyr::bind_cols(sum = sapply(model_df_apl, sum, na.rm = TRUE))
  # %>%
  #   bind_rows(data.frame(variable = "(Intercept)", sum = nrow(model_df_apl), type = "intercept"))


  # Add intercept info if necessary
  if (with_intercept) {
    independent_variable_info <- dplyr::bind_rows(independent_variable_info, tibble::tibble(variable = "(Intercept)", sum = nrow(model_df_apl), type = "intercept"))
  }

  # Combine dependent and independent data
  modeling_df <- dplyr::bind_cols(model_dep_df, model_df_apl[rownames(model_df_apl) %in% rownames(model_dep_df),])


  # Fit linear model
  lm_model_formula <- as.formula(if (with_intercept) "Y ~ ." else "Y ~ . - 1")
  lm_model <- lm(lm_model_formula, data = modeling_df)

  # Retrieve base model
  get_base_model(
    lm_model, modeling_df,independent_variable_info, pos_vars, neg_vars,
    var_agg_delimiter, run_up_to_flexi_vars,
    vif_threshold, pvalue_thresholds ,
    drop_pvalue_precision, discard_estimate_sign , drop_highest_estimate ,
    get_model_object
  )
}

#' Collate Base Models
#'
#' This function collates base models for linear regression from a list of candidate
#' variable sets, using the `assemble_base_models` function for model assembly.
#'
#' @inheritParams assemble_base_models
#' @param candidate_variables_list A list of lists, where each sublist contains
#'   candidate predictors for a model.
#' @param model_df A dataframe of independent variables for all models.
#' @param model_dep_df A dataframe of the dependent variable 'Y' for all models.
#' @param pos_sign_variables A vector of variable names expected to have a positive
#'   sign in the model.
#' @param neg_sign_variables A vector of variable names expected to have a negative
#'   sign in the model.
#' @param base_variables A vector of variable names representing base. Its coefficient will be 1.
#' @return A list containing the collated base models and associated statistics.
#' @importFrom purrr map_dfr
#'
#' @export
#' @examples
#' \dontrun{
#'   candidate_variables_list <- list(
#'     list(fixed = c("var1"), flexible = c("var3")),
#'     list(fixed = c("var2"), flexible = c("var4"))
#'   )
#'   independent_df <- data.frame(var1 = rnorm(100), var2 = rnorm(100))
#'   dependent_df <- data.frame(Y = rnorm(100))
#'
#'   results <- collate_base_models(
#'     candidate_variables_list,
#'     model_df = independent_df,
#'     model_dep_df = dependent_df
#'     # Additional parameters can be set as needed
#'   )
#' }
collate_base_models <- function(candidate_variables_list, model_df, model_dep_df,
                                pos_sign_variables, neg_sign_variables,  base_variables = NA,
                                with_intercept = TRUE,
                                var_agg_delimiter = "\\|", run_up_to_flexi_vars = 0,
                                vif_threshold = 10, pvalue_thresholds = c(intercept = 0.15, fixed = 0.15, flexible = 0.15),
                                drop_pvalue_precision = 2, discard_estimate_sign = TRUE, drop_highest_estimate = FALSE,
                                get_model_object = FALSE) {
  # Ensure proper data types and structures
  stopifnot(is.list(candidate_variables_list))
  stopifnot(is.data.frame(model_df))
  stopifnot(is.data.frame(model_dep_df))

  dependent_sum <- sum(model_dep_df$Y)

  if(any(!is.na(base_variables))){
    base_data<-model_df %>%
      dplyr::select(tidyselect::all_of(base_variables))
    model_dep_df_modified<-model_dep_df %>%
      dplyr::transmute(Y = .data$Y-rowSums(base_data))
  } else {
    base_data<-NA
    model_dep_df_modified<-model_dep_df
  }

  base_models <- purrr::map(candidate_variables_list, ~assemble_base_models(
    .x, model_df, model_dep_df_modified,
    pos_sign_variables, neg_sign_variables,
    with_intercept,
    var_agg_delimiter, run_up_to_flexi_vars,
    vif_threshold, pvalue_thresholds,
    drop_pvalue_precision, discard_estimate_sign, drop_highest_estimate,
    get_model_object
  ))

  model_coef_all <- purrr::map_dfr(base_models, 1, .id = "model_id")

  if(any(!is.na(base_variables))){
    model_coef_all <- model_coef_all %>%
      dplyr::bind_rows(t(as.data.frame(base_data %>%
                                         dplyr::summarise(across(everything(), sum))))%>%
                         as.data.frame() %>%
                         tibble::as_tibble(rownames = "variable") %>%
                         dplyr::rename(sum="V1") %>%
                         dplyr::mutate(Estimate = 1, type = "offset") %>%
                         dplyr::cross_join(model_coef_all %>%
                                             dplyr::select(tidyselect::all_of(c("model_id","loop_id"))) %>%
                                             dplyr::distinct())
      )
  }
  model_coef_all <- model_coef_all %>%
    dplyr::mutate(dep_sum = dependent_sum)

  model_smry_all<-purrr::map_dfr(base_models, 2, .id = "model_id") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      residuals=sum(unlist(.data$residuals_all)),
      rmse = sqrt(mean((unlist(.data$residuals_all))^2)),
      mae = mean(abs(unlist(.data$residuals_all))),
      mape=mean(abs(unlist(.data$residuals_all))/model_dep_df$Y*100),
      dependent_sum=dependent_sum) %>%
    dplyr::select(-"residuals_all")

  lm_model_all <- purrr::map(base_models, 3)

  list(model_coef_all, model_smry_all, lm_model_all)
}

#' Collate Models
#'
#' This function extends the `collate_base_models` function to accommodate
#' additional preprocessing of the dependent variable.
#'
#' @inheritParams collate_base_models
#' @param dep_var_info Information about the dependent variable, such as its
#'   transformation and lag.
#' @param pos_sign_variables A vector of variable names expected to have a positive
#'   sign in the model.
#' @param neg_sign_variables A vector of variable names expected to have a negative
#'   sign in the model.
#' @param mdl_start_date The start date to filter the data for model assembly.
#' @param mdl_end_date The end date to filter the data for model assembly.
#' @param apl_delimiter The delimiter used in variable names to separate APL components.
#' @param var_apl_delimiter The delimiter used in-between variable names and apl.
#'
#' @return A list containing the collated models after filtering and preprocessing,
#'   along with associated statistics.
#'
#' @importFrom pbapply pbapply
#' @export
#' @examples
#' \dontrun{
#'   # Assuming 'date' is a column in 'independent_df' and 'dependent_df' containing
#'   # Date class objects.
#'   filter_start <- as.Date("2000-01-01")
#'   filter_end <- as.Date("2005-12-31")
#'
#'   dep_info <- list(transformation = "log", lag = 1)
#'
#'   results <- collate_models(
#'     candidate_variables_list,
#'     model_df = independent_df,
#'     model_dep_df = dependent_df,
#'     dep_var_info = dep_info,
#'     filter_start_date = filter_start,
#'     filter_end_date = filter_end
#'     # Additional parameters can be set as needed
#'   )
#' }
collate_models<-function(candidate_variables_list, model_df, dep_var_info,
                         pos_sign_variables, neg_sign_variables,
                         mdl_start_date=NA, mdl_end_date=NA,
                         base_variables = NA, with_intercept = TRUE,
                         var_agg_delimiter = "|",apl_delimiter = "_",var_apl_delimiter = "|",
                         run_up_to_flexi_vars = 100,
                         vif_threshold = 10, pvalue_thresholds = c(intercept = 0.15, fixed = 0.15, flexible = 0.15),
                         drop_pvalue_precision = 2, discard_estimate_sign = TRUE, drop_highest_estimate = FALSE,
                         get_model_object = FALSE){

  dep_apl_df_list<-generate_model_dependent(dep_var_info,model_df, apl_delimiter , var_apl_delimiter)
  dep_apl_df_list[[1]] <- dep_apl_df_list[[1]] %>% tibble::rownames_to_column("dependent_id")

  if(!is.na(mdl_start_date)){
    dep_apl_df_list[[2]]<-purrr::map(dep_apl_df_list[[2]], function(x) x[rownames(x)>=mdl_start_date,,drop=F])
  }
  if(!is.na(mdl_start_date)){
    dep_apl_df_list[[2]]<-purrr::map(dep_apl_df_list[[2]], function(x) x[rownames(x)<=mdl_end_date,,drop=F])
  }

  #Aggregate independent variable
  candidate_variables_list_variable<-unique(unlist(lapply(unlist(candidate_variables_list, recursive = F), names)))
  model_df_rel<-aggregate_columns(model_df, candidate_variables_list_variable, delimeter = var_agg_delimiter)

  model_result <- purrr::map(dep_apl_df_list[[2]],~collate_base_models(candidate_variables_list,  model_df_rel, .x %>% dplyr::rename(Y = names(.x)[1]),
                                                                       pos_sign_variables , neg_sign_variables ,
                                                                       base_variables, with_intercept,
                                                                       var_agg_delimiter , run_up_to_flexi_vars,
                                                                       vif_threshold, pvalue_thresholds,
                                                                       drop_pvalue_precision, discard_estimate_sign, drop_highest_estimate,
                                                                       get_model_object ))

  model_coef_all <- purrr::map_dfr(model_result, 1, .id = "dependent_id") %>%
                       dplyr::mutate(contri = .data[["Estimate"]] * .data[["sum"]],
                                     contri_perc = .data[["contri"]]/.data[["dep_sum"]]*100)

  model_smry_all <- purrr::map_dfr(model_result, 2, .id = "dependent_id")

  # Initial summarization of flags
  mdl_smry_flag <- model_coef_all %>%
    dplyr::group_by(.data$dependent_id, .data$model_id, .data$loop_id) %>%
    dplyr::summarise(
      flag_pvalue = sum(.data[["flag_pvalue"]], na.rm = TRUE),
      flag_sign = sum(.data[["flag_sign"]], na.rm = TRUE),
      flag_vif = sum(.data[["flag_vif"]], na.rm = TRUE),
      .groups = "drop"
    )

  # Summarize by type and then pivot to long format
  smry_var_type <- model_coef_all %>%
    dplyr::group_by(.data$dependent_id, .data$model_id, .data$loop_id, .data$type) %>%
    dplyr::summarise(
      contri = sum(.data[["contri"]], na.rm = TRUE),
      contri_perc = sum(.data[["contri_perc"]], na.rm = TRUE),
      vars_count = dplyr::n(),
      adstock_avg = mean(.data[["adstock"]], na.rm = TRUE),
      power_avg = mean(.data[["power"]], na.rm = TRUE),
      pvalue_avg = mean(.data[["Pr(>|t|)"]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = "contri":"pvalue_avg",
      names_to = "variable_name",
      values_to = "value"
    ) %>%
    dplyr::mutate(variable = paste(.data[["type"]], .data[["variable_name"]], sep = "_")) %>%
    dplyr::select(-"type", -"variable_name")

  # Pivot wider and prepare for join
  mdl_smry_var_type <- smry_var_type %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "value")

  # Summarize contributions and percentages, then pivot longer
  mdl_smry_var <- model_coef_all %>%
    dplyr::select("dependent_id", "model_id", "loop_id", "variable", "contri", "contri_perc") %>%
    dplyr::group_by(.data$dependent_id, .data$model_id, .data$loop_id, .data$variable) %>%
    dplyr::summarise(
      contri = sum(.data$contri),
      contri_perc = sum(.data$contri_perc),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = "contri":"contri_perc",
      names_to = "variable_name",
      values_to = "value"
    ) %>%
    dplyr::mutate(variable_new = paste(.data$variable, .data$variable_name, sep = "_")) %>%
    dplyr::select(-"variable", -"variable_name")

  # Pivot wider and prepare for join
  mdl_smry_var_wide <- mdl_smry_var %>%
    tidyr::pivot_wider(names_from = "variable_new", values_from = "value")

  # Join all the summaries
  mdl_smry <- dplyr::full_join(mdl_smry_flag, mdl_smry_var_type, by = c("dependent_id", "model_id", "loop_id")) %>%
    dplyr::full_join(mdl_smry_var_wide, by = c("dependent_id", "model_id", "loop_id"))

  # Add flag number and join everything
  mdl_smry <- mdl_smry %>%
    dplyr::mutate(flag_num = rowSums(dplyr::across(c("flag_pvalue", "flag_sign", "flag_vif")), na.rm = TRUE))

  model_smry_all <- dplyr::full_join(model_smry_all, mdl_smry, by = c("dependent_id", "model_id", "loop_id"))

  lm_model_all <- purrr::map(model_result, 3)

  list(dep_apl_df_list[[1]], model_smry_all, model_coef_all, lm_model_all)
}
