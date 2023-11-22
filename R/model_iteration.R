#' Assemble Base Models
#'
#' Assembles base models for linear regression by applying various statistical
#' checks and thresholds. The function handles model creation, variable selection,
#' and preliminary analysis, preparing the ground for more detailed modeling.
#'
#' @param candidate_predictors A list containing fixed and flexible candidate
#'   predictors for the model. Each element in the list should be a list of variables,
#'   categorized as 'fixed' and 'flexible'.
#' @param candidate_predictors_info A dataframe containing independent information
#'   for each variable. This should include columns for variable name, type, adstock,
#'   power, lag, expected_sign, critical_pvalue, and critical_vif.
#' @param model_indep_df A dataframe containing the independent variables.
#' @param model_dep_df A dataframe containing the dependent variable.
#' @param with_intercept Logical; if TRUE, an intercept is included in the model.
#'   Default is TRUE.
#' @param drop_flexi_vars Logical; specifies the method for handling flexible variables.
#'   If TRUE, flexible variables are dropped iteratively. If FALSE, independent
#'   variables are tested only once in the model.
#' @param run_up_to_flexi_vars An integer representing the number of flexible
#'   variables to consider up to. Default is 0.
#' @param drop_pvalue_precision The precision for the p-value to drop variables.
#'   Default is 2.
#' @param discard_estimate_sign Logical; if TRUE, the sign of the estimate is
#'   ignored when dropping variables. Default is TRUE.
#' @param drop_highest_estimate Logical; if TRUE, the variable with the highest
#'   estimate is dropped. Default is FALSE.
#' @param get_model_object Logical; if TRUE, the function returns the linear model
#'   object instead of the default list. Default is FALSE.
#' @param always_check_vif Logical; if TRUE, the Variance Inflation Factor (VIF)
#'   is always checked. If FALSE, VIF will only be checked if there are no flags
#'   for p-value and no signs for the estimate. Default is FALSE.
#'
#' @return Depending on the value of `get_model_object`, the function returns either
#'   a list containing the base model and its statistics or the linear model object.
#'
#' @examples
#' \dontrun{
#'   candidate_predictors <- list(
#'     fixed = c("var1", "var2"),
#'     flexible = c("var3", "var4")
#'   )
#'   candidate_predictors_info <- data.frame(variable = c("var1", "var2", "var3", "var4"),
#'                                           type = rep("fixed", 4),
#'                                           adstock = rep(NA, 4),
#'                                           power = rep(NA, 4),
#'                                           lag = rep(NA, 4),
#'                                           expected_sign = rep(NA, 4),
#'                                           critical_pvalue = rep(0.05, 4),
#'                                           critical_vif = rep(5, 4))
#'   independent_df <- data.frame(var1 = rnorm(100), var2 = rnorm(100))
#'   dependent_df <- data.frame(dep_var = rnorm(100))
#'   base_models <- assemble_base_models(
#'     candidate_predictors = candidate_predictors,
#'     candidate_predictors_info = candidate_predictors_info,
#'     model_indep_df = independent_df,
#'     model_dep_df = dependent_df,
#'     with_intercept = TRUE,
#'     run_up_to_flexi_vars = 0,
#'     drop_pvalue_precision = 2,
#'     discard_estimate_sign = TRUE,
#'     drop_highest_estimate = FALSE,
#'     get_model_object = FALSE
#'   )
#' }
#'
#' @importFrom stats lm as.formula
#' @importFrom magrittr %>%
#'
assemble_base_models <- function(candidate_predictors, candidate_predictors_info, model_indep_df, model_dep_df,
                                 with_intercept = TRUE,
                                 drop_flexi_vars = TRUE, run_up_to_flexi_vars = 0,
                                 drop_pvalue_precision = 2, discard_estimate_sign = TRUE, drop_highest_estimate = FALSE,
                                 get_model_object = FALSE, always_check_vif = FALSE) {
  # Apply the 'apl' function on independent variables and replace NA with zero
  model_df_apl <- apply_apl(model_indep_df, unlist(unname(candidate_predictors), recursive = F))
  model_df_apl[is.na(model_df_apl)] <- 0

  independent_variable_info<-merge(candidate_predictors_info,
                                   data.frame(sum = c(sapply(model_df_apl, sum, na.rm = TRUE), setNames(nrow(model_dep_df),"(Intercept)"))),
                                   by.x="variable", by.y="row.names", all.x=T)

  # Combine dependent and independent data
  modeling_df <- dplyr::bind_cols(model_dep_df, model_df_apl[rownames(model_df_apl) %in% rownames(model_dep_df),])

  # Fit linear model
  lm_model_formula <- as.formula(if (with_intercept) "Y ~ ." else "Y ~ . - 1")
  lm_model <- lm(lm_model_formula, data = modeling_df)

  get_base_model(
    lm_model, modeling_df,independent_variable_info,
    drop_flexi_vars, run_up_to_flexi_vars,
    drop_pvalue_precision, discard_estimate_sign , drop_highest_estimate ,
    get_model_object,
    always_check_vif
  )
}

#' Collate Base Models
#'
#' Collates base models for linear regression from a list of candidate variable sets.
#' Utilizes the `assemble_base_models` function for model assembly, applying
#' various statistical checks and thresholds to each set of candidate predictors.
#'
#' @inheritParams assemble_base_models
#' @param candidate_variables_list A list of lists, where each sublist contains
#'   candidate predictors for a model. Each sublist should have a structure similar
#'   to the `candidate_predictors` parameter in `assemble_base_models`.
#' @param model_df A dataframe containing independent variables for all models.
#' @param model_dep_df A dataframe of the dependent variable 'Y' for all models.
#' @param pos_sign_variables A vector of variable names expected to have a positive
#'   sign in the model.
#' @param neg_sign_variables A vector of variable names expected to have a negative
#'   sign in the model.
#' @param base_variables A vector of variable names representing base variables
#'   whose coefficients are set to 1 in the model. Default is NA, indicating no base variables.
#' @param always_check_vif Logical; if TRUE, the Variance Inflation Factor (VIF)
#'   is always checked. If FALSE, VIF will only be checked if there are no flags
#'   for p-value and no signs for the estimate. Default is FALSE.
#' @param var_agg_delimiter A character string used as the delimiter to aggregate
#'   variables. Default is "\\|".
#' @param vif_threshold A numeric threshold for the Variance Inflation Factor (VIF)
#'   above which variables are dropped. Default is 10.
#' @param pvalue_thresholds A named list containing p-value thresholds for
#'   intercept, fixed, and flexible variables. Default is c(intercept = 0.15,
#'   fixed = 0.15, flexible = 0.15).
#'
#' @return A list containing collated base models and associated statistics.
#'   The list includes model coefficients (`model_coef_all`), summary statistics
#'   (`model_smry_all`), and the linear model objects (`lm_model_all`).
#'
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
#'     model_dep_df = dependent_df,
#'     pos_sign_variables = c("var1", "var2"),
#'     neg_sign_variables = c("var3", "var4"),
#'     base_variables = c("var1", "var2")
#'   )
#' }
#'
#' @importFrom purrr map_dfr
#'
collate_base_models <- function(candidate_variables_list, model_df, model_dep_df,
                                pos_sign_variables, neg_sign_variables,  base_variables = NA,
                                with_intercept = TRUE,
                                var_agg_delimiter = "|",
                                drop_flexi_vars = TRUE, run_up_to_flexi_vars = 0,
                                vif_threshold = 10, pvalue_thresholds = c(intercept = 0.15, fixed = 0.15, flexible = 0.15),
                                drop_pvalue_precision = 2, discard_estimate_sign = TRUE, drop_highest_estimate = FALSE,
                                get_model_object = FALSE, always_check_vif = FALSE) {

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

 candidate_variables_df<-purrr::map_dfr(candidate_variables_list, function(candidate_variables_l){
        apl_df<-lapply(candidate_variables_l, function(df) as.data.frame(t(data.frame(df, check.names = F))))
        apl_df_wt_names<- purrr::map2(names(candidate_variables_l), apl_df, function(type_of_var,df) {
            df$type <- type_of_var
            df$variable <- row.names(df)
            df
        })
      dplyr::bind_rows(apl_df_wt_names)},.id = "model_id")

  if (with_intercept) {
    intercetp_df<-data.frame(variable ="(Intercept)", adstock = NA, power = NA, lag = NA, type = "intercept", row.names = "(Intercept)")
    intercetp_df <- intercetp_df[rep(1, length(candidate_variables_list)), ]
    intercetp_df$model_id<-1:nrow(intercetp_df)
    candidate_variables_df<-rbind(candidate_variables_df,intercetp_df)
  }

  expected_sign <- determine_expected_sign(names(model_df), pos_sign_variables, neg_sign_variables, var_agg_delimiter)
  expected_sign_df<-data.frame(variable = names(model_df),expected_sign =expected_sign)
  candidate_variables_df<-merge(candidate_variables_df, expected_sign_df, by ="variable", all.x=T)

  critical_pval_df <- data.frame(type = names(pvalue_thresholds), critical_pvalue = pvalue_thresholds)
  candidate_variables_df<-merge(candidate_variables_df, critical_pval_df, by ="type", all.x=T)

  candidate_variables_df<-candidate_variables_df %>%
    dplyr::group_by(.data$model_id) %>%
    dplyr::mutate(count_fixed_var=sum(.data$type=="fixed")) %>%
    dplyr::ungroup()
  candidate_variables_df$critical_vif<-vif_threshold
  candidate_variables_df$critical_vif[candidate_variables_df$count_fixed_var==1]<-Inf

  candidate_variables_df_list<-candidate_variables_df[,c("model_id", "variable", "type", "adstock", "power", "lag",
                                                         "expected_sign", "critical_pvalue", "critical_vif")] %>%
    dplyr::group_split(as.factor(.data$model_id), .keep = FALSE)

  base_models <- purrr::pmap(list(candidate_variables_list,candidate_variables_df_list), function(candidate_predictors,candidate_predictors_info)
    assemble_base_models(
      candidate_predictors, candidate_predictors_info, model_df, model_dep_df_modified,
      with_intercept, drop_flexi_vars, run_up_to_flexi_vars,
      drop_pvalue_precision, discard_estimate_sign, drop_highest_estimate,
      get_model_object, always_check_vif)
  )


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
#' Extends the `collate_base_models` function to include additional preprocessing
#' of the dependent variable. This function collates models for linear regression
#' from a list of candidate variable sets, applying preprocessing steps to the
#' dependent variable and assembling the models.
#'
#' @inheritParams collate_base_models
#' @param dep_var_info Information about the dependent variable, such as its
#'   transformation and lag, provided in a specific format.
#' @param pos_sign_variables A vector of variable names expected to have a positive
#'   sign in the model.
#' @param neg_sign_variables A vector of variable names expected to have a negative
#'   sign in the model.
#' @param mdl_start_date The start date for filtering the data before model assembly.
#'   Should be of class `Date`. Default is NA.
#' @param mdl_end_date The end date for filtering the data before model assembly.
#'   Should be of class `Date`. Default is NA.
#' @param apl_delimiter The delimiter used in variable names to separate APL components.
#'   Default is "_".
#' @param var_apl_delimiter The delimiter used between variable names and APL attributes.
#'   Default is "|".
#' @param drop_discard_estimate_sign  Logical; if TRUE, the sign of the estimate is
#'   ignored when dropping variables. Default is TRUE.
#' @param always_check_vif Logical; if TRUE, the Variance Inflation Factor (VIF)
#'   is always checked. If FALSE, VIF will only be checked if there are no flags
#'   for p-value and no signs for the estimate. Default is FALSE.
#'
#' @return A list containing the collated models after filtering and preprocessing,
#'   along with associated statistics. This list includes details about the dependent
#'   variable transformations, summary statistics, model coefficients, and linear model objects.
#'
#' @importFrom pbapply pbapply
#' @export
#' @examples
#' \dontrun{
#'   candidate_variables_list <- list(
#'     list(fixed = c("var1"), flexible = c("var3")),
#'     list(fixed = c("var2"), flexible = c("var4"))
#'   )
#'   independent_df <- data.frame(var1 = rnorm(100), var2 = rnorm(100))
#'   dependent_df <- data.frame(Y = rnorm(100))
#'   dep_info <- list(transformation = "log", lag = 1)
#'   filter_start <- as.Date("2000-01-01")
#'   filter_end <- as.Date("2005-12-31")
#'
#'   results <- collate_models(
#'     candidate_variables_list,
#'     model_df = independent_df,
#'     model_dep_df = dependent_df,
#'     dep_var_info = dep_info,
#'     mdl_start_date = filter_start,
#'     mdl_end_date = filter_end
#'     # Additional parameters can be set as needed
#'   )
#' }
#'
collate_models<-function(candidate_variables_list, model_df, dep_var_info,
                         with_intercept,
                         base_variables,
                         pos_sign_variables = NA, neg_sign_variables = NA,
                         mdl_start_date=NA, mdl_end_date=NA,
                         var_agg_delimiter = "|",apl_delimiter = "_",var_apl_delimiter = "|",
                         drop_flexi_vars = TRUE, run_up_to_flexi_vars = NA,
                         vif_threshold = 10, pvalue_thresholds = c(intercept = 0.15, fixed = 0.15, flexible = 0.15),
                         drop_pvalue_precision = 2, drop_discard_estimate_sign = TRUE, drop_highest_estimate = FALSE,
                         get_model_object = FALSE, always_check_vif = FALSE){

  dep_apl_df_list<-generate_model_dependent(dep_var_info,model_df, apl_delimiter , var_apl_delimiter)
  dep_apl_df_list[[1]] <- dep_apl_df_list[[1]] %>% tibble::rownames_to_column("dependent_id")

  dep_apl_df_list[[2]] <-lapply(dep_apl_df_list[[2]],  function(x) data.frame(Y=rowSums(x)))

  if(!is.na(mdl_start_date)){
    dep_apl_df_list[[2]]<-purrr::map(dep_apl_df_list[[2]], function(x) x[rownames(x)>=mdl_start_date,,drop=F])
  }
  if(!is.na(mdl_start_date)){
    dep_apl_df_list[[2]]<-purrr::map(dep_apl_df_list[[2]], function(x) x[rownames(x)<=mdl_end_date,,drop=F])
  }

  #Aggregate independent variable
  candidate_variables_list_variable<-unique(unlist(lapply(unlist(candidate_variables_list, recursive = F), names)))
  if(any(!is.na(base_variables))){
    candidate_variables_list_variable<-unique(c(candidate_variables_list_variable,base_variables))
  }
  model_df_rel<-aggregate_columns(model_df, candidate_variables_list_variable, delimiter = var_agg_delimiter)

  if(is.na(run_up_to_flexi_vars)){
    run_up_to_flexi_vars <- 1000000 # Assuming that there won't be any regression with more than 1 million independent variables
  }

  model_result <- purrr::map(dep_apl_df_list[[2]],~collate_base_models(candidate_variables_list,  model_df_rel, .x ,
                                                                       pos_sign_variables , neg_sign_variables ,
                                                                       base_variables, with_intercept,
                                                                       var_agg_delimiter ,
                                                                       drop_flexi_vars, run_up_to_flexi_vars,
                                                                       vif_threshold, pvalue_thresholds,
                                                                       drop_pvalue_precision, drop_discard_estimate_sign, drop_highest_estimate,
                                                                       get_model_object, always_check_vif ))

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
