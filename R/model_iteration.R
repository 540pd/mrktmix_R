#' Collate Base Models
#'
#' This function processes a series of models and performs various operations
#' such as binding columns, fitting linear models, and extracting model statistics.
#' It handles various options for model fitting and processing, including intercept inclusion,
#' flexible variable handling, p-value precision, estimate sign considerations, and more.
#'
#' @param model_dep_df A dataframe containing the dependent variable.
#' @param model_apl_list A list of dataframes corresponding to each model's independent variables.
#' @param with_intercept Logical; if TRUE, an intercept is included in the model.
#' @param base_data A dataframe containing base data to be used in the model.
#' @param independent_variable_info_list A list of dataframes, each containing information about the independent variables used in the corresponding model.
#' @param dependent_sum A numeric value representing the sum of the dependent variable, used for model summary calculations.
#' @param drop_flexi_vars Logical; specifies whether to drop flexible variables from the model.
#' @param run_up_to_flexi_vars An integer specifying the number of flexible variables to consider in the model.
#' @param drop_pvalue_precision The precision level for p-value used in deciding whether to drop variables from the model.
#' @param discard_estimate_sign Logical; if TRUE, the sign of the estimate is ignored when considering variable inclusion in the model.
#' @param drop_highest_estimate Logical; if TRUE, the variable with the highest estimate is dropped from the model.
#' @param get_model_object Logical; if TRUE, the function returns the linear model object; otherwise, it returns a list of model summaries.
#' @param always_check_vif Logical; if TRUE, the Variance Inflation Factor (VIF) is always checked in the model.
#'
#' @return A list containing three elements: model coefficients, model summary statistics, and linear model objects, depending on the provided parameters.
#'
#' @examples
#' \dontrun{
#'   model_dep_df <- data.frame(Y = rnorm(100))
#'   model_apl_list <- list(data.frame(var1 = rnorm(100)), data.frame(var2 = rnorm(100)))
#'   independent_variable_info_list <- list(
#'     data.frame(variable = "var1"),
#'     data.frame(variable = "var2")
#'   )
#'   base_data <- data.frame(base_var = rnorm(100))
#'   collate_base_models(
#'     model_dep_df = model_dep_df,
#'     model_apl_list = model_apl_list,
#'     with_intercept = TRUE,
#'     base_data = base_data,
#'     independent_variable_info_list = independent_variable_info_list,
#'     dependent_sum = 100,
#'     drop_flexi_vars = TRUE,
#'     run_up_to_flexi_vars = 0,
#'     drop_pvalue_precision = 2,
#'     discard_estimate_sign = TRUE,
#'     drop_highest_estimate = FALSE,
#'     get_model_object = FALSE,
#'     always_check_vif = FALSE
#'   )
#' }
#'
#' @importFrom dplyr bind_cols filter group_by mutate rowwise select summarise ungroup
#' @importFrom purrr map2 map_dfr
#' @importFrom stats lm as.formula
#'
collate_base_models <-
  function(model_dep_df,
           model_apl_list,
           with_intercept,
           base_data,
           independent_variable_info_list,
           dependent_sum,
           drop_flexi_vars,
           run_up_to_flexi_vars,
           drop_pvalue_precision,
           discard_estimate_sign ,
           drop_highest_estimate ,
           get_model_object,
           always_check_vif) {
    base_models <-
      purrr::map2(model_apl_list, independent_variable_info_list, function(model_df_apl,
                                                                           independent_variable_info) {
        modeling_df <-
          cbind(model_dep_df, model_df_apl)

        # Fit linear model
        lm_model_formula <-
          as.formula(if (with_intercept)
            "Y ~ ."
            else
              "Y ~ . - 1")
        lm_model <- lm(lm_model_formula, data = modeling_df)

        get_base_model(
          lm_model,
          modeling_df,
          independent_variable_info,
          drop_flexi_vars,
          run_up_to_flexi_vars,
          drop_pvalue_precision,
          discard_estimate_sign ,
          drop_highest_estimate ,
          get_model_object,
          always_check_vif
        )

      },
      .progress = T)


    model_coef_all <-
      purrr::map_dfr(base_models, 1, .id = "model_id")

    if (ncol(base_data)) {
      model_coef_all <- model_coef_all %>%
        dplyr::bind_rows(
          t(as.data.frame(
            base_data %>%
              dplyr::summarise(across(everything(), sum))
          )) %>%
            as.data.frame() %>%
            tibble::as_tibble(rownames = "variable") %>%
            dplyr::rename(sum = "V1") %>%
            dplyr::mutate(Estimate = 1, type = "offset") %>%
            dplyr::cross_join(
              model_coef_all %>%
                dplyr::select(tidyselect::all_of(c(
                  "model_id", "loop_id"
                ))) %>%
                dplyr::distinct()
            )
        )
    }
    model_coef_all <- model_coef_all %>%
      dplyr::mutate(dep_sum = dependent_sum)

    model_smry_all <-
      purrr::map_dfr(base_models, 2, .id = "model_id") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        residuals = sum(unlist(.data$residuals_all)),
        rmse = sqrt(mean((
          unlist(.data$residuals_all)
        ) ^ 2)),
        mae = mean(abs(unlist(
          .data$residuals_all
        ))),
        mape = mean(abs(unlist(
          .data$residuals_all
        )) / model_dep_df$Y * 100),
        dependent_sum = dependent_sum
      ) %>%
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

#' @param candidate_variables_list A list of lists, where each sublist contains
#'   candidate predictors for a model. Each sublist should have a structure similar
#'   to the `candidate_predictors` parameter in `assemble_base_models`.
#' @param model_df A dataframe containing independent variables for all models.
#' @param dep_var_info Information about the dependent variable, such as its
#'   transformation and lag, provided in a specific format.
#' @param with_intercept Logical; if TRUE, an intercept is included in the model.
#'   Default is TRUE.
#' @param base_variables A vector of variable names representing base variables
#'   whose coefficients are set to 1 in the model. Default is NA, indicating no base variables.
#' @param pos_sign_variables A vector of variable names expected to have a positive
#'   sign in the model.
#' @param neg_sign_variables A vector of variable names expected to have a negative
#'   sign in the model.
#' @param mdl_start_date The start date for filtering the data before model assembly.
#'   Should be of class `Date`. Default is NA.
#' @param mdl_end_date The end date for filtering the data before model assembly.
#'   Should be of class `Date`. Default is NA.
#' @param var_agg_delimiter A character string used as the delimiter to aggregate
#'   variables. Default is "\\|".
#' @param apl_delimiter The delimiter used in variable names to separate APL components.
#'   Default is "_".
#' @param var_apl_delimiter The delimiter used between variable names and APL attributes.
#'   Default is "|".
#' @param drop_flexi_vars Logical; specifies the method for handling flexible variables.
#'   If TRUE, flexible variables are dropped iteratively. If FALSE, independent
#'   variables are tested only once in the model.
#' @param run_up_to_flexi_vars An integer representing the number of flexible
#'   variables to consider up to. Default is 0.
#' @param vif_threshold A numeric threshold for the Variance Inflation Factor (VIF)
#'   above which variables are dropped. Default is 10.
#' @param pvalue_thresholds A named list containing p-value thresholds for
#'   intercept, fixed, and flexible variables. Default is c(intercept = 0.15,
#'   fixed = 0.15, flexible = 0.15).
#' @param drop_pvalue_precision The precision for the p-value to drop variables.
#'   Default is 2.
#' @param drop_discard_estimate_sign  Logical; if TRUE, the sign of the estimate is
#'   ignored when dropping variables. Default is TRUE.
#' @param drop_highest_estimate Logical; if TRUE, the variable with the highest
#'   estimate is dropped. Default is FALSE.
#' @param get_model_object Logical; if TRUE, the function returns the linear model
#'   object instead of the default list. Default is FALSE.
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
collate_models <-
  function(candidate_variables_list,
           model_df,
           dep_var_info,
           with_intercept,
           base_variables,
           pos_sign_variables = NA,
           neg_sign_variables = NA,
           mdl_start_date = NA,
           mdl_end_date = NA,
           var_agg_delimiter = "|",
           apl_delimiter = "_",
           var_apl_delimiter = "|",
           drop_flexi_vars = TRUE,
           run_up_to_flexi_vars = NA,
           vif_threshold = 10,
           pvalue_thresholds = c(intercept = 0.15,
                                 fixed = 0.15,
                                 flexible = 0.15),
           drop_pvalue_precision = 2,
           drop_discard_estimate_sign = TRUE,
           drop_highest_estimate = FALSE,
           get_model_object = FALSE,
           always_check_vif = FALSE) {
    # Dependent Series
    dep_apl_df_list <-
      generate_model_dependent(dep_var_info, model_df, apl_delimiter , var_apl_delimiter)
    dep_apl_df_list[[1]] <-
      dplyr::bind_rows(dep_apl_df_list[1],  .id = "dependent_id")

    dep_apl_df_list[[2]] <-
      lapply(dep_apl_df_list[[2]],  function(x)
        data.frame(Y = rowSums(x)))

    if (!is.na(mdl_start_date)) {
      dep_apl_df_list[[2]] <-
        purrr::map(dep_apl_df_list[[2]], function(x)
          x[rownames(x) >= mdl_start_date, , drop = F])
    }
    if (!is.na(mdl_start_date)) {
      dep_apl_df_list[[2]] <-
        purrr::map(dep_apl_df_list[[2]], function(x)
          x[rownames(x) <= mdl_end_date, , drop = F])
    }

    dependent_sum_list <- purrr::map_vec(dep_apl_df_list[[2]], sum)

    if (any(!is.na(base_variables))) {
      base_data <- model_df %>%
        dplyr::select(tidyselect::all_of(base_variables))
      dep_apl_df_list[[2]] <-
        map(dep_apl_df_list[[2]], function(dep_series) {
          dep_series - rowSums(base_data)
        })
    } else {
      base_data <- data.frame()
    }

    #Aggregate independent variable
    candidate_variables_list_variable <-
      unique(unlist(lapply(
        unlist(candidate_variables_list, recursive = F), names
      )))
    if (any(!is.na(base_variables))) {
      candidate_variables_list_variable <-
        unique(c(candidate_variables_list_variable, base_variables))
    }
    model_df_rel <-
      aggregate_columns(model_df, candidate_variables_list_variable, delimiter = var_agg_delimiter)

    # Model Parameters
    candidate_variables_df <-
      purrr::map_dfr(candidate_variables_list, function(candidate_variables_l) {
        apl_df <-
          lapply(candidate_variables_l, function(df)
            as.data.frame(t(data.frame(
              df, check.names = F
            ))))
        apl_df_wt_names <-
          purrr::map2(names(candidate_variables_l), apl_df, function(type_of_var, df) {
            df$type <- type_of_var
            df$variable <- row.names(df)
            df
          })
        dplyr::bind_rows(apl_df_wt_names)
      }, .id = "model_id")
    candidate_variables_df$model_id <-
      as.numeric(candidate_variables_df$model_id)

    if (with_intercept) {
      intercept_df <-
        data.frame(
          variable = "(Intercept)",
          adstock = NA,
          power = NA,
          lag = NA,
          type = "intercept",
          row.names = "(Intercept)"
        )
      intercept_df <-
        intercept_df[rep(1, length(candidate_variables_list)),]
      intercept_df$model_id <- 1:nrow(intercept_df)
      candidate_variables_df <-
        rbind(candidate_variables_df, intercept_df)
    }

    expected_sign <-
      determine_expected_sign(
        c(names(model_df_rel), "(Intercept)"),
        pos_sign_variables,
        neg_sign_variables,
        var_agg_delimiter
      )
    expected_sign_df <-
      data.frame(variable = c(names(model_df_rel), "(Intercept)"),
                 expected_sign = expected_sign)
    candidate_variables_df <-
      merge(candidate_variables_df,
            expected_sign_df,
            by = "variable",
            all.x = T)

    critical_pval_df <-
      data.frame(type = names(pvalue_thresholds),
                 critical_pvalue = pvalue_thresholds)
    candidate_variables_df <-
      merge(candidate_variables_df,
            critical_pval_df,
            by = "type",
            all.x = T)

    candidate_variables_df <- candidate_variables_df %>%
      dplyr::group_by(.data$model_id) %>%
      dplyr::mutate(count_fixed_var = sum(.data$type == "fixed")) %>%
      dplyr::ungroup()
    candidate_variables_df$critical_vif <- vif_threshold
    candidate_variables_df$critical_vif[candidate_variables_df$count_fixed_var ==
                                          1] <- Inf

    candidate_variables_df_list <-
      candidate_variables_df[, c(
        "model_id",
        "variable",
        "type",
        "adstock",
        "power",
        "lag",
        "expected_sign",
        "critical_pvalue",
        "critical_vif"
      )] %>%
      dplyr::group_split(as.factor(.data$model_id), .keep = FALSE)

    # Apply the 'apl' function on independent variables and replace NA with zero
    model_apl_list <-
      map(candidate_variables_list, function(candidate_predictors) {
        model_df_apl <-
          apply_apl(model_df_rel, unlist(unname(candidate_predictors), recursive = F))
        model_df_apl[is.na(model_df_apl)] <- 0
        model_df_apl
      })

    if (!is.na(mdl_start_date)) {
      model_apl_list <-
        purrr::map(model_apl_list, function(x)
          x[rownames(x) >= mdl_start_date, , drop = F])
    }
    if (!is.na(mdl_start_date)) {
      model_apl_list <-
        purrr::map(model_apl_list, function(x)
          x[rownames(x) <= mdl_end_date, , drop = F])
    }

    candidate_variables_sum_list <-
      purrr::map(model_apl_list, function(model_df_apl) {
        data.frame(sum = c(
          sapply(model_df_apl, sum, na.rm = TRUE),
          setNames(nrow(model_df_apl), "(Intercept)")
        ))
      })

    independent_variable_info_list <-
      purrr::map2(candidate_variables_df_list, candidate_variables_sum_list,
                  function(candidate_predictors_info,
                           variable_sum) {
                    merge(
                      candidate_predictors_info,
                      variable_sum,
                      by.x = "variable",
                      by.y = "row.names",
                      all.x = T
                    )
                  })


    if (is.na(run_up_to_flexi_vars)) {
      run_up_to_flexi_vars <-
        Inf
    }


    model_result <-
      purrr::map2(
        dep_apl_df_list[[2]],
        dependent_sum_list,
        ~ collate_base_models(
          .x ,
          model_apl_list,
          with_intercept,
          base_data,
          independent_variable_info_list,
          .y,
          drop_flexi_vars,
          run_up_to_flexi_vars,
          drop_pvalue_precision,
          drop_discard_estimate_sign,
          drop_highest_estimate,
          get_model_object,
          always_check_vif
        )
      )

    model_coef_all <-
      purrr::map_dfr(model_result, 1, .id = "dependent_id") %>%
      dplyr::mutate(contri = .data[["Estimate"]] * .data[["sum"]],
                    contri_perc = .data[["contri"]] / .data[["dep_sum"]] *
                      100)

    model_smry_all <-
      purrr::map_dfr(model_result, 2, .id = "dependent_id")

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
      dplyr::group_by(.data$dependent_id,
                      .data$model_id,
                      .data$loop_id,
                      .data$type) %>%
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
      dplyr::select(-"type",-"variable_name")

    # Pivot wider and prepare for join
    mdl_smry_var_type <- smry_var_type %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "value")

    # Summarize contributions and percentages, then pivot longer
    mdl_smry_var <- model_coef_all %>%
      dplyr::select("dependent_id",
                    "model_id",
                    "loop_id",
                    "variable",
                    "contri",
                    "contri_perc") %>%
      dplyr::group_by(.data$dependent_id,
                      .data$model_id,
                      .data$loop_id,
                      .data$variable) %>%
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
      dplyr::select(-"variable",-"variable_name")

    # Pivot wider and prepare for join
    mdl_smry_var_wide <- mdl_smry_var %>%
      tidyr::pivot_wider(names_from = "variable_new", values_from = "value")

    # Join all the summaries
    mdl_smry <-
      dplyr::full_join(mdl_smry_flag,
                       mdl_smry_var_type,
                       by = c("dependent_id", "model_id", "loop_id")) %>%
      dplyr::full_join(mdl_smry_var_wide,
                       by = c("dependent_id", "model_id", "loop_id"))

    # Add flag number and join everything
    mdl_smry <- mdl_smry %>%
      dplyr::mutate(flag_num = rowSums(dplyr::across(
        c("flag_pvalue", "flag_sign", "flag_vif")
      ), na.rm = TRUE))

    model_smry_all <-
      dplyr::full_join(model_smry_all,
                       mdl_smry,
                       by = c("dependent_id", "model_id", "loop_id"))

    lm_model_all <- purrr::map(model_result, 3)

    list(dep_apl_df_list[[1]],
         model_smry_all,
         model_coef_all,
         lm_model_all)
  }
