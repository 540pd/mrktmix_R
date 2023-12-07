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
#' @param expected_coef_sign A named logical vector indicating the expected sign of
#'   each variable's coefficient in the model. The names of the vector correspond to
#'   the variable names. For each variable, if the expected coefficient sign is positive,
#'   the value should be \code{TRUE}. If the expected sign is negative, the value should
#'   be \code{FALSE}. If the sign is uncertain or not specified, the value can be \code{NA}.
#' @param drop_pvalue_precision The precision level for p-value used in deciding whether to drop variables from the model.
#' @param discard_estimate_sign Logical; if TRUE, the sign of the estimate is ignored when considering variable inclusion in the model.
#' @param drop_highest_estimate Logical; if TRUE, the variable with the highest estimate is dropped from the model.
#' @param get_model_object Logical; if TRUE, the function returns the linear model object; otherwise, it returns a list of model summaries.
#' @param defer_intercept_test Logical; if TRUE, the intercept test is deferred.
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
#' @importFrom purrr map2 map_dfr list_rbind
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
           expected_coef_sign,
           drop_pvalue_precision,
           discard_estimate_sign ,
           drop_highest_estimate ,
           get_model_object,
           defer_intercept_test,
           always_check_vif) {
    base_models <-
      purrr::map2(model_apl_list, independent_variable_info_list, function(model_df_apl,
                                                                           independent_variable_info) {
        modeling_df <-
          cbind(model_dep_df, model_df_apl)

        # flexi variables
        flexi_vars <-
          independent_variable_info$variable[independent_variable_info$type == "flexible"]

        # Fit linear model
        lm_model_formula <-
          as.formula(if (with_intercept)
            "Y ~ ."
            else
              "Y ~ . - 1")
        lm_model <- lm(lm_model_formula, data = modeling_df)

        if (drop_flexi_vars) {
          lm_model <- cleanse_model_singularity(lm_model,
                                                modeling_df,
                                                flexi_vars,
                                                round_digits = 2)
          lm_model <- cleanse_model_perfect_fit(
            lm_model,
            modeling_df,
            flexi_vars,
            drop_highest_estimate = FALSE,
            ignore_estimate_sign = TRUE
          )

          get_base_model(
            lm_model,
            modeling_df,
            expected_coef_sign,
            setNames(
              independent_variable_info$critical_pvalue,
              independent_variable_info$variable
            ),
            setNames(
              independent_variable_info$critical_vif,
              independent_variable_info$variable
            ),
            flexi_vars,
            run_up_to_flexi_vars,
            drop_pvalue_precision,
            discard_estimate_sign,
            drop_highest_estimate,
            defer_intercept_test,
            always_check_vif
          )

        } else{
          lm_accumulator <- list()
          lm_accumulator <- c(lm_accumulator, list(lm_model))

          vif_accumulator <- list()
          mdl_vif <- calculate_vif(lm_model)
          vif_accumulator <- c(vif_accumulator, list(mdl_vif))

          list(lm_accumulator, vif_accumulator)
        }


      })

    model_vif <-
      purrr::list_rbind(purrr::map(base_models, function(x) {
        purrr::list_rbind(purrr::map(x[[2]], function(y) {
          data.frame(
            variable = names(y),
            vif = y,
            row.names = NULL,
            check.rows = FALSE,
            check.names = F
          )
        }), names_to = "loop_id")
      })
      , names_to = "model_id")
    model_vif$variable <- gsub("`", "", model_vif$variable)

    model_vif_with_sign <-
      merge(
        model_vif,
        data.frame(expected_sign = expected_coef_sign),
        by.x = "variable",
        by.y = "row.names",
        all.x = T
      )

    model_coef <-
      purrr::list_rbind(purrr::map(base_models, function(x) {
        purrr::list_rbind(purrr::map(x[[1]], function(y) {
          model_coef <- as.data.frame(summary(y)$coefficients)
          model_coef$variable <-
            gsub("`", "", rownames(model_coef))
          rownames(model_coef) <- NULL
          model_coef
        }), names_to = "loop_id")
      })
      , names_to = "model_id")

    model_coef_all <-
      model_coef %>%
      dplyr::full_join(model_vif_with_sign, by = c("model_id", "loop_id", "variable")) %>%
      dplyr::left_join(
        purrr::list_rbind(independent_variable_info_list, names_to = "model_id"),
        by = c("model_id", "variable")
      ) %>%
      dplyr::mutate(
        dep_sum = dependent_sum,
        flag_pvalue = .data[["Pr(>|t|)"]] > .data[["critical_pvalue"]],
        flag_sign = .data[["expected_sign"]] != (.data[["Estimate"]] > 0),
        flag_vif = .data[["critical_vif"]] <= .data[["vif"]]
      ) %>%
      dplyr::select(tidyselect::all_of(
        c(
          "model_id",
          "loop_id",
          "variable",
          "type",
          "adstock",
          "power",
          "lag",
          "sum",
          "Estimate",
          "Std. Error",
          "t value",
          "Pr(>|t|)",
          "vif",
          "critical_pvalue",
          "expected_sign",
          "critical_vif",
          "flag_pvalue",
          "flag_sign",
          "flag_vif",
          "dep_sum"
        )
      ))

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

    model_smry_all <-
      purrr::list_rbind(purrr::map(base_models, function(x) {
        purrr::list_rbind(purrr::map(x[[1]], function(lm_object) {
          model_summary <- summary(lm_object)
          model_residuals <- residuals(model_summary)
          data.frame(
            sigma = model_summary$sigma,
            r_squared = model_summary$r.squared,
            adj_r_squared = model_summary$adj.r.squared,
            residuals = sum(model_residuals),
            rmse = sqrt(mean(model_residuals ^ 2)),
            mae = mean(abs(model_residuals)),
            mape = mean(abs(model_residuals) / model_dep_df$Y * 100),
            dependent_sum = dependent_sum
          )

        }), names_to = "loop_id")
      })
      , names_to = "model_id")

    if (get_model_object) {
      lm_model_all <-
        purrr::list_rbind(purrr::map(base_models, function(x) {
          purrr::list_rbind(purrr::map(x[[1]], function(y) {
            tibble(lm_model = list(y))
          }), names_to = "loop_id")
        })
        , names_to = "model_id")
    } else {
      lm_model_all <- data.frame()
    }


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
#' @param dep_var_info Information about the dependent variable, which can be
#'   provided in one of the following formats:
#'   \enumerate{
#'     \item \strong{String}: A string representing the name of the dependent variable.
#'           This variable name must be present in the modeling dataset (model_df).
#'     \item \strong{Named Vector}: A named vector where names represent the variable
#'           along with its adstock, power, and lag information, denoted by consistent
#'           delimiters (e.g., "var_adstock_0.5_power_2_lag_3"). The values in the
#'           vector represent the coefficient of the variable.
#'     \item \strong{List}: A list that can take two forms:
#'       \enumerate{
#'         \item A list of named vectors, where each named vector follows the
#'               format specified in the Named Vector option.
#'         \item A list containing lists of adstock, power, and lag information.
#'               Each of these (adstock, power, lag) is itself a list containing
#'               named vectors with 'start', 'stop', and 'step', representing the
#'               parameters for adstock, power, and lag transformations.
#'       }
#'   }
#'   This parameter is crucial for specifying how the dependent variable is treated
#'   in the model, including any transformations or lags applied.
#' @param with_intercept Logical; if TRUE, an intercept is included in the model.
#'   Default is TRUE.
#' @param base_variables A character vector of variable names representing base
#'   variables in the model. The coefficients of these variables are set to 1.
#'   Use \code{NA} if no base variable is required in the model.
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
#' @param defer_intercept_test Logical; if TRUE, the intercept test is deferred.
#' @param always_check_vif Logical; if TRUE, the Variance Inflation Factor (VIF)
#'   is always checked. If FALSE, VIF will only be checked if there are no flags
#'   for p-value and no signs for the estimate. Default is FALSE.
#'
#' @return A list containing the collated models after filtering and preprocessing,
#'   along with associated statistics. This list includes details about the dependent
#'   variable transformations, summary statistics, model coefficients, and linear model objects.
#'
#' @importFrom tidyr separate
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
           defer_intercept_test = FALSE,
           always_check_vif = FALSE) {
    # Dependent Series
    dep_apl_df_list <-
      generate_model_dependent(dep_var_info, model_df, apl_delimiter , var_apl_delimiter)
    # Dependent Variable information
    dep_apl_df_list[[1]] <-
      dplyr::bind_rows(dep_apl_df_list[1],  .id = "dependent_id")
    # Dependent Variable for modeling
    dep_apl_df_list[[2]] <-
      lapply(dep_apl_df_list[[2]],  function(x)
        data.frame(Y = rowSums(x)))
    #trucate dependent series with given start date and end date
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
    #vector with sum of dependent series
    dependent_sum_list <- purrr::map_vec(dep_apl_df_list[[2]], sum)

    # Base variable
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
      unique(unlist(lapply(unname(
        unlist(candidate_variables_list, recursive = F)
      )
      , names)))
    if (any(!is.na(base_variables))) {
      candidate_variables_list_variable <-
        unique(c(candidate_variables_list_variable, base_variables))
    }
    model_df_rel <-
      aggregate_columns(model_df, candidate_variables_list_variable, delimiter = var_agg_delimiter)


    # Apply the 'apl' function on independent variables and replace NA with zero
    model_apl_list <-
      purrr::map(candidate_variables_list, function(candidate_predictors) {
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

    candidate_variables_sum <-
      dplyr::bind_rows(purrr::map(model_apl_list, function(model_df_apl) {
        sum_including_intercept <- c(sapply(model_df_apl, sum, na.rm = TRUE),
                                     setNames(nrow(model_df_apl), "(Intercept)"))
        data.frame(
          variable = names(sum_including_intercept),
          sum = sum_including_intercept,
          row.names = NULL,
          check.names = F
        )
      }), .id = "model_id")
    candidate_variables_sum$model_id <-
      as.numeric(candidate_variables_sum$model_id)

    #expected sign
    expected_sign <-
      setNames(
        determine_expected_sign(
          c(names(model_df_rel), "(Intercept)"),
          pos_sign_variables,
          neg_sign_variables,
          var_agg_delimiter
        ),
        c(names(model_df_rel), "(Intercept)")
      )


    # Model Parameters
    candidate_variables_df <-
      dplyr::bind_rows(purrr::map(candidate_variables_list, function(x) {
        df <- t(data.frame(x))
        row_names <- unlist(lapply(1:length(x), function(list_id) {
          paste(names(x[list_id]), names(x[[list_id]]), sep = ".")
        }))
        data.frame(cbind(type.variable = row_names), df, row.names = NULL)
      }), .id = "model_id") %>%
      tidyr::separate(
        col = .data[["type.variable"]],
        into = c("type", "variable"),
        sep = "\\.",
        extra = "merge"
      )
    candidate_variables_df$model_id <-
      as.numeric(candidate_variables_df$model_id)

    # add intercept and its parameters
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


    #pvalue
    critical_pval_df <-
      data.frame(type = names(pvalue_thresholds),
                 critical_pvalue = pvalue_thresholds)
    candidate_variables_df <-
      merge(candidate_variables_df,
            critical_pval_df,
            by = "type",
            all.x = T)

    #effective vif threshold
    candidate_variables_df <- candidate_variables_df %>%
      dplyr::group_by(.data$model_id) %>%
      dplyr::mutate(count_fixed_var = sum(.data$type == "fixed")) %>%
      dplyr::ungroup()
    candidate_variables_df$critical_vif <- vif_threshold

    candidate_variables_df <- candidate_variables_df %>%
      left_join(candidate_variables_sum, by = c("model_id", "variable"))

    candidate_variables_df_list <-
      candidate_variables_df[, c(
        "model_id",
        "variable",
        "type",
        "adstock",
        "power",
        "lag",
        "sum",
        "critical_pvalue",
        "critical_vif"
      )] %>%
      dplyr::group_split(as.factor(.data$model_id), .keep = FALSE)


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
          candidate_variables_df_list,
          .y,
          drop_flexi_vars,
          run_up_to_flexi_vars,
          expected_sign,
          drop_pvalue_precision,
          drop_discard_estimate_sign,
          drop_highest_estimate,
          get_model_object,
          defer_intercept_test,
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
      filter(.data$type == "fixed") %>%
      dplyr::select(
        "dependent_id",
        "model_id",
        "loop_id",
        "variable",
        "adstock",
        "power",
        "lag",
        "contri",
        "contri_perc"
      ) %>%
      dplyr::group_by(.data$dependent_id,
                      .data$model_id,
                      .data$loop_id,
                      .data$variable) %>%
      dplyr::summarise(
        contri = sum(.data$contri),
        contri_perc = sum(.data$contri_perc),
        adstock = mean(.data$adstock, na.rm = TRUE),
        power = mean(.data$power, na.rm = TRUE),
        lag = mean(.data$lag, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = c("contri", "contri_perc", "adstock", "power", "lag"),
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
