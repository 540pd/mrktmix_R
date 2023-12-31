% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_base.R
\name{identify_drop_variable}
\alias{identify_drop_variable}
\title{Identify the Variable to Drop from a Linear Model}
\usage{
identify_drop_variable(
  coef_df,
  pvalue_precision,
  discard_sign,
  highest_estimate,
  run_up_to_flexi_vars
)
}
\arguments{
\item{coef_df}{A data frame of model coefficients, typically containing columns
like 'Estimate', 'Pr(>|t|)', and 'type', along with flag information.}

\item{pvalue_precision}{An integer specifying the number of decimal places to
round the p-values for comparison purposes.}

\item{discard_sign}{A logical value; if TRUE, the sign of the estimates is
ignored in the decision-making process.}

\item{highest_estimate}{A logical value; if TRUE, the variable with the highest
absolute estimate is considered for dropping.}

\item{run_up_to_flexi_vars}{An integer indicating the maximum number of flexible
variables to consider for retention in the final model.}
}
\value{
A character string indicating the name of the variable suggested to be
dropped from the model. The decision is based on p-value magnitude, and
optionally, the estimate magnitude.
}
\description{
This function identifies which variable should be dropped from a linear model based
on criteria involving the type of variable, p-values, and estimates of the model's
coefficients. It is useful in model simplification and refinement processes.
}
\details{
The function performs the following steps:
\itemize{
\item Filters out variables of type 'flexible' and 'intercept'.
\item Arranges the remaining variables in descending order of their p-values.
\item Rounds the p-values to the specified precision.
\item Adjusts estimates based on the \code{discard_sign} parameter.
\item Identifies the variable to be dropped, considering both the p-value and
possibly the estimate magnitude, in accordance with \code{highest_estimate} and
\code{run_up_to_flexi_vars} criteria.
}
}
\examples{
\dontrun{
  coef_df <- data.frame(
    variable = c("Var1", "Var2", "Var3"),
    Estimate = c(1.5, -0.3, 2.0),
    `Pr(>|t|)` = c(0.04, 0.06, 0.01),
    type = c("fixed", "fixed", "flexible"),
    flag_pvalue = c(TRUE, FALSE, TRUE),
    flag_sign = c(FALSE, TRUE, FALSE),
    flag_vif = c(FALSE, TRUE, FALSE)
  )
  identify_drop_variable(coef_df, 2, TRUE, FALSE, 1)
}

}
