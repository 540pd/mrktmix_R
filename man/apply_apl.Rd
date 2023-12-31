% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apl_transformation.R
\name{apply_apl}
\alias{apply_apl}
\title{Apply Adstock, Power, and Lag Transformation to Multiple Variables}
\usage{
apply_apl(modeling_df, candidate_variables)
}
\arguments{
\item{modeling_df}{A data frame containing the input variables.}

\item{candidate_variables}{A list specifying adstock, power, and lag for each variable.
Each element in \code{candidate_variables} should be a named vector with components:
\describe{
\item{adstock}{Adstock rate for the transformation.}
\item{power}{Power transformation to capture non-linear effectiveness of marketing activity.}
\item{lag}{Lag parameter for the time lag in the transformation.}
}}
}
\value{
A data frame with Adstock, Power, and Lag transformed values for each variable.
}
\description{
This function extends the functionality of \code{compute_apl_values} to apply Adstock,
Power, and Lag transformations to multiple variables in a data frame. It allows
for different transformation parameters to be specified for each variable.
}
\details{
The function iteratively applies the specified Adstock, Power, and Lag transformations
to each variable in the \code{modeling_df} data frame based on the parameters defined in
\code{candidate_variables}. This is useful for market mix modeling where different marketing
channels may require different transformation parameters.
}
\examples{
\dontrun{
  model_df <- cbind(advertising, event)
  candidate_variables <- list(
    TV = setNames(c(.1, .3, 1), c("adstock", "power", "lag")),
    Radio = setNames(c(.2, .4, 0), c("adstock", "power", "lag"))
  )
  apply_apl(model_df, candidate_variables)
}

}
