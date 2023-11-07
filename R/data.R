#' Advertising Data (Internal Use)
#'
#' A dataset containing simulated advertising expenses and sales figures
#' over a period of 12 weeks starting from January 1, 2021. This data is
#' purely fictional and is used for illustrative purposes and development
#' within the package.
#'
#' @format A data frame with 12 rows and 4 variables:
#' \describe{
#'   \item{Sales}{Numeric vector representing simulated sales data.}
#'   \item{TV}{Numeric vector representing simulated TV advertising expenses.}
#'   \item{Radio}{Numeric vector representing simulated radio advertising expenses.}
#'   \item{Newspaper}{Numeric vector representing simulated newspaper advertising expenses.}
#' }
#' @usage data(advertising)
#' @export
#' @examples
#' # This data is for internal development use only and not exported.
#' data(advertising)
"advertising"

#' Events Data (Internal Use)
#'
#' A dataset containing one-hot encoded representations of dates corresponding
#' to the advertising dataset. Each column represents a unique week, and this
#' data is used internally for modeling event impacts on sales during package development.
#'
#' @format A data frame with 12 rows and a variable number of columns (depending on the number of weeks).
#' Each column has a name corresponding to a date in the format `Date_YYYY_MM_DD` with a binary
#' indication of whether the event occurred on that date.
#' @usage data(events)
#' @export
#' @examples
#' # This data is for internal development use only and not exported.
#' data(events)
"events"
