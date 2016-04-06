#' Get the tenors from the yield curve on a given retrieval date.
#'
#' @param retrieval_date Retrieval date.
#' @param ycurve_dt \code{Data.table} of historical yield curves.
#'
#' @return values vector of tenors (time left to maturity).
#'
#' @examples
#' retrieval_date <- lubridate::ymd( "2013-03-01" )
#' dates <- retrieval_date + 0:9*months(1)
#' OVERNIGHT <- dates + lubridate::ddays(1)
#' MONTH3 <- dates + months(3)
#' MONTH6 <- dates + months(6)
#' maturity_dt <- data.table::data.table( dates=dates, OVERNIGHT=OVERNIGHT, MONTH3=MONTH3, MONTH6=MONTH6 )
#' ycurve_tenors( retrieval_date=retrieval_date, ycurve_dt=maturity_dt )
#'
ycurve_tenors <- function( retrieval_date, ycurve_dt ){

  # Get values on settlement date
  sub_dt <- ycurve_for_interpolation( retrieval_date=retrieval_date, ycurve_dt=ycurve_dt )

  # Extract names from columns
  values <- names( sub_dt )

  # Return unamed vector
  stopifnot( is.vector(values) )
  return( values )
}