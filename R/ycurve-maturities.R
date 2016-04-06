#' Get the maturity dates from the yield curve values on a given retrieval date.
#'
#' @param retrieval_date Retrieval date.
#' @param ycurve_dt \code{Data.table} of historical yield curves.
#'
#' @return values vector of maturity dates.
#'
#' @examples
#' retrieval_date <- lubridate::ymd( "2013-03-01" )
#' dates <- retrieval_date + 0:9*months(1)
#' OVERNIGHT <- dates + lubridate::ddays(1)
#' MONTH3 <- dates + months(3)
#' MONTH6 <- dates + months(6)
#' maturity_dt <- data.table::data.table( dates=dates, OVERNIGHT=OVERNIGHT, MONTH3=MONTH3, MONTH6=MONTH6 )
#' ycurve_maturities( retrieval_date=retrieval_date, ycurve_dt=maturity_dt )
#'
ycurve_maturities <- function( retrieval_date, ycurve_dt ){

  # Get values on settlement date
  sub_dt <- ycurve_for_interpolation( retrieval_date=retrieval_date, ycurve_dt=ycurve_dt )

  # Convert to characters with given format
  maturity_dt <- sapply( sub_dt, format, format = "%Y-%m-%d" )

  # Convert to values
  values <- unname( unlist( maturity_dt, use.names = FALSE ) )

  # Return unamed vector
  stopifnot( is.vector(values) )
  return( lubridate::ymd( values ) )
}