#' Get the yields from the yield curve on a given retrieval date.
#'
#' @param retrieval_date Retrieval date.
#' @param ycurve_dt \code{Data.table} of historical yield curves.
#'
#' @return values vector of yields (rates).
#'
#' @examples
#' retrieval_date <- lubridate::ymd( "2013-03-01" )
#' dates <- retrieval_date + 0:9*months(1)
#' ycurve_dt <- data.table::data.table( dates=dates, OVERNIGHT=0:9, MONTH3=1:10, MONTH6=2:11 )
#' ycurve_yields( retrieval_date=retrieval_date, ycurve_dt = ycurve_dt )
#'
ycurve_yields <- function( retrieval_date, ycurve_dt ){

  # Get values on settlement date
  sub_dt <- ycurve_for_interpolation( retrieval_date=retrieval_date, ycurve_dt=ycurve_dt )

  # Convert to values
  values <- unname( unlist( sub_dt, use.names = FALSE ) )

  # Return unamed vector
  stopifnot( is.vector(values) )
  return( values )
}