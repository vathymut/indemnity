#' Convert the annual rate to the bond equivalent yield
#'
#' @param retrieval_date Retrieval date.
#' @param maturity_date Maturity date.
#' @param annual_rate Annual rate.
#'
#' @return bey Bond equivalent yield.
#'
#' @examples
#' retrieval_date <- lubridate::ymd( "2015-01-30" )
#' maturity_date <- lubridate::ymd( "2015-01-30" )
#' annual_to_bey( annual_rate = 0.02, retrieval_date, maturity_date )
#'
annual_to_bey <- function( annual_rate, retrieval_date, maturity_date ){
  # Validate input
  stopifnot( lubridate::is.POSIXct( retrieval_date ),
             lubridate::is.POSIXct( maturity_date ) )
  
  # Get number of days between settlement and maturity
  dates_interval <- lubridate::interval( retrieval_date, maturity_date )
  t <- dates_interval / lubridate::edays( 1 )
  
  # Group common terms
  factor_exp <- 182.5/t
  factor_rate <- t/36500
  
  # Get bond equivalent yield in percentage
  bey <- ( ( 1 + annual_rate * factor_rate )^factor_exp - 1 ) * 200
  return( bey )
}