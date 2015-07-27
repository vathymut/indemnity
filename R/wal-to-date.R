#' Convert weighted average life in years (decimal) to date
#'
#' Link for more details is here: 
#' \url{https://www.cmhc-schl.gc.ca/en/hoficlincl/mobase/upload/nha_mbs_indemnity_calculation_methodology.pdf}
#'
#' @param settlement_date Settlement date.
#' @param wal Weighted average life of NHA MBS pool.
#'
#' @return wal_date Weighted average life as a date.
#'
#' @examples
#' settlement_date <- lubridate::ymd( "2015-01-30" )
#' wal_to_date( settlement_date, 3.25 )
#' wal_to_date( settlement_date, 4.89 )
#'
wal_to_date <- function( settlement_date, wal ){
  # Validate input
  stopifnot( lubridate::is.POSIXct( settlement_date ), is.numeric( wal ) )
  
  # Get number of days elapsed
  days_elapsed <- round( wal*365.25, 0 )
  
  # Get wal as a date
  wal_date <- settlement_date + lubridate::days( days_elapsed )
  return( wal_date )
}