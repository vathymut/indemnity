#' Convert weighted average life in years (decimal) to date
#'
#' Link for more details is here: 
#' \url{https://www.cmhc-schl.gc.ca/en/hoficlincl/mobase/upload/nha_mbs_indemnity_calculation_methodology.pdf}
#'
#' @param start_date Starting date.
#' @param wal Weighted average life of NHAMBS pool.
#'
#' @return wal_date Weighted average life as a date.
#'
#' @examples
#' start_date <- lubridate::ymd( "2015-01-30" )
#' wal_to_date( start_date, 3.25 )
#' wal_to_date( start_date, 4.89 )
#'
wal_to_date <- function( start_date, wal ){
  # Validate input
  stopifnot( lubridate::is.POSIXct( start_date ), is.numeric( wal ) )
  
  # Get number of days elapsed
  days_elapsed <- round( wal*365.25, 0 )
  
  # Get wal as a date
  wal_date <- start_date + lubridate::days( days_elapsed )
  return( wal_date )
}