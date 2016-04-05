#' Calculate the interpolated bond equivalent yield.
#'
#' Link for more details is here: 
#' \url{https://www.cmhc-schl.gc.ca/en/hoficlincl/mobase/upload/nha_mbs_indemnity_calculation_methodology.pdf}
#' 
#' @param bey_short Bond equivalent yield of short-term tenor.
#' @param bey_long Bond equivalent yield of long-term tenor.
#' @param short_date Maturity date of short-term tenor.
#' @param long_date Maturity date of long-term tenor.
#' @param wal_date Weighted average life as a date.
#'
#' @return bey_interpolated Interpolated yield for indemnity calculation.
#'
#' @export
#'
#' @examples
#' bey_short <- 1.363
#' bey_long <- 1.501
#' short_date <- ymd( "2016-06-01" ) 
#' long_date <- ymd( "2017-09-01" ) 
#' wal_date <- ymd( "2016-11-23" ) 
#' bey_indemnity( bey_short, bey_long, short_date, long_date, wal_date )
#' 
bey_indemnity <- function( bey_short, bey_long, short_date, long_date, wal_date ){
  
  # Difference in yields
  diff_bey <- unname( bey_long - bey_short )
  
  # Difference in days between dates of wal and short(er) maturity
  diff_num <- lubridate::interval( short_date, wal_date ) / lubridate::ddays( 1 )
  stopifnot( wal_date >= short_date ) # Sanity check
  
  # Difference in days between dates of long(er) and short(er) maturity
  diff_denom <- lubridate::interval( short_date, long_date ) / lubridate::ddays( 1 )
  
  # Calculate the interpolated GoC yield
  factor <- diff_num/diff_denom
  bey_interpolated <- bey_short + diff_bey*factor
  return( round( bey_interpolated, 3 ) )
  
}