#' Calculate the interpolated bond equivalent yield for indemnity calculation.
#'
#' Link for more details is here: 
#' \url{https://www.cmhc-schl.gc.ca/en/hoficlincl/mobase/upload/nha_mbs_indemnity_calculation_methodology.pdf}
#' 
#' @param bey_short Bond equivalent yield of short-maturity tenor.
#' @param bey_long Bond equivalent yield of long-maturity tenor.
#' @param date_short Maturity date of short-maturity tenor.
#' @param date_long Maturity date of long-maturity tenor.
#' @param date_wal Weighted average life as a date.
#'
#' @return bey_interpolated Interpolated yield for indemnity calculation.
#'
#' @export
#'
#' @examples
#' bey_short <- 1.363
#' bey_long <- 1.501
#' date_short <- ymd( "2016-06-01" ) 
#' date_long <- ymd( "2017-09-01" ) 
#' date_wal <- ymd( "2016-11-23" ) 
#' bey_indemnity( bey_short, bey_long, date_short, date_long, date_wal )
#' 
bey_indemnity <- function( bey_short, bey_long, date_short, date_long, date_wal ){
  
  # Difference in yields
  diff_bey <- unname( bey_long - bey_short )
  
  # Difference in days between dates of wal and short(er) maturity
  diff_num <- lubridate::interval( date_short, date_wal ) / lubridate::edays( 1 )
  stopifnot( date_wal >= date_short ) # Sanity check
  
  # Difference in days between dates of long(er) and short(er) maturity
  diff_denom <- lubridate::interval( date_short, date_long ) / lubridate::edays( 1 )
  
  # Calculate the interpolated GoC yield
  factor <- diff_num/diff_denom
  bey_interpolated <- bey_short + diff_bey*factor
  return( round( bey_interpolated, 3 ) )
  
}