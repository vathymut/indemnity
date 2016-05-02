#' Find the interpolated bond equivalent yield (BEY) from the WAL.
#'
#' @param wal Weighted average life of security (in years).
#' @param settlement_date Settlement date.
#' @param yields_dt \code{Data.table} of yield of each (CAD curve) tenor.
#' @param maturity_dt \code{Data.table} of maturity of each (CAD curve) tenor.
#'
#' @return bey_interpolated Interpolated BEY.
#'
#' @export
#'
#' @examples
#' settlement_date <- lubridate::ymd( "2012-12-01" )
#' data( bloomberg_goc, package = "blpxl" )
#' data( bloomberg_cad_maturity, package = "blpxl" ) 
#' wal <- 3.812
#' bey_from_wal( wal, settlement_date, yields_dt = bloomberg_goc, maturity_dt = bloomberg_cad_maturity )
#' 
bey_from_wal <- function( 
  wal,
  settlement_date, 
  maturity_dt, 
  yields_dt ){
  
  # Get the WAL as a date
  wal_date <- wal_to_date( retrieval_date=settlement_date, wal=wal )
  
  # Get the yields for short-term and long-term bond (bill)
  pairs <- interpolation_pairs( settlement_date, wal_date,  maturity_dt, yields_dt )
  
  # Get the BEY for the short and long bond (bill)
  bey_short <- pairs$yields$short
  bey_long <- pairs$yields$long
  
  # Update bey if tenor is less than a year
  update_short <- is_tenor_short( pairs$tenors$short )
  update_long <- is_tenor_short( pairs$tenors$long )
  if( update_short ) bey_short <- annual_to_bey( bey_short, settlement_date, maturity_date )
  if( update_long ) bey_long <- annual_to_bey( bey_long, settlement_date, maturity_date )

  # Calculate the interpolated GoC yield
  bey_interpolated <- bey_indemnity( 
    bey_short = bey_short, 
    bey_long = bey_long, 
    maturity_short = pairs$maturities$short, 
    maturity_long = pairs$maturities$long, 
    wal_date = wal_date )
  return( bey_interpolated )
}