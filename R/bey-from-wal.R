#' Find the interpolated bond equivalent yield (BEY) from the WAL.
#'
#' @param wal Weighted average life of security (in years).
#' @param start_date Retrieval date.
#' @param yields_dt \code{Data.table} of yield of each (CAD curve) tenor.
#' @param maturity_dt \code{Data.table} of maturity of each (CAD curve) tenor.
#'
#' @return bey_interpolated Interpolated BEY.
#'
#' @export
#'
#' @examples
#' start_date <- lubridate::ymd( "2012-12-01" )
#' data( bloomberg_goc, package = "blpxl" )
#' data( bloomberg_cad_maturity, package = "blpxl" ) 
#' wal <- 3.812
#' bey_from_wal( wal, start_date, yields_dt = bloomberg_goc, maturity_dt = bloomberg_cad_maturity )
#' 
bey_from_wal <- function( 
  wal,
  start_date, 
  maturity_dt, 
  yields_dt,
  should_convert_to_bey = TRUE ){
  
  # Get the WAL as a date
  wal_date <- wal_to_date( start_date = start_date, wal = wal )
  
  # Get the yields for short-term and long-term bond (bill)
  results_list <- interpolation_pairs( 
    retrieval_date = start_date, 
    wal_date = wal_date,  
    maturity_dt = maturity_dt, 
    yields_dt = yields_dt )
  yield_short <- results_list[['yields']][[1]]
  yield_long <- results_list[['yields']][[2]]
  
  # Get the dates for short-term and long-term maturity
  date_short <- results_list[['dates']][[1]]
  date_long <- results_list[['dates']][[2]]
  
  # Get the tenors for the short-term and long-term bond (bill)
  tenor_short <- results_list[['tenors']][[1]]
  tenor_long <- results_list[['tenors']][[2]]
  
  # Get the BEY for the short and long bond (bill)
  bey_short <- yield_short
  bey_long <- yield_long
  
  # Update bey if tenor is less than a year
  if( should_convert_to_bey ){
    if( must_convert_to_bey( tenor = tenor_short ) ) 
      bey_short <- annual_to_bey( 
        annual_rate = yield_short, 
        retrieval_date = start_date, 
        maturity_date = maturity_date )
    if( must_convert_to_bey( tenor = tenor_long ) ) 
      bey_long <- annual_to_bey( 
        annual_rate = yield_long, 
        retrieval_date = start_date, 
        maturity_date = maturity_date )
  }
  
  # Calculate the interpolated GoC yield
  bey_interpolated <- bey_indemnity( 
    bey_short = bey_short, 
    bey_long = bey_long, 
    short_date = date_short, 
    long_date = date_long, 
    wal_date = wal_date )
  return( bey_interpolated )
}