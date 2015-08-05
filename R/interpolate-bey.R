#' Find the interpolated bond equivalent yield for indemnity calculation.
#'
#' Link for more details is here: 
#' \url{https://www.cmhc-schl.gc.ca/en/hoficlincl/mobase/upload/nha_mbs_indemnity_calculation_methodology.pdf}
#'
#' @param retrieval_date Date on which the yield curve is interpolated.
#' @param settlement_date Settlement date.
#' @param maturity_date Maturity date.
#' @param yields_dt \code{Data.table} of yield of each (CAD curve) tenor.
#' @param maturity_dt \code{Data.table} of maturity of each (CAD curve) tenor.
#' @param wal Weighted average life of security (NHA MBS pool).
#' @param cmch_audit Default is \code{TRUE} to include changes from CMHC audit.
#'
#' @return bey_interpolated Interpolated BEY for indemnity calculation.
#'
#' @export
#'
#' @examples
#' retrieval_date <- lubridate::ymd( "2013-01-29" )
#' settlement_date <- lubridate::ymd( "2013-01-31" )
#' maturity_date <- lubridate::ymd( "2017-09-01" )
#' data( bloomberg_goc, package = "blpxl" )
#' data( bloomberg_cad_maturity, package = "blpxl" ) 
#' wal <- 3.812
#' interpolate_bey( retrieval_date, settlement_date, maturity_date, yields_dt = bloomberg_goc, maturity_dt = bloomberg_cad_maturity, wal )
#' 
interpolate_bey <- function( 
  retrieval_date, 
  settlement_date, 
  maturity_date, 
  maturity_dt, 
  yields_dt, 
  wal ){

  # Get the WAL as a date
  wal_date <- wal_to_date( start_date = settlement_date, wal = wal )
  
  # Get the yields for GoC short and long bond (bill)
  results_list <- interpolation_pairs( 
    retrieval_date, wal_date,  maturity_dt, yields_dt )
  yield_short <- results_list[['yields']][[1]]
  yield_long <- results_list[['yields']][[2]]

  # Get the dates for GoC short and long maturity
  date_short <- results_list[['dates']][[1]]
  date_long <- results_list[['dates']][[2]]

  # Get the bond equivalent yield for GoC short and long bond (bill)
  tenor_short <- results_list[['tenors']][[1]]
  tenor_long <- results_list[['tenors']][[2]]

  # Get the BEY for GoC short and long bond (bill)
  bey_short <- yield_short
  bey_long <- yield_long
  
  # Update bey if tenor is less than a year: STILL TO WRITE
  if( must_convert_to_bey( tenor_short ) ) bey_short <- annual_to_bey( 
    yield_short, settlement_date, maturity_date )
  if( must_convert_to_bey( tenor_long ) ) bey_long <- annual_to_bey( 
    yield_long, settlement_date, maturity_date )

  # Calculate the interpolated GoC yield
  bey_interpolated <- bey_indemnity( 
    bey_short = bey_short, 
    bey_long = bey_long, 
    date_short = date_short, 
    date_long = date_long, 
    date_wal = wal_date )
  return( bey_interpolated )
}