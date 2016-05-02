#' Get interpolation pairs for the yields, maturity dates and tenors.
#'
#' @param retrieval_date Retrieval date of the yield curve.
#' @param maturity_dt \code{Datatable} of maturity of each (CAD curve) tenor.
#' @param yields_dt \code{Datatable} of yield of each (CAD curve) tenor.
#' @param wal_date Weighted average life as a date.
#'
#' @return yields Yields of short and long tenor to interpolate between.
#'
#' @examples
#' retrieval_date <- lubridate::ymd( "2013-03-01" )
#' wal_date <- lubridate::ymd( "2014-06-21" )
#' data( bloomberg_goc, package = "blpxl" )
#' data( bloomberg_cad_maturity, package = "blpxl" ) 
#' list_args_pt1 <- list( wal_date = wal_date, retrieval_date = retrieval_date )
#' list_args_pt2 <- list( maturity_dt = bloomberg_cad_maturity, yields_dt = bloomberg_goc )
#' do.call( interpolation_pairs, args = c( list_args_pt1, list_args_pt2 ) )
#'
interpolation_pairs <- function( retrieval_date, wal_date, maturity_dt, yields_dt ){
  
  # Get maturities on date
  maturities <- ycurve_maturities( retrieval_date=retrieval_date, ycurve_dt=maturity_dt )

  # Get yields on date
  yields <- ycurve_yields( retrieval_date=retrieval_date, ycurve_dt=yields_dt )

  # Get tenors on date
  tenors <- ycurve_tenors( retrieval_date=retrieval_date, ycurve_dt=maturity_dt )
  
  # Get index pair
  indices_list <- pair_indices( wal_date, maturities )
  index_short <- indices_list$short
  index_long <- indices_list$long

  # Get yield pair
  yield_list <- pair_selected( index_short, index_long, yields )

  # Get maturity pair
  maturity_list <- pair_selected( index_short, index_long, maturities )
  
  # Get tenors
  tenor_list <- pair_selected( index_short, index_long, tenors )

  # Return list
  yields_list <- list( "short" = yield_list$short, "long" = yield_list$long )
  dates_list <- list( "short" = maturity_list$short, "long" = maturity_list$long )
  tenor_list <- list( "short" = tenor_list$short, "long" = tenor_list$long )
  return( list( yields = yields_list, maturities = dates_list, tenors = tenor_list ) )
}