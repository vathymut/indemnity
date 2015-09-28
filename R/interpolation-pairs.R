#' Get interpolation pairs for the yields, maturity dates and tenors.
#'
#' @param retrieval_date Retrieval (anchor) date.
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
  
  # Get maturities on reporting date
  maturities_subset <- ycurve_for_interpolation( retrieval_date, ycurve_dt = maturity_dt )
  maturities_with_nas <- sapply( maturities_subset, format, format = "%Y-%m-%d" )
  maturities_selected_index <- unname( !is.na( maturities_with_nas ) )
  maturities <- maturities_with_nas[ maturities_selected_index ]
  maturities <- lubridate::ymd( maturities )

  # Get yields on reporting date
  yields_subset <- ycurve_for_interpolation( retrieval_date, ycurve_dt = yields_dt )
  yields_with_nas <- unlist( yields_subset, use.names = FALSE )
  yields_selected_index <- unname( !is.na( yields_with_nas ) )
  yields <- yields_with_nas[ yields_selected_index ]
  
  #### Adjust indices to ensure non-NA matches for yields ####
  is_OK <- identical( maturities_selected_index, yields_selected_index )
  if( !is_OK ){
    msg_template <- "On %s, there are missing yields and/or maturity dates."
    msg_warning <- sprintf( msg_template, format( retrieval_date, "%Y-%m-%d" ) )
    warning( msg_warning )
    maturities <- maturities_with_nas[ yields_selected_index ]
    maturities <- lubridate::ymd( maturities )
  }

  # Get indices of yields to interpolate
  index_short <- findInterval( wal_date, maturities )
  index_long <- index_short + 1L

  # Get the yields for GoC short and long bond (bill)
  yield_short <- yields[index_short]
  yield_long <- yields[index_long]

  # Get the dates for GoC short and long maturity
  date_short <- maturities[index_short]
  date_long <- maturities[index_long]

  # Get column names
  maturities_colnames <- names( maturities_subset )
  yields_colnames <- names( yields_subset )
  
  # Sanity check
  stopifnot( length( setdiff(maturities_colnames, yields_colnames) ) == 0L )
  
  # Get tenors
  tenor_short <- maturities_colnames[yields_selected_index][ index_short ]
  tenor_long <- maturities_colnames[yields_selected_index][ index_long ]

  # Return list
  yields_list <- list( "short" = yield_short, "long" = yield_long )
  dates_list <- list( "short" = date_short, "long" = date_long )
  tenor_list <- list( "short" = tenor_short, "long" = tenor_long )
  return( list( yields = yields_list, dates = dates_list, tenors = tenor_list ) )
}