#' Get the indices from the vector of maturity dates corresponding to
#' to the short and long (pair) Government of Canada Bill or Bond.
#' The WAL (date) falls between these two - short and long - maturity dates.
#'
#' @param wal_date Weighted average life as a date.
#' @param maturities (Sorted) maturity dates of the GoC yield curve.
#'
#' @return indices_list List of short and long indices.
#'
#' @examples
#' wal_date <- lubridate::ymd( "2014-06-21" )
#' maturities <- wal_date + c(-4:4)*years(1)
#' maturities_test_1 <- maturities
#' maturities_test_2 <- maturities
#' maturities_test_1[5] <- NA
#' maturities_test_2[5:6] <- NA
#' pair_indices( wal_date, maturities )
#' pair_indices( wal_date, maturities_test_1 )
#' pair_indices( wal_date, maturities_test_2 )
#'
pair_indices <- function( wal_date, maturities ){

  # Validate inputs
  stopifnot( lubridate::is.POSIXct( wal_date ), lubridate::is.POSIXct( maturities )  )
  
  # Get indices of yields to interpolate
  index_short <- tail( which( maturities <= wal_date ), 1 )
  index_long <- head( which( maturities >= wal_date ), 1 )

  # Adjust if short and long bonds are the same
  index_short <- as.integer( index_short )
  index_long <- as.integer( index_long )
  if( identical( index_short, index_long ) ) index_long <- index_long + 1L

  # Validate output
  nas <- is.na( c(index_short, index_long) )
  stopifnot( any( nas ) == FALSE, index_long <= length(maturities) )

  # Return list
  indices_list <- list( "short" = index_short, "long" = index_long )
  return( indices_list )
}