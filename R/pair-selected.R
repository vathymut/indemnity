#' Get the yields/maturities for the short and long (pair) Government of Canada Bill or Bond.
#'
#' @param index_short Index of the short GoC Bond.
#' @param index_long Index of the long GoC Bond.
#' @param yields Vector of yields (maturities or tenors).
#'
#' @return yield_list List of values corresponding to the short and long GoC Bond.
#'
#' @examples
#' index_short <- 3L
#' index_long <- 4L
#' yields <- seq( from=0.5, to=3.0, by=0.5 )
#' pair_selected( index_short=index_short, index_long=index_long, yields=yields )
#'
pair_selected <- function( index_short, index_long, yields ){
  
  # Validate inputs
  stopifnot( length( yields ) >= index_long, index_short < index_long )
  
  # Get tenors
  yield_short <- yields[ index_short ]
  yield_long <- yields[ index_long ]

  # Validate output
  nas <- is.na( c(yield_short, yield_long) )
  stopifnot( any( nas ) == FALSE )

  # Return list
  yield_list <- list( "short" = yield_short, "long" = yield_long )
  return( yield_list )
}