#' Check if tenor must be converted to the bond equivalent yield.
#'
#' @param tenor Tenor (or pillar) of the security.
#' @param tenors_to_convert Character vector of tenors to convert.
#'
#' @return \code{TRUE} if tenor must be converted to bond equivalent yield.
#'
#' @examples
#' must_convert_to_bey( tenor = "MONTH3" )
#' must_convert_to_bey( tenor = "MONTH24" )
#' must_convert_to_bey( tenor = "OVERNIGHT" )
#'
must_convert_to_bey <- function( 
  tenor, 
  tenors_to_convert = c( "OVERNIGHT", "MONTH3", "MONTH6", "MONTH12") ){
  
  # Validate inputs
  stopifnot( is.character( tenor ), is.character( tenors_to_convert ) )
  return( tolower(tenor) %in% tolower(tenors_to_convert) )
}