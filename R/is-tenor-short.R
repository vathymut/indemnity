#' Check if tenor has a short maturity (matures within the year).
#'
#' @param tenor Tenor (or pillar) of the security.
#' @param tenors_to_convert Character vector of tenors to convert.
#'
#' @return \code{TRUE} if tenor must be converted to bond equivalent yield.
#'
#' @examples
#' is_tenor_short( tenor = "MONTH3" )
#' is_tenor_short( tenor = "MONTH24" )
#' is_tenor_short( tenor = "OVERNIGHT" )
#'
is_tenor_short <- function( 
  tenor, 
  tenors_to_convert = c( "OVERNIGHT", "MONTH3", "MONTH6", "MONTH12") ){
  
  # Validate inputs
  stopifnot( is.character( tenor ), is.character( tenors_to_convert ) )
  return( tolower(tenor) %in% tolower(tenors_to_convert) )
}