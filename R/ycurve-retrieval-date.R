#' Get the retrieval date for the yield curve for indemnity interpolation.
#'
#' @param issue_date Issue date.
#' @param menu_list List of dates on which the yield curve is interpolated.
#' @param cmhc_audit \code{TRUE}. Applies CMHC Audit: first business day of the month.
#'
#' @return retrieval_date Date on which the yield curve is interpolated.
#'
#' @export
#'
#' @examples
#' issue_date <- lubridate::ymd( "2015-01-30" )
#' menu_list <- menu_retrieval_dates( ymd( "2015-01-31" ), cmhc_audit = TRUE )
#' ycurve_retrieval_date( issue_date, menu_list )
#'
ycurve_retrieval_date <- function( 
  issue_date, 
  menu_list ){
  
  # Validate input
  stopifnot( lubridate::is.POSIXt( issue_date ),
             all( c("old", "new") %in% names( menu_list ) ),
             all( sapply( menu_list, inherits, what = "POSIXt" ) ) )
  
  # Choose either third last or last business day based on issue date
  REGULATORY_DATE_CHANGE <- lubridate::ymd( "2014-11-01" )
  regime_changed <- issue_date >= REGULATORY_DATE_CHANGE
  if ( regime_changed ){
    # third last business day of the month
    retrieval_date <- menu_list[["new"]]
  }else{
    # Changed after CMHC audit on on 06/11/2015: See Outlook task for more info
    # first business day of the month if cmhc_audit is TRUE
    retrieval_date <- menu_list[["old"]]
  }
  return( retrieval_date )
}