#' Create menu of retrieval dates for the yield curve interpolation.
#'
#' @param settlement_date Settlement date.
#' @param cmhc_audit \code{TRUE}. Applies CMHC Audit changes.
#'
#' @return menu_list List of dates on which the yield curve is interpolated.
#'
#' @export
#'
#' @examples
#' issue_date <- lubridate::ymd( "2015-01-30" )
#' settlement_date <- lubridate::ymd( "2015-01-30" )
#' ycurve_retrieval_date( issue_date, settlement_date )
#'
menu_retrieval_dates <- function( 
  settlement_date, 
  cmhc_audit = TRUE ){
  
  # Validate input
  stopifnot( lubridate::is.POSIXct( settlement_date ) )
  
  # Change day to first of the month
  first_of_month <- settlement_date
  lubridate::day( first_of_month ) <- 1L
  lubridate::month( first_of_month ) <- lubridate::month( first_of_month ) + 1L
  
  # Get end of month
  eom <- first_of_month - lubridate::days( 1 )
  
  # Get 7 days before end of month and keep business days
  week_before_eom <- eom - c( 0:7 ) * lubridate::days( 1 )
  is_weekday <- lubridate::wday( week_before_eom ) %in% c( 2:6 )
  business_days_before <- week_before_eom[ is_weekday ]
  
  # Get 7 days after first of the month and keep business days
  week_after_first <- first_of_month + c( 0:7 ) * lubridate::days( 1 )
  is_weekday <- lubridate::wday( week_after_first ) %in% c( 2:6 )
  business_days_after <- week_after_first[ is_weekday ]
  
  # Get the last and the first three business days of the month
  last_three <- head( business_days_before, 3 )
  first_three <- head( business_days_after, 3 )
  
  # third last business day of the month
  retrieval_date_new <- tail( last_three, 1 )
  
  # first business day of the month
  if( cmhc_audit ) retrieval_date_old <- head( first_three, 1 )
  if( !cmhc_audit ) retrieval_date_old <- tail( last_three, 1 )
  
  # Return list
  menu_list <- list( "old" = retrieval_date_old, "new" = retrieval_date_new )
  return( menu_list )
}