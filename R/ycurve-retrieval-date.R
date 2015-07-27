#' Get the retrieval date for the yield curve for indemnity interpolation.
#'
#' @param issue_date Issue date.
#' @param settlement_date Settlement date.
#' @param cmhc_audit \code{TRUE}. Applies CMHC Audit: first business day of the month.
#'
#' @return retrieval_date Date on which the yield curve is interpolated.
#'
#' @examples
#' issue_date <- lubridate::ymd( "2015-01-30" )
#' settlement_date <- lubridate::ymd( "2015-01-30" )
#' ycurve_retrieval_date( issue_date, settlement_date )
#'
ycurve_retrieval_date <- function( issue_date, settlement_date, cmhc_audit = TRUE ){
  # Validate input
  stopifnot( lubridate::is.POSIXct( issue_date ),
             lubridate::is.POSIXct( settlement_date ) )
  
  # Change day to first of the month
  day_reporting <- lubridate::day( settlement_date )
  working_date <- settlement_date
  lubridate::day( working_date ) <- 1
  
  # Get end of month
  first_of_month <- working_date
  if( day_reporting > 15 ) first_of_month <- working_date + months( 1 )
  eom <- first_of_month - lubridate::days( 1 )
  
  # Get 7 days before end of month and keep business days
  week_before_eom <- eom - c( 0:7 ) * lubridate::days( 1 )
  # Note: c( 2:6 ) corresponds to Monday through Friday
  is_weekday <- lubridate::wday( week_before_eom ) %in% c( 2:6 )
  business_days_before <- week_before_eom[ is_weekday ]

  # Get 7 days after first of the month and keep business days
  week_after_first <- first_of_month + c( 0:7 ) * lubridate::days( 1 )
  is_weekday <- lubridate::wday( week_after_first ) %in% c( 2:6 )
  business_days_after <- week_after_first[ is_weekday ]
  
  # Get the third last business day of the month
  last_three <- head( business_days_before, 3 )
  next_three <- head( business_days_after, 3 )
  
  # Choose either third last or last business day based on issue date
  REGULATORY_DATE_CHANGE <- lubridate::ymd( "2014-11-01" )
  regime_changed <- issue_date >= REGULATORY_DATE_CHANGE
  if ( regime_changed ){
    # third last business day of the month
    retrieval_date <- tail( last_three, 1 )
  }else{
    # Changed after CMHC audit on on 06/11/2015: See Outlook task for more info
    # first business day of the month
    retrieval_date <- head( next_three, 1 )
    if( !cmhc_audit ) retrieval_date <- tail( last_three, 1 )
  }
  return( retrieval_date )
}