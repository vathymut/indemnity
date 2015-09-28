#' Get the reporting date for indemnity calculation.
#'
#' The reporting date is the last business day of the month prior
#' to the month the prepayment is passed through to investors.
#'
#' Link for more details is here: 
#' \url{https://www.cmhc-schl.gc.ca/en/hoficlincl/mobase/upload/nha_mbs_indemnity_calculation_methodology.pdf}
#'
#' @param starting_date Starting date.
#'
#' @return reporting_date Reporting date.
#'
#' @examples
#' starting_date <- lubridate::ymd( "2015-02-01" )
#' indemnity_reporting_date( starting_date )
#'
indemnity_reporting_date <- function( starting_date ){
  
  # Get end of month (eom) date for prior month
  first_of_month <- starting_date
  lubridate::day( first_of_month ) <- 1
  eom <- first_of_month - lubridate::days( 1 )
  
  # Keep business days before end of month
  week_before_eom <- eom - c( 0:7 )*lubridate::days( 1 )
  # Note: c( 2:6 ) corresponds to Monday through Friday
  is_weekday <- lubridate::wday( week_before_eom ) %in% c( 2:6 )
  business_days_before <- week_before_eom[ is_weekday ]
  
  # Last business day of the month
  reporting_date <- head( business_days_before, 1 )
  return( reporting_date )
}