#' Check that the settlement date is the report date for indemnity calculation.
#'
#' The reporting date is the last business day of the month prior
#' to the month the prepayment is passed through to investors.
#'
#' Link for more details is here: 
#' \url{https://www.cmhc-schl.gc.ca/en/hoficlincl/mobase/upload/nha_mbs_indemnity_calculation_methodology.pdf}
#'
#' @param settlement_date Settlement date.
#'
#' @return test_pass \code{TRUE} if settlement date is the last business day of the month.
#'
#' @examples
#' settlement_date <- lubridate::ymd( "2015-02-01" )
#' indemnity_reporting_date( settlement_date )
#'
check_settlement_date <- function( settlement_date ){
  
  # Last business day of the month
  reporting_date <- indemnity_reporting_date( settlement_date )

  # Check if the same date
  test_pass <- identical( reporting_date, settlement_date )
  warning_msg <- sprintf( "Should Settlement date be: %s?", settlement_date )
  if( !test_pass ) warning( warning_msg )
  return( test_pass )
}