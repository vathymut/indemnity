#' Get the yield curve to interpolate for indemnity calculation 
#'
#' @param retrieval_date Retrieval (anchor) date.
#' @param ycurve_dt Datatable of historical yield curves.
#' @param date_re Regex to uniquely match the column with dates in \code{yields_dt}.
#'
#' @return dt \code{Data.table} of yield curve for indemnity calculation.
#'
#' @examples
#' retrieval_date <- lubridate::ymd( "2013-03-01" )
#' data( bloomberg_goc, package = "blpxl" )
#' ycurve_for_interpolation( retrieval_date, ycurve_dt = bloomberg_goc )
#'
ycurve_for_interpolation <- function( retrieval_date, ycurve_dt, date_re = "date" ){
  
  # Get columns with yield rates
  re <- stringr::regex( date_re, ignore_case = TRUE )
  all_cols <- names( ycurve_dt )
  value_cols <- all_cols[ !stringr::str_detect( all_cols, pattern = re ) ]
  date_col <- all_cols[ stringr::str_detect( all_cols, pattern = re ) ]
  
  # validate inputs
  stopifnot( lubridate::is.POSIXct( retrieval_date ),
             data.table::is.data.table( ycurve_dt ),
             length( value_cols ) > 1L,
             length( date_col ) == 1L )
  
  # Get data
  data.table::setkeyv( ycurve_dt, date_col )
  retrieval_dt <- ycurve_dt[ J( retrieval_date ), roll = TRUE, rollends = FALSE ]
  dt <- retrieval_dt[ , value_cols, with = F ]
  return( dt )
}