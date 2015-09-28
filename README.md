README
========================================================

## About
`indemnity` calculates the NHAMBS bond equivalent yield for indemnity payments using interpolation.

The methodology for indemnity calculation is outlined 
[`here`](https://www.cmhc-schl.gc.ca/en/hoficlincl/mobase/upload/nha_mbs_indemnity_calculation_methodology.pdf). 

## Installation
You can get the development version by running:
```{r}
devtools::install_github("vathymut/indemnity")
```

## Usage
To calculate the bond equivalent yield for indemnity pricing, use:
```{r}
issue_date <- lubridate::ymd( "2012-12-01" )
settlement_date <- lubridate::ymd( "2013-01-31" )
maturity_date <- lubridate::ymd( "2017-09-01" )
# Assumes package blpxl is available (Check library path: .libPaths()).
data( bloomberg_goc, package = "blpxl" )
data( bloomberg_cad_maturity, package = "blpxl" ) 
wal <- 3.812
interpolate_bey( issue_date, settlement_date, wal, yields_dt = bloomberg_goc, maturity_dt = bloomberg_cad_maturity )
```