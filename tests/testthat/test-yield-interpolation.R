library( indemnity )
context( "Yield Interpolation" )

#### General yield curves data ####
# Download yields_dt here
userlib_path <- "T:/Landmark/mbs-pricing"
.libPaths( c( .libPaths(), userlib_path ) )
data( "yields_bofc", package = "bondyields" )

#### Test ycurve_for_interpolation ####
test_that(
  "ycurve_for_interpolation", {

    expect_equal( ycurve_for_interpolation( retrieval_date = ymd( "2013-03-01" ), 
                                            ycurve_dt = yields_bofc )[['MONTH1']],
                  expected = yields_bofc[ J( ymd( "2013-03-01" ) ) ][['MONTH1']] )
    
    expect_equal( ycurve_for_interpolation( retrieval_date = ymd( "2013-03-01" ), 
                                            ycurve_dt = yields_bofc )[['OVERNIGHT']],
                  expected = yields_bofc[ J( ymd( "2013-03-01" ) ) ][['OVERNIGHT']] )
    
    expect_equal( ycurve_for_interpolation( retrieval_date = ymd( "2015-02-25" ), 
                                            ycurve_dt = yields_bofc )[['MONTH36']],
                  expected = yields_bofc[ J( ymd( "2015-02-25" ) ) ][['MONTH36']] )
    
    expect_equal( ycurve_for_interpolation( retrieval_date = ymd( "2015-02-25" ), 
                                            ycurve_dt = yields_bofc )[['MONTH84']],
                  expected = yields_bofc[ J( ymd( "2015-02-25" ) ) ][['MONTH84']] )

  })

#### Test yields_for_interpolation ####
data( "bloomberg_cad_maturity", package = "blpxl" )
data( "bloomberg_goc", package = "blpxl" )

test_that(
  "interpolation_pairs", {

    expect_equal( interpolation_pairs( retrieval_date = ymd( "2013-01-29" ), 
                                       wal_date = ymd( "2016-11-23" ),
                                       maturity_dt = bloomberg_cad_maturity,
                                       yields_dt = bloomberg_goc )[['yields']],
                  expected = list( "short" = 1.363, "long" = 1.501 ) )
    
    expect_equal( interpolation_pairs( retrieval_date = ymd( "2013-01-29" ), 
                                       wal_date = ymd( "2016-11-23" ),
                                       maturity_dt = bloomberg_cad_maturity,
                                       yields_dt = bloomberg_goc )[['dates']],
                  expected = list( "short" = ymd("2016-06-01"), "long" = ymd("2017-09-01") ) )

    expect_equal( interpolation_pairs( retrieval_date = ymd( "2013-01-29" ), 
                                       wal_date = ymd( "2016-11-23" ),
                                       maturity_dt = bloomberg_cad_maturity,
                                       yields_dt = bloomberg_goc )[['tenors']],
                  expected = list( "short" = "MONTH48", "long" = "MONTH60" ) )

  })

#### Interpolated GoC yield ####
test_that(
  "bey_indemnity", {

    expect_equal( bey_indemnity( bey_short = 1.363, 
                                 bey_long = 1.501, 
                                 date_short = ymd( "2016-06-01" ), 
                                 date_long = ymd( "2017-09-01" ) , 
                                 date_wal = ymd( "2016-11-23" ) ),
                  expected = 1.416,
                  tolerance = 1e-6 )

    
  })

#### Interpolated GoC yield ####
test_that(
  "interpolate_bey", {

    expect_equal( interpolate_bey( issue_date = ymd( "2012-12-01" ), 
                                   settlement_date  = ymd( "2013-01-31" ),
                                   maturity_date = ymd( "2017-09-01" ),
                                   maturity_dt = bloomberg_cad_maturity,
                                   yields_dt = bloomberg_goc,
                                   wal = 3.812,
                                   cmhc_audit = FALSE ),
                  expected = 1.416,
                  tolerance = 1e-6 )

  })
