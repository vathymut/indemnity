library( indemnity )
context( "Date Interpolation" )

#### Test wal_to_date ####
test_that(
  "wal_to_date", {
    
    expect_equal( wal_to_date( start_date = ymd( "2015-02-22" ), wal = 14 ),
                  expected = ymd( "2029-02-22" ) )
    
    expect_equal( wal_to_date( start_date = ymd( "2015-02-22" ), wal = 6.5 ),
                  expected = ymd( "2021-08-23" ) )
    
    expect_equal( wal_to_date( start_date = ymd( "2015-02-22" ), wal = 3.25 ),
                  expected = ymd( "2018-05-24" ) )

    expect_equal( wal_to_date( start_date = ymd( "2013-01-31" ), wal = 3.812 ),
                  expected = ymd( "2016-11-23" ) )
    
    
  })

#### Test wal_to_date ####
test_that(
  "indemnity_reporting_date", {
    
    expect_equal( indemnity_reporting_date( settlement_date = ymd( "2015-08-07" ) ),
                  expected = ymd( "2015-07-31" ) )
    
    expect_equal( indemnity_reporting_date( settlement_date = ymd( "2015-07-03" ) ),
                  expected = ymd( "2015-06-30" ) )
    
    expect_equal( indemnity_reporting_date( settlement_date = ymd( "2015-06-05" ) ),
                  expected = ymd( "2015-05-29" ) )

  })

#### Test ycurve_retrieval_date ####
test_that(
  "ycurve_retrieval_date", {

    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2013-02-01" ), 
                                         settlement_date = ymd( "2013-02-01" ) ),
                  expected = ymd( "2013-02-01" ) )
    
    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2015-02-01" ), 
                                         settlement_date = ymd( "2015-02-01" ) ),
                  expected = ymd( "2015-01-28" ) )
    
    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2015-02-01" ), 
                                         settlement_date = ymd( "2015-03-01" ) ),
                  expected = ymd( "2015-02-25" ) )
    
    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2013-02-01" ), 
                                         settlement_date = ymd( "2015-03-01" ) ),
                  expected = ymd( "2015-03-02" ) )

    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2012-12-01" ), 
                                         settlement_date = ymd( "2013-01-01" ),
                                         cmhc_audit = FALSE ),
                  expected = ymd( "2012-12-27" ) )

    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2012-12-01" ), 
                                         settlement_date = ymd( "2013-01-31" ),
                                         cmhc_audit = FALSE ),
                  expected = ymd( "2013-01-29" ) )

  })