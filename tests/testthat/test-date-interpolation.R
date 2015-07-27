library( indemnity )
context( "Date Interpolation" )

#### Test wal_to_date ####
test_that(
  "wal_to_date", {
    
    expect_equal( wal_to_date( settlement_date = ymd( "2015-02-22" ), wal = 14 ),
                  expected = ymd( "2029-02-22" ) )
    
    expect_equal( wal_to_date( settlement_date = ymd( "2015-02-22" ), wal = 6.5 ),
                  expected = ymd( "2021-08-23" ) )
    
    expect_equal( wal_to_date( settlement_date = ymd( "2015-02-22" ), wal = 3.25 ),
                  expected = ymd( "2018-05-24" ) )

    expect_equal( wal_to_date( settlement_date = ymd( "2013-01-31" ), wal = 3.812 ),
                  expected = ymd( "2016-11-23" ) )
    
    
  })

#### Test ycurve_retrieval_date ####
test_that(
  "ycurve_retrieval_date", {

    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2013-02-22" ), 
                                         settlement_date = ymd( "2013-02-22" ) ),
                  expected = ymd( "2013-03-01" ) )
    
    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2015-02-22" ), 
                                         settlement_date = ymd( "2015-02-22" ) ),
                  expected = ymd( "2015-02-25" ) )
    
    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2015-02-22" ), 
                                         settlement_date = ymd( "2015-03-01" ) ),
                  expected = ymd( "2015-02-25" ) )
    
    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2013-02-22" ), 
                                         settlement_date = ymd( "2015-03-01" ) ),
                  expected = ymd( "2015-03-02" ) )

    expect_equal( ycurve_retrieval_date( issue_date = ymd( "2012-12-01" ), 
                                         settlement_date = ymd( "2013-01-31" ),
                                         cmhc_audit = FALSE ),
                  expected = ymd( "2013-01-29" ) )

  })