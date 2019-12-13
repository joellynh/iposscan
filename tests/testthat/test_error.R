test_that("Error messages appear at stop", {
  expect_error(iposscan::getbydate(date = "2018-08-23", type = "random"))
  expect_error(iposscan::dfbydate(date = "01-01-2019"))
  expect_error(iposscan::dftwodates(startdate = "1", enddate = "2"))
  expect_error(iposscan::vizbygeog(startdate = "2018-08-23", enddate = "2018-08-26", role = "anyone"))
  expect_error(iposscan::similarpatent(startdate = "2018-08-21", enddate = "2018-08-23"))

}
)
