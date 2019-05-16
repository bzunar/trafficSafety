library(testthat)
library(trafficSafety)

setwd(dirname(system.file("extdata", "accident_2013.csv.bz2", package = "trafficSafety")))

test_that("outputs are the right kind of objects", {
    expect_true(class(make_filename(2013)) == "character")
    expect_true(class(fars_read_years(c(2013, 2014))) == "list")
    expect_true(class(fars_read_years(c(2013, 2014))[[2]])[1] == "tbl_df")
    expect_true(class(fars_summarize_years(c(2013, 2014, 2015)))[1] == "tbl_df")
    })
