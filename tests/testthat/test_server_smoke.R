server <- source("../../server.R")$value

# Smoke test: server loads and core utilities are present

shiny::testServer(server, {
  testthat::test_that("server initializes", {
    testthat::expect_true(dir.exists("outputDir"))
  })
  
  testthat::test_that("core functions available", {
    testthat::expect_true(exists("lof"))
    testthat::expect_true(exists("getC"))
    testthat::expect_true(exists("getS"))
    testthat::expect_true(exists("showMSE"))
    testthat::expect_true(exists("process_status"))
  })
})
