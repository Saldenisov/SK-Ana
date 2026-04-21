repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(repo_root)
withr::defer(setwd(old_wd))
source("global.R")
server <- source("server.R")$value

# Unit tests for a subset of pure functions within the server environment

shiny::testServer(server, {
  testthat::test_that("lof works on identical matrices", {
    m <- matrix(c(1,2,3,4), nrow = 2)
    testthat::expect_equal(lof(m, m), 0)
  })
  
  testthat::test_that("showMSE logic", {
    testthat::expect_false(showMSE(NULL, 1:3, 2))
    testthat::expect_false(showMSE("avrg", 1:3, 2))
    testthat::expect_false(showMSE("tileDel", 1, 2))
    testthat::expect_false(showMSE("tileDel", 1:3, 1))
    testthat::expect_true(showMSE("tileDel", 1:3, 2))
  })
  
  testthat::test_that("process_status handles NULL", {
    s <- process_status(NULL)
    testthat::expect_true(is.null(s$running))
    testthat::expect_true(is.null(s$result))
  })

  testthat::test_that("background runner availability check is safe", {
    testthat::expect_true(exists("background_runner_available"))
    available <- background_runner_available()
    testthat::expect_type(available, "logical")
    testthat::expect_length(available, 1)
  })

  testthat::test_that("ALS helper functions compute expected defaults", {
    testthat::expect_equal(als_nstart(4, "seq"), 2L)
    testthat::expect_equal(als_nstart(4, "SVD"), 4L)

    Inputs$maskSpExp <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
    delay_id <- c(1, 1, 2, 2)
    null_c <- als_null_constraints(delay_id, 2)
    testthat::expect_true(is.matrix(null_c))
    testthat::expect_equal(null_c[, 1], c(1, 1, 0, 0))
    testthat::expect_equal(null_c[, 2], c(0, 0, 1, 1))
  })
  
  testthat::test_that("getC/getS with linear solve (nonneg=FALSE)", {
    # Design a tiny consistent system
    S <- diag(2)
    data <- matrix(c(1, 2,
                     3, 4), nrow = 2, byrow = TRUE)
    C0 <- matrix(0, nrow = 2, ncol = 2)
    C <- getC(S = S, data = data, C = C0, nonnegC = FALSE, nullC = NA, closeC = FALSE, wCloseC = 0)
    testthat::expect_equal(C, data, tolerance = 1e-12)
    
    Ceye <- diag(2)
    S0 <- matrix(0, nrow = 2, ncol = 2)
    Sout <- getS(C = Ceye, data = t(Ceye), S = S0, xS = 1:2,
                 nonnegS = FALSE, uniS = FALSE, S0 = NULL, normS = FALSE,
                 smooth = 0, SumS = FALSE, hardS0 = FALSE, wHardS0 = 1)
    testthat::expect_equal(Sout, diag(2), tolerance = 1e-12)
  })
})
