repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(repo_root)
withr::defer(setwd(old_wd))

source("global.R")
server <- source("server.R")$value

make_dxw_file <- function() {
  path <- tempfile(fileext = ".dat")
  lines <- c(
    "0.0000\t10.5000\t10.6000\t10.7000",
    "222.2916\t-0.0204\t-0.0088\t-0.0068",
    "322.8481\t-0.0043\t-0.0119\t-0.0096",
    "423.4047\t0.0083\t-0.0086\t-0.0151"
  )
  writeLines(lines, path)
  path
}

make_wxd_file <- function() {
  path <- tempfile(fileext = ".dat")
  lines <- c(
    "0.0000\t222.2916\t322.8481\t423.4047",
    "10.5000\t-0.0204\t-0.0043\t0.0083",
    "10.6000\t-0.0088\t-0.0119\t-0.0086",
    "10.7000\t-0.0068\t-0.0096\t-0.0151"
  )
  writeLines(lines, path)
  path
}

shiny::testServer(server, {
  testthat::test_that("server session initializes without reactive context errors", {
    testthat::expect_true(dir.exists("outputDir"))
    testthat::expect_true(exists("getOneMatrix"))
  })

  testthat::test_that("dxw TSV imports with delays from the top row and wavelengths from the first column", {
    path <- make_dxw_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(style = "elyseStyle", compFacD = 1, compFacW = 1, transformDelay = 0)
    parsed <- getOneMatrix(path)

    testthat::expect_false(is.null(parsed))
    testthat::expect_equal(dim(parsed$mat), c(3, 3))
    testthat::expect_equal(parsed$delay, c(10.5, 10.6, 10.7))
    testthat::expect_equal(parsed$wavl, c(222.2916, 322.8481, 423.4047))
    testthat::expect_equal(unname(parsed$mat[1, ]), c(-0.0204, -0.0043, 0.0083))
  })

  testthat::test_that("wxd TSV imports without transposition", {
    path <- make_wxd_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(style = "elyseStyle", compFacD = 1, compFacW = 1, transformDelay = 0)
    parsed <- getOneMatrix(path)

    testthat::expect_false(is.null(parsed))
    testthat::expect_equal(dim(parsed$mat), c(3, 3))
    testthat::expect_equal(parsed$delay, c(10.5, 10.6, 10.7))
    testthat::expect_equal(parsed$wavl, c(222.2916, 322.8481, 423.4047))
    testthat::expect_equal(unname(parsed$mat[, 1]), c(-0.0204, -0.0088, -0.0068))
  })
})
