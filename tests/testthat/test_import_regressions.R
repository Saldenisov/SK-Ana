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

make_ambiguous_dxw_file <- function() {
  path <- tempfile(fileext = ".dat")
  lines <- c(
    "0.0000\t277.5735\t377.5907\t477.6078\t577.6245",
    "222.2916\t-0.0204\t-0.0088\t-0.0068\t-0.0051",
    "272.8481\t-0.0043\t-0.0119\t-0.0096\t-0.0087",
    "323.4047\t0.0083\t-0.0086\t-0.0151\t-0.0122",
    "373.9612\t0.0012\t-0.0044\t-0.0102\t-0.0099"
  )
  writeLines(lines, path)
  path
}

expect_export_grid_equal <- function(grid, expected) {
  testthat::expect_equal(
    unname(matrix(as.numeric(grid), nrow = nrow(grid), ncol = ncol(grid))),
    expected
  )
}

shiny::testServer(server, {
  testthat::test_that("server session initializes without reactive context errors", {
    testthat::expect_true(dir.exists("outputDir"))
    testthat::expect_true(exists("getOneMatrix"))
  })

  testthat::test_that("explicit wxd selection is preserved even when file looks dxw", {
    path <- make_dxw_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(style = "elyseStyle", compFacD = 1, compFacW = 1, transformDelay = 0)
    parsed <- getOneMatrix(path)

    testthat::expect_false(is.null(parsed))
    testthat::expect_equal(dim(parsed$mat), c(3, 3))
    testthat::expect_equal(parsed$datStr, "wxd")
    testthat::expect_equal(parsed$inferredDatStr, "dxw")
    testthat::expect_true(parsed$orientationMismatch)
    testthat::expect_equal(parsed$delay, c(222.2916, 322.8481, 423.4047))
    testthat::expect_equal(parsed$wavl, c(10.5, 10.6, 10.7))
    testthat::expect_equal(unname(parsed$mat[1, ]), c(-0.0204, -0.0088, -0.0068))
  })

  testthat::test_that("wxd TSV imports without transposition", {
    path <- make_wxd_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(style = "elyseStyle", compFacD = 1, compFacW = 1, transformDelay = 0)
    parsed <- getOneMatrix(path)

    testthat::expect_false(is.null(parsed))
    testthat::expect_equal(dim(parsed$mat), c(3, 3))
    testthat::expect_equal(parsed$datStr, "wxd")
    testthat::expect_equal(parsed$inferredDatStr, "wxd")
    testthat::expect_false(parsed$orientationMismatch)
    testthat::expect_equal(parsed$delay, c(10.5, 10.6, 10.7))
    testthat::expect_equal(parsed$wavl, c(222.2916, 322.8481, 423.4047))
    testthat::expect_equal(unname(parsed$mat[, 1]), c(-0.0204, -0.0088, -0.0068))
  })

  testthat::test_that("explicit dxw selection imports dxw file with rotation only by user choice", {
    path <- make_dxw_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(
      style = "otherStyle",
      header = FALSE,
      sep = "\t",
      dec = ".",
      datStr = "dxw",
      compFacD = 1,
      compFacW = 1,
      transformDelay = 0
    )
    parsed <- getOneMatrix(path)

    testthat::expect_false(is.null(parsed))
    testthat::expect_equal(parsed$datStr, "dxw")
    testthat::expect_equal(parsed$inferredDatStr, "dxw")
    testthat::expect_false(parsed$orientationMismatch)
    testthat::expect_equal(parsed$delay, c(10.5, 10.6, 10.7))
    testthat::expect_equal(parsed$wavl, c(222.2916, 322.8481, 423.4047))
    testthat::expect_equal(unname(parsed$mat[1, ]), c(-0.0204, -0.0043, 0.0083))
  })

  testthat::test_that("auto mode keeps the default wxd orientation until transpose is confirmed", {
    path <- make_dxw_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(style = "autoStyle", compFacD = 1, compFacW = 1, transformDelay = 0)
    inputStyle$header <- FALSE
    inputStyle$sep <- "\t"
    inputStyle$dec <- "."
    inputStyle$datStr <- "wxd"
    parsed <- getOneMatrix(path)

    testthat::expect_false(is.null(parsed))
    testthat::expect_equal(parsed$datStr, "wxd")
    testthat::expect_equal(parsed$inferredDatStr, "dxw")
    testthat::expect_true(parsed$orientationMismatch)
    testthat::expect_equal(parsed$delay, c(222.2916, 322.8481, 423.4047))
    testthat::expect_equal(parsed$wavl, c(10.5, 10.6, 10.7))

    orientationPrompt$confirmedDatStr <- "dxw"
    parsed_confirmed <- getOneMatrix(path)

    testthat::expect_equal(parsed_confirmed$datStr, "dxw")
    testthat::expect_equal(parsed_confirmed$inferredDatStr, "dxw")
    testthat::expect_false(parsed_confirmed$orientationMismatch)
    testthat::expect_equal(parsed_confirmed$delay, c(10.5, 10.6, 10.7))
    testthat::expect_equal(parsed_confirmed$wavl, c(222.2916, 322.8481, 423.4047))
  })

  testthat::test_that("ambiguous spectral scales infer dxw and trigger mismatch under wxd preset", {
    path <- make_ambiguous_dxw_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(style = "elyseStyle", compFacD = 1, compFacW = 1, transformDelay = 0)
    parsed <- getOneMatrix(path)

    testthat::expect_false(is.null(parsed))
    testthat::expect_equal(parsed$datStr, "wxd")
    testthat::expect_equal(parsed$inferredDatStr, "dxw")
    testthat::expect_true(parsed$orientationMismatch)
    testthat::expect_equal(parsed$delay, c(222.2916, 272.8481, 323.4047, 373.9612))
    testthat::expect_equal(parsed$wavl, c(277.5735, 377.5907, 477.6078, 577.6245))
  })

  testthat::test_that("dxw export preserves original on-disk orientation", {
    path <- make_dxw_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(
      style = "otherStyle",
      header = FALSE,
      sep = "\t",
      dec = ".",
      datStr = "dxw",
      compFacD = 1,
      compFacW = 1,
      transformDelay = 0
    )
    parsed <- getOneMatrix(path)
    exported <- buildMatrixExportGrid(parsed$mat, parsed$wavl, parsed$delay, parsed$datStr)

    expected <- matrix(c(
      0, 10.5, 10.6, 10.7,
      222.2916, -0.0204, -0.0088, -0.0068,
      322.8481, -0.0043, -0.0119, -0.0096,
      423.4047, 0.0083, -0.0086, -0.0151
    ), nrow = 4, byrow = TRUE)

    expect_export_grid_equal(exported, expected)
  })

  testthat::test_that("wxd export preserves original on-disk orientation", {
    path <- make_wxd_file()
    withr::defer(unlink(path), envir = parent.frame())

    session$setInputs(style = "elyseStyle", compFacD = 1, compFacW = 1, transformDelay = 0)
    parsed <- getOneMatrix(path)
    exported <- buildMatrixExportGrid(parsed$mat, parsed$wavl, parsed$delay, parsed$datStr)

    expected <- matrix(c(
      0, 222.2916, 322.8481, 423.4047,
      10.5, -0.0204, -0.0043, 0.0083,
      10.6, -0.0088, -0.0119, -0.0086,
      10.7, -0.0068, -0.0096, -0.0151
    ), nrow = 4, byrow = TRUE)

    expect_export_grid_equal(exported, expected)
  })
})
