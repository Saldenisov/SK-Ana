repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(repo_root)
withr::defer(setwd(old_wd))
source("global.R")
server <- source("server.R")$value

shiny::testServer(server, {
  testthat::test_that("RAM helper is safe across platforms", {
    testthat::expect_true(exists("get_available_ram_gb"))
    ram_gb <- get_available_ram_gb()
    testthat::expect_type(ram_gb, "double")
    testthat::expect_length(ram_gb, 1)
    testthat::expect_true(is.na(ram_gb) || (is.finite(ram_gb) && ram_gb > 0))
  })

  testthat::test_that("selectionFileMatchesProject checks both matrix dimensions", {
    saved <- list(projectTag = "proj", matDims = c(5, 10))
    good_mat <- matrix(0, nrow = 5, ncol = 10)
    bad_mat <- matrix(0, nrow = 5, ncol = 11)

    testthat::expect_true(selectionFileMatchesProject(saved, "proj", good_mat))
    testthat::expect_false(selectionFileMatchesProject(saved, "proj", bad_mat))
    testthat::expect_false(selectionFileMatchesProject(saved, "other", good_mat))
  })

  testthat::test_that("loadSelectionState reads ll only and does not leak objects", {
    tmp <- tempfile(fileext = ".Rda")
    src <- new.env(parent = emptyenv())
    src$ll <- list(projectTag = "proj", matDims = c(2, 2))
    src$malicious <- "should_not_leak"
    save(list = c("ll", "malicious"), file = tmp, envir = src)

    testthat::expect_false(exists("malicious", inherits = FALSE))
    loaded <- loadSelectionState(tmp)
    testthat::expect_true(is.list(loaded))
    testthat::expect_equal(loaded$projectTag, "proj")
    testthat::expect_false(exists("malicious", inherits = FALSE))

    unlink(tmp)
  })

  testthat::test_that("loadSelectionState rejects files without ll list", {
    tmp <- tempfile(fileext = ".Rda")
    src <- new.env(parent = emptyenv())
    src$bad <- 123
    save(list = c("bad"), file = tmp, envir = src)

    loaded <- loadSelectionState(tmp)
    testthat::expect_null(loaded)

    unlink(tmp)
  })

  testthat::test_that("selection state round-trip preserves payload", {
    tmp <- tempfile(fileext = ".Rda")
    payload <- list(projectTag = "proj", matDims = c(2, 2), keepWlRange = c(300, 400))
    state <- buildSelectionState(payload, project_tag = "proj")

    testthat::expect_false(is.null(writeSelectionState(tmp, state)))
    loaded <- loadSelectionState(tmp)

    testthat::expect_true(is.list(loaded))
    testthat::expect_equal(loaded$projectTag, "proj")
    testthat::expect_equal(loaded$keepWlRange, c(300, 400))

    unlink(tmp)
  })

  testthat::test_that("mask registry helpers add and remove names safely", {
    testthat::expect_false(hasMask("wl", "keepWlMask1"))
    registerMask("wl", "keepWlMask1")
    testthat::expect_true(hasMask("wl", "keepWlMask1"))
    unregisterMask("wl", "keepWlMask1")
    testthat::expect_false(hasMask("wl", "keepWlMask1"))
  })

  testthat::test_that("mask registry setter normalizes duplicates", {
    setMaskRegistry("dl", c("keepDlMask1", "keepDlMask1", "keepDlMask2"))
    testthat::expect_equal(maskRegistry("dl"), c("keepDlMask1", "keepDlMask2"))
    setMaskRegistry("dl", character(0))
    testthat::expect_length(maskRegistry("dl"), 0)
  })

  testthat::test_that("delay glitch helpers update state predictably", {
    resetDelayGlitch()
    testthat::expect_true(anyNA(Inputs$delayGlitch))

    addDelayGlitch(c(1, 2, 2))
    testthat::expect_equal(Inputs$delayGlitch, c(1, 2))

    addDelayGlitch(c(2, 3))
    testthat::expect_equal(Inputs$delayGlitch, c(1, 2, 3))

    removeLastDelayGlitch()
    testthat::expect_equal(Inputs$delayGlitch, c(1, 2))

    removeLastDelayGlitch()
    removeLastDelayGlitch()
    testthat::expect_true(anyNA(Inputs$delayGlitch))
  })

  testthat::test_that("project config helpers keep reset explicit", {
    setProjectConfig(list(name = "demo", version = 1L))
    testthat::expect_equal(projectConfigValue()$name, "demo")
    clearProjectConfig()
    testthat::expect_null(projectConfigValue())
  })

  testthat::test_that("ALS correction override disables original observer", {
    testthat::expect_true(exists("doALS_Enhanced"))
    testthat::expect_true(exists("doALSOriginalDisabled"))
    testthat::expect_true(isTRUE(doALSOriginalDisabled))
  })

  testthat::test_that("background runner env helper returns supported shape", {
    testthat::expect_true(exists("background_runner_env"))
    env <- background_runner_env()
    testthat::expect_true(is.null(env) || is.character(env))
    if (!is.null(env)) {
      testthat::expect_named(env, "R_ARCH")
    }
  })

  testthat::test_that("background launch reports unavailable optional dependency", {
    testthat::expect_true(exists("launch_background_job"))
    launch <- launch_background_job(
      identity,
      args = list(1),
      job_name = "Test job",
      namespace_available = function() FALSE
    )
    testthat::expect_null(launch$job)
    testthat::expect_match(launch$error, "callr")
  })

  testthat::test_that("background launch returns job when launcher succeeds", {
    fake_job <- structure(list(pid = 123), class = "fake_job")
    launch <- launch_background_job(
      identity,
      args = list(1),
      job_name = "Test job",
      namespace_available = function() TRUE,
      launcher = function(func, args, package, stdout = NULL, stderr = NULL, env = NULL) fake_job
    )
    testthat::expect_identical(launch$job, fake_job)
    testthat::expect_null(launch$error)
  })

  testthat::test_that("background launch can run a real trivial job when callr is available", {
    testthat::skip_if_not(background_runner_available())

    launch <- launch_background_job(
      function(x) x + 1,
      args = list(1),
      job_name = "Integration background job"
    )

    testthat::expect_null(launch$error)
    testthat::expect_false(is.null(launch$job))

    launch$job$wait(5000)
    testthat::expect_equal(launch$job$get_result(), 2)
  })

  testthat::test_that("scheme setters update reactive scheme state", {
    setSchemeField("gotData", FALSE)
    setSchemeFields(list(
      species = c("A", "B"),
      nbSpecies = 2,
      gotData = TRUE
    ))

    testthat::expect_true(isTRUE(Scheme$gotData))
    testthat::expect_equal(Scheme$species, c("A", "B"))
    testthat::expect_equal(Scheme$nbSpecies, 2)
  })

  testthat::test_that("kinetic external spectra store is reactive", {
    testthat::expect_type(externalSpectra(), "list")
    externalSpectra(list(A = c(1, 2, 3)))
    testthat::expect_equal(externalSpectra()$A, c(1, 2, 3))
    externalSpectra(list())
    testthat::expect_equal(length(externalSpectra()), 0)
  })

  testthat::test_that("ALS external spectra helper returns selected columns", {
    testthat::expect_type(externalSpectraALS(), "list")
    externalSpectraALS(list(A = c(1, 2), B = c(3, 4)))
    session$setInputs(fixALS_A = TRUE, fixALS_B = FALSE)

    selected <- selectedExternalSpectraALS()
    testthat::expect_true(is.matrix(selected))
    testthat::expect_equal(as.numeric(selected[, 1]), c(1, 2))

    session$setInputs(fixALS_A = FALSE, fixALS_B = FALSE)
    testthat::expect_null(selectedExternalSpectraALS())
    testthat::expect_equal(als_fixed_spectra_count(softS0 = FALSE), 0L)
    externalSpectraALS(list())
  })

  testthat::test_that("ALS fixed spectra count follows soft/fixed selection rules", {
    externalSpectraALS(list(A = c(1, 2), B = c(3, 4)))
    session$setInputs(fixALS_A = TRUE, fixALS_B = TRUE)

    testthat::expect_equal(als_fixed_spectra_count(softS0 = FALSE), 2L)
    testthat::expect_equal(als_fixed_spectra_count(softS0 = TRUE), 0L)

    externalSpectraALS(list())
    session$setInputs(fixALS_A = FALSE, fixALS_B = FALSE)
  })
})
