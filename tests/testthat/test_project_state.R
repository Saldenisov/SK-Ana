repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(repo_root)
withr::defer(setwd(old_wd))
source("global.R")
server <- source("server.R")$value

shiny::testServer(server, {
  testthat::test_that("project config keeps only persisted slider fields", {
    cfg <- build_project_config(list(
      keepDoRange = c(0, 1),
      keepWlRange = c(300, 500),
      projectTag = "demo",
      randomField = 42
    ))

    testthat::expect_named(cfg, c("keepDoRange", "keepWlRange"))
    testthat::expect_equal(cfg$keepDoRange, c(0, 1))
  })

  testthat::test_that("project state round-trip preserves config and data", {
    tmp <- tempfile(fileext = ".Rda")
    on.exit(unlink(tmp), add = TRUE)

    state <- build_project_state(
      input_values = list(
        projectTag = "demo",
        keepDoRange = c(-1, 1),
        keepWlRange = c(350, 550)
      ),
      input_state = list(
        gotData = TRUE,
        validData = TRUE,
        fileOrig = c("a.csv"),
        matOrig = matrix(1:4, nrow = 2),
        wavlOrig = c(350, 450),
        delayOrig = c(0, 1),
        dlScaleFacOrig = 1
      )
    )

    testthat::expect_false(is.null(write_project_state(tmp, state)))
    loaded <- read_project_state(tmp)

    testthat::expect_equal(loaded$version, project_state_format_version)
    testthat::expect_equal(loaded$metadata$project_tag, "demo")
    testthat::expect_equal(loaded$config$keepWlRange, c(350, 550))
    testthat::expect_equal(loaded$data$fileOrig, c("a.csv"))
  })

  testthat::test_that("legacy project state still loads from Inputs object", {
    tmp <- tempfile(fileext = ".Rda")
    on.exit(unlink(tmp), add = TRUE)

    legacy_env <- new.env(parent = emptyenv())
    legacy_env$Inputs <- list(
      gotData = TRUE,
      validData = TRUE,
      fileOrig = c("legacy.csv"),
      matOrig = matrix(1:4, nrow = 2),
      wavlOrig = c(300, 400),
      delayOrig = c(0, 1),
      dlScaleFacOrig = 1
    )
    save(list = c("Inputs"), file = tmp, envir = legacy_env)

    loaded <- read_project_state(tmp)
    testthat::expect_equal(loaded$version, 0L)
    testthat::expect_equal(loaded$data$fileOrig, c("legacy.csv"))
  })

  testthat::test_that("apply project state restores Inputs", {
    state <- list(
      version = 1L,
      metadata = list(project_tag = "demo"),
      config = list(
        keepDoRange = c(1, 4),
        keepWlRange = c(300, 400),
        keepWlCut = 350,
        keepDlRange = c(0, 1),
        keepDlCut = 0.5,
        keepCbl = 0
      ),
      data = list(
        gotData = TRUE,
        validData = TRUE,
        process = TRUE,
        finish = TRUE,
        fileOrig = c("demo.csv"),
        matOrig = matrix(c(1, 2, 3, 4), nrow = 2),
        wavlOrig = c(300, 400),
        delayOrig = c(0, 1),
        dlScaleFacOrig = 1,
        delayGlitch = NA
      )
    )

    testthat::expect_true(apply_project_state(state))
    testthat::expect_equal(Inputs$fileOrig, c("demo.csv"))
    testthat::expect_true(isTRUE(projectState()$metadata$project_tag == "demo"))
  })
})
