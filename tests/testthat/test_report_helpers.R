repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(repo_root)
withr::defer(setwd(old_wd))
source("global.R")
server <- source("server.R")$value

shiny::testServer(server, {
  testthat::test_that("report template path resolves to an existing file", {
    testthat::expect_true(exists("report_template_path"))
    path <- report_template_path()
    testthat::expect_type(path, "character")
    testthat::expect_length(path, 1)
    testthat::expect_true(file.exists(path))
  })

  testthat::test_that("matching output files handles regex characters safely", {
    testthat::expect_true(exists("matching_output_files"))

    tmp_dir <- tempfile("report-output-")
    dir.create(tmp_dir, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    project_tag <- "demo[1]+test"
    wanted <- file.path(tmp_dir, paste0(project_tag, "_Matrix.csv"))
    other <- file.path(tmp_dir, "demoX1_test_Matrix.csv")

    writeLines("a,b", wanted)
    writeLines("a,b", other)

    matches <- matching_output_files(project_tag, output_dir = tmp_dir)
    testthat::expect_equal(basename(matches), basename(wanted))
  })

  testthat::test_that("matching output files returns empty when directory is missing", {
    missing_dir <- tempfile("missing-output-")
    matches <- matching_output_files("project", output_dir = missing_dir)
    testthat::expect_length(matches, 0)
  })
})
