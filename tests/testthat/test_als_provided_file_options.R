repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(repo_root)
withr::defer(setwd(old_wd))

source("global.R")
server <- source("server.R")$value

provided_dat_path <- Sys.getenv(
  "SK_ANA_PROVIDED_DAT",
  unset = ""
)

run_with_timeout <- function(job, timeout_sec = 120) {
  waited <- 0
  while (job$is_alive() && waited < timeout_sec) {
    Sys.sleep(1)
    waited <- waited + 1
  }
  list(
    alive = job$is_alive(),
    waited = waited,
    exit_status = job$get_exit_status()
  )
}

shiny::testServer(server, {
  testthat::test_that("provided file supports ALS option sweep", {
    testthat::skip_if_not(
      nzchar(provided_dat_path) && file.exists(provided_dat_path),
      message = "Set SK_ANA_PROVIDED_DAT to run the external ALS option sweep."
    )

    session$setInputs(
      style = "elyseStyle",
      compFacD = 1,
      compFacW = 1,
      postCompFacD = 1,
      postCompFacW = 1,
      transformDelay = 0,
      nALS = 2,
      maxiter = 100,
      alsThresh = -6,
      useCorrectionSpectra = FALSE,
      lambdaCorrectionSpectra = -2,
      initALS = "SVD",
      useFiltered = FALSE,
      optS1st = FALSE,
      nonnegS = TRUE,
      perComponentS = FALSE,
      uniS = FALSE,
      broadeningS = FALSE,
      perComponentBroadening = FALSE,
      broadeningMaxPct = 10,
      normS = TRUE,
      normMode = "intensity",
      smooth = 0,
      SumS = FALSE,
      nonnegC = TRUE,
      closeC = FALSE,
      wCloseC = 1,
      softS0 = FALSE,
      wSoftS0 = 1,
      nSV = 2
    )

    parsed <- getOneMatrix(provided_dat_path)
    testthat::expect_false(is.null(parsed))

    setInputValues(list(
      gotData = TRUE,
      validData = TRUE,
      process = TRUE,
      finish = TRUE,
      fileOrig = provided_dat_path,
      mat = parsed$mat,
      wavl = parsed$wavl,
      delay = parsed$delay,
      delaySave = parsed$delay,
      delayId = rep(1L, length(parsed$delay)),
      matOrig = parsed$mat,
      wavlOrig = parsed$wavl,
      delayOrig = parsed$delay,
      dlScaleFacOrig = 1,
      baselineMask = rep(1, length(parsed$delay)),
      wavlMask = rep(1, length(parsed$wavl)),
      delayMask = rep(1, length(parsed$delay)),
      maskSpExp = matrix(1, nrow = 1, ncol = 2),
      delayTrans = ""
    ))

    build_als_launch <- function(case) {
      session$setInputs(
        initALS = case$initALS,
        useFiltered = case$useFiltered,
        optS1st = case$optS1st,
        nonnegS = case$nonnegS,
        perComponentS = case$perComponentS,
        uniS = case$uniS,
        broadeningS = case$broadeningS,
        perComponentBroadening = case$perComponentBroadening,
        broadeningMaxPct = case$broadeningMaxPct,
        normS = case$normS,
        normMode = case$normMode,
        smooth = case$smooth,
        SumS = case$SumS,
        nonnegC = case$nonnegC,
        closeC = case$closeC,
        wCloseC = case$wCloseC,
        useCorrectionSpectra = FALSE
      )

      if (isTRUE(case$perComponentS)) {
        for (i in seq_len(case$nALS)) {
          do.call(
            session$setInputs,
            stats::setNames(list(case$per_component_nonneg[[i]]), paste0("nonnegS_", i))
          )
        }
      }

      if (isTRUE(case$broadeningS) && isTRUE(case$perComponentBroadening)) {
        for (i in seq_len(case$nALS)) {
          do.call(
            session$setInputs,
            stats::setNames(list(case$per_component_broadening[[i]]), paste0("broadenComp_", i))
          )
        }
      }

      selected_data <- als_selected_data(use_filtered = isTRUE(case$useFiltered))
      testthat::expect_false(is.null(selected_data), info = paste(case$name, "selected data"))

      nullC <- als_null_constraints(selected_data$delayId, case$nALS)
      nStart <- als_nstart(case$nALS, case$initALS)
      init_values <- als_initial_factors(case$initALS, nStart, selected_data$mat)
      testthat::expect_false(is.null(init_values), info = paste(case$name, "init values"))

      norm_mode <- als_norm_mode()
      nonnegS_vec <- als_nonnegS_constraints(case$nALS)
      broadening_vec <- als_broadening_flags(case$nALS)

      usingCoupledALS <- FALSE
      als_fun <- als
      als_args <- list(
        selected_data$delay,
        selected_data$delayId,
        selected_data$wavl,
        selected_data$mat,
        nullC,
        init_values$S,
        init_values$C,
        nAls = case$nALS,
        nStart = nStart,
        S0 = NULL,
        maxiter = case$maxiter,
        uniS = case$uniS,
        nonnegS = nonnegS_vec,
        nonnegC = case$nonnegC,
        thresh = 10^case$alsThresh,
        normS = case$normS,
        hardS0 = !case$softS0,
        wHardS0 = 10^case$wSoftS0,
        optS1st = case$optS1st,
        smooth = case$smooth,
        SumS = case$SumS,
        closeC = case$closeC,
        wCloseC = 10^case$wCloseC,
        normMode = norm_mode,
        state_file = tempfile(fileext = ".rds"),
        update_interval = 10,
        broadening = broadening_vec,
        broadening_max_pct = case$broadeningMaxPct
      )

      testthat::expect_false(usingCoupledALS, info = paste(case$name, "standard ALS path"))

      stdout_file <- tempfile(pattern = "als_case_", fileext = ".stdout")
      launch <- launch_background_job(
        als_fun,
        args = als_args,
        stdout = stdout_file,
        stderr = stdout_file,
        job_name = paste("ALS option sweep", case$name)
      )

      testthat::expect_false(is.null(launch$job), info = paste(case$name, launch$error))

      status <- run_with_timeout(launch$job, timeout_sec = case$timeout_sec)
      testthat::expect_false(status$alive, info = paste(case$name, "timed out"))
      testthat::expect_identical(status$exit_status, 0L, info = paste(case$name, readLines(stdout_file, warn = FALSE) |> paste(collapse = "\n")))

      result <- launch$job$get_result()
      testthat::expect_false(is.null(result), info = paste(case$name, "null result"))
      testthat::expect_equal(dim(result$C), c(length(selected_data$delay), case$nALS), info = case$name)
      testthat::expect_equal(dim(result$S), c(length(selected_data$wavl), case$nALS), info = case$name)
      testthat::expect_true(result$iter <= case$maxiter, info = case$name)
      testthat::expect_true(is.character(result$msg) && nzchar(result$msg), info = case$name)
      testthat::expect_true(is.finite(lof(result$C %*% t(result$S), selected_data$mat)), info = case$name)

      invisible(result)
    }

    cases <- list(
      list(
        name = "baseline_svd",
        nALS = 2, initALS = "SVD", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = TRUE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = TRUE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = TRUE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "pca_init",
        nALS = 2, initALS = "PCA", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = TRUE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = TRUE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = TRUE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "nmf_init",
        nALS = 2, initALS = "NMF", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = TRUE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = TRUE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = TRUE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "sequential_init",
        nALS = 2, initALS = "seq", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = TRUE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = TRUE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = TRUE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "filtered_optS_first",
        nALS = 2, initALS = "SVD", useFiltered = TRUE, optS1st = TRUE,
        nonnegS = TRUE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = TRUE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = TRUE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "signed_components_no_norm",
        nALS = 2, initALS = "SVD", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = FALSE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = FALSE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = FALSE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "l1_norm_sum_smooth",
        nALS = 2, initALS = "SVD", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = TRUE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = TRUE, normMode = "l1", smooth = 0.2,
        SumS = TRUE, nonnegC = TRUE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "closure_constraint",
        nALS = 2, initALS = "SVD", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = TRUE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = TRUE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = TRUE, closeC = TRUE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "per_component_nonnegativity",
        nALS = 2, initALS = "SVD", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = TRUE, perComponentS = TRUE, per_component_nonneg = list(TRUE, FALSE),
        uniS = FALSE, broadeningS = FALSE, perComponentBroadening = FALSE, per_component_broadening = NULL,
        broadeningMaxPct = 10, normS = TRUE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = TRUE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      ),
      list(
        name = "broadening_per_component",
        nALS = 2, initALS = "SVD", useFiltered = FALSE, optS1st = FALSE,
        nonnegS = TRUE, perComponentS = FALSE, per_component_nonneg = NULL,
        uniS = FALSE, broadeningS = TRUE, perComponentBroadening = TRUE, per_component_broadening = list(TRUE, FALSE),
        broadeningMaxPct = 5, normS = TRUE, normMode = "intensity", smooth = 0,
        SumS = FALSE, nonnegC = TRUE, closeC = FALSE, wCloseC = 1,
        softS0 = FALSE, wSoftS0 = 1, alsThresh = -6, maxiter = 100, timeout_sec = 120
      )
    )

    for (case in cases) {
      build_als_launch(case)
    }
  })
})
