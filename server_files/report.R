report_template_path <- safely(function() {
  normalizePath("reportTemplate.Rmd", mustWork = TRUE)
}, return_on_error = NULL)

matching_output_files <- safely(function(project_tag, output_dir = "outputDir") {
  if (is.null(project_tag) || !nzchar(project_tag) || !dir.exists(output_dir)) {
    return(character(0))
  }

  escaped_tag <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", project_tag)
  list.files(output_dir, pattern = paste0("^", escaped_tag), full.names = TRUE)
}, return_on_error = character(0))

render_project_report <- safely(function(output_file, envir) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to generate reports.")
  }

  src <- report_template_path()
  if (is.null(src)) {
    return(NULL)
  }

  tmp_input <- tempfile(pattern = "reportTemplate_", fileext = ".Rmd", tmpdir = tempdir())
  ok <- file.copy(src, tmp_input, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Failed to prepare temporary report template.")
  }

  out <- rmarkdown::render(
    input = tmp_input,
    output_format = "html_document",
    output_file = basename(output_file),
    output_dir = dirname(output_file),
    intermediates_dir = tempdir(),
    clean = TRUE,
    envir = envir
  )

  normalizePath(out, mustWork = TRUE)
}, return_on_error = NULL)

includeInReport = reactiveVal("SVD") 
  
observeEvent(
  input$projectTag,
  updateTextInput(
    session,
    inputId = 'reportName',
    value = paste0(input$projectTag,'_Report')
  )
)

output$report = downloadHandler(
  filename = function() {
    paste0(input$reportName, '.html')
  },
  content = function(file) {
    id = showNotification(
      "Generating report (in your 'Downloads' folder)", 
      type = "message",
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(
      removeNotification(id), 
      add = TRUE
    )
    out <- render_project_report(file, envir = environment())
    if (is.null(out)) {
      stop("Report generation failed.")
    }
    
    # No other formats available on styx (install pandoc ???)
    #       out <- rmarkdown::render('reportTemplate.Rmd',
    #                                switch(
    #                                  input$format,
    #                                  html = "html_document",
    #                                  pdf  = "pdf_document",
    #                                  docx = "word_document"
    #                                ))

    if (!identical(normalizePath(out, mustWork = TRUE), normalizePath(file, mustWork = FALSE))) {
      file.rename(out, file)
    }
  }
)

output$getMyFiles <- downloadHandler(
  filename = function ()
    paste0(input$projectTag,'_files.zip'),
  content = function(fname) {
    fs <- matching_output_files(input$projectTag, output_dir = "outputDir")
    if (length(fs) == 0) {
      stop("No project files found in outputDir.")
    }
    # Zip'em
    utils::zip(zipfile = fname, files = fs)
    # cf. https://groups.google.com/d/msg/shiny-discuss/D5F2nqrIhiM/JDEX0b6we1cJ
    if(file.exists(paste0(fname, ".zip"))) 
      file.rename(paste0(fname, ".zip"), fname)
  },
  contentType = "application/zip"
)
