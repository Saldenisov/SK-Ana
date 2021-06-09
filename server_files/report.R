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
    src <- normalizePath('reportTemplate.Rmd')
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'reportTemplate.Rmd')
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
    out <- rmarkdown::render('reportTemplate.Rmd',"html_document",
                             clean = TRUE)
    
    # No other formats available on styx (install pandoc ???)
    #       out <- rmarkdown::render('reportTemplate.Rmd',
    #                                switch(
    #                                  input$format,
    #                                  html = "html_document",
    #                                  pdf  = "pdf_document",
    #                                  docx = "word_document"
    #                                ))

    file.rename(out, file)
  }
)

output$getMyFiles <- downloadHandler(
  filename = function ()
    paste0(input$projectTag,'_files.zip'),
  content = function(fname) {
    wd = getwd()
    setwd("outputDir")
    fs= list.files(pattern=input$projectTag)
    # Zip'em
    zip(zipfile=fname, files=fs)
    # cf. https://groups.google.com/d/msg/shiny-discuss/D5F2nqrIhiM/JDEX0b6we1cJ
    if(file.exists(paste0(fname, ".zip"))) 
      file.rename(paste0(fname, ".zip"), fname)
    setwd(wd)
  },
  contentType = "application/zip"
)
