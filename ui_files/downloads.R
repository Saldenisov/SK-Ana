sidebarPanel(
  width = sideWidth,
  wellPanel(
    h4("Generate Report"),
    checkboxGroupInput(
      inputId = 'inReport', 
      label   = 'Include these results', 
      choices = c(
        'SVD' = 'SVD',
        'ALS' = 'ALS'), 
      selected = c('SVD')
    ),
    # No other formats than html available on styx (install pandoc ???)
    #       radioButtons(
    #         inputId = 'format',
    #         label   = 'Document format',
    #         choices = c('html', 'pdf', 'docx'),
    #         inline  = TRUE
    #       ),
    verticalLayout(
      textInput(
        inputId = 'reportName', 
        label   = 'Report Name', 
        value   = "SK-Ana"
      ),
      downloadButton(
        outputId = 'report',
        label    = 'Download  (Ctrl+Click)'
      )#,
      # bookmarkButton()
    )
  ),
  wellPanel(
    h4("Get my files"),
    h5("Download the files you saved during your session"),
    downloadButton(
      outputId = 'getMyFiles',
      label    = 'Download  (Ctrl+Click)'
    )
  )
)
