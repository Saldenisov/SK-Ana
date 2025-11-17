# Shared helpers used across ALS and Kinet modules

showMSE <- safely(function(a, b, c) {
  if (is.null(a)) return(FALSE)
  if (a != "tileDel") return(FALSE)
  if (length(b) <= 1) return(FALSE)
  if (c <= 1) return(FALSE)
  return(TRUE)
}, return_on_error = FALSE)

getExternalSpectra <- safely(function(ui, inputFile, wavl, tag) {
  # Get spectra on file(s), interpolate them on wavl grid
  # and generate selection ui

  offset = length(ui)
  isp = 0
  extSpectra = list()
  for (i in seq_along(inputFile$datapath)) {
    fname = inputFile[i,'name']
    fN    = inputFile[i,'datapath']
    tmp   = try(
      read.table(
        file   = fN,
        header = TRUE,
        dec    = inputStyle$dec,
        sep    = inputStyle$sep,
        colClasses = "numeric",
        stringsAsFactors = FALSE
      ),
      silent = TRUE
    )
    if(class(tmp) == 'try-error') {
      id = showNotification(
        paste0('Error while reading file: ',fname),
        type = "error",
        duration = NULL
      )
    } else {
      for (k in 2:ncol(tmp)) {
        isp = isp + 1
        sp = colnames(tmp)[k]
        
        # Interpolate on wavl grid
        S0 = spline(tmp[, 1], tmp[, k], xout = wavl)$y
        
        # Normalize using absolute values to handle negatives
        norm_factor = max(abs(S0))
        S0 = S0 / ifelse(norm_factor > 0, norm_factor, 1)
        
        # Store in global list
        extSpectra[[paste0("S_",sp)]] = S0
        
        # Generate selection control
        ui[[isp + offset]] <-
          checkboxInput(
            inputId = paste0(tag,sp),
            label   = paste0(sp,' (orig: ',fname,')'),
            value   = FALSE
          )
      }
    }
  }
  return(list(ui = ui, extSpectra = extSpectra))
}, return_on_error = list(ui = list(), extSpectra = list()))
