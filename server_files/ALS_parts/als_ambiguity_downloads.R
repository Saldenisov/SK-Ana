
output$rotAmbVecDownload <- downloadHandler(
  filename = function() {
    paste0(input$projectTag, "_ALS_", input$nALS, "sp_ambiguity.zip")
  },
  content = function(fname) {
    req(alsOut <- resALS$results)
    req(ambOut <- resAmb$results)
    
    # Create temporary directory
    tmpdir <- tempdir()
    
    solutions = ambOut$solutions
    
    C <- alsOut$C
    xC <- alsOut$xC
    nC <- ncol(C)
    S <- alsOut$S
    xS <- alsOut$xS
    nS <- ncol(S)
    for (i in 1:ncol(C)) {
      nn <- sum(S[, i])
      S[, i] <- S[, i] / nn
      C[, i] <- C[, i] * nn
    }
    
    nkeep <- length(solutions) - 2
    allVec <- 1:nC
    rotVec <- solutions$rotVec
    sel <- allVec %in% rotVec
    nvec <- length(rotVec)
    eps <- solutions$eps
    
    # Estimate ranges of S
    S1 <- S[, sel]
    Smax <- matrix(eps, nrow = nrow(S1), ncol = ncol(S1))
    Smin <- matrix(1e30, nrow = nrow(S1), ncol = ncol(S1))
    for (i in 1:nkeep) {
      for (j in 1:nvec) {
        vec <- solutions[[i]]$S1[, j]
        nn <- sum(vec)
        for (k in 1:nrow(S1)) {
          val <- vec[k] / nn
          Smin[k, j] <- min(Smin[k, j], val, na.rm = TRUE)
          Smax[k, j] <- max(Smax[k, j], val, na.rm = TRUE)
        }
      }
    }
    S_out <- cbind(Inputs$wavl, Smin, Smax)
    colnames(S_out) <- c(
      'wavl', 
      paste0(colnames(S1),'_min'),
      paste0(colnames(S1),'_max')
    )
    
    # Save Spectra
    spectra_file <- file.path(tmpdir, paste0(
      input$projectTag,
      "_ambSpectra_",
      input$nALS, "sp.csv"
    ))
    write.csv(S_out, file = spectra_file, row.names = FALSE)
    
    # Estimate ranges of C
    C1 <- C[, sel]
    Cmax <- matrix(eps, nrow = nrow(C1), ncol = ncol(C1))
    Cmin <- matrix(1e30, nrow = nrow(C1), ncol = ncol(C1))
    for (i in 1:nkeep) {
      for (j in 1:nvec) {
        vec <- solutions[[i]]$S1[, j]
        nn <- sum(vec)
        for (k in 1:nrow(C1)) {
          val <- solutions[[i]]$C1[k, j] * nn # Normalize
          Cmin[k, j] <- min(Cmin[k, j], val, na.rm = TRUE)
          Cmax[k, j] <- max(Cmax[k, j], val, na.rm = TRUE)
        }
      }
    }
    
    C_out <- cbind(Inputs$delaySave, Cmin, Cmax)
    colnames(C_out) <- c(
      'delay', 
      paste0(colnames(C1),'_min'),
      paste0(colnames(C1),'_max')
    )
    
    # Save Kinetics
    kinets_file <- file.path(tmpdir, paste0(
      input$projectTag,
      "_ambKinets_",
      input$nALS, "sp.csv"
    ))
    write.csv(C_out, file = kinets_file, row.names = FALSE)
    
    # Zip the files
    files_to_zip <- c(spectra_file, kinets_file)
    zip(zipfile = fname, files = files_to_zip, flags = "-j")
    
    # Handle .zip extension issue
    if(file.exists(paste0(fname, ".zip"))) {
      file.rename(paste0(fname, ".zip"), fname)
    }
  },
  contentType = "application/zip"
)
