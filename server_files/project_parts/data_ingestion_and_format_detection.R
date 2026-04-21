inputStyle = reactiveValues(
  # TBD: check reactivity to inputStyle when otherStyle is used.
  #      (does not work for datStr...)
  header = FALSE,
  sep    = ",",
  dec    = ".",
  datStr = "wxd"
)
accumulatedFiles = reactiveValues(
  files = NULL  # Will store accumulated file info
)
dataLoaded = reactive({
  Inputs$gotData & Inputs$validData
})
# Functions ####
inferDataStructureFromCoords <- safely(function(row_coords, col_coords, default = "wxd") {
  row_coords <- suppressWarnings(as.numeric(row_coords))
  col_coords <- suppressWarnings(as.numeric(col_coords))
  row_coords <- row_coords[is.finite(row_coords)]
  col_coords <- col_coords[is.finite(col_coords)]

  if (length(row_coords) < 2 || length(col_coords) < 2) {
    return(default)
  }

  looks_like_wavelength <- function(x) {
    rng <- range(x)
    spread <- diff(rng)
    step <- median(abs(diff(x)), na.rm = TRUE)
    rng[1] >= 150 && rng[2] <= 2000 && spread >= 50 && is.finite(step) && step > 0
  }

  looks_like_delay <- function(x) {
    rng <- range(x)
    step <- median(abs(diff(x)), na.rm = TRUE)
    has_zero <- any(abs(x) < 1e-8)

    small_scale <- rng[1] >= -1e-8 && rng[2] <= 100
    zero_based_scale <- has_zero && rng[2] < 250
    relative_scale <- max(x, na.rm = TRUE) < max(col_coords, na.rm = TRUE) / 2

    (small_scale || zero_based_scale || relative_scale) && is.finite(step) && step > 0
  }

  row_is_delay <- looks_like_delay(row_coords)
  row_is_wavelength <- looks_like_wavelength(row_coords)
  col_is_delay <- looks_like_delay(col_coords)
  col_is_wavelength <- looks_like_wavelength(col_coords)

  if (row_is_delay && col_is_wavelength) {
    return("dxw")
  }
  if (row_is_wavelength && col_is_delay) {
    return("wxd")
  }

  default
}, return_on_error = "wxd")

detectFileFormat <- safely(function(dataFile) {
  # Try to detect file format automatically
  # Returns a list with header, sep, dec, datStr
  
  # Read first few lines as text
  lines <- readLines(dataFile, n = 10, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0]
  if(length(lines) < 2) return(NULL)
  
  # Detect field and decimal separators jointly.
  score_pair <- function(potential_sep, potential_dec) {
    if (potential_sep == potential_dec) return(-Inf)
    n_lines <- min(5, length(lines))
    subset_lines <- lines[1:n_lines]

    split_line <- function(line) {
      if (potential_sep == " ") {
        parts <- strsplit(trimws(line), "\\s+")[[1]]
      } else {
        parts <- strsplit(line, potential_sep, fixed = TRUE)[[1]]
      }
      parts[nchar(trimws(parts)) > 0]
    }

    split_vals <- lapply(subset_lines, split_line)
    token_counts <- vapply(split_vals, length, numeric(1))
    if (any(token_counts < 2)) return(-Inf)

    tokens <- unlist(split_vals, use.names = FALSE)
    tokens <- trimws(tokens)
    if (potential_dec == ",") {
      tokens <- gsub(",", ".", tokens, fixed = TRUE)
    }
    is_num <- !is.na(suppressWarnings(as.numeric(tokens)))

    consistency <- mean(token_counts) / (1 + stats::sd(token_counts))
    numeric_ratio <- mean(is_num)
    consistency * numeric_ratio
  }

  separators <- c("\t", ";", ",", " ")
  decimals <- c(".", ",")
  best_score <- -Inf
  sep <- ","
  dec <- "."
  for (potential_sep in separators) {
    for (potential_dec in decimals) {
      score <- score_pair(potential_sep, potential_dec)
      if (is.finite(score) && score > best_score) {
        best_score <- score
        sep <- potential_sep
        dec <- potential_dec
      }
    }
  }
  
  # Detect if first line is header
  split_first <- if (sep == " ") {
    strsplit(trimws(lines[1]), "\\s+")[[1]]
  } else {
    strsplit(lines[1], sep, fixed = TRUE)[[1]]
  }
  first_vals <- unlist(split_first)
  first_vals <- first_vals[nchar(trimws(first_vals)) > 0]
  
  if(length(first_vals) == 0) {
    header <- FALSE
  } else {
    parsed_vals <- first_vals
    if (dec == ",") {
      parsed_vals <- gsub(",", ".", parsed_vals, fixed = TRUE)
    }
    is_num <- !is.na(suppressWarnings(as.numeric(parsed_vals)))

    # Common file layout: first label then numeric columns.
    if (!is_num[1] && length(is_num) > 1 && mean(is_num[-1]) >= 0.75) {
      header <- TRUE
    } else {
      header <- mean(is_num) < 0.5
    }
  }
  
  # Infer axis orientation from the first row/column when possible.
  parse_numeric_token <- function(token) {
    token <- trimws(token)
    if (dec == ",") {
      token <- gsub(",", ".", token, fixed = TRUE)
    }
    suppressWarnings(as.numeric(token))
  }

  row_coords <- vapply(first_vals[-1], parse_numeric_token, numeric(1))
  col_vals <- vapply(lines[-1], function(line) {
    split_line <- if (sep == " ") {
      strsplit(trimws(line), "\\s+")[[1]]
    } else {
      strsplit(line, sep, fixed = TRUE)[[1]]
    }
    split_line <- split_line[nchar(trimws(split_line)) > 0]
    if (length(split_line) == 0) {
      return(NA_real_)
    }
    parse_numeric_token(split_line[1])
  }, numeric(1))

  datStr <- inferDataStructureFromCoords(row_coords, col_vals, default = "wxd")
  
  return(list(
    header = header,
    sep = sep,
    dec = dec,
    datStr = datStr
  ))
})

downsizeMatrix <- safely(function(delay, wavl, mat, fwD = 1, fwW = fwD) {
  # Downsize matrix by factors fwD (delay) and fwW (wavl)
  
  # Pad matrix with Nas
  newNrow <- ceiling(nrow(mat) / fwD) * fwD
  newNcol <- ceiling(ncol(mat) / fwW) * fwW
  lmat <- matrix(NA, nrow = newNrow, ncol = newNcol)
  lmat[1:nrow(mat), 1:ncol(mat)] <- mat
  
  # Block average
  nRowBloc <- newNrow / fwD
  nColBloc <- newNcol / fwW
  
  amat <- matrix(NA, nrow = nRowBloc, ncol = nColBloc)
  for (i in 1:nRowBloc)
    for (j in 1:nColBloc)
      amat[i, j] <- mean(
        lmat[
          ((i - 1) * fwD + 1):(i * fwD),
          ((j - 1) * fwW + 1):(j * fwW)
          ],
        na.rm = TRUE
      )
  
  ldelay <- rep(NA, newNrow)
  ldelay[1:length(delay)] <- delay
  adelay <- c()
  for (i in 1:nRowBloc)
    adelay[i] <- mean(
      ldelay[((i - 1) * fwD + 1):(i * fwD)],
      na.rm = TRUE
    )
  
  lwavl <- rep(NA, newNcol)
  lwavl[1:length(wavl)] <- wavl
  awavl <- c()
  for (i in 1:nColBloc)
    awavl[i] <- mean(
      lwavl[((i - 1) * fwW + 1):(i * fwW)],
      na.rm = TRUE
    )
  
  return(
    list(
      mat = amat,
      delay = adelay,
      wavl = awavl
    )
  )
}, return_on_error = NULL)

getOneMatrix  <- safely(function(dataFile) {
  # Read first row to check if it's a header or data
  first_row_raw = try(
    as.vector(
      read.table(
        dataFile,
        nrows = 1,
        header = FALSE,
        sep = inputStyle$sep,
        stringsAsFactors = FALSE,
        dec = inputStyle$dec,
        fileEncoding = "ISO-8859-1",
        quote = ""
      )
    ),
    silent = TRUE
  )
  
  # Determine if first row is a header (has non-numeric first element)
  has_header = FALSE
  if(!is.null(first_row_raw) && length(first_row_raw) > 0) {
    first_elem = suppressWarnings(as.numeric(first_row_raw[1]))
    has_header = is.na(first_elem)
  }
  
  raw_table = try(
    read.table(
      dataFile, 
      header = FALSE, 
      skip = if(has_header) 1 else 0,
      dec = inputStyle$dec, 
      sep = inputStyle$sep,
      colClasses= 'numeric',
      stringsAsFactors = FALSE,
      quote = ""
    ),
    silent = TRUE
  )
  if(class(raw_table) == 'try-error') 
    return(NULL) 

  raw_table <- as.matrix(raw_table)
  if (nrow(raw_table) < 2 || ncol(raw_table) < 2) {
    return(NULL)
  }

  row_coords <- suppressWarnings(as.numeric(raw_table[1, -1]))
  col_coords <- suppressWarnings(as.numeric(raw_table[-1, 1]))
  mat <- raw_table[-1, -1, drop = FALSE]

  if (length(row_coords) == 0 || length(col_coords) == 0 ||
      any(!is.finite(row_coords)) || any(!is.finite(col_coords))) {
    return(NULL) 
  }

  effectiveDatStr <- inferDataStructureFromCoords(
    row_coords = row_coords,
    col_coords = col_coords,
    default = inputStyle$datStr
  )

  if (effectiveDatStr == 'dxw') {
    delay <- row_coords
    wavl  <- col_coords
    mat   <- t(mat)
  } else {
    delay <- col_coords
    wavl  <- row_coords
  }

  cat(sprintf("After reading: delay=%d, wavl=%d, mat=%dx%d\n",
              length(delay), length(wavl), nrow(mat), ncol(mat)))
  
  # Note: Duplicate removal was here but removed as it was causing data loss
  # If needed, check for actual duplicates in your data file
  
  mat[!is.finite(mat)] = 0

  # Ensure increasing coordinates
  iord = order(wavl, decreasing = FALSE)
  wavl = wavl[iord]
  mat  = mat[, iord]
  iord = order(delay, decreasing = FALSE)
  delay = delay[iord]
  mat = mat[iord, ]
  
  cat(sprintf("After ordering: delay=%d, wavl=%d, mat=%dx%d\n", 
              length(delay), length(wavl), nrow(mat), ncol(mat)))
  
  # Downsize
  if(input$compFacD >= 2 | input$compFacW >= 2) {
    dsm = downsizeMatrix(delay,wavl,mat,
                         fwD=input$compFacD, 
                         fwW=input$compFacW)
    mat   = dsm$mat
    delay = dsm$delay
    wavl  = dsm$wavl
    rm(dsm)
  }
  
  return(list(mat=mat, wavl=wavl, delay=delay, 
              delaySave=delay, delayId= rep(1,length(delay))))
  
}, return_on_error = NULL)

getRawData    <- safely(function(fileNames) {
  if (is.null(fileNames) || nrow(fileNames) == 0) {
    replaceRawData(list())
    setInputValues(list(gotData = FALSE, validData = FALSE))
    return(NULL)
  }

  initInputs()  # (Re)initialize data tables
  rawData <- vector("list", nrow(fileNames))
  validData <- TRUE
  
  # Init progress bar
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  updateProgress <- function(value = NULL, detail = NULL) {
    progress$set(value = value, detail = detail)
  }
  progress$set(message = "Reading data file(s) ", value = 0)
  
  # Load data files
  for(i in 1:nrow(fileNames)) {
    fName = fileNames[i,'name']
    updateProgress(value  = i / nrow(fileNames),detail = fName)
    O = getOneMatrix(fileNames[i,'datapath'])
    if (!is.null(O)) { 
      O$name = fName
    } else {
      validData <- FALSE
      showModal(modalDialog(
        title = ">>>> Data problem <<<< ",
        paste0("The chosen data type does not ",
               "correspond to the opened data file(s)!"),
        easyClose = TRUE, 
        footer = modalButton("Close"),
        size = 's'
      ))
    }
    rawData[[i]] <- O
  }

  if (!validData || any(vapply(rawData, is.null, logical(1)))) {
    replaceRawData(list())
    setInputValues(list(
      gotData = FALSE,
      validData = FALSE
    ))
    return(NULL)
  }

  replaceRawData(rawData)
  setInputValues(list(
    gotData = TRUE,
    validData = TRUE
  ))
}, return_on_error = NULL)

# Predefined input styles ####
observeEvent(
  input$style, 
  isolate({
    switch( input$style,
            autoStyle = {
              # Auto-detect format when file is loaded
              if(!is.null(input$dataFile)) {
                detected <- detectFileFormat(input$dataFile$datapath[1])
                if(!is.null(detected)) {
                  inputStyle$header = detected$header
                  inputStyle$sep    = detected$sep
                  inputStyle$dec    = detected$dec
                  inputStyle$datStr = detected$datStr
                } else {
                  # Fallback to CSV defaults
                  inputStyle$header = FALSE
                  inputStyle$sep    = ","
                  inputStyle$dec    = "."
                  inputStyle$datStr = "wxd"
                }
              } else {
                # No file loaded yet, use CSV defaults
                inputStyle$header = FALSE
                inputStyle$sep    = ","
                inputStyle$dec    = "."
                inputStyle$datStr = "wxd"
              }
            },
            csvStyle = {
              inputStyle$header = FALSE
              inputStyle$sep    = ","
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            # otherStyle = {
            #   inputStyle$header = FALSE
            #   inputStyle$sep    = ","
            #   inputStyle$dec    = "."
            #   inputStyle$datStr = "wxd"
            # },
            # munichStyle = {
            #   inputStyle$header = FALSE
            #   inputStyle$sep= "\t"
            #   inputStyle$dec= "."
            #   inputStyle$datStr= "wxd"
            # },
            elyseStyle = {
              inputStyle$header = FALSE
              inputStyle$sep    = "\t"
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            heleneStyle={
              inputStyle$header = FALSE
              inputStyle$sep    = ";"
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            streakStyle = {
              inputStyle$header = TRUE
              inputStyle$sep    = ","
              inputStyle$dec    = "."
              inputStyle$datStr = "wxd"
            },
            otherStyle = {
              inputStyle$header = input$header
              inputStyle$sep    = input$sep
              inputStyle$dec    = input$dec
              inputStyle$datStr = input$datStr
            }       
    )
    # updateCheckboxInput(session, 
    #                     inputId = "header", 
    #                     value   = header)
    # updateRadioButtons(session,
    #                    inputId  = "sep",
    #                    selected = sep)
    # updateRadioButtons(session,
    #                    inputId  = "dec",
    #                    selected = dec)
    # updateRadioButtons(session,
    #                    inputId  = "datStr",
    #                    selected = datStr)
  })
)

# Clear files button ####
observeEvent(
  input$clearFiles, {
    accumulatedFiles$files <- NULL
    # Reset file input (note: this doesn't visually clear in all browsers)
    # but the accumulated files will be cleared
  }
)

# New project ####
observeEvent(
  input$dataFile,{
    # Determine which files to process
    filesToProcess <- input$dataFile
    
    if(!is.null(input$appendFiles) && input$appendFiles) {
      # Append mode: combine with previously loaded files
      if(!is.null(accumulatedFiles$files)) {
        # Merge new files with accumulated files
        # Avoid duplicates based on name
        existingNames <- accumulatedFiles$files$name
        newFiles <- filesToProcess[!filesToProcess$name %in% existingNames, ]
        
        if(nrow(newFiles) > 0) {
          accumulatedFiles$files <- rbind(accumulatedFiles$files, newFiles)
        }
        filesToProcess <- accumulatedFiles$files
      } else {
        # First time loading files
        accumulatedFiles$files <- filesToProcess
      }
    } else {
      # Overwrite mode: replace accumulated files
      accumulatedFiles$files <- filesToProcess
    }
    
    # If auto mode is selected, detect format from first file
    if(!is.null(input$style) && input$style == "autoStyle") {
      detected <- detectFileFormat(filesToProcess$datapath[1])
      if(!is.null(detected)) {
        inputStyle$header = detected$header
        inputStyle$sep    = detected$sep
        inputStyle$dec    = detected$dec
        inputStyle$datStr = detected$datStr
      }
    }
    
    getRawData(filesToProcess)
    # # New log files for optimizer
    # glOptOut <<- tempfile(tmpdir = '/tmp',fileext = '_glob.stdout')
    # file.create(glOptOut, showWarnings = FALSE)
    # locOptOut <<- tempfile(tmpdir = '/tmp',fileext = '_loc.stdout')
    # file.create(locOptOut, showWarnings = FALSE)
  }
)
output$loadMsg <- renderUI({
  if(dataLoaded()) {
      ll = list(
      h4('Data loaded !')
    )
  } else {
    ll = list(
      h4('No data loaded'),
      h5('Please select data file(s)...')
    )
  }
  return(ll)
})
output$rawData = DT::renderDataTable({
  if( !dataLoaded() )
    return(NULL)
  
  ndelay  = nwavl = name = size = c()
  for (i in 1:length(RawData)) {
    name[i]   = RawData[[i]]$name
    ndelay[i] = length(RawData[[i]]$delay)
    nwavl[i]  = length(RawData[[i]]$wavl)
    size[i]   = format(
      object.size(RawData[[i]]$mat),
      units="Mb")
  }
  DT::datatable(
    cbind(id=1:length(RawData),name,ndelay,nwavl,size),
    options = list(
      paging    = FALSE,
      ordering  = FALSE,
      searching = FALSE,
      dom       = 't'   ),
    selection=list(
      target='row',
      selected=1:length(RawData)
    ), 
    escape    = FALSE
  )
})
output$sel     = renderPrint({
  if( !dataLoaded() )
    return(NULL)
  cat(
    paste0(
      'Selected file(s) :',
      ifelse(
        length(input$rawData_rows_selected) != 0 ,
        paste0(input$rawData_rows_selected,collapse=','),
        ' none'
      )
    )
  )
  Inputs$process <- FALSE
  Inputs$finish  <- FALSE
})
output$showsel = reactive({
  Inputs$gotData & Inputs$validData 
})
outputOptions(output, "showsel", suspendWhenHidden = FALSE)
