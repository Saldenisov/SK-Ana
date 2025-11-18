Version = "3.5.1"
DateVersion = "2025-11-18"

# Load error handler
source("error_handler.R")

# enableBookmarking("server")

Sys.setlocale(category = "LC_NUMERIC", locale = "C")

options(
  shiny.maxRequestSize = 20 * 1024^2,
  width = 60,
  warn = 0
)

# options(shiny.json.digits=32)

# Controle reactivity of sliders
debounceDelay = 750 # ms

# Libraries ####
libs <- c(
  "outliers", "nnls", "Iso", "httpuv",
  "changepoint", "shiny", "shinyBS", "DT", "Rsolnp",
  "fields", "NMFN", "tools", "shinycssloaders",
  "rgenoud", "mvtnorm", "deSolve", "msm", "xtable",
  "shinythemes","magrittr","callr","processx"
  # ,"readr", "shinyCopy2clipboard"
)
#remotes::install_github("deepanshu88/shinyCopy2clipboard")
for (lib in libs) {
  # # Avoid installing packages because of renv packages management
  # if (!require(lib, character.only = TRUE, quietly = TRUE)) {
  #   install.packages(
  #     lib,
  #     dependencies = TRUE,
  #     repos = "https://cran.univ-paris1.fr"
  #   )
  library(lib, character.only = TRUE, quietly = TRUE)
  # }
}

# Colors ####
col2tr <- safely(function(col, alpha)
  rgb(unlist(t(col2rgb(col))), alpha = alpha, maxColorValue = 255), return_on_error = NULL)

# Replacement for inlmisc::GetColors using common palettes
GetColors <- safely(function(n, scheme = NULL, alpha = 1) {
  pal_fun <- switch(
    if (is.null(scheme)) "viridis" else scheme,
    # Approximate 'jet' using a classic blue-cyan-yellow-red ramp
    jet = grDevices::colorRampPalette(c("#00007F", "#00FFFF", "#FFFF00", "#FF0000")),
    # Approximate 'BuRd' using a diverging red-blue palette (reversed to match "BuRd")
    BuRd = grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu"))),
    # Use a modern sequential palette for 'davos'
    davos = function(k) viridisLite::mako(k),
    # Default to viridis
    viridis = function(k) viridisLite::viridis(k)
  )
  cols <- pal_fun(as.integer(n))
  # Apply alpha if specified (0..1)
  if (!is.null(alpha) && !is.na(alpha) && is.finite(alpha)) {
    cols <- grDevices::adjustcolor(cols, alpha.f = max(0, min(1, alpha)))
  }
  cols
}, return_on_error = NULL)

imgColors <- GetColors(255, scheme = "davos")
cutColors <- GetColors(255, scheme = "jet")
resColors <- GetColors(15,  scheme = "BuRd")
lineColors <- rev(GetColors(8))
colo_tr  <- rev(GetColors(8,  alpha = 0.25))
cyan_tr  <- colo_tr[5]
pink_tr  <- colo_tr[3]
mask_tr  <- colo_tr[8]
colo_tr2 <- rev(GetColors(8, alpha = 0.5))
cyan_tr2 <- colo_tr2[5]
pink_tr2 <- colo_tr2[3]
mask_tr2 <- colo_tr2[8]

# Global graphical params ####
cex <- 1.25
mar <- c(4, 4, 2, 1)
mgp <- c(2, .75, 0)
pty <- "m"
tcl <- -0.5
plotHeight = 500 # px

# Proportions of Side/Main Panels ####
sideWidth <- 4
mainWidth <- 12 - sideWidth

# Misc. Functions ####
string2Expr <- safely(function(string) {
  dat <- try(parse(text = string), silent = TRUE)
  if (!is(dat, "try-error")) {
    return(dat)
  } else {
    return(NULL)
  }
}, return_on_error = NULL)

string2Num <- safely(function(x)
  as.numeric(eval(parse(text = eval(string2Expr(x))))), return_on_error = NULL)
