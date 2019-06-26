# enableBookmarking("server")

Sys.setlocale(category = "LC_NUMERIC", locale = "C")

options(
  shiny.maxRequestSize = 20 * 1024^2,
  width = 60,
  warn = 0
)

# options(shiny.json.digits=32)

# Libraries ####
libs <- c(
  "outliers", "nnls", "Iso", "httpuv",
  "changepoint", "shiny", "shinyBS", "DT", "Rsolnp",
  "fields", "NMFN", "tools", "shinycssloaders",
  "rgenoud", "mvtnorm", "deSolve", "msm", "xtable",
  "inlmisc","shinythemes"
)
for (lib in libs) {
  if (!require(lib, character.only = TRUE, quietly = TRUE)) {
    install.packages(
      lib,
      dependencies = TRUE,
      repos = "https://cran.univ-paris1.fr"
    )
    library(lib, quietly = TRUE)
  }
}


# Colors ####
col2tr <- function(col, alpha)
  rgb(unlist(t(col2rgb(col))), alpha = alpha, maxColorValue = 255)

imgColors <- inlmisc::GetColors(255, scheme = "davos")
cutColors <- inlmisc::GetColors(255, scheme = "jet")
resColors <- inlmisc::GetColors( 15, scheme = "BuRd")
lineColors <- rev(inlmisc::GetColors(8))
colo_tr  <- rev(inlmisc::GetColors(8, alpha = 0.25))
cyan_tr  <- colo_tr[5]
pink_tr  <- colo_tr[3]
mask_tr  <- colo_tr[8]
colo_tr2 <- rev(inlmisc::GetColors(8, alpha = 0.5))
cyan_tr2 <- colo_tr2[5]
pink_tr2 <- colo_tr2[3]

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
string2Expr <- function(string) {
  dat <- try(parse(text = string), silent = TRUE)
  if (!is(dat, "try-error")) {
    return(dat)
  } else {
    return(NULL)
  }
}
string2Num <- function(x)
  as.numeric(eval(parse(text = eval(string2Expr(x)))))
