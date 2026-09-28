args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Expected output .Rda path")
project_state <- list(
  version = 1L,
  metadata = list(project_tag = "synthetic_project"),
  config = list(),
  data = list(
    mat = matrix(1:12, nrow = 3),
    delay = c(10, 11, 12),
    delaySave = c(0, 1, 2),
    delayId = c(1, 1, 1),
    wavl = c(400, 500, 600, 700),
    dataDatStr = "wxd"
  )
)
save(project_state, file = args[1])
