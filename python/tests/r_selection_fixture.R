args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Expected output .Rda path")
selection_state <- list(
  version = 1L,
  metadata = list(project_tag = "synthetic_project"),
  data = list(
    projectTag = "synthetic_project",
    matDims = c(8L, 6L),
    dlScaleFacOrig = 10,
    keepDlRange = c(0.2, 0.7),
    keepWlRange = c(430, 650),
    nMasksBaseline = 1L,
    keepBaselineMask1 = c(0.2, 0.3),
    nMasksDl = 1L,
    keepDlMask1 = c(0.4, 0.5),
    nMasksWl = 1L,
    keepWlMask1 = c(490, 510)
  )
)
save(selection_state, file = args[1])
