# install_inlmisc.R
options(repos = c(CRAN = 'https://cloud.r-project.org'))
message('R version: ', R.version.string)
message('LibPaths: ', paste(.libPaths(), collapse='; '))
message('Installing inlmisc from source...')
install.packages('inlmisc', dependencies = TRUE, type = 'source')
message('Installed packages containing inlmisc:')
print(installed.packages()["inlmisc", c("Package", "Version", "LibPath")])
message('Trying to load inlmisc...')
library(inlmisc)
message('Loaded inlmisc: ', as.character(packageVersion('inlmisc')))

