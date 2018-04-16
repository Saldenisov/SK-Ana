
[![DOI](https://zenodo.org/badge/87315085.svg)](https://zenodo.org/badge/latestdoi/87315085)


**SK-Ana**: **S**pectro**K**inetic **Ana**lysis

`Shiny` interface for the analysis of spectrokinetic matrices:

* Singular Value Decomposition (SVD)

* Multi Curve Resolution by Alternated Least Squares (MCR-ALS)

<!--The code can be tested here: https://upsa.shinyapps.io/SK-Ana/-->


**Installation** 

* Download the latest release [here](https://github.com/ppernot/SK-Ana/releases/latest) 
  and decompress the archive in a dedicated directory. If you want the most recent code
  version (with more bugs risks) download it [here](https://github.com/ppernot/SK-Ana/archive/master.zip)

* You will also need

    + [R](https://cran.rstudio.com/) (Mandatory)
    
    + [RStudio](https://www.rstudio.com/products/rstudio/download/#download) (Optional)

**How to run SK-Ana**

* Using `R`: go to the installation directory and run the command `shiny::runApp()` in a `R` console
    
    
* Using `RStudio`

    1. go to the installation directory and double-click on `server.R` or `ui.R`

    2. in `RStudio` click on `Run App`

For other options and more detailed information see [here](https://shiny.rstudio.com/articles/running.html).

In both cases, a web interface should open in your favorite browser. At the first launch,
the code will install a set of packages, if they are not installed already: 
`outliers`, `nnls`, `Iso`, `viridis`, `httpuv`, `changepoint`, `shiny`, `shinyBS`, `DT`, 
`Rsolnp`, `fields`, `NMFN`, `tools`, `shinycssloaders`, `rgenoud`, `mvtnorm`, `deSolve`, 
`msm`, and `xtable`. 
Depending on your OS, you might have to install them manually.

**How to cite SK-Ana**

If you use SK-Ana in one of your publications, do not forget to cite it and include the version you used for reproducibility:

> Pernot, P. (2018) SK-Ana: Analysis of Spectro-Kinetic Data (Version X.X).    
> https://doi.org/10.5281/zenodo.1064370

**References**

* C. Ruckebusch, M. Sliwa, P. Pernot, A. d. Juan, R. Tauler, "Comprehensive data analysis of femtosecond transient absorption spectra: A review", J. Photochem. Photobiol., C, vol. 13, 2012, pp. 1–27. (http://dx.doi.org/10.1016/j.jphotochemrev.2011.10.002)

* J. Ma, P. Archirel, P. Pernot, U. Schmidhammer, S. L. Caër, M. Mostafavi, "Identification of Transient Radical Anions (LiClO4)n (n= 1-3) in THF Solutions: Experimental and Theoretical Investigation on Electron Localization in Oligomers", J. Phys. Chem. B, vol. 120, 2016, pp. 773–784. (http://dx.doi.org/10.1021/acs.jpcb.5b11315)

* J. Ma, P. Archirel, U. Schmidhammer, J. Teuler, P. Pernot, M. Mostafavi, "Reduction of Earth Alkaline Metal Salts in THF Solution Studied by Picosecond Pulse Radiolysis", J. Phys. Chem. A, vol. 117, 2013, pp. 14048–14055. (http://dx.doi.org/10.1021/jp410598y)
