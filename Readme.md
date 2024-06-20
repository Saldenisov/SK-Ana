
[![DOI](https://zenodo.org/badge/87315085.svg)](https://zenodo.org/badge/latestdoi/87315085)


# **SK-Ana**: **S**pectro**K**inetic **Ana**lysis

`Shiny` interface for the analysis of spectrokinetic matrices:

* Singular Value Decomposition (SVD)

* Multi Curve Resolution by Alternated Least Squares (MCR-ALS)

* Hybrid hard-soft modeling (DAS, homogeneous chemistry...)


<!--The code can be tested here: https://upsa.shinyapps.io/SK-Ana/-->

## User's manual

__New__: online [here](https://ppernot.github.io/SK-Ana/index.html)

<!--
## Local install 

* Download the latest release [here](https://github.com/ppernot/SK-Ana/releases/latest) 
  and decompress the archive in a dedicated directory. If you want the most recent code
  version (with more bugs risks) download it [here](https://github.com/ppernot/SK-Ana/archive/master.zip)

* You will also need

    + [R](https://cran.rstudio.com/) (Mandatory)
    
    + [RStudio](https://www.rstudio.com/products/rstudio/download/#download) (Optional)

### How to run SK-Ana (for non-docker-based installs)

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
-->

## Docker container

For cross-plateform compatibility issues, the preferred installation
method is through a docker container.

The [skana](https://hub.docker.com/repository/docker/ppernot1/skana)
[Docker](https://www.docker.com/) container has all elements preinstalled.

To run the container:

0. Install [Docker](https://www.docker.com/products/docker-desktop)

1. Type the following command in a terminal
```
docker run -d -p 3840:3840 --name skana ppernot1/skana
```      

2. Access SK-Ana at http://localhost:3840 in your favorite browser

3. When finished
```
docker kill skana
```

4. For further sessions
```
docker restart skana
```

5. To cleanup
```
docker remove -v skana
```

6. To get the latest version
```
docker pull ppernot1/skana
```      


## How to cite SK-Ana

If you use SK-Ana in one of your publications, do not forget to cite it and include the version you used for reproducibility:

> Pernot, P. (2018) SK-Ana: Analysis of Spectro-Kinetic Data (Version X.X).    
> https://doi.org/10.5281/zenodo.1064370

## References

### Methods

* C. Ruckebusch, M. Sliwa, P. Pernot, A. d. Juan, R. Tauler (2012) 
"Comprehensive data analysis of femtosecond transient absorption spectra: 
A review", _J. Photochem. Photobiol. C_ __13__:1–27. [DOI](http://dx.doi.org/10.1016/j.jphotochemrev.2011.10.002)

### Tutorial (in french)

* _Apport de la simulation à la détermination de mécanismes réactionnels en 
chimie sous rayonnement_. Action Nationale de Formation CNRS "Chimie sous
Rayonnement et Radiochimie" (Oléron, 2017/09) 
[PDF](https://universite-paris-saclay.hal.science/hal-04618186)


### Works using SK-Ana (please let me know of any missing reference)

* D.H. Cruz Neto, J. Soto, N. Maity, Ch. Lefumeux, T. Nguyen, P. Pernot, K. Steenkeste, D. Peláez, M.-H. Ha-Thi and Th. Pino (2023) _J. Phys. Chem. Letters_ __14__:4789–4795.
(https://doi.org/10.1021/acs.jpclett.3c00594)

* M. Knezevic, V.-D. Quach, I. Lampre, M. Erard, P. Pernot, D. Berardan, Ch. Colbeau-Justin and M.N. Ghazzal (2023) _J. Mater. Chem. A_ __11__:6226-6236. (https://doi.org/10.1039/D2TA09920A)

* K. Iwamatsu, R. Gakhar, Ph. Halstenberg, B. Layne, S.M. Pimblott and J.F. Wishart (2022) _Phys. Chem. Chem. Phys._ __24__:25088-25098. (https://doi.org/10.1039/D2CP01194H)

* C. Shang and P.E. Reiller (2021) _Dalton Trans._  __50__:17165-17180. (https://doi.org/10.1039/D1DT03204F)

* M. Puget, V. Shcherbakov, S. Denisov, P. Moreau, J.-P. Dognon, M. Mostafavi and S. Le Caër (2021) _Chem. Eur. J._  __27__:8185. (https://doi.org/10.1002/chem.202100562) 

* R. Kaczmarek, S. Ward, D. Debnath, T. Jacobs, A. D. Stark, D. Korczyński, A. Kumar, M. D. Sevilla, S. A. Denisov, V. Shcherbakov, P. Pernot, M. Mostafavi, R. Dembinski and A. Adhikary (2020) _Chemistry – A European Journal_ __26__:9495–9505. (http://dx.doi.org/10.1002/chem.202000247)

* S. Al Gharib, J.-L. Marignier, A.K. El Omar, A. Naja, S. Le Caer, M. Mostafavi and J. Belloni (2019) _J. Phys. Chem. C_  __123__:22624-22633. (http://dx.doi.org/10.1021/acs.jpcc.9b06090)

* F. Wang, P. Pernot, J.-L. Marignier, P. Archirel and M. Mostafavi (2019) _J. Phys. Chem. B_ __123_, 2019, pp. __:6599-6608. (http://dx.doi.org/10.1021/acs.jpcb.9b05560)

* F. Wang, G.P. Horne, P. Pernot, P. Archirel and M. Mostafavi (2018) _J. Phys. Chem. B_  __122_, 2018, pp. __:7134-7142. (http://dx.doi.org/10.1021/acs.jpcb.8b03715)

* J. Ma, A. Kumar, Y. Muroya, S. Yamashita, T. Sakurai, S.A. Denisov, M.D. Sevilla, A. Adhikary, S. Seki and M. Mostafavi (2019) _Nat. Commun._ __10__:102. (https://doi.org/10.1038/s41467-018-08005-z)

* J. Ma, S. A. Denisov, J. Marignier, P. Pernot, A. Adhikary, S. Seki and M. Mostafavi (2018) _J. Phys. Chem. Lett._  __9__:5105-5109. (http://dx.doi.org/10.1021/acs.jpclett.8b02170)

* J. Ma, J. Marignier, P. Pernot, C. Houée-Levin, A. Kumar, M. D. Sevilla, A. Adhikary and M. Mostafavi (2018) _Phys. Chem. Chem. Phys._  __20__:14927-14937. (http://dx.doi.org/10.1039/C8CP00352A)

* J. Ma, P. Archirel, P. Pernot, U. Schmidhammer, S. L. Caër and M. Mostafavi (2016) _J. Phys. Chem. B_ __120__:773–784. (http://dx.doi.org/10.1021/acs.jpcb.5b11315)

* J. Ma, P. Archirel, U. Schmidhammer, J. Teuler, P. Pernot and M. Mostafavi (2013) _J. Phys. Chem. A_ __117__:14048–14055. (http://dx.doi.org/10.1021/jp410598y)










