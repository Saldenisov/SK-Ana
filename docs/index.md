SK-Ana: Spectro-Kinetic matrices Analysis
================
P. Pernot
(2020-09-11)


# Introduction

The `SK-Ana` graphical interface is organized in sequential order of
project management:

  - [`Project`](project.html): define a project’s name and load the data

  - [`Data Selection`](data.html): define the data subset to be treated

  - [`SVD`](svd.html): perform Singular Values Decomposition analysis

  - [`ALS`](als.html): perform Alternated Least-Squares decomposition

  - [`Kinet`](kinet.html): constrain the analysis by a kinetic model

  - [`Downloads`](downloads.html): 
  download saved the results and/or a report

  - [`About`](about.html): information about the code

A good introduction to the methods can be found in the article

> C. Ruckebusch, M. Sliwa, P. Pernot, A. d. Juan and R. Tauler (2012)
> “Comprehensive data analysis of femtosecond transient absorption
> spectra: A review”. *J. Photochem. Photobiol. C* **13**:1–27.
> (<http://dx.doi.org/10.1016/j.jphotochemrev.2011.10.002>)

# Workflow

A typical workflow consists in the sequence:

    `Project` > (`Data Selection` > `SVD` > `ALS`) > `Downloads`

where the sequence between parentheses is iterated until *satisfecit*.
Technically, the `SVD` step could be avoided for an ALS analysis, but it
provides a lot of useful information and should not be overlooked.

To perform a SAS or DAS analysis, the sequence would be

    `Project` > `Data Selection` > `SVD` > `Kinet` > `Downloads`

here again, the `SVD` step helps to decide the number of species to
include in the chemical scheme.

