[![Build
Status](https://travis-ci.org/n8thangreen/QALY.svg?branch=master)](https://travis-ci.org/n8thangreen/QALY)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/n8thangreen/QALY?branch=master&svg=true)](https://ci.appveyor.com/project/n8thangreen/QALY)
[![Coverage
status](https://codecov.io/gh/n8thangreen/QALY/branch/master/graph/badge.svg)](https://codecov.io/github/n8thangreen/QALY?branch=master)

QALY
====

> A QALY is a QALY is a QALY

An R package for quality-adjusted life-years (QALY) calculation and
manipulation.

Currently contains functions to:

-   Create QALY type object
-   Discounting
-   Health State Utility Values (HSUV)
-   Inflation-adjusted costs

To do
-----

Request welcome; please use
[Issues](https://github.com/n8thangreen/QALY/issues)

-   Other HSUV methods
-   Improved plotting

Installing QALY
---------------

To install the development version from github:

    library(devtools)
    install_github("n8thangreen/QALY")

Then, to load the package, use:

    library(QALY)

Motivation
----------

In cost-utility analyses the most commonly used unit of health is the
QALY. The are others, most notably the diability-adjusted life-year
(DALY), but these are not so ubiquitous. Calculation of QALY consists of
time and utility components. Time may be some predefined time horizon or
an individuals excess life time. The utilities are health preference
measures scaled between 0 and 1. These can be derived from such things
as patient recorded outcome measures (PROMs) using surveys such as
EuroQol-5D (EQ-5D) or the Short Form-36D/Short Form-6D (SF-6D).
Co-morbidities, requiring some combining of utilities, may also be
required.

QALY calculation in cost-effectiveness analyses is often done in an
ad-hoc, project-by-project way. The aim of this package is to
standardise the calculations and provide an easy-to-use suite of
functions for the most common operations involving QALYs.

Basic example
-------------

    suppressMessages(library(QALY))

Combined two co-morbidity utilities using the product approach:

    HSUV_prod <- HSUV(method = "prod") 
    tot_utility <- HSUV_prod(c(0.9, 0.8))

TODO use expected remaining life time…

Create a adjusted life-year type object which contains all the
information needed to do subsequent operations:

    AdjLifeYears <- adjusted_life_years(
                        start_year = 2016,
                        end_year = 2020,
                        age = 33,
                        time_horizon = NA,
                        utility = tot_utility,
                        discount_rate = 0.035)

Calculate QALYS:

    QALYs <- total_QALYs(AdjLifeYears)

    print(QALYs)
    summary(QALYs)

Print a graph of the QALYs over time: using ggplot2?

    plot(QALYs)

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.png)

See package
[vignette](http://htmlpreview.github.io/?https://github.com/n8thangreen/QALY/blob/master/inst/doc/vignette_main.html)
for more details and examples.

License
-------

GPL-3 ©
