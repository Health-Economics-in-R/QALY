---
title: "QALY"
author: "Nathan Green"
date: "2018-12-11"
output:
  html_document: 
    keep_md: yes
---

[![Build Status](https://travis-ci.org/n8thangreen/QALY.svg?branch=master)](https://travis-ci.org/n8thangreen/QALY)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/n8thangreen/QALY?branch=master&svg=true)](https://ci.appveyor.com/project/n8thangreen/QALY)
[![Coverage status](https://codecov.io/gh/n8thangreen/QALY/branch/master/graph/badge.svg)](https://codecov.io/github/n8thangreen/QALY?branch=master)
[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)





> A QALY is a QALY is a QALY

An R package for quality-adjusted life-years (QALY) calculation and manipulation.

Currently contains functions to:

- Create QALY type object
- Discounting
- Health State Utility Values (HSUV)
- Inflation-adjusted costs

## To do
Request welcome; please use [Issues](https://github.com/n8thangreen/QALY/issues)

- Other HSUV methods
- Improved plotting


## Installing QALY

To install the development version from github:

```r
library(devtools)
install_github("n8thangreen/QALY")
```

Then, to load the package, use:

```r
library(QALY)
```


## Motivation
In cost-utility analyses arguably the most commonly used unit of health outcome is the QALY.
There are others, most notably the diability-adjusted life-year (DALY).
Calculation of QALYs consist of time and utility components.
Time may be some predefined time horizon or an individual's excess life time.
The utilities are health preference measures scaled between 0 and 1.
These can be derived from such things as patient recorded outcome measures (PROMs), using surveys such as EuroQol-5D (EQ-5D) or the Short Form-36D/Short Form-6D (SF-6D).
Co-morbidities, requiring some combining of utilities, may also be required.

QALY calculation in cost-effectiveness analyses is often done in an ad-hoc, project-by-project way.
The aim of this package is to standardise these calculations and provide an easy-to-use suite of functions for the most common operations involving QALYs.


## Basic example


```r
suppressMessages(library(QALY))
```

Combined two co-morbidity utilities using the product approach:


```r
HSUV_prod <- HSUV(method = "prod") 
tot_utility <- HSUV_prod(c(0.9, 0.8))
```

TODO use expected remaining life time...

Create a adjusted life-year type object which contains all the information needed to do subsequent operations:


```r
personHealthYears <-
  person_health_years(
    start_year = 2016,
    end_year = 2020,
    age = 33,
    time_horizon = NA,
    utility = tot_utility,
    discount_rate = 0.035)
```

Calculate QALYS:

```r
HRQoL_year <- total_QALYs(personHealthYears)
```


```r
print(HRQoL_year)
```

```
#>   [1] 0.5900000 0.5700483 0.5507713 0.5321462        NA        NA        NA
#>   [8]        NA        NA        NA        NA        NA        NA        NA
#>  [15]        NA        NA        NA        NA        NA        NA        NA
#>  [22]        NA        NA        NA        NA        NA        NA        NA
#>  [29]        NA        NA        NA        NA        NA        NA        NA
#>  [36]        NA        NA        NA        NA        NA        NA        NA
#>  [43]        NA        NA        NA        NA        NA        NA        NA
#>  [50]        NA        NA        NA        NA        NA        NA        NA
#>  [57]        NA        NA        NA        NA        NA        NA        NA
#>  [64]        NA        NA        NA        NA        NA        NA        NA
#>  [71]        NA        NA        NA        NA        NA        NA        NA
#>  [78]        NA        NA        NA        NA        NA        NA        NA
#>  [85]        NA        NA        NA        NA        NA        NA        NA
#>  [92]        NA        NA        NA        NA        NA        NA        NA
#>  [99]        NA        NA
#> attr(,"class")
#> [1] "HRQoL"   "numeric"
#> attr(,"person_health_years")
#> $start_year
#> [1] 2016
#> 
#> $end_year
#> [1] 2020
#> 
#> $delay
#> [1] 0
#> 
#> $age
#> [1] 33
#> 
#> $time_horizon
#> [1] 4
#> 
#> $utility
#> [1] 0.72 0.72 0.72 0.72
#> 
#> $QoL
#> [1] 0.87 0.87 0.87 0.87 0.87
#> 
#> $discount_rate
#> [1] 0.035
#> 
#> $death
#> [1] NA
#> 
#> $utility_method
#> [1] "add"
#> 
#> $period
#> [1] 1 1 1 1
#> 
#> attr(,"class")
#> [1] "person_health_years" "list"
```

```r
summary(HRQoL_year)
```

```
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.5322  0.5461  0.5604  0.5607  0.5750  0.5900      96
```


Print a graph of the HRQoL over time:
using ggplot2?


```r
plot(HRQoL_year)
```

![](README_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

See package [vignette](http://htmlpreview.github.io/?https://github.com/n8thangreen/QALY/blob/master/inst/doc/vignette_main.html) for more details and examples.

## License

GPL-3 © 
