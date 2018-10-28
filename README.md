
[![Rdoc](http://www.rdocumentation.org/badges/version/roxygen2)](http://www.rdocumentation.org/packages/roxygen2) [![Build Status](https://travis-ci.org/rmsharp/rmsutilityr.svg?branch=master)](https://travis-ci.org/rmsharp/rmsutilityr) [![codecov](https://codecov.io/gh/rmsharp/rmsutilityr/branch/master/graph/badge.svg)](https://codecov.io/gh/rmsharp/rmsutilityr)

<!-- README.md is generated from README.Rmd. Please edit that file -->
rmsutilityr
===========

Installation
------------

### Installation from Source

A very manual approach is to use the following code, which assumes you have all of the dependencies already installed:

``` r
install_path <- "c:/R Library"
source_path <- "d:Labkey data"
source <- "rmsutilityr.1.0.63.tar.gz"
install.packages(paste0(source_path, "/", source), type = "source", repos = NULL,
                     lib = install_path)
```

This will fail if you do not have the dependencies already installed, but the error message will provide the name(s) of the packages needed. However, as soon as the source packages is updated that code no longer work because the filename is wrong.

### Github.com Installation

It is much easier to install directly from [github.com/rmsharp/rmsutilityr](https://github.com/rmsharp/rmsutilityr) as all of the dependencies are automatically installed.

You can install **rmsutilityr** from github with:

``` r
install.packages("devtools")
devtools::install_github("rmsharp/rmsutilityr")
```

All missing dependencies should be automatically installed.
