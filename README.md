# zooimageui - A Shiny interface for zooimage

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/zooimageui/workflows/R-CMD-check/badge.svg)](https://github.com/SciViews/zooimageui/actions) [![CRAN Status](https://www.r-pkg.org/badges/version/zooimageui)](https://cran.r-project.org/package=zooimageui) [![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.html) [![Life cycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

<!-- badges: end -->

A Shiny interface to ZooImage that allows to use main features (create samples, training sets and classifiers, view content of all these objects and calculate results).

## Installation

You can install the latest development version of {zooimageui}. Make sure you have the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {zooimageui} package from Github (source from **main** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/zooimageui")
```

R should install all required dependencies automatically, and then it should compile and install {zooimageui}.

Latest devel version of {zooimageui} (source + Windows binaries for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/zooimageui/build/artifacts).

## Further explore {zooimageui}

You can get further help about this package this way: Make the {zooimageui} package available in your R session:

``` r
library("zooimageui")
```

Start the Shiny interface:

``` r
run_app()
```

Get help about this package:

``` r
library(help = "zooimageui")
help("zooimageui-package")
```

For further instructions, please, refer to these help pages at <https://www.sciviews.org/zooimageui/>.

## Code of Conduct

Please note that the {svSocket} project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
