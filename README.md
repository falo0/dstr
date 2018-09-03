
<!-- README.md is generated from README.Rmd. Please edit that file -->
dstr - Dependency Structure Analysis
====================================

Package Description
-------------------

dstr's goal is to reveal the dependency structure of one or several given package/s. E.g. it helps to find out which packages are eventually required (all dependencies of dependencies of...), how they are connected, and how easy or hard it would be to remove certain packages from the dependency structure completely.

Setup
-----

``` r
library(devtools)
install_github("falo0/dstr")
library(dstr)
```

Usage
-----

Use Case: The dependency structure of a package in the current working directory has to be analyzed

``` r
dstr()
plotdstr()
dstr_data(outtype = "all")
```

Use Case: The dependency structure of a package on github has to be analyzed. Accepting multiple gihub link formats

``` r
dstr("Stan125/GREA")
plotdstr("https://github.com/Stan125/GREA")
View(dstr_data("https://github.com/Stan125/GREA/blob/master/DESCRIPTION",
               outtype = "tree"))
```

Use Case: The dependency structure of one or several packages on CRAN have to be analyzed, including base packages

``` r
dstr(pkg = "ggplot2", includebasepkgs = T)
plotdstr(pkg = "ggplot2", includebasepkgs = T)
View(dstr_data(pkg = "ggplot2", outtype = "tree", includebasepkgs = T))
```

Use Case: What would happen to the dependency structure, if a certain package on github would also depend on some more CRAN packages?

``` r
dstr("Stan125/GREA", pkg = c("astro", "celestial"))
plotdstr("Stan125/GREA", pkg = c("astro", "celestial"))
```

Use Caes: The user wants certain data about the dependency structure to use for his own analysis

``` r
data <- dstr_data("Stan125/GREA",
                  outtype = c("root", "lvl1", "all",
                              "tree", "list", "unique", "unique2",
                              "edgelist", "edgelist2", "network"))
# Key data about the dependency structure
data$root
data$lvl1
data$all
data$tree
# Dependencies per first level dependency
data$list
data$unique
data$unique2
# For plotting and network analysis
data$edgelist
data$edgelist2
data$network
```
