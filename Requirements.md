Requirements
================
UWI-couRse
26/02/2021

## R and RStudio

Let started with [R](https://www.r-project.org/), a powerful open-source
software/language environment for statistics analysis ([R Development
Core Team](https://www.r-project.org/contributors.html)). R will be the
environment for all statistical and graphical techniques present in this
course.

[RStudio](https://rstudio.com/), which is an open-source integrated
development environment (IDE) for R, great facilitate code editing,
plotting visualization and workplace management. But if you are familiar
with another IDE or you are advanced user of R you may not need to
install Rstudio (but I do).

We also will use many contributed packages for different statistical
technique and data processing. For more detail see [CRAN
package repository](https://cran.r-project.org/) (The Comprehensive R
Archive Network). For each course section we will include the packages
required to be installed in advance in a page named Scope_&_Prerequisites.

### Installing

1.  Started downloading and installing R
    [Windows](https://cran.r-project.org/bin/windows/) or
    [Mac](https://cran.r-project.org/bin/macosx/).

2.  Then install [RStudio](https://rstudio.com/) Desktop as well.
    RStudio is available for Windows, macOS, and Linux, download
    [here](https://rstudio.com/products/rstudio/download/#download).
    
If you have any doubt, visit the FAQ sections for Windows and Mac available in both official sites or the chapter about installation in the book *Hands-On Programming with R* [here](https://rstudio-education.github.io/hopr/starting.html) or *R Programming for Data Science* [here](https://bookdown.org/rdpeng/rprogdatascience/getting-started-with-r.html).

R packages: finding packages that suit your needs
This section will focus on data cleaning and processing using the tidy data concept.

### Packages

For instance,
`install.packages("tidyverse")`

or throughout the couRse with

`packages_list <- c("tidyverse", "lubridate")`

`new.packages <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]`
`if(length(new.packages)) install.packages(new.packages)`

`invisible(lapply(packages_list, library, character.only = T, quietly = TRUE, warn.conflicts = F))`

Package that should to be installed are in the packages_list_prior.

`packages_list_prior <- c("tidyverse", "lubridate", "kableExtra","scales", "tidyquant", "cowplot",
                       "gridExtra", "RColorBrewer", "colorspace", "summarytools", "corrr", "GGally",
                       "minerva", "magrittr", "vip", "tidymodels", "infer", "moderndive", "extRemes",
                       "bayesplot", "bayestestR", "rstanarm", "insight", "modelbased", "performance", 
                       "see", "car", "lmtest", "plsmod","mixOmics","modelbased", "performance", "see",
                       "randomForest","MASS", "corrr", "car", "e1071", "astsa", "ggridges", "anytime", 
                       "Evapotranspiration", "xts", "gamair", "mgcv", "sp")`
                       
`new.packages <- packages_list_prior[!(packages_list %in% installed.packages()[,"Package"])]`
`if(length(new.packages)) install.packages_list_prior(new.packages)`
                         
`packages_list_priorII <- c("mapview", "webshot", "suncalc", "raster", "httr", "sf", "fasterize", 
                           "rasterVis", "RColorBrewer", "geobuffer", "ggspatial")`
