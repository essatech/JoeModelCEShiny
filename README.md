
# JoeModelCEShiny <img src="www/img/JoeModelCE_small.png" align="right" style="max-width: 120px;"/>

<!-- badges: start -->
<!-- badges: end -->


## Developing a Cumulative Effects Modelling Framework for the Recovery of Aquatic Species at Risk

This is the location of the Shiny UI associated with the [`JoeModelCE`](github.com/essatech/JoeModelCE) package. `JoeModelCE` is a collection of functions to support the application of the [Alberta Environmental Parks Cumulative Effects Assessment Joe Model (AEP CEM; DFO 2019)] (<a href="https://waves-vagues.dfo-mpo.gc.ca/Library/40871344.pdf" target="_blank">PDF.</a>), coupled with a flexible population modelling framework.

The `JoeModelCE` package is part of a larger initiative to develop a framework for modelling cumulative impacts to Species at Risk (SAR) to guide recovery planning and adaptive management based on stressor-response functions related to taxa-specific threats.  The intent is to develop a modelling framework that can generate models across a range of complexity and data quality, treating stressor-response functions as modular entities.


#### Contributors:
This is a broad collaboration between groups Fisheries and Oceans Canada (DFO), B.C. Ministry of Environment and Climate Change Strategy (ECCS), Alberta Environment and Parks (AEP), and Simon Fraser University (SFU. 

Contributors include:

-   [Dr. Eva Enders](https://profils-profiles.science.gc.ca/en/profile/eva-enders ): Project Lead; DFO Research Scientist
-   [Dr. Jordan Rosenfeld](http://www.aferu.ca/rosenfeld-lab): Project design and coordination; ECCS Aquatic Ecologist
-   [Dr. Andrew Paul](https://github.com/andrewpaul68): Collaborator; AEP Research Scientist
-   [Dr. Kyle Wilson](https://github.com/klwilson23): Population model development.
-   [Isuru Dharmasena](https://www.linkedin.com/in/isuru-dharmasena-90269895/?originalSubdomain=ca): Core Shiny application development
-   [Matthew Bayly](https://github.com/mattjbayly), Marc Porter, Alejandra Urcelay, and [Julian Heavyside](https://github.com/julianheavyside) from [ESSA Technologies Ltd.](https://essa.com/): Support R package and Shiny App development.


## Features
-   Run custom implementations of the Joe Model on non-standard data formats.
-   Batch-run the integrated Joe Model/Population model across large datasets.
-   Run sensitivity tests.
-   Explore model extensions.


## Installation

The easiest way to install the `JoeModelCE` package is from within the [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) using `remotes::install_github()`. At this time the package has not been published to CRAN so the default `install.packages()` will not work. Instead use remotes (or devtools):
``` r
# You may need to install remotes
library(remotes)
remotes::install_github("essatech/JoeModelCE")
```

## Code of Conduct

Please note that the `JoeModelCE` package is released with a [Contributor Code of Conduct](https://pkgs.rstudio.com/rmarkdown/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.