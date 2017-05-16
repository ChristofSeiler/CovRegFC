# CovRegFC

## R Package for Bayesian Analysis of Functional Connectivity

This packge uses the covariance regression model introduced by [Hoff and Niu](https://arxiv.org/abs/1102.5721) to jointly predict univariate channel brain activity and functional connectivity from experimental conditions. It uses a linear model akin to standard univariate linear models.

## Installation

The R package is available from github and can be installed by running the following command in R:

```
install.packages("devtools")
devtools::install_github("ChristofSeiler/CovRegFC",build_vignettes = TRUE)
```

## Getting Started

Read the vignette for a step-by-step example workflow:

```
vignette("workflow", package = "CovRegFC")
```
