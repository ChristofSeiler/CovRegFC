# CovRegFC

## R Package for Bayesian Analysis of Functional Connectivity

This package implements our paper:

```
Multivariate Heteroscedasticity Models for Functional Brain Connectivity, 
C. Seiler and S. Holmes, 
bioRxiv 2017.
```

It uses a linear model akin to standard univariate linear models and extends it to model the functional connectivity matrix.

## Installation

The R package is available from github and can be installed by running the following command in R:

```
install.packages("devtools")
devtools::install_github("ChristofSeiler/CovRegFC",build_vignettes = TRUE)
```

## Getting Started

Read the step-by-step example analysis of data from the Human Connectome Project available here:

https://github.com/ChristofSeiler/CovRegFC_HCP
