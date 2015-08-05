---
title: "randr"
author: "Thomas Hopper"
date: "July 26,2015"
output: html_document
---

# randr

Provides convenience functions for generating random numbers. The main intent is to utilize expert estimations of key distribution characteristics to generate random numbers for monte carlo simulation.

The function names all begin with distribution function from `base` or `VGAM` and end with an extension related to the function's operation, as `rnorm_within()` or `runif_digits()`. Currently supported are `rnorm`, `runif`, `rtriangle` and `rbeta`.

`*_within()` generates within the given range with a `confidence_level` tolerance interval with `lower` and `upper` as the $(1 - \mathrm{confidence_level})/2$ and $(1 + \mathrm{confidence_level})/2$ limits, respectively.

`*_between()` all numbers generated are between the given range, inclusive. $X \in \left[\mathrm{lower}, \mathrm{upper}\right]$.

`runif_digits` generates a uniformly-distributed integer with length equal to the given number of digits. 

## Installation

devtools::install_github("tomhopper/randr")

