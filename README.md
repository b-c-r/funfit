# funfit

**This package is under development. Try it carefully! (And share suggestions ;-))**

Please note that this is my first attempt to write an R-Package. Moreover, it is a fun project, so it might take some time for me to make progress!

## About

A package for fitting numerically simulated functional response models to experimental data. This package is based on the paper "[Fitting functional responses: Direct parameter estimation by simulating differential equations](https://doi.org/10.1111/2041-210X.13039)" (Rosenbaum & Rall 2018).

Fitting a numerically simulated curve using an iterative maximum likelihood process is very time-consuming, especially in the case of the functional response, as nearly all data points require a unique time series simulation. I, therefore, chose the simulation method based on Rcpp via the [odeintr](https://github.com/thk686/odeintr) package as described in the [manual, chapter 7.7](https://besjournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F2041-210X.13039&file=mee313039-sup-0002-Manual.pdf) of Rosenbaum & Rall (2018). To accelerate the fitting process further, I decided to allow for parallel computing options using the [foreach](https://CRAN.R-project.org/package=foreach) package. The maximum likelihood estimation itself is based on Ben Bolkers work, the [bbmle package](https://CRAN.R-project.org/package=bbmle).

It is often challenging to find good starting parameters for a non-linear fit, especially when fitting a functional response to data. Inspired by the [openGUTS project](https://openguts.info/), I decided to add some automatic starting parameter sampling using Latin hypercube sampling from the [lhs package](https://CRAN.R-project.org/package=lhs).

## What you can already do:

-   It is possible to fit a type II, type III, and generalized functional response to data using the `fr_fit()` function.

-   You can select the type of the functional response, and you don't need to worry about starting parameters.

-   You can also compute the 95% confidence limits for the parameters.

-   I included a penalty for the Hill exponent, *h*. With this penalty, the negative likelihood function becomes large if Hill becomes small (i.e.: *h* \< 1) or large. This prevents errors in the fitting algorithm. I'll talk about that later.

## Perspectives

-   I want to add the possibility to compute confidence bands around the floating mean of the functional response for nice plots, as done in [openGUTS](https://openguts.info/).

-   I will add a much better description of the model!

-   I intend to add the possibility of adding factorials (e.g., levels of starvation, habitat complexity, or others)

-   It is also possible to integrate the natural mortality of the prey ([Rosenbaum & Rall 2018](https://doi.org/10.1111/2041-210X.13039)) when simulating the feeding process. I aim to add this feature in the future.

-   ...

## Author

Björn C. Rall ([0000-0002-3191-8389](https://orcid.org/0000-0002-3191-8389))

## References

I developed this package using many different internet sources and built upon existing R-packages and scientific publications. I am grateful that so many people published help I could use. Furthermore, I will list my sources below (which may grow over time):

-   Using Roxygen2 with Rcpp: Post on [R-bloggers](https://www.r-bloggers.com/2016/08/rcpp-and-roxygen2/) by Brandon Bertelsen.

-   I had some issues with building and installing from a running R session. This [post](https://stackoverflow.com/questions/6313079/quit-and-restart-a-clean-r-session-from-within-r) made restarting R easier.

-   ...
