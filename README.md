# Donor Anonymity and Compensation

### Summary

This repository provides the data and code to replicate the following paper:

    "Sperm Donor Anonymity and Compensation: An Experiment with American Sperm Donors. I. Glenn Cohen, 
    Travis G. Coan, Michelle Ottey, and Christina Boyd. 2016. Journal of Law and the Biosciences."

### Dependencies

The scripts to replicate were testing in R version 3.2.3 (2015-12-10) and require the following packages:

* [plyr](https://cran.r-project.org/web/packages/plyr/index.html)  (tested on version 1.8.3)
* [rstan](http://mc-stan.org/interfaces/rstan)  (tested on version 2.12.1)
* [rethinking](http://xcelab.net/rm/software/)  (tested on version 1.58)
* [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) (tested on verison 2.1.0)
* [grid](https://cran.r-project.org/src/contrib/Archive/grid/) (tested on verison 3.2.3)
* [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html) (tested on verison 2.2.1)

Note that the R scripts are Linux-centric, so you will need to modify the paths to the directories if using a Windows system.

### Usage

The following three scripts located in the R directory will replicate the main findings in the paper:

* replication.R (main script to replicate Figures 1, 2, and 3)
* stan_models.R (loads the various models used in the paper)
* estimation_functions.R (makes the necessary calls to Stan via rstan)
* plot_functions.R (creates the ggplot2 objects to output)

Note that the replicated plots may be slightly different from the publication plots due to random differences across simulations (i.e., I am an idiot and forgot to set the intial seed) and you will need to change the paths if on a Windows machine.
