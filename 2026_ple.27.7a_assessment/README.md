# 2026_ple.27.7a_assessment
2026 - Plaice (Pleuronectes platessa) in Division 7.a (Irish Sea) - WGCSE(ADGCS)

## R 
R version 4.4.0 (2024-04-24 ucrt)

## R packages
The following R packages from CRAN are required to run the assessment:

``` r
icesTAF
icesAdvice
ggplot2
reshape
captioner
pander
rmarkdown
cowplot
knitr
stockassessment
```

They can be installed with:

``` r
### list with required packages
req_pkgs <- c("icesTAF", "icesAdvice", "ggplot2", "reshape", "captioner", "pander", "rmarkdown", "cowplot", "knitr")
### install packages which are not installed on the system
install.packages(req_pkgs[!req_pkgs %in% installed.packages()])
```
Stockassessment R-package containing the state-space assessment model (SAM) can be installed by typing:

``` r
devtools::install_github("fishfollower/SAM/stockassessment",
                          INSTALL_opts=c("--no-multiarch"))
```
## Running the assessment
The easiest way to run the assessment is to clone or download this repository and run:

``` r
### load the icesTAF and stockassessment packages
library(stockassessment)
library(icesTAF)
### load data
taf.bootstrap(taf = TRUE)
### clean all folders and run all scripts
sourceAll()

```
The code runs the entire data compilation and assessment and creates key tables and figures of the assessment along with a Word document and a HTML file summarising all the many tables and figures presented in the WG report.
