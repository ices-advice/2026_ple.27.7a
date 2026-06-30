## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(stockassessment)

mkdir("data")
sourceTAF("parameters.R", quiet=TRUE)

#  Read underlying data from bootstrap/data
# quick utility function
read.ices.taf <- function(...) {
  read.ices(taf.data.path("sam_data", ...))
}

#  ## Catch-numbers-at-age ##
cn <- read.ices.taf("cn.dat")

#  ## Catch-weight-at-age ##
cw <- read.ices.taf("cw.dat")
dw <- read.ices.taf("dw.dat")
lw <- read.ices.taf("lw.dat")

#  ## Natural-mortality ##
nm <- read.ices.taf("nm.dat")

# maturity
mo <- read.ices.taf("mo.dat")

#  ## Proportion of F before spawning ##
pf <- read.ices.taf("pf.dat")

#  ## Proportion of M before spawning ##
pm <- read.ices.taf("pm.dat")

#  ## Stock-weight-at-age ##
sw <- read.ices.taf("sw.dat")

# Landing fraction in catch at age
lf <- read.ices.taf("lf.dat")

# survey data
surveys<-read.ices.taf("survey.dat")
surveys<-c(surveys, read.ices.taf("Survey_DARDS.dat"))

### remove NaN #first check dw and lw in case there are NaN
dw[which(!is.finite(dw))] <- 0
lw[which(!is.finite(lw))] <- 0
sw[which(!is.finite(sw))] <- 0
cw[which(!is.finite(cw))] <- 0

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf)
###################################################
### function for using different discard survival rates
set_survival <- function(survival_rate = 0, ### proportion of discards surviving
                         surveys, residual.fleet, prop.mature, 
                         stock.mean.weight, catch.mean.weight, dis.mean.weight,
                         land.mean.weight, prop.f, prop.m, natural.mortality,
                         land.frac, ...){
  
  ### separate landings and discards numbers
  landings.n <- residual.fleet * land.frac # catch.n *  landings.n/catch.n
  discards.n <- residual.fleet * (1 - land.frac)
  #all.equal((landings.n + discards.n), residual.fleet) ### worked
  
  ### apply survival rate to discards->only dead portion of discards is considered in the assessment
  discards.n <- discards.n * (1 - survival_rate)
  
  ### sum up to total catch numbers
  residual.fleet <- landings.n + discards.n
  
  ### update landings proportion, as discards only include now dead portion
  land.frac <- landings.n / (landings.n + discards.n)
  ### remove NaN
  land.frac[which(!is.finite(land.frac))] <- 1
  
  
  
  ### calculate catch weight as weighted mean.   
  catch.mean.weight <- land.mean.weight * (landings.n / residual.fleet) +
    dis.mean.weight * (discards.n / residual.fleet)
  catch.mean.weight[which(!is.finite(catch.mean.weight))] <- dis.mean.weight[which(!is.finite(catch.mean.weight))] 
  catch.mean.weight[which(!is.finite(catch.mean.weight))] <- land.mean.weight[which(!is.finite(catch.mean.weight))]
  
  ### set catch weight as stock weight
  stock.mean.weight <- catch.mean.weight
  
  
  ### set up SAM input object
  res <- setup.sam.data(surveys = surveys,
                        residual.fleet = residual.fleet,
                        prop.mature = prop.mature,
                        stock.mean.weight = stock.mean.weight,
                        catch.mean.weight = catch.mean.weight,
                        dis.mean.weight = dis.mean.weight,
                        land.mean.weight = land.mean.weight,
                        prop.f = prop.f,
                        prop.m = prop.m,
                        natural.mortality = natural.mortality,
                        land.frac = land.frac)
  
  ### return
  return(res)
  
}


dat <- set_survival(survival_rate = Live_discards,
                    surveys = surveys, residual.fleet = cn, prop.mature = mo,
                    stock.mean.weight = sw, catch.mean.weight = cw, 
                    dis.mean.weight = dw, land.mean.weight = lw, prop.f = pf,
                    prop.m = pm, natural.mortality = nm, land.frac = lf)
########################################################################################

## 3 Write TAF tables to data directory
write.taf(
  c(
    "cn", "mo", "sw",
    "cw", "dw", "lw", "pf", "pm",
    "nm", "lf"
  ),
  dir = "data"
)

save(dat, file = "data/data.RData")



