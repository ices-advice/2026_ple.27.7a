## Run analysis, write model results

## Before:
## After:

library(icesTAF)
library(stockassessment)

mkdir("model")
sourceTAF("parameters.R", quiet=TRUE)

# load data required
load("data/data.RData", verbose = TRUE)

# setup configuration
conf <-
  loadConf(
    dat,
    "boot/data/sam_config/model.cfg",
    patch = TRUE
  )

# define parameters
par<-defpar(dat,conf)

# fit model
fit<-sam.fit(dat,conf,par)

# retrospective fits
n_cores <- 1 #Number of cores used for calculating residuals, needs to be 1 for windows
nyears <- 5
retro_fit <- retro(fit, year = nyears, ncores = n_cores)
sapply(retro_fit, function(x){x$opt$message})  ## relative converge of retro analysis

# ## add current assessment to object
# retro_fit[[as.character(tail(fit$data$years, 1))]] <- fit

names(retro_fit) <- unlist(lapply(retro_fit, function(x) {
  tail(x$data$year, 1)
}))

# save model fits

save(fit, file="model/fit.RData")

save(retro_fit, file="model/retro_fit.RData")

# run forecast after model

sourceTAF("model_forecast.R")
