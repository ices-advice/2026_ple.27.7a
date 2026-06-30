## Run analysis, write model forecast results

## Before:
## After:

library(icesTAF)
library(stockassessment)

mkdir("model")

sourceTAF("parameters.R", quiet=TRUE)

#load("run/model.RData")
load("model/fit.RData")

######################################

#f_mean <- mean(tail(fbartable(fit)[,"Estimate"],3))

# in 2025 for the intermediate year assumptions used the f in 2024 
f_mean <- tail(fbartable(fit)[,"Estimate"],1)

logobs <- cbind(as.data.frame(fit$data$aux), logobs = fit$data$logobs)
### remove log scale
logobs$obs <- exp(logobs$logobs)

#Extra line added in 2021 because there  were NAs in the observations from the missing 2020 survey:
logobs <- subset(logobs, fleet==1) 
### remove columns not required anymore
catch <- logobs[, c("year", "age", "obs")]
catchL <- catch
catch <- data.frame(year=unique(catchL$year))
for (a in unique(catchL$age)) catch <- cbind(catch, catchL$obs[catchL$age==a])
colnames(catch) <- c("year", unique(catchL$age))


### remove SSB column (age -1), if available
catch <- catch[, setdiff(names(catch), "-1")]
### remove year column and set year as row name

row.names(catch) <- catch$year
catch <- catch[, setdiff(names(catch), "year")]
### replace NAs with 0
catch[is.na(catch)] <- 0


assessment_summary <- as.data.frame(summary(fit))
assessment_summary$deadcatch <- rowSums(catch*fit$data$catchMeanWeight)
assessment_summary$ICESl <- rowSums(catch*fit$data$landFrac*fit$data$landMeanWeight)
assessment_summary$ICESd <- rowSums(catch*(1-fit$data$landFrac)*fit$data$disMeanWeight)/Dead_discards
assessment_summary$ICESd_d <- assessment_summary$ICESd * Dead_discards    #Dead discards
assessment_summary$ICESd_l <- assessment_summary$ICESd * Live_discards    #Live discards

###########################################
deaddisrate <- mean(tail(assessment_summary$ICESd_d / assessment_summary$deadcatch, 3))   ##Proportion of the dead catch that is landed # mean of the last 3 years??



#########  forecast   ##########

FC<-list()

# 2021F=Fsq then Fmsy
set.seed(12345) #sets the starting number used to generate a sequence of random numbers - it ensures that you get the same result if you start with that same seed each time you run the same process
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,F_MSY,F_MSY), 
                               label=paste0(ass_yr,"F=Fsq then Fmsy"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


# 2021F=Fsq then Fmsy lower = 0.133
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,F_MSY_lower,F_MSY_lower), 
                               label=paste0(ass_yr,"F=Fsq then Fmsy lower"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


# 2021F=Fsq then Fmsy upper = 0.293
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,F_MSY_upper,F_MSY_upper), 
                               label=paste0(ass_yr,"F=Fsq then Fmsy upper"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


# 2021F=Fsq then zero
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,0,0), fval=c(NA,f_mean,NA,NA),
                               label=paste0(ass_yr,"F=Fsq then zero"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


# 2021F=Fsq then Fpa = 0.355
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,Fpa,Fpa), 
                               label=paste0(ass_yr,"F=Fsq Fpa"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


# 2021F=Fsq then Flim = 0.495
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,Flim,Flim), 
                               label=paste0(ass_yr,"F=Fsq then Flim"),rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


#F that will get us to Blim
optim_B <- function(f2target, target, f_mean, F_MSY, B_yr,
                    ave.years, rec.years){
  set.seed(12345)
  f1 <- forecast(fit, 
                 fscale =   c(1,          NA,       NA, NA), 
                 fval =     c(NA, f_mean, f2target, F_MSY), 
                 catchval = c(NA,         NA,       NA, NA),
                 ave.years = ave.years, 
                 rec.years = rec.years, savesim = TRUE)
  ssb <- attr(f1,"shorttab")["ssb", as.character(B_yr)]
  out <- 1E+3*(ssb - target)^2
  return(out)
}
f_val <- optimise(f = optim_B, interval = seq(from = 0, to = 3, by = 0.001), 
                  target = Blim, f_mean = f_mean, F_MSY = F_MSY,
                  B_yr = (ass_yr+2), 
                  ave.years = max(fit$data$years) + (-2:0), 
                  rec.years = 2017:(ass_yr-1))$minimum
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale = c(1, NA, NA, 1), 
                               fval = c(NA, f_mean, f_val, NA), 
                               ave.years = max(fit$data$years) + (-2:0), 
                               rec.years = 2017:(ass_yr-1),
                               label=paste0("Fsq, SSB(",ass_yr+2,")=Blim"), splitLD=TRUE, savesim = TRUE)


#F that will get us to Bpa
f_val <- optimise(f = optim_B, interval = seq(from = 0, to = 3, by = 0.001), 
                  target = Bpa, f_mean = f_mean, F_MSY = F_MSY,
                  B_yr = (ass_yr+2), 
                  ave.years = max(fit$data$years) + (-2:0), 
                  rec.years = 2017:(ass_yr-1))$minimum
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale = c(1, NA, NA, 1), 
                               fval = c(NA, f_mean, f_val, NA), 
                               ave.years = max(fit$data$years) + (-2:0), 
                               rec.years = 2017:(ass_yr-1),
                               label=paste0("Fsq, SSB(",ass_yr+2,")=Bpa"), splitLD=TRUE, savesim = TRUE)


#F that will get us to MSY Btrigger
f_val <- optimise(f = optim_B, interval = seq(from = 0, to = 3, by = 0.001), 
                  target = MSY_Btrigger, f_mean = f_mean, F_MSY = F_MSY,
                  B_yr = (ass_yr+2), 
                  ave.years = max(fit$data$years) + (-2:0), 
                  rec.years = 2017:(ass_yr-1))$minimum
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale = c(1, NA, NA, 1), 
                               fval = c(NA, f_mean, f_val, NA), 
                               ave.years = max(fit$data$years) + (-2:0), 
                               rec.years = 2017:(ass_yr-1),
                               label=paste0("Fsq, SSB(",ass_yr+2,")=MSY Btrigger"), splitLD=TRUE, savesim = TRUE)


# SQ
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,f_mean,f_mean), 
                               label=paste0(ass_yr,"F=Fsq then Fsq"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)




#Rollover TAC
TAC_red <- prev.TAC /  ((1-deaddisrate) + deaddisrate / Dead_discards)
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,NA,NA),
                               catchval=c(NA,NA,TAC_red, TAC_red),
                               label=paste0(ass_yr,"F=Fsq then TAC"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)




#Rollover advice
advice_red <- prev.advice /  ((1-deaddisrate) + deaddisrate / Dead_discards)
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,NA,NA),
                               catchval=c(NA,NA,advice_red, advice_red), 
                               label=paste0(ass_yr,"F=Fsq then"," ", ass_yr," ","advice"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


#F that will get us to MSY Btrigger

B_target <- median(FC[[1]][[3]]$ssb    )  ##SSB at the start of advice year
f_val <- optimise(f = optim_B, interval = seq(from = 0, to = 3, by = 0.001), 
                  target = B_target, f_mean = f_mean, F_MSY = F_MSY,
                  B_yr = (ass_yr+2), 
                  ave.years = max(fit$data$years) + (-2:0), 
                  rec.years = 2017:(ass_yr-1))$minimum
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale = c(1, NA, NA, 1), 
                               fval = c(NA, f_mean, f_val, NA), 
                               ave.years = max(fit$data$years) + (-2:0), 
                               rec.years = 2017:(ass_yr-1),
                               label=paste0("Fsq, SSB(",ass_yr+2,")=SSB(",ass_yr+1,")"), splitLD=TRUE, savesim = TRUE)


###New forecast options in 2024
# 2021F=Fsq then Fmsy scaled
scale_factor <- pmin(1, median(FC[[1]][3][[1]]$ssb)/MSY_Btrigger)
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,F_MSY*scale_factor,F_MSY*scale_factor), 
                               label=paste0(ass_yr,"F=Fsq then Fmsy scaled"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


# 2021F=Fsq then Fmsy lower scaled
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,F_MSY_lower*scale_factor,F_MSY_lower*scale_factor), 
                               label=paste0(ass_yr,"F=Fsq then Fmsy lower scaled"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


# 2021F=Fsq then Fmsy upper scaled
set.seed(12345)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(1,NA,NA,NA), fval=c(NA,f_mean,F_MSY_upper*scale_factor,F_MSY_upper*scale_factor), 
                               label=paste0(ass_yr,"F=Fsq then Fmsy upper scaled"), rec.years=2017:(ass_yr-1),
                               ave.years = max(fit$data$years) + (-2:0),
                               splitLD = TRUE, savesim = TRUE)


save(FC, file="model/forecast.RData")
save(catch, file="model/catch.RData")
save(assessment_summary, file="model/assessment_summary.RData")
