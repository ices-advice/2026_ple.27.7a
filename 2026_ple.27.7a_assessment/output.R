## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(stockassessment)

mkdir("output")
sourceTAF("parameters.R", quiet=TRUE)

# load 
load("model/fit.rData", verbose = TRUE)
load("model/forecast.rData")
load("model/catch.rData")
load("model/assessment_summary.rData")
load("model/retro_fit.rData")

# Model Parameters
partab <- partable(fit)

# Fs
fatage <- faytable(fit)
fatage <- fatage[, -1]
fatage <- as.data.frame(fatage)

# Ns
natage <- as.data.frame(ntable(fit))

# Catch
catab <- as.data.frame(catchtable(fit))
colnames(catab) <- c("Catch", "Low", "High")

# TSB
tsb <- as.data.frame(tsbtable(fit))
colnames(tsb) <- c("TSB", "Low", "High")

# Summary Table
tab_summary <- cbind(as.data.frame(summary(fit)), tsb)
tab_summary <- cbind(tab_summary, catab)
# should probably make Low and High column names unique R_Low etc.

mohns_rho <- stockassessment::mohn(retro_fit)  #(mohn from stockassessment package)
mohns_rho <- as.data.frame(t(mohns_rho))


############## Forecast data tables ##################################################

assessment_summary <- as.data.frame(summary(fit))
assessment_summary$deadcatch <- rowSums(catch*fit$data$catchMeanWeight)
assessment_summary$ICESl <- rowSums(catch*fit$data$landFrac*fit$data$landMeanWeight)
assessment_summary$ICESd <- rowSums(catch*(1-fit$data$landFrac)*fit$data$disMeanWeight)/Dead_discards
assessment_summary$ICESd_d <- assessment_summary$ICESd * Dead_discards    #Dead discards
assessment_summary$ICESd_l <- assessment_summary$ICESd * Live_discards    #Live discards


deaddisrate <- mean(tail(assessment_summary$ICESd_d / assessment_summary$deadcatch, 3)) ##Proportion of the dead catch that is landed # mean of the last 3 years??

#save PLE dead disc rate for mixfish
write.taf(data.frame(PL_dis_rate = deaddisrate),
          file = "output/deaddisrate.csv", quote = TRUE)

Table_2 <- NULL
Table_3 <- NULL
for (i in 1:length(FC))
{
  Table_2 <- rbind(Table_2, data.frame(Basis=attributes(FC[[i]])$label,
                                     Ftot=median(FC[[i]][[2]]$fbar),
                                     SSB = median(FC[[i]][[3]]$ssb    ),
                                     R1 =  median(FC[[i]][[2]]$rec),
                                     R2 =  median(FC[[i]][[3]]$rec),
                                     TotCatch=0,
                                     Landings=median(FC[[i]][[2]]$catch) *(1-deaddisrate),
                                     Discards=0,
                                     SurvDis = median(FC[[i]][[2]]$catch) * deaddisrate * Live_discards/Dead_discards,
                                     DeadDis = median(FC[[i]][[2]]$catch) * deaddisrate,
                                     DeadCatch=median(FC[[i]][[2]]$catch)
                                     
  ) )
  
  Table_3 <- rbind(Table_3, data.frame(Basis=attributes(FC[[i]])$label,
                                     TotCatch=0,
                                     WCatch=0,
                                     UCatch =0,
                                     Ftot=median(FC[[i]][[3]]$fbar),
                                     Fw=median(FC[[i]][[3]]$fbarL),
                                     Fu=0,
                                     SSB = median(FC[[i]][[4]]$ssb    ),
                                     SSBpc =NA,
                                     TACpc=NA,
                                     Advicepc = NA,
                                     DeadCatch=median(FC[[i]][[3]]$catch),
                                     prob_below_Blim=100*mean(FC[[i]][[4]]$ssb < Blim)
  ) )
}



############################################################################

catch <- apply(FC[[1]][[3]]$catchatage, 1, median) * colMeans(tail(fit$data$catchMeanWeight,3))      #catchn
SSB <- exp(apply(FC[[1]][[4]]$sim, 2, median)) [1:8]  * colMeans(tail(fit$data$catchMeanWeight[,,1] * fit$data$propMat,3))   #stockn

my_summary <- function(x)(median(x) + c(-2, 0, 2) * sd(x))

exp(my_summary(log(FC[[1]][[2]]$ssb)))   #Extra data for sAG intermediate year # SSB data for 2021 to include in the Excel file "StandardGraphs_Template" for ICES plots
exp(my_summary(log(FC[[1]][[2]]$rec)))   # Recruits data for 2021 to include in the Excel file "StandardGraphs_Template" for ICES plots

assessment_summary <- rbind(assessment_summary,NA)
rownames(assessment_summary)[nrow(assessment_summary)] <- as.numeric(rownames(assessment_summary)[nrow(assessment_summary)-1])+1

assessment_summary[nrow(assessment_summary),c(2,1,3,5,4,6)] <-
  c(exp(my_summary(log(FC[[1]][[2]]$rec))) , exp(my_summary(log(FC[[1]][[2]]$ssb))))

assessment_summary <- assessment_summary[,c(2,1,3,5,4,6,8,7,9,10, 11, 12, 13)]

SAG_inputs<-assessment_summary # save data for SAG - Excel file "StandardGraphs_Template"


###############################################################################
# SAG input for report_sg.R
##############################################################################
tab_summary_new<- tab_summary
tab_summary_new$Year<- rownames(tab_summary_new)
Years<-as.numeric(tab_summary_new$Year)
SAG_inputs$year <- append(Years, max(Years)+1) 
SAG_inputs[as.character(ass_yr),c(1,2,3)] <- exp(my_summary(log(FC[[1]][[2]]$rec))) 
SAG_inputs[as.character(ass_yr),c(4,5,6)] <- exp(my_summary(log(FC[[1]][[2]]$ssb))) 


library(dplyr)

SAG_inputs<- SAG_inputs %>%
  dplyr::rename('Low_Recruitment' = "Low",
         "Recruitment" = "R(age 1)",
         "High_Recruitment" = "High",
         "Low_Spawning_Stock_Biomass" = "Low.1",
         "Spawning_Stock_Biomass" = "SSB",
         "High_Spawning_Stock_Biomass" = "High.1",
         "Low_FishingPressure" = "Low.2",
         "FishingPressure" = "Fbar(3-6)",
         "High_FishingPressure" = "High.2",
         "Dead_catch" = "deadcatch",
         "Landings" = "ICESl",
         "Discards" = "ICESd",
         "Dead_discards" = "ICESd_d"
  )

###############################################################################

m <- fit$data$natMor

cay <- ntable(fit)*(1-exp(-faytable(fit)-m)) * faytable(fit)/(faytable(fit)+m)

lest <- cay * fit$data$landFrac[,,1]  #model estimated landings

dest <- cay * (1-fit$data$landFrac[,,1])/Dead_discards #model estimated  total discards


## Write tables to output directory
suppressWarnings(write.taf(
  c("partab", "tab_summary", "natage", "fatage", "mohns_rho","SAG_inputs","Table_2", "Table_3" ),
  dir = "output", quote=TRUE
))

