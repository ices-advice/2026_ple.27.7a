library(icesTAF)
library(stockassessment)
library(icesAdvice)

mkdir("report/tables")

sourceTAF("parameters.R", quiet=TRUE)

load("model/fit.RData", verbose = TRUE)
load("model/assessment_summary.RData", verbose = TRUE)
Table2 <- read.csv("output/Table_2.csv")
Table3 <- read.csv("output/Table_3.csv")


years <- unique(fit$data$aux[, "year"])


################################################################################
############################## Forecast ########################################
Table2$Discards <- Table2$DeadDis + Table2$SurvDis
Table2$TotCatch <- Table2$Landings + Table2$Discards
Table2

#Stuff added at ADG 2020
Table3 <- data.frame(Table3[,1:3], surv.u=NA, dead.u=NA, Table3[-(1:3)])

deaddisrate <- mean(tail(assessment_summary$ICESd_d / assessment_summary$deadcatch, 3))   ##Proportion of the dead catch that is landed # mean of the last 3 years

Table3$WCatch <- Table3$DeadCatch * (1-deaddisrate)
Table3$UCatch <- Table3$DeadCatch * deaddisrate / Dead_discards
Table3$dead.u <- Table3$DeadCatch * deaddisrate 
Table3$surv.u <- Table3$DeadCatch * deaddisrate * Live_discards / Dead_discards
Table3$Fu <- Table3$Ftot - Table3$Fw
Table3$TotCatch <- Table3$WCatch + Table3$UCatch


Table3$SSBpc <- 100 * (Table3$SSB/Table2$SSB -1)
Table3$TACpc <- 100 * (Table3$TotCatch/prev.TAC - 1)
Table3$Advicepc <- 100 * (Table3$TotCatch/prev.advice - 1)
     
write.taf(Table3,"report/tables/Table_2.csv",quote=TRUE)
write.taf(Table2,"report/tables/Table_1.csv",quote=TRUE)


###############################################################################
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


#Information needed for report table s25.9 - 25.13
## catage
catage <- read.taf("data/cn.csv")

catage <- cbind(catage, total = rowSums(catage))
catage<-format(round(catage, 0), nsmall = 0)#Apply format Function to Control Decimal Places
catage<- cbind(years,catage)
names(catage)[names(catage) == 'years'] <- 'year/age'
write.taf(catage, "report/tables/catage.csv")

#Landings Table 25.9
catch1<- catch
Landings_25.9<- catch1 * fit$data$landFrac
Landings_25.9<- cbind(Landings_25.9, total = rowSums(Landings_25.9))
Landings_25.9<-format(round(Landings_25.9, 0), nsmall = 0)#Apply format Function to Control Decimal Places
Landings_25.9<- cbind(years,Landings_25.9)
names(Landings_25.9)[names(Landings_25.9) == 'years'] <- 'year/age'
write.taf(Landings_25.9, "report/tables/Landings_25.9.csv")

#Discards 25.10
Discards_25.10<- catch1 *(1- fit$data$landFrac) / Dead_discards
Discards_25.10<- cbind(Discards_25.10, total = rowSums(Discards_25.10))
Discards_25.10<-format(round(Discards_25.10, 0), nsmall = 0)#Apply format Function to Control Decimal Places
Discards_25.10<- cbind(years,Discards_25.10)
names(Discards_25.10)[names(Discards_25.10) == 'years'] <- 'year/age'
write.taf(Discards_25.10,"report/tables/Discards_25.10.csv")

#stock numbers 25.11
Stock_n_25.11<- ntable(fit)
Stock_n_25.11<-as.data.frame(Stock_n_25.11)
Stock_n_25.11<- cbind(Stock_n_25.11, total = rowSums(Stock_n_25.11))
Stock_n_25.11<-format(round(Stock_n_25.11, 0), nsmall = 0)#Apply format Function to Control Decimal Places
Stock_n_25.11<- cbind(years,Stock_n_25.11)
names(Stock_n_25.11)[names(Stock_n_25.11) == 'years'] <- 'year/age'
write.taf(Stock_n_25.11,"report/tables/Stock_n_25.11.csv")

#f at age 25.12
f_at_age_25.12<- faytable(fit)
f_at_age_25.12<-as.data.frame(f_at_age_25.12)
f_at_age_25.12<-apply(f_at_age_25.12, 2, icesRound)#Apply format ICES Function to Control Decimal Places
f_at_age_25.12<- as.data.frame(f_at_age_25.12)

#f estimate (Low and High)
f_at_age_25.12_all<-fbartable(fit)
f_at_age_25.12_all<- as.data.frame(f_at_age_25.12_all)
f_at_age_25.12_all<-apply(f_at_age_25.12_all, 2, icesRound)#Apply format ICES Function to Control Decimal Places
f_at_age_25.12_all<-as.data.frame(f_at_age_25.12_all)
  
#f estimate
f <- f_at_age_25.12_all[, c("Estimate")]
f_at_age_25.12<- cbind(f_at_age_25.12, f)
#f_at_age_25.12<-format(round(f_at_age_25.12, 3), nsmall = 3)#Apply format Function to Control Decimal Places
f_at_age_25.12<- cbind(years, f_at_age_25.12)
names(f_at_age_25.12)[names(f_at_age_25.12) == 'years'] <- 'year/age'
names(f_at_age_25.12)[names(f_at_age_25.12) == 'f'] <- 'Fbar(3-6)'

write.taf(f_at_age_25.12,"report/tables/f_at_age_25.12.csv")
write.taf(f_at_age_25.12_all, "report/tables/f_at_age_25.12_all.csv")

# table 25.13
Summary_25.13<- summary(fit)
Summary_25.13<-as.data.frame(Summary_25.13)
Summary_25.13_f <- Summary_25.13[ -c(7:9) ]
Summary_25.13<- cbind(Summary_25.13_f, f_at_age_25.12_all)
names(Summary_25.13)[names(Summary_25.13) == 'Estimate'] <- 'Fbar(3-6)'
Summary_25.13<- Summary_25.13[, c(2, 1, 3, 5, 4, 6, 8, 7, 9)]
names(Summary_25.13)[names(Summary_25.13) == 'Low.1'] <- 'Low'
names(Summary_25.13)[names(Summary_25.13) == 'Low.2'] <- 'Low'
names(Summary_25.13)[names(Summary_25.13) == 'High.1'] <- 'High'
names(Summary_25.13)[names(Summary_25.13) == 'High.2'] <- 'High'
suppressWarnings(write.taf(Summary_25.13, "report/tables/Summary_25.13.csv"))

# TSB
Summary_TSB_25.13<-tsbtable(fit)
Summary_TSB_25.13<-as.data.frame(Summary_TSB_25.13)
Summary_TSB_25.13<-format(round(Summary_TSB_25.13, 0), nsmall = 0)#Apply format Function to Control Decimal Places
names(Summary_TSB_25.13)[names(Summary_TSB_25.13) == 'Estimate'] <- 'TSB'
Summary_TSB_25.13<- Summary_TSB_25.13[, c(2, 1, 3)]
write.taf(Summary_TSB_25.13, "report/tables/Summary_TSB_25.13.csv")

# dead catch
Summary_dead_catch_25.13<-catchtable(fit)
Summary_dead_catch_25.13<-as.data.frame(Summary_dead_catch_25.13)
Summary_dead_catch_25.13<-format(round(Summary_dead_catch_25.13, 0), nsmall = 0)#Apply format Function to Control Decimal Places
names(Summary_dead_catch_25.13)[names(Summary_dead_catch_25.13) == 'Estimate'] <- 'Catch'
Summary_dead_catch_25.13<- Summary_dead_catch_25.13[, c(2, 1, 3)]
write.taf(Summary_dead_catch_25.13, "report/tables/Summary_dead_catch_25.13.csv")

# Summary_25.13 all
Summary_25.13_all <-cbind(Summary_25.13,Summary_TSB_25.13)
Summary_25.13_all <-cbind(Summary_25.13_all,Summary_dead_catch_25.13)
Summary_25.13_all<- cbind(years,Summary_25.13_all)
names(Summary_25.13_all)[names(Summary_25.13_all) == 'years'] <- 'Year'
suppressWarnings(write.taf(Summary_25.13_all, "report/tables/Summary_25.13_all.csv"))

##########################################################################

#proportion of SSB in the plusgroup in last 5 years of assessment

other_pars <- data.frame(name=rep("",3), val=rep(NA,3))

other_pars$name[1] <- "Proportion SSB on plus group"
other_pars$val[1] <- mean(tail((ntable(fit)*fit$data$stockMeanWeight*fit$data$propMat)[,"8"]/ssbtable(fit)[,"Estimate"], 5))


#percentage by weight 
y04 <- as.numeric(rownames(assessment_summary))>2003 #2004 onwards
other_pars$name[2] <- "Discard percentage by weight"
other_pars$val[2] <- 100*mean((assessment_summary$ICESd/(assessment_summary$ICESl+ assessment_summary$ICESd))[y04], na.rm=TRUE)


#percentage by number
other_pars$name[3] <- "Discard percentage by number"
other_pars$val[3] <- 100*mean((rowSums(catch1 *(1- fit$data$landFrac) / Dead_discards)/
            rowSums(catch1*(fit$data$landFrac + (1- fit$data$landFrac) / Dead_discards)))[y04], na.rm=TRUE)

write.taf(other_pars, "report/tables/Other values.csv")

