library(icesTAF)
library(stockassessment)
library(reshape)
library(ggplot2)

source("utilities.R")

mkdir("report/plots")

load("model/fit.rData")
load("model/retro_fit.rData")
load("model/forecast.rData")
load("model/assessment_summary.rData")
sourceTAF("parameters.R", quiet=TRUE)


## input data plots

## model output plots ##
taf.png("report/plots/summary", width = 1600, height = 2000)
plot(fit)
dev.off()

taf.png("report/plots/SSB")
ssbplot(fit, xlab = "year", addCI = TRUE)
abline(h=Blim, col="red")
abline(h=Bpa, col="orange")
abline(h=MSY_Btrigger, col="green")#
#text(1990, 30000, "Blim", col="red")
#text(1990, 25000, "Bpa", col="orange")
#text(1990, 20000, "MSY_Btrigger", col="green")
legend(1985, 30000, c("Blim", "Bpa", "MSY_Btrigger"), 
       col=c("red","orange","green"),lty=1)

dev.off()

taf.png("report/plots/Fbar")
fbarplot(fit, xlab = "year", partial = FALSE)
abline(h=Flim, col="red")
abline(h=Fpa, col="orange")
abline(h=F_MSY, col="green")
abline(h=F_MSY_lower, col="green", lty=2)
abline(h=F_MSY_upper, col="green", lty=2)

legend(2007, 0.85, c("Flim", "Fpa", "FMSY", "FMSY lower/upper"), 
       col=c("red","orange","green","green"),lty=c(1,1,1,2))

dev.off()

taf.png("report/plots/Rec")
recplot(fit, xlab = "year")
dev.off()

taf.png("report/plots/Landings")
catchplot(fit, xlab = "year")
dev.off()

## Plot Fig.25.13 with reference points
taf.png("report/plots/Fig.25.13_ref_points", width = 2000, height = 2000)

par(mfrow=c(2,2),oma=c(2,2,0,0))

# SSB
ssbplot(fit, xlab = "", addCI = TRUE)
abline(h=Blim, col="red")
abline(h=Bpa, col="orange")
abline(h=MSY_Btrigger, col="green")#

legend(1985, 30000, c("Blim", "Bpa", "MSY_Btrigger"), 
       col=c("red","orange","green"),lty=1)

# Fbar
fbarplot(fit, xlab = "", partial = FALSE)
abline(h=Flim, col="red")
abline(h=Fpa, col="orange")
abline(h=F_MSY, col="green")
abline(h=F_MSY_lower, col="green", lty=2)
abline(h=F_MSY_upper, col="green", lty=2)

legend(2004, 0.92, c("Flim", "Fpa", "FMSY", "FMSY lower/upper"), 
       col=c("red","orange","green","green"),lty=c(1,1,1,2))

# Rec
recplot(fit, xlab = "")

# Landings
catchplot(fit, xlab = "")

mtext("year",side=1,line=0,outer=TRUE,cex=0.9)
# Back to the original graphics device
#par(mfrow = c(1, 1))

dev.off()

## Plot Fig.25.13 with reference points (alternate years on x-axis)
taf.png("report/plots/Fig.25.13_ref_points_bis", width = 2000, height = 2000)

par(mfrow = c(2, 2), oma = c(2, 2, 0, 0))

# ---- helper to get year range from 'fit' and draw alternate-year axis ----
get_years_from_fit <- function(fit) {
  yrs <- NULL
  
  # 1) extracting years from an FLQuant returned by ssb(fit)
  try({
    dn <- dimnames(ssb(fit))
    if (!is.null(dn) && "year" %in% names(dn)) {
      yrs <- as.integer(dn$year)
    }
  }, silent = TRUE)
  
  # 2) Fallback: try range(fit)
  if (is.null(yrs)) {
    try({
      r <- range(fit)
      if (!is.null(r) && all(c("minyear", "maxyear") %in% names(r))) {
        yrs <- seq(r["minyear"], r["maxyear"])
      }
    }, silent = TRUE)
  }
  
  # 3) take from current plot limits (after plotting)
  if (is.null(yrs)) {
    usr <- par("usr")  # c(xmin, xmax, ymin, ymax)
    yrs <- seq(floor(usr[1]), ceiling(usr[2]))
  }
  
  yrs
}

add_alternate_year_axis <- function(yrs, cex_axis = 0.55, las = 2) {
  axis(
    side   = 1,
    at     = yrs,
    labels = ifelse(yrs %% 2 == 0, yrs, ""),
    cex.axis = cex_axis,
    las = las
  )
}

# -------------------
# 1) SSB
# -------------------
ssbplot(fit, xlab = "", addCI = TRUE, xaxt = "n")
abline(h = Blim,        col = "red")
abline(h = Bpa,         col = "orange")
abline(h = MSY_Btrigger,col = "green")

yrs <- get_years_from_fit(fit)
add_alternate_year_axis(yrs, cex_axis = 0.55, las = 2)

legend(1985, 30000, c("Blim", "Bpa", "MSY_Btrigger"),
       col = c("red", "orange", "green"), lty = 1)

# -------------------
# 2) Fbar
# -------------------
fbarplot(fit, xlab = "", partial = FALSE, xaxt = "n")
abline(h = Flim,        col = "red")
abline(h = Fpa,         col = "orange")
abline(h = F_MSY,       col = "green")
abline(h = F_MSY_lower, col = "green", lty = 2)
abline(h = F_MSY_upper, col = "green", lty = 2)

yrs <- get_years_from_fit(fit)
add_alternate_year_axis(yrs, cex_axis = 0.55, las = 2)

legend(2004, 0.92, c("Flim", "Fpa", "FMSY", "FMSY lower/upper"),
       col = c("red", "orange", "green", "green"),
       lty = c(1, 1, 1, 2))

# -------------------
# 3) Recruitment
# -------------------
recplot(fit, xlab = "", xaxt = "n")
yrs <- get_years_from_fit(fit)
add_alternate_year_axis(yrs, cex_axis = 0.55, las = 2)

# -------------------
# 4) Landings / Catch
# -------------------
catchplot(fit, xlab = "", xaxt = "n")
yrs <- get_years_from_fit(fit)
add_alternate_year_axis(yrs, cex_axis = 0.55, las = 2)

# Outer x-axis label
mtext("year", side = 1, line = 0, outer = TRUE, cex = 0.9)

dev.off()

############################
### plot outputs: : Fig. 25.13
taf.png("report/plots/Fig.25.13")
options(scipen = 999) ### no exponential notation for numbers > 4 digits
modelled<- plot_SAM_list2(input_list = list(baseline9=fit ), group = "model",  
               SAM_to_ggplot2 = SAM_to_ggplot2, rec_factor = 1000,
               plot_catch = TRUE, plot_catch_data = TRUE)

plot(modelled)
dev.off()

### plot selectivity
taf.png("report/plots/selectivity")
selectivity<- plot_selectivity(input_list = list(baseline9 = fit), 
                 group = "model", standardize = 0)

plot(selectivity)
dev.off()

### plot selectivity, separate discards and landings: Fig. 25.8
taf.png("report/plots/Fig.25.8") #### This plot does not work ###############
selectivity2<- plot_selectivity(input_list = list(baseline9 = fit), 
                 group = "model", standardize = 0, sep_ld = TRUE)

plot(selectivity2)
dev.off()

### plot catchability: Fig. 25.9
taf.png("report/plots/Fig.25.9") 
catchability<- plot_catchability(fit_list = list(baseline = fit), fleet_SSB = c(3, 4),
                  fleet_names = c("catches", "UK-BTS-Q3", "NIGFS-WIBTS-Q1",
                                  "NIGFS-WIBTS-Q4"),
                  group = "model")

plot(catchability)
dev.off()

##### plot residuals: Fig. 25.10
n_cores <- 1 #Number of cores used for calculating residuals, needs to be 1 for windows

taf.png("report/plots/Fig.25.10") 
residuals<-plot_residuals(fit_list = list(baseline9 = fit), n_cores = n_cores,
                            fleet_names = c("catches", "UK-BTS-Q3", "NIGFS-WIBTS-Q1",
                                            "NIGFS-WIBTS-Q4"))
plot(residuals)
dev.off()

#'##############################################################################
#'                              Retrospective                                  # 
#'##############################################################################

taf.png("report/plots/retrospective", width = 1600, height = 2000)
plot(retro_fit)
dev.off()


## add current assessment to object
retro_fit[[as.character(tail(fit$data$years, 1))]] <- fit

### plot: Fig. 25.14: Retrospective assessments
taf.png("report/plots/Fig.25.14") 
retrospecitve<- plot_SAM_list2(input_list = retro_fit, group = "retro year",
               SAM_to_ggplot2 = SAM_to_ggplot2, rec_factor = 1000,
               plot_catch = TRUE, plot_catch_data = 1)

retrospecitve <- retrospecitve +
  labs(colour = "retro year", fill = "retro year", linetype = "retro year")

plot(retrospecitve)
dev.off()

### plot: Fig. 25.15: Zoom  of the retrospective assessments 
taf.png("report/plots/Fig.25.15") 
retrospective_zoom<- plot_SAM_list2(input_list = retro_fit, group = "retro year",
               SAM_to_ggplot2 = SAM_to_ggplot2, rec_factor = 1000,
               plot_catch = TRUE, plot_catch_data = 1, plot_zoom = (ass_yr-12:-1)) +
               scale_x_continuous(breaks = seq(ass_yr-12, ass_yr+1, 2))

retrospective_zoom <- retrospective_zoom +
  labs(colour = "retro year", fill = "retro year", linetype = "retro year")

plot(retrospective_zoom)
dev.off()

#'##############################################################################
#'                     Forecast plots                                          #
#'##############################################################################

for (i in 1:length(FC))
{
  taf.png(paste0("report/plots/", attributes(FC[[i]])$label, ".png")) 

  plot(FC[[i]])
  title(main=attributes(FC[[i]])$label, outer=F, cex.main=1)
  
  dev.off()
}


# Forecast makeup plot

catch <- apply(FC[[1]][[3]]$catchatage, 1, median) * colMeans(tail(fit$data$catchMeanWeight,3))      #catchn
SSB <- exp(apply(FC[[1]][[4]]$sim, 2, median)) [1:8]  * colMeans(tail(fit$data$catchMeanWeight[,,1] * fit$data$propMat,3))   #stockn


plot.data <- rbind(
  data.frame(cohort=ass_yr + 2:-5, val=SSB, type=paste(ass_yr+2,"SSB")),
  data.frame(cohort=ass_yr + 1:-6, val=c(catch), type=paste(ass_yr+1,"Dead catch"))
  
)
#plot.data$est <- plot.data$cohort >= ass_yr
plot.data$est <- ifelse(plot.data$cohort >= ass_yr, "Forecast","Assessment")

pg_cohort <- min(plot.data$cohort)
plot.data$cohort <- factor(plot.data$cohort, levels=c("plusgroup", rev(unique(plot.data$cohort))))
plot.data$cohort[as.character(plot.data$cohort) == pg_cohort] <- "plusgroup"
plot.data$cohort[as.character(plot.data$cohort) == pg_cohort+1 & 
                   plot.data$type == paste(ass_yr+2,"SSB")] <- "plusgroup"

Forecast_makeup<- ggplot(plot.data, aes(x=factor(cohort), y=val, fill=est)) + facet_wrap(~type, scales="free") + geom_col() +
  xlab("Cohort") +ylab("Contribution (tonnes)") +scale_fill_discrete(name="Source")



ggsave(paste0("report/plots/Forecast_makeup.png"),
       width = 30, height = 22.5, units = "cm", dpi = 300, type = "cairo-png")


