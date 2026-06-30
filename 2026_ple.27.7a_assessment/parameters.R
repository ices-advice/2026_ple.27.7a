# Read all the parameters used for the assessment 

sam_assessment <- "Ple.7a_26"  # this is the name from Stockassessment.org

# assessment year
ass_yr <- 2026

#Discards 
# applied to (model_forecast.R, output.R, data.R)
Dead_discards<- 0.6 #(Catchpole et al. 2015)
Live_discards<- 0.4 #(Catchpole et al. 2015)

# Reference point
F_MSY <- 0.196
F_MSY_lower <- 0.133
F_MSY_upper <- 0.293
Fpa <- 0.403 #Fp05 #Ppa = 0.355 last year 2020
Flim <- 0.495
Blim <- 3958
Bpa <- 5294
MSY_Btrigger <- 8757

#TAC
prev.TAC <- 614 #1504 
prev.advice <- 614 #1504 

