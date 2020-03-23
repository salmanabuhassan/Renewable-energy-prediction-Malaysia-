library(readxl)
dat <- read_xlsx("fuel-input-to-power-stations-by-fuel-type.xlsx")

#make new columns for percentage of the sources 
dat$renewablepercentage <- ((dat$Hydropower+dat$Solar+dat$Biogas+dat$Biomass)/dat$Total)*100
dat$HydroPercentage <- dat$Hydropower/dat$Total * 100
dat$OtherPercentage <- (dat$Solar+dat$Biogas+dat$Biomass)/dat$Total * 100
dat$SolarPercentage <- (dat$Solar)/dat$Total * 100
dat$BiomassPercentage <- (dat$Biomass)/dat$Total * 100
dat$BiogasPercentage <- (dat$Biogas)/dat$Total * 100
#plot a line for each of the variable for their prediction
#hydropower
plot(dat$Year[30:39],dat$HydroPercentage[30:39])
pred1 <- lm(dat$HydroPercentage[30:39] ~ dat$Year[30:39]) # take the coefficient for the prediction
abline(pred1)
pred1
#take only 5 last rows for solar since it has only been around recently
#solar
plot(dat$Year[35:39],dat$SolarPercentage[35:39])
pred2 <- lm(dat$SolarPercentage[35:39] ~ dat$Year[35:39])
abline(pred2)
pred2
#biomass
plot(dat$Year[30:39],dat$BiomassPercentage[30:39])
pred3 <- lm(dat$BiomassPercentage[30:39] ~ dat$Year[30:39])
abline(pred3)
pred3
#biogas
plot(dat$Year[35:39],dat$BiogasPercentage[35:39])
pred4 <- lm(dat$BiogasPercentage[35:39] ~ dat$Year[35:39])
abline(pred4)
pred4

futureprediction <- function(year){
  cat("Percentage of renewable energy for the year ",as.character(year),"\n")
  hydropred <- -1161.5771 + 0.5816*year #pred1 Hydropower
  cat("Hydropower:",as.character(hydropred),"\n")
  solarpred <- -110.3932 + 0.0549*year #pred2 Solar
  cat("Solar:",as.character(solarpred),"\n")
  biomasspred <- -77.51332 + 0.03861*year #pred3 Biomass
  cat("Biomass:",as.character(biomasspred),"\n")
  biogaspred <- -21.40087 + 0.01064*year #pred4 Biogas
  cat("Biogas:",as.character(biogaspred),"\n")
 # Otherpred <- -46.41308 + 0.02329*year #combination of solar,biomas,biogas for simplification
 # cat("Solar+Biomas+Biogas(Combined for simplification)/Others:",as.character(Otherpred),"\n")
  fpred <- hydropred + solarpred +biomasspred +biogaspred
  cat("Renewable energy percentage prediction:",as.character(fpred),"\n")
}
futureprediction(2025)
futureprediction(2030)
