
rm(list=ls())                                                                   # removes old stuff from workspace

library("here")
library("readr")

ds <- data(dfr_vp)
ds <- read_csv(here("data_raw","data_sentis.csv"))

ds$Nstart <- ds$Daphnia_density
ds$Neaten <- ds$Daphnia_eaten

plot(ds$Nstart, ds$Neaten)

system.time(
  frres_gen <- funfit::fr_fit(
    fr_type = "gen",
    Neaten = ds$Neaten,
    Nstart = ds$Nstart,
    P = rep(1, nrow(ds)),
    tend = rep(1, nrow(ds)),
    MC = TRUE,
    noC = 10,
  )
)
frres_gen
bbmle::summary(frres_gen)
AIC(frres_gen)
fr_CI_profile(frres_gen)


system.time(
  frres_II <- funfit::fr_fit(
    fr_type = "II",
    Neaten = ds$Neaten,
    Nstart = ds$Nstart,
    P = rep(1, nrow(ds)),
    tend = rep(1, nrow(ds)),
    MC = TRUE,
    noC = 10,
  )
)
frres_II
bbmle::summary(frres_II)
AIC(frres_II)
fr_CI_profile(frres_II)


system.time(
  frres_III <- funfit::fr_fit(
    fr_type = "III",
    Neaten = ds$Neaten,
    Nstart = ds$Nstart,
    P = rep(1, nrow(ds)),
    tend = rep(1, nrow(ds)),
    MC = TRUE,
    noC = 10,
  )
)
frres_III
bbmle::summary(frres_III)
AIC(frres_III)
fr_CI_profile(frres_III)



AIC(frres_II, frres_III, frres_gen)

BIC(frres_II, frres_III, frres_gen)
