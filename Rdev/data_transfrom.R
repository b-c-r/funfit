
# converting the data from Vucic-Pestic et al. (2010) and
# Rosenbaum & Rall (2018, 2019) from a csv-file to an RData-file
# to store it in the package. Data source:
# https://datadryad.org/stash/downloads/file_stream/54277

dfr_vp <- read.csv("data_raw/data_vucic-pestic_manual.csv")
str(dfr_vp)

dfr_vp <- data.frame(Nstart = dfr_vp$N0,
                     Neaten = dfr_vp$Neaten)

save(dfr_vp, file = "data/dfr_vp.RData")
