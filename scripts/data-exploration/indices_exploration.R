## Script to look for high-level patterns in diel cycles in the KFP (2025)
## Goal is to track ACI, AR, and BI for sounds between 0-2 kHz (frequency band I'll be searching manually; lower than treefrogs)


# Load packages -----------------------------------------------------------

library(tidyverse)
library(seewave)   # install.packages("seewave")
library(tuneR)  # install.packages("tuneR")




# Get ACI -------------------------------------------------------------

files <- list.files("C:/Users/hng9/Music/BDAKFP_2025_LVR/", full.names = TRUE)


## first batch
file_list <- mapply(readWave, 
                    files[1:1000], 
                    from = 0, 
                    to = 1, 
                    units = "minutes")

aci_raw <- lapply(file_list,
                   ACI,
                   wl = 2048,
                   flim = c(0,2))

aci_raw <- as.data.frame(aci_raw)

aci_fin <- t(aci_raw) |> 
  as.data.frame() |> 
  rename(aci_value = V1)
aci_fin <- aci_fin |> mutate(date = substr(row.names(aci_fin), 61, 75))




## second batch
file_list <- mapply(readWave, 
                    files[1001:2000], 
                    from = 0, 
                    to = 1, 
                    units = "minutes")

aci_raw <- lapply(file_list,
                  ACI,
                  wl = 2048,
                  flim = c(0,2))

aci_raw <- as.data.frame(aci_raw)

aci_proc <- t(aci_raw) |> 
  as.data.frame() |> 
  rename(aci_value = V1)
aci_proc <- aci_proc |> mutate(date = substr(row.names(aci_proc), 61, 75))

aci_fin <- aci_fin |> 
  rbind(aci_proc)




## third batch
file_list <- mapply(readWave, 
                    files[2001:length(files)], 
                    from = 0, 
                    to = 1, 
                    units = "minutes")

aci_raw <- lapply(file_list,
                  ACI,
                  wl = 2048,
                  flim = c(0,2))

aci_raw <- as.data.frame(aci_raw)

aci_proc <- t(aci_raw) |> 
  as.data.frame() |> 
  rename(aci_value = V1)
aci_proc <- aci_proc |> mutate(date = substr(row.names(aci_proc), 61, 75))

aci_fin <- aci_fin |> 
  rbind(aci_proc)


