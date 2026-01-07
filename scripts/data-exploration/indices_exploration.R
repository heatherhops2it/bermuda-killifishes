## Script to look for high-level patterns in diel cycles in the KFP (2025)
## Goal is to track ACI, AR, and BI for sounds between 0-2 kHz (frequency band I'll be searching manually; lower than treefrogs)
## Looking at the four ponds in the central management group (BHB, TRT, MGV, and SHB) to figure out which ones I should keep. 
## Looking at the first ~24 hours of recordings (100 files) on each of these ponds


# Load packages -----------------------------------------------------------

library(tidyverse)
library(seewave)   # install.packages("seewave")
library(tuneR)  # install.packages("tuneR")
library(soundecology)



# Get ACI in BHB  --------------------------------------------------------
## Starting with BHB
files <- list.files("C:/Users/hng9/Music/BDAKFP_2025_BHB/", full.names = TRUE)

  

## first batch
file_list <- mapply(readWave, 
                    files, 
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
aci_fin <- aci_fin |> mutate(date = substr(row.names(aci_fin), 61, 75)) |> 
  mutate(location = "BHB")





# Get ACI in TRT  --------------------------------------------------------
## Starting with TRT
files <- list.files("C:/Users/hng9/Music/BDAKFP_2025_TRT/", full.names = TRUE)



## first batch
file_list <- mapply(readWave, 
                    files, 
                    from = 0, 
                    to = 1, 
                    units = "minutes")

aci_raw <- lapply(file_list,
                  ACI,
                  wl = 2048,
                  flim = c(0,2))

aci_raw <- as.data.frame(aci_raw) |> 
  t() |> 
  as.data.frame() |> 
  rename(aci_value = V1) |> 
  mutate(location = "TRT") |> 
  mutate(date = substr(row.names(aci_fin), 61, 75))

aci_fin <- aci_fin |> rbind(aci_raw)
  





# Add location & export ---------------------------------------------------


write.csv(aci_fin, "processed_data/exploration_indices/BHBTRT_indices.csv")


