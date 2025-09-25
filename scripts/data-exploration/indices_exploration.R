## Script to look for high-level patterns in diel cycles in the KFP (2025)
## Goal is to track ACI, AR, and BI for sounds between 0-2 kHz (frequency band I'll be searching manually; lower than treefrogs)


# Load packages -----------------------------------------------------------

library(tidyverse)
library(seewave)   # install.packages("seewave")
library(tuneR)  # install.packages("tuneR")
library(soundecology)




# test file ---------------------------------------------------------------

aci_values <- data.frame(NULL)

mgv_0518_2100 <- readWave("C:/Users/hng9/Downloads/output_tests/S1130_048K_MGV_10457639_20250518_210000.wav")

interim_aci <- c("mgv_0518_2100", ACI(mgv_0518_2100, flim = c(0,2), wl = 2048, nbwindows = 10))

aci_values <- rbind(aci_values, interim_aci)

rm(mgv_0518_2100)



# test loop ---------------------------------------------------------------
## stopping at 41 files...
aci_values <- data.frame(NULL)
my_list <- NULL

file_list <- list.files("C:/Users/hng9/Music/BDAKFP_2025_LVR/", full.names = TRUE)
file_p1 <- file_list[1:50]


for (i in file_list) {
  soundwave <- readWave(i)
  interim_aci <- acoustic_complexity(soundwave, max_freq = 2000, j = 10, fft_w = 2048)
  
  my_list <- interim_aci[c(1,3)] |>
    as.data.frame()
  my_list$date <- substr(i, 61, 75)
  
  aci_values <- rbind(aci_values, my_list)
  
  rm(soundwave)
}




# test mapply -------------------------------------------------------------

files <- list.files("C:/Users/hng9/Music/BDAKFP_2025_LVR/", full.names = TRUE)

file_list <- mapply(readWave, 
                    files[1:100], 
                    from = 0, 
                    to = 1, 
                    units = "minutes")

aci_list <- lapply(file_list,
                   ACI,
                   wl = 2048,
                   flim = c(0,2))

aci_list <- as.data.frame(aci_list)

testing <- t(aci_list) |> 
  as.data.frame() |> 
  mutate(date = substr(row.names(testing), 61, 75))




###############################


for (i in file_list) {
  acoustic_complexity(i, max_freq = 2000, fft_w = 2048)
}





aci_list <- lapply(file_list,
                   acoustic_complexity, 
                     max_freq = 2000,
                     fft_w = 2048)

aci_values <- aci_list[c(1,3)] |> 
  as.data.frame()





aci.low.values <- c(aci.low.start[1,])

