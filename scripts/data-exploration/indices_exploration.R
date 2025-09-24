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

aci_values <- data.frame(NULL)
my_list <- NULL

file_list <- list.files("C:/Users/hng9/Music/BDAKFP_2025_LVR/", full.names = TRUE)
file_p1 <- file_list[1:50]


for (i in file_p1) {
  soundwave <- readWave(i)
  interim_aci <- acoustic_complexity(soundwave, max_freq = 2000, j = 10, fft_w = 2048)
  
  my_list <- interim_aci[c(1,3)] |>
    as.data.frame()
  my_list$date <- substr(i, 61, 75)
  
  aci_values <- rbind(aci_values, my_list)
  
  rm(soundwave)
}
