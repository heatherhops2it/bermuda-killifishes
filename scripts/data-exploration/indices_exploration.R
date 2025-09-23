## Script to look for high-level patterns in diel cycles in the KFP (2025)
## Goal is to track ACI, AR, and BI for sounds between 0-2 kHz (frequency band I'll be searching manually; lower than treefrogs)


# Load packages -----------------------------------------------------------

library(tidyverse)
library(seewave)   # install.packages("seewave")
library(tuneR)  # install.packages("tuneR")





# test file ---------------------------------------------------------------

aci_values <- data.frame(NULL)

mgv_0518_2100 <- readWave("C:/Users/hng9/Downloads/output_tests/S1130_048K_MGV_10457639_20250518_210000.wav")

interim_aci <- c("mgv_0518_2100", ACI(mgv_0518_2100, flim = c(0,2), wl = 2048, nbwindows = 10))

aci_values <- rbind(aci_values, interim_aci)

rm(mgv_0518_2100)



# test loop ---------------------------------------------------------------

aci_values <- data.frame(NULL)

file_list <- list.files("C:/Users/hng9/Downloads/output_tests/", full.names = TRUE)


for (i in file_list) {
  soundwave <- readWave(i)
  
  interim_aci <- c(substr(i, 62, 76), ACI(soundwave, f = 48000, flim = c(0,2), wl = 2048, nbwindows = 10))
  
  aci_values <- rbind(aci_values, interim_aci)
  
  rm(soundwave)
}




# test flac-wav conversion ------------------------------------------------


wav_list <- list.files("C:/Users/hng9/Downloads/tests/", full.names = TRUE)

wav2flac(file = wav_list[1], reverse = TRUE, path2exe = "C:/flac/flac-1.5.0-win/Win64/")

unlink("C:/Users/hng9/Downloads/tests/S1130_048K_MGV_10457639_20250518_210000.wav")




