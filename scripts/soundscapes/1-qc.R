## LVR vs MGV bioacoustic diversity
## for use in JMIH 2026 poster presentation: Bioacoustic characteristics of Bermudian anchialine ponds


# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)




# load data ---------------------------------------------------------------

## first step is quality control. I have separated each annotation into quality categories of blue/teal/green (green is best quality, blue is worst).
## attend to initial QC (only looking at green) while loading in the data

mgv <- read_delim(file = "raw_data/soundscapes/S1130_048K_MGV_10457639_19700101_000006.Table.1.selections.txt") |> 
  filter(colour == "green") |> 
  mutate(site = "MGV")

lvr <- read_delim(file = "raw_data/soundscapes/S1130_048K_LVR_10458035_20250519_143322.Table.1.selections.txt") |> 
  filter(colour == "green") |> 
  mutate(site = "LVR")


## so now the only rows in the datasets are high-quality green sounds
## we've also added a row for site 



# export high-quality data ------------------------------------------------

write_tsv(mgv, file = "processed_data/soundscapes/2025-mgv-qc.txt")
write_tsv(lvr, file = "processed_data/soundscapes/2025-lvr-qc.txt")

