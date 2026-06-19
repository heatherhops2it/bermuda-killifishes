library(tidyverse)

## remove type pulse
## add back in anurans

temp <- read_delim(file = "processed_data/soundscapes/2025-lvr-qc.txt") |> 
  filter(source == "anuran")
lvr <- read_delim(file = "processed_data/soundscapes/2025-lvr-qc.txt") |> 
  filter(source %in% c("aquatic", "anthrophonic", "avian", "anuran")) |> 
  filter(type %in% c("chain", "harmonic")) |> 
  bind_rows(temp)


mgv <- read_delim(file = "processed_data/soundscapes/2025-mgv-qc.txt") |> 
  filter(source %in% c("aquatic", "anthrophonic", "avian", "anuran")) |> 
  filter(type %in% c("chain", "harmonic"))


write_tsv(lvr, file = "processed_data/soundscapes/2025-lvr-qc-source.txt")
write_tsv(mgv, file = "processed_data/soundscapes/2025-mgv-qc-source.txt")


