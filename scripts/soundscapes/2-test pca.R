library(tidyverse)
library(janitor)
library(stats)

lvr <- read_delim(file = "processed_data/soundscapes/2025-lvr-qc.txt")
mgv <- read_delim(file = "processed_data/soundscapes/2025-mgv-qc.txt")

df <- bind_rows(lvr, mgv) |> 
  clean_names() |> 
  select(-selection, -view, -channel, -begin_time_s, -end_time_s, 
         -low_freq_hz, -high_freq_hz, -begin_file, -begin_path,
         -file_offset_s, -colour, -source, -type)


pc <- prcomp(df[,-15], 
       center = TRUE,
       scale. = TRUE)

biplot(pc)


install.packages("ggplot2")
install.packages("ggfortify")
library(ggplot2)
library(ggfortify)




autoplot(pc, data = df, colour = 'site', 
        loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3)
