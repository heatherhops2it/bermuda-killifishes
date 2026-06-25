library(tidyverse)
library(janitor)
library(stats)

lvr <- read_delim(file = "processed_data/soundscapes/2025-lvr-qc-source.txt")
mgv <- read_delim(file = "processed_data/soundscapes/2025-mgv-qc-source.txt")

df <- bind_rows(lvr, mgv) |> 
  clean_names() |> 
  select(-selection, -view, -channel, -begin_time_s, -end_time_s, 
         -low_freq_hz, -high_freq_hz, -begin_file, -begin_path,
         -file_offset_s, -colour)


pc <- df |> 
  select(-type, -site, -source) |> 
  prcomp(center = TRUE,
         scale. = TRUE)



library(ggplot2)  ## install.packages("ggplot2")
library(ggfortify)  ## install.packages("ggfortify")




autoplot(pc, data = df, colour = 'source', alpha = 0.5, 
        loadings = FALSE)




## what happens if we isolate frogs?

frogs <- df |> 
  filter(source == "anuran") |> 
  select(-source)

pc <- frogs |> 
  select(-type, -site) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

autoplot(pc, data = frogs, 
         loadings = FALSE) +
  labs(title = "frogs only")


## and remove frogs?

nonfrogs <- df |> 
  anti_join(frogs)

pc <- nonfrogs |> 
  select(-type, -site, -source) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

autoplot(pc, data = nonfrogs, colour = 'site', alpha = 0.5,
         loadings = FALSE) +
  labs(title = "'fish' only")



## by type?

pulses <- nonfrogs |> 
  filter(type == "pulse")
pc <- pulses |> 
  select(-type, -site) |> 
  prcomp(center = TRUE,
         scale. = TRUE)
autoplot(pc, data = pulses, colour = 'site', 
         loadings = FALSE)


chains <- nonfrogs |> 
  filter(type == "chain") |> 
  na.omit(pulse_count) |> 
  mutate(pulse_density = pulse_count/dur_90_percent_s)
pc <- chains |> 
  select(-source, -type, -site) |> 
  prcomp(center = TRUE,
         scale. = TRUE)
autoplot(pc, data = chains, colour = 'site', 
         loadings = FALSE)


harmonics <- nonfrogs |> 
  filter(type == "harmonic")
pc <- harmonics |> 
  select(-type, -site) |> 
  prcomp(center = TRUE,
         scale. = TRUE)
autoplot(pc, data = harmonics, colour = 'site', 
         loadings = FALSE)




# frogs: time of day? -----------------------------------------------------

lvfg <- lvr |> 
  clean_names() |> 
  filter(source == "anuran") |> 
  select(-selection, -view, -channel, -begin_time_s, -end_time_s, 
         -low_freq_hz, -high_freq_hz, -begin_path,
         -file_offset_s, -colour, -source) |> 
  mutate(dates = as_datetime(substr(begin_file, 25, 39))) |> 
  mutate(times = substr(begin_file, 34,39)) |> 
  mutate(hours = substr(times, 1,2))


pc <- lvfg |> 
  select(-begin_file, -type, -site, -dates, -times, -hours, -pulse_count) |> 
  prcomp(center = TRUE, 
         scale. = TRUE)
autoplot(pc, data = lvfg, colour = 'dates', loadings = FALSE)


## general data exploration of frogs vs time

lvfg |> 
  ggplot(aes(x = dates, y = dur_90_percent_s)) +
  geom_point() +
  geom_smooth()

