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
  filter(type == "chain")
pc <- chains |> 
  select(-type, -site) |> 
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




# trying with a different package -----------------------------------------

## install.packages("FactoMineR")
## install.packages("factoextra")
library(FactoMineR)
library(factoextra)

# Perform PCA using FactoMineR
pca_result_fm <- nonfrogs |> 
  select(-type, -site) |> 
  PCA(scale.unit = TRUE, graph = FALSE)

# Create a biplot using FactoMineR and factoextra
fviz_pca_biplot(pca_result_fm, repel = TRUE,
                col.var = "blue", # Variables color
                col.ind = nonfrogs$site, # Individuals color by groups
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                addEllipses = TRUE, ellipse.level = 0.95)
