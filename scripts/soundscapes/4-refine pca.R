
# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggfortify)
library(reshape2)  # install.packages("reshape2")


# load data ---------------------------------------------------------------


lvr <- read_delim(file = "processed_data/soundscapes/2025-lvr-qc-source.txt")
mgv <- read_delim(file = "processed_data/soundscapes/2025-mgv-qc-source.txt")

## combine into one dataframe
df <- bind_rows(lvr, mgv) |> 
  clean_names() |> 
  select(-view, -channel, -begin_time_s, -end_time_s, 
         -low_freq_hz, -high_freq_hz, -begin_file, -begin_path,
         -file_offset_s, -colour)

## only show the aquatic sounds
fsh <- df |> 
  filter(source == 'aquatic') |> 
  select(-source)



# all aquatic sounds ------------------------------------------------------

## run the PCA
pc_all <- fsh |> 
  select(-selection, -type, -site, -pulse_count) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

## make a plot
autoplot(pc_all, data = fsh, colour = 'site', 
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "all aquatic sounds")

## the first two PCs explain 59.95% of the variation


## check for correlation between variables
cor_fsh <- fsh |> 
  select(-selection, -type, -site, -pulse_count) |> 
  cor(method = c("pearson", "kendall", "spearman")) |> 
  melt()
cor_fsh |> 
  mutate(rounded = round(value, digits = 2)) |> 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = rounded)) +
  theme(axis.text.x = element_text(angle = 45))
# summarise the correlation values to see if there are some variables that typically have less correlation
cor_fsh |> 
  group_by(Var1) |> 
  summarise(total = sum(value))



## refine the dataframe to use variables with max eigenvalues and min correlation(?)
fsh_ref <- fsh |> 
  select(selection, freq_95_percent_hz, time_25_percent_rel, dur_50_percent_s, site, type)
pc_ref <- fsh_ref |> 
  select(-selection, -site, -type) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

autoplot(pc_ref, data = fsh_ref, colour = 'site', shape = 'type',
         loadings = TRUE, loadings.label = TRUE)

## now the first two PCs explain 69.58% of the variation




# chains with pulse counts ------------------------------------------------

## make a new dataframe
fsh_cha <- fsh |> 
  filter(type == 'chain') |> 
  select(-selection, -type) |> 
  na.omit() 
## run the PCA
pc_cha <- fsh_cha |> 
  select(-site) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

## make a plot
autoplot(pc_cha, data = fsh_cha, colour = 'site', 
         loadings = FALSE) +
  labs(title = "chains with pulse counts")

## the first two PCs explain 57.65% of the variation



# harmonics ---------------------------------------------------------------

## make a new dataframe
fsh_ham <- fsh |> 
  filter(type == 'harmonic') |> 
  select(-selection, -type, -pulse_count)
## run the PCA
pc_ham <- fsh_ham |> 
  select(-site) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

## make a plot
autoplot(pc_ham, data = fsh_ham, colour = 'site', 
         loadings = FALSE) +
  labs(title = "harmonics")

## the first two PCs explain 51.42% of the variation
