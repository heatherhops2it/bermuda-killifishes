
# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggfortify)
library(ggforce)  # install.packages("ggforce")
library(reshape2)  # install.packages("reshape2")


# load data ---------------------------------------------------------------


lvr <- read_delim(file = "processed_data/soundscapes/2025-lvr-qc-source.txt")
mgv <- read_delim(file = "processed_data/soundscapes/2025-mgv-qc-source.txt")

## combine into one dataframe
df <- bind_rows(lvr, mgv) |> 
  clean_names() |> 
  select(-view, -channel, -begin_time_s, -end_time_s, 
         -low_freq_hz, -high_freq_hz, -begin_file, -begin_path,
         -file_offset_s, -colour) |> 
  filter(site %in% c("LVR", "MGV")) |> 
  filter(selection != "4262")

## only show the aquatic sounds
fsh <- df |> 
  filter(source == 'aquatic') |> 
  select(-source) |> 
  filter(selection != "4262") |> 
  filter(type %in% c("chain", "harmonic"))



# all sounds --------------------------------------------------------------

## run the PCA
pc_all <- df |> 
  select(-selection, -source, -type, -site, -pulse_count) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

## make a plot
autoplot(pc_all, data = df, colour = 'source', shape = 'site', size = 4,
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "all sounds")

## the first two PCs explain 59.89% of the variation


## check for correlation between variables
cor_df <- df |> 
  select(-selection, -source, -type, -site, -pulse_count) |> 
  cor(method = c("pearson", "kendall", "spearman")) |> 
  melt()
cor_df |> 
  mutate(rounded = round(value, digits = 2)) |> 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = rounded)) +
  theme(axis.text.x = element_text(angle = 45))
# summarise the correlation values to see if there are some variables that typically have less correlation
cor_df |> 
  group_by(Var1) |> 
  summarise(total = sum(value))



## refine the dataframe to use variables with max eigenvalues and min correlation(?)
df_ref <- df |> 
  select(site, type, source,
         center_time_rel, dur_50_percent_s, bw_50_percent_hz, freq_5_percent_hz)
pc_df_ref <- df_ref |> 
  select(-site, -type, -source) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

autoplot(pc_df_ref, data = df_ref, colour = 'source', shape = 'site', size = 4,
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "refined all sounds")

## now the first two PCs explain XX% of the variation





# all aquatic sounds ------------------------------------------------------

## run the PCA
pc_aq <- fsh |> 
  select(-selection, -type, -site, -pulse_count) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

## make a plot
autoplot(pc_aq, data = fsh, colour = 'site', 
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
  select(site, type,
         dur_50_percent_s, center_freq_hz, freq_95_percent_hz, time_25_percent_rel)
pc_ref <- fsh_ref |> 
  select(-site, -type) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

autoplot(pc_ref, data = fsh_ref, colour = 'site',
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "refined all aquatic sounds") +
  stat_ellipse(aes(colour = site, group = site))

## now the first two PCs explain 77.44% of the variation




# chains with pulse counts ------------------------------------------------

## make a new dataframe
fsh_cha <- fsh |> 
  filter(type == 'chain') |> 
  select(-selection, -type) |> 
  na.omit() |> 
  mutate(pulse_density = pulse_count/dur_90_percent_s)
## run the PCA
pc_cha <- fsh_cha |> 
  select(-site) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

## make a plot
autoplot(pc_cha, data = fsh_cha, colour = 'site', 
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "chains with pulse counts")

## the first two PCs explain 57.65% of the variation

## check for correlation between the variables
cor_cha <- fsh_cha |> 
  select(-site) |> 
  cor(method = c("pearson", "kendall", "spearman")) |> 
  melt()
cor_cha |> 
  mutate(rounded = round(value, digits = 2)) |> 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = rounded)) +
  theme(axis.text.x = element_text(angle = 45))
# summarise the correlation values to see if there are some variables that typically have less correlation
cor_cha |> 
  group_by(Var1) |> 
  summarise(total = sum(value))


## refine the dataframe to use variables with max eigenvalues and min correlation(?)
cha_ref <- fsh_cha |> 
  select(time_25_percent_rel, time_75_percent_rel, dur_50_percent_s, pulse_density, site)
pc_cha_ref <- cha_ref |> 
  select(-site) |> 
  prcomp(center = TRUE,
         scale. = TRUE)

autoplot(pc_cha_ref, data = cha_ref, colour = 'site',
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "refined chains with pulse counts")

## now the first two PCs explain 70.71% of the variation


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
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "harmonics")

## the first two PCs explain 51.42% of the variation

## somewhat dffcult to move past this point while still comparing the two sites, since there are only two LVR harmonics





