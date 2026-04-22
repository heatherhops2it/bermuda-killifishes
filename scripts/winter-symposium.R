


library(tidyverse)
library(janitor)


# load in data ------------------------------------------------------------
## only the lunar data (full moon and new moon) for MGV/LVR
winterlunar <- read.delim(file = "raw_data/winter-symposium/both_fullmoon_selections.txt") |> 
  clean_names()

View(winterlunar)

## clean up the data
cleanwinter <- winterlunar |> 
  select(-selection, -view, -channel, -begin_path, -file_offset_s, -shape) |> 
  mutate(dates = as_datetime(substr(begin_file, 25, 39), tz = "GMT")) |> 
  mutate(dates = with_tz(dates, tzone = "Atlantic/Bermuda")) |> 
  filter(pulses != "many", pulses != "") |> 
  mutate(pulses = as.numeric(pulses)) |> 
  mutate(site = substr(begin_file, 12, 14))




# split it up by frequency band -------------------------------------------
## technically this doesn't capture everything!!!
high_freqs <- cleanwinter |> 
  filter(low_freq_hz > 2000) |> 
  mutate(freq_band = "high")
low_freqs <- cleanwinter |> 
  filter(high_freq_hz < 2000) |> 
  mutate(freq_band = "low")

## put them back together and summarise
sumwinter <- rbind(high_freqs, low_freqs) |> 
  group_by(freq_band, site, dates) |> 
  summarise(call_density = sum(pulses))


## make hour groups (every 3 hours)
groupwinter <- sumwinter |> 
  mutate(hour = hour(dates)) |> 
  mutate(groupy = floor(hour/3))




# separate little section for government report ---------------------------
## making a graph of just the high-frequency/treefrogs

highwinter <- high_freqs |> 
  group_by(freq_band, site, dates) |> 
  summarise(call_density = sum(pulses))
highgroup <- highwinter |> 
  mutate(hour = hour(dates)) |> 
  mutate(groupy = floor(hour/3))
write_csv(highgroup, file = "raw_data/winter-symposium/grouped_high_data.csv")
himport <- read_csv("raw_data/winter-symposium/grouped_high_data.csv")
himport |> 
  mutate(groupy = as.character(groupy)) |> 
  ggplot(aes(x = groupy, y = call_density)) +
  geom_col(aes(fill = site), position = position_dodge(0.9)) +
  scale_x_discrete(labels = c('00-02', '03-05', '06-08', '09-11', '12-14', '15-17', '18-20', '21-23')) +
  labs(
    x = "Hour (ADT)", 
    y = "Number of Calls",
    fill = "Site") +
  theme_minimal() +
  scale_fill_manual(values = c("#707070", "#303030"))



# make graphs -------------------------------------------------------------

## date on x, density on y, colour is frequency band
groupwinter |> 
  mutate(groupy = as.character(groupy)) |> 
  ggplot(aes(x = groupy, y = call_density)) +
  geom_col(aes(fill = freq_band), position = position_dodge(0.9)) +
  scale_x_discrete(labels = c('00-02', '03-05', '06-08', '09-11', '12-14', '15-17', '18-20', '21-23')) +
  labs(
    x = "Hour (ADT)", 
    y = "Number of Calls",
    fill = "Frequency Band")

## date on x, density on y, colour is site
groupwinter |>  
  mutate(groupy = as.character(groupy)) |> 
  ggplot(aes(x = groupy, y = call_density)) +
  geom_col(aes(fill = site), position = position_dodge(0.9)) +
  scale_x_discrete(labels = c('00-02', '03-05', '06-08', '09-11', '12-14', '15-17', '18-20', '21-23')) +
  labs(
    x = "Hour (ADT)", 
    y = "Number of Calls",
    fill = "Site")



## date on x, density on y, colour is site, frequency band
groupwinter |> 
  mutate(groupy = as.character(groupy)) |> 
  ggplot(aes(x = groupy, y = call_density)) +
  geom_col(aes(fill = site), position = position_dodge(0.9)) +
  scale_x_discrete(labels = c('00-02', '03-05', '06-08', '09-11', '12-14', '15-17', '18-20', '21-23')) +
  labs(
    x = "Hour (ADT)", 
    y = "Number of Calls",
    fill = "Site") +
  facet_wrap(~ freq_band, nrow = 2)


## date on x, density on y, colour is frequency band, site
groupwinter |> 
  mutate(groupy = as.character(groupy)) |> 
  ggplot(aes(x = groupy, y = call_density)) +
  geom_col(aes(fill = freq_band), position = position_dodge()) +
  scale_x_discrete(labels = c('00-02', '03-05', '06-08', '09-11', '12-14', '15-17', '18-20', '21-23')) +
  labs(
    x = "Hour (ADT)", 
    y = "Number of Calls",
    fill = "Freq Band") +
  facet_wrap(~ site, nrow = 2)


## that's not displaying nicely, so i need to export and manually add in zeroes for some of the site/time combinations. it's stupid. 
write_csv(groupwinter, file = "raw_data/winter-symposium/grouped_data.csv")


groupeddata <- read_csv("raw_data/winter-symposium/grouped_data.csv")


groupeddata |> 
  mutate(groupy = as.character(groupy)) |> 
  ggplot(aes(x = groupy, y = call_density)) +
  geom_col(aes(fill = freq_band), position = position_dodge()) +
  scale_x_discrete(labels = c('00-02', '03-05', '06-08', '09-11', '12-14', '15-17', '18-20', '21-23')) +
  labs(
    x = "Hour (ADT)", 
    y = "Number of Calls",
    fill = "Freq Band") +
  theme_classic(base_size = 23) +
  facet_wrap(~ site, nrow = 2)
