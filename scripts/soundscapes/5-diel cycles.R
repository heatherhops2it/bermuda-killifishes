
# load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(AICcmodavg)


# load in data ------------------------------------------------------------

lvr <- read_delim(file = "processed_data/soundscapes/2025-lvr-qc-source.txt")
mgv <- read_delim(file = "processed_data/soundscapes/2025-mgv-qc-source.txt")

## combine into one dataframe
df <- bind_rows(lvr, mgv) |> 
  clean_names() |> 
  select(-view, -channel, -begin_time_s, -end_time_s, 
         -low_freq_hz, -high_freq_hz, -begin_path,
         -file_offset_s, -colour) |> 
  filter(site != is.na(site)) |>    ## remove any annotations without a site listed
  mutate(dates = as_datetime(substr(begin_file, 25, 39), tz = "GMT")) |>   ## extract the datetime from the file name & ensure that it is in GMT/UTC
  mutate(dates = with_tz(dates, tzone = "Atlantic/Bermuda")) |>   ## change timezone to Bermuda 
  mutate(hours = hour(dates)) |>   
  mutate(count = 1) |> 
  filter(source %in% c("anuran", "aquatic"))  ## only looking at anuran & aquatic sounds

df_sm <- df |> 
  group_by(hours, source, site) |> 
  summarise(call_density = sum(count)) |> 
  mutate(groupy = floor(hours/3))   ## group the call counts into 3-hour bins


## need to export the summary file and re-load it. because I still don't know how to add in the 0-values in R...
write_csv(df_sm, file = "processed_data/soundscapes/2025-lvrmgv-summary.csv")
df_in <- read_csv(file = "processed_data/soundscapes/2025-lvrmgv-summary.csv")


# make plots --------------------------------------------------------------


df_in |> 
  mutate(groupy = as.character(groupy)) |> 
  ggplot(aes(x = groupy, y = call_density)) +
  geom_col(aes(fill = source), position = position_dodge(0.9)) +
  scale_x_discrete(labels = c('00-02', '03-05', '06-08', '09-11', '12-14', '15-17', '18-20', '21-23')) +
  labs(
    x = "Hour (ADT)", 
    y = "Number of Calls",
    fill = "Source") +
  theme_minimal()




# run stats ---------------------------------------------------------------

## One-way ANOVA for just hours
diel_oneway <- aov(call_density ~ hours, data = df_sm)

## Two-way ANOVA to test for patterns by hour by source
diel_twoway <- aov(call_density ~ hours + source, data = df_sm)

## Two-way ANOVA to test for patterns as interaction of hour and source
summary(aov(call_density ~ hours * source, data = df_sm))

## Check to see which model is best-fit
model_set <- list(diel_oneway, diel_twoway)
model_names <- c("oneway", "twoway")
aictab(model_set, modnames = model_names)

## Check best-fit model for homoscedasticity
par(mfrow=c(2,2))
plot(diel_oneway)
par(mfrow=c(1,1))

## Since the Q-Q Residuals don't exactly have a 1-1 relationship (although they're good along the slope), and the red line in Scale-Location is not totally horizontal, it's possible I need to do the non-parametric test: Kruskal-Wallis
kruskal.test(call_density ~ hours * source, data = df_sm)
