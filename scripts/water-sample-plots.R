library(tidyverse)
library(janitor)


# load in data and clean --------------------------------------------------


df <- read_csv(file = "raw_data/water-samples/2025 diluted samples conc ppb.csv") |> 
  clean_names() 

lt <- colnames(df)

df <- df |> 
  mutate_at(vars(lt[8:38]), as.numeric) |> 
  pivot_longer(cols = lt[8:38], names_to = "test") |> 
  drop_na(sample_date) |> 
  mutate(site = substr(sample_name, 1, 3))



# test out a graph --------------------------------------------------------


df |> filter(test == "x75_as_he") |> 
  group_by(site) |> 
  ggplot(aes(x = sample_date, y = value)) +
  geom_point() +
  facet_wrap(~site) +
  labs(title = "75 As [ He ]")




# graph all contaminants --------------------------------------------------


## Fort et al (2015) looked at As, Cd, Cr, Cu, Fe, Pb, Ni, Zn, and Hg, which would be the following variables as a list:

sample_list <- c("x75_as_he", "x111_cd_he", "x52_cr_he", "x63_cu_he", "x56_fe_he", "x208_pb_he", "x60_ni_he", "x66_zn_he")


daygroups <- data.frame(sample_date = c("6/9/2025", "6/16/2025", "6/6/2025", "6/15/2025", "6/17/2025", "6/7/2025" ),
                        sample_group = c("early", "late", "early", "late", "late", "early"))

df_s <- df |>  select(site, sample_date, test, value) |> 
  left_join(daygroups, by = join_by(sample_date)) |> 
  group_by(test, sample_group) |> 
  summarise(avg_val = mean(value, na.rm = TRUE))

df_s |> filter(test %in% sample_list) |> 
  ggplot(aes(x = sample_group, y = avg_val)) +
  geom_point() +
  facet_wrap(~test, nrow = 2)



# compare directly to fort ------------------------------------------------

fort <- read_csv(file = "raw_data/water-samples/fort 2015.csv") |> 
  clean_names()

df_p <- df |> 
  select(site, test, value) |> 
  filter(site %in% c("WWK", "MGV", "LVR")) |> 
  left_join(fort, by = join_by(test, site)) |> 
  na.omit()


## try plotting it?

df_p













