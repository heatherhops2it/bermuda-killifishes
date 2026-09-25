## the script is written to determine whether the 2025 samples are within the calibration curves for the ICPMS


# libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)


# load in data, clean -----------------------------------------------------

## create a list of "cleaner" element names for our elements of interest
sample_list <- data.frame(test = c("x27_al_he", "x75_as_he", "x111_cd_he", "x52_cr_he", "x59_co_he", "x63_cu_he", "x56_fe_he", "x208_pb_he", "x95_mo_he", "x60_ni_he", "x78_se_he", "x51_v_he", "x66_zn_he"),
                          new_names = c("Al", "As", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mo", "Ni", "Se", "V", "Zn"))

## starting with the UNDILUTED samples
df_u <- read_csv(file = "raw_data/water-samples/2025 undiluted samples conc ppb.csv") |> 
  clean_names() 

lt <- colnames(df_u)

df_u <- df_u |> 
  mutate_at(vars(lt[8:38]), as.numeric) |> 
  pivot_longer(cols = lt[8:38], names_to = "test") |> 
  mutate(site = substr(sample_name, 1, 3)) |> 
  filter(type %in% c("CalBlk", "CalStd", "Sample")) |> 
  left_join(sample_list, by = join_by(test)) |> 
  drop_na(new_names) |> 
  select(type, level, value, site, new_names)


## repeat with the DILUTED samples
df_d <- read_csv(file = "raw_data/water-samples/2025 diluted samples conc ppb.csv") |> 
  clean_names() 

lt <- colnames(df_d)

df_d <- df_d |> 
  mutate_at(vars(lt[8:38]), as.numeric) |> 
  pivot_longer(cols = lt[8:38], names_to = "test") |> 
  mutate(site = substr(sample_name, 1, 3)) |> 
  filter(type %in% c("CalBlk", "CalStd", "Sample")) |> 
  left_join(sample_list, by = join_by(test)) |> 
  drop_na(new_names) |> 
  select(type, level, value, site, new_names)




# graph calibration curves ------------------------------------------------

## again, we'll start with the UNDILUTED samples
## due to visualisation problems, we'll do this one element at a time...

df_u |> 
  filter(new_names == "Al") |> 
  ggplot(aes(x = value, y = site, colour = new_names)) +
  geom_point()



