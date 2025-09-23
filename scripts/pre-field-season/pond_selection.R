library(tidyverse)


ponds <- read_csv(file = "raw_data/recovery_plan_ponds.csv")


ponds |> ggplot(aes(x = salinity)) +
  geom_bar(aes(fill = pollution_profile))


chosen_ponds <- ponds |> 
  filter(pollution_profile == "Y") |> 
  bind_rows(filter(ponds, pollution_profile == "N", species == "F. relictus"))


chosen_ponds |> ggplot(aes(x = salinity)) +
  geom_bar(aes(fill = naturalness))
