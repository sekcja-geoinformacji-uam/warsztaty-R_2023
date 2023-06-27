library(dplyr)
library(tidyr)

# dane ćwiczeniowe
data = data.frame(
  id = 1:10,
  NDVI = runif(10),
  NDMI = runif(10),
  MBI = runif(10)
)

# przekształcenie na formę długą
data_long = data |>
  pivot_longer(cols = 2:4, names_to = "index", values_to = "value")
data_long

# przekształcenie na formę szeroką
data_wide = data_long |>
  pivot_wider(id_cols = 1, names_from = "index", values_from = "value")
data_wide
