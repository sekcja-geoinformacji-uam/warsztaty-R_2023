library(dplyr)
library(tidyr)
library(sf)

# wczytanie danych
powiaty = st_read("powiaty.gpkg")
bezrob = read.csv("bezrobocie_powiaty.csv", dec = ",", fileEncoding = "utf-8")

# przetworzenie danych
powiaty = powiaty |>
  select(jpt_kod_je, jpt_nazwa_)
bezrob = bezrob |>
  mutate(Kod = sprintf("%07d", Kod) |> as.character())

# złączenie wg TERYT
powiaty = powiaty |>
  left_join(
    bezrob |> select(Kod, ogółem),
    by = c("jpt_kod_je" = "Kod")
  ) |>
  rename(bezrobocie = ogółem)
powiaty["bezrobocie"] |> plot()

# obliczenie średniego bezrobocia oraz powierzchni dla województw
powiaty |>
  mutate(woj = substr(jpt_kod_je, 1, 2)) |>
  group_by(woj) |>
  summarise(
    geom = st_union(geom),
    bezrobocie = mean(bezrobocie)
  ) |>
  select(bezrobocie) |>
  plot()

powiaty |>
  mutate(woj = substr(jpt_kod_je, 1, 2)) |>
  group_by(woj) |>
  summarise(
    mean_area = mean(st_area(geom))
  ) |>
  select(mean_area) |>
  plot()

# powiaty w formie okręgów w centroidzie
powiaty |>
  st_centroid() |>
  st_buffer(10000) |>
  select(bezrobocie) |>
  plot()

# filtrowanie powiatów po powierzchni
powiaty |>
  mutate(area_sf = as.double(st_area(geom)) / 1000000) |>
  filter(area_sf > 1000) |>
  select(bezrobocie) |>
  plot()
