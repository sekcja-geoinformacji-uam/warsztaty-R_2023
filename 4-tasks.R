library(dplyr)
library(tidyr)
library(tibble)
library(sf)

# ZAD1 - Wybierz 3 najdłuższe płatki dla każdego gatunku
iris |>
  group_by(Species) |>
  arrange(desc(Petal.Length)) |>
  slice_head(n = 3)

# ZAD2 - wybrać tylko samochody, które spalają mniej niż 10l/100km (przeliczyć mile na gallon na litry na 100km)
mtcars |>
  mutate(kpg = mpg * 1.609) |>
  mutate(lp100km = 100 / kpg * 3.785) |>
  filter(lp100km < 10) |>
  rownames()

# ZAD3 - obliczyć średni stosunek długości do szerokości dla każdego gatunku dla obu liści, przetworzyć do formy długiej
iris |>
  mutate(
    petal_ratio = Petal.Length / Petal.Width,
    sepal_ratio = Sepal.Length / Sepal.Width
  ) |>
  group_by(Species) |>
  summarise(
    mean_petal_ratio = mean(petal_ratio),
    mean_sepal_ratio = mean(sepal_ratio)
  ) |>
  rename(Petal = mean_petal_ratio, Sepal = mean_sepal_ratio) |>
  pivot_longer(cols = 2:3, names_to = "leaf", values_to = "ratio")

# ZAD4 - oblicz BMI bohaterów (nie-droidów) Star Wars i sklasyfikuj wg wartości:
# < 0.74 = Thin
# 0.74 - 1 = Normal
# 1 - 1.2 = Overweight
# > 1.2 = Obese
# sprawdź liczbę osób dla każdej kategorii
# sprawdź z jakiej planety pochodzi najwięcej osób otyłych
starwars |>
  drop_na(height, mass) |>
  filter(species != "Droid") |>
  mutate(BMI = (mass / ((height / 100) ^ 2)) / 25) |>
  mutate(obesity = cut(BMI, breaks = c(-Inf, 0.74, 1, 1.2, Inf), labels = c("thin", "normal", "overweight", "obese"))) |>
  group_by(obesity) |>
  summarise(count = n()) |>
  arrange(desc(count))

starwars |>
  drop_na(height, mass) |>
  filter(species != "Droid") |>
  mutate(BMI = (mass / ((height / 100) ^ 2)) / 25) |>
  mutate(obesity = cut(BMI, breaks = c(-Inf, 0.74, 1, 1.2, Inf), labels = c("thin", "normal", "overweight", "obese"))) |>
  filter(obesity %in% c("overweight", "obese")) |>
  group_by(homeworld) |>
  summarise(count = n()) |>
  slice_max(count, n = 1)

# ZAD5 - zmień listy w kolumnach zestawu starwars na ich długości (funkcja lengths) i sprawdź który człowiek grał w największej liczbie filmów
starwars |>
  mutate(across(films:starships, ~ lengths(.x))) |>
  filter(species == "Human") |>
  slice_max(films, n = 1)

# ZAD6 - Wybierz samochody, które mają pojemność silnika (disp) większą niż średnia dla samochodów z daną liczbą cylindrów.
mtcars |>
  mutate(car = rownames(mtcars)) |>
  relocate(car, .before = mpg) |>
  tibble::remove_rownames() |>
  group_by(cyl) |>
  filter(disp > mean(disp))

# ZAD7 - oblicz stosunek powierzchni do obwodu każdego powiatu i oblicz średnie wartości dla województw
# do obwodu st_cast do MULTILINESTRING
powiaty = st_read("powiaty.gpkg")
powiaty |>
  mutate(
    area = st_area(geom) |> as.double() / 1000000,
    perim = geom |> st_cast("MULTILINESTRING") |> st_length() |> as.double() / 1000
  ) |>
  mutate(
    woj = substr(jpt_kod_je, 1, 2),
    shape_index = (4 * pi * area) / (perim ^ 2)
  ) |>
  # group_by(woj) |>
  # summarise(
  #   shape_index = mean(shape_index)
  # ) |>
  select(shape_index) |>
  plot()
