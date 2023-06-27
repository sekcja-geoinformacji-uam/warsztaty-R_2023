library(dplyr)
library(tidyr)

# wybieranie kolumn
iris |> select(Sepal.Length, Species)
mtcars |> select(mpg, cyl, qsec)
starwars |> select(height, mass)
iris |> select(contains("Sepal"))

# wybieranie wierszy
mtcars |> slice(1:10)
iris |> slice_min(Petal.Length, n = 5)
iris |> slice_sample(n = 20)

# filtrowanie
iris |> filter(Sepal.Width > 4)
iris |> filter(Species == "setosa")
mtcars |> filter(between(mpg, 15, 20))

# sortowanie wg kolumny
iris |> arrange(Petal.Width)
starwars |> arrange(desc(height))

# tworzenie nowej kolumny
mtcars |> mutate(kpg = mpg * 1.609)
mtcars |> mutate(if_fast = ifelse(qsec < 17, "fast", "slow"))

# grupowanie wg kolumny
iris |>
  group_by(Species) |>
  summarise(cnt = n(),
            mean_pet_len = mean(Petal.Length))

mtcars |>
  group_by(cyl) |>
  summarise(mean_mpg = mean(mpg))

# usuwanie duplikatów i wybieranie wartości z jednej kolumny
iris |> distinct(Species)
mtcars |>
  distinct(am) |>
  pull(am)

# zmiana kolejności kolumn
iris |> relocate(Species, .before = Sepal.Length)

# zmiana nazw kolumn
iris |> rename(sepal_length = Sepal.Length)
iris |> rename_with(tolower)
iris |>
  rename_with(
    ~paste0("X_", .x),
    starts_with("Sepal")
  )

# stosowanie działania na wielu kolumnach
mtcars |>
  mutate(across(disp:qsec, ~ .x * 2))

# kodowanie kolumny
iris |>
  mutate(species_code = recode(Species, setosa = "A", versicolor = "B", virginica = "C", .default = "-"))

starwars |>
  mutate(height_cat = cut(height, breaks = c(-Inf, 150, 180, 200, Inf), labels = c("low", "medium", "high", "giant")))

# wypełnianie/usuwanie braków danych
starwars |>
  mutate(hair_color = replace_na(hair_color, "not specified"))

starwars |> drop_na()
