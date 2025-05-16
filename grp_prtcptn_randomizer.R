# Table Group & Participation Randomizer ----
# John Spence
# 8/25/2024
library(tidyverse)

# Define rosters ----
econ_a <- c(
  "Arianna",
  "Elliot",
  "Luke",
  "Oliver",
  "Winn",
  "Sarah",
  "Alexis",
  "Johnny",
  "James",
  "Henry",
  "William",
  "Madeline",
  "Casey",
  "Hanako",
  "McKinley",
  "John",
  "Andrew"  
)

econ_d <- c(
  "Annabelle",
  "Tyler",
  "George",
  "Zade",
  "Ted",
  "Winslow",
  "Aiden",
  "Audrey",
  "Ava",
  "Tenzin",
  "Lila",
  "Matthew",
  "Nathaniel",
  "Finnegan",
  "Sophia",
  "Alise"
)

im2_c <- c(
  "Theo",
  "Elliot",
  "Sophie",
  "Alexis",
  "Priya",
  "J.J.",
  "Alex",
  "Caitlin",
  "Quinn",
  "Federico",
  "Ella",
  "Dilan",
  "Jared",
  "Oli",
  "Sukhmit"
)

im2_g <- c(
  "Aliou",
  "Alfie",
  "Anderson",
  "Bobby",
  "Oscar",
  "Arjun",
  "Brynn",
  "Yanick",
  "Tensae",
  "Max",
  "Ariel",
  "Ellie",
  "Phoebe",
  "Bennett",
  "Michael",
  "Theo",
  "Forest"
)

# Table Randomizer ----

tbl_grps <- function(class) {
  
  tbl_n <- rep_along(class, 1:5) |> sample(size = length(class))
  
  df_nmbrd <- tibble(name = class, tbl_n = tbl_n)
  
  df_nmbrd |>
    arrange(tbl_n) |>
    group_by(tbl_n) |>
    mutate(n = row_number(tbl_n)) |>
    pivot_wider(names_from = tbl_n,
                names_prefix = "Table ",
                values_from = name,
                values_fill = "") |>
    select(-n)

}

# Select Table Groups ----
tbl_grps(econ_a)

tbl_grps(econ_d)

tbl_grps(im2_c)

tbl_grps(im2_g)

# Participation randomizer ----
im2_c |> sample(4)
im2_g |> sample(4)
