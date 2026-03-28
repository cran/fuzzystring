## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fuzzystring)

## ----quick-start--------------------------------------------------------------
# Your messy data
x <- data.frame(
  name = c("Idea", "Premiom", "Very Good"), 
  id = 1:3
)

# Reference data
y <- data.frame(
  approx_name = c("Ideal", "Premium", "VeryGood"), 
  grp = c("A", "B", "C")
)

# Fuzzy join with max distance of 2 edits
fuzzystring_inner_join(
  x, y,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)

## ----join-datasets------------------------------------------------------------
x_join <- data.frame(
  name = c("Idea", "Premiom", "Very Good", "Gooood"),
  id = 1:4
)

y_join <- data.frame(
  approx_name = c("Ideal", "Premium", "VeryGood", "Good"),
  grp = c("A", "B", "C", "D")
)

## ----join-inner, eval = TRUE--------------------------------------------------
fuzzystring_inner_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)

## ----join-left, eval = TRUE---------------------------------------------------
fuzzystring_left_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)

## ----join-right, eval = TRUE--------------------------------------------------
fuzzystring_right_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)

## ----join-full, eval = TRUE---------------------------------------------------
fuzzystring_full_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)

## ----join-semi, eval = TRUE---------------------------------------------------
fuzzystring_semi_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2
)

## ----join-anti, eval = TRUE---------------------------------------------------
fuzzystring_anti_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2
)

## ----join-generic, eval = TRUE------------------------------------------------
fuzzystring_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  mode = "left",
  distance_col = "distance"
)

## ----distance-methods, eval = FALSE-------------------------------------------
# # Optimal String Alignment (default)
# fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "osa")
# 
# # Damerau-Levenshtein
# fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "dl")
# 
# # Jaro-Winkler (good for names)
# fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "jw")
# 
# # Soundex (phonetic matching)
# fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "soundex")

## ----ignore-case, eval = FALSE------------------------------------------------
# fuzzystring_inner_join(
#   x, y,
#   by = c(name = "approx_name"),
#   ignore_case = TRUE,
#   max_dist = 1
# )

## ----multi-column, eval = FALSE-----------------------------------------------
# x_multi <- data.frame(
#   first = c("Jon", "Maira"),
#   last = c("Smyth", "Gonzales")
# )
# 
# y_multi <- data.frame(
#   first_ref = c("John", "Maria"),
#   last_ref = c("Smith", "Gonzalez"),
#   customer_id = 1:2
# )
# 
# fuzzystring_inner_join(
#   x_multi, y_multi,
#   by = c(first = "first_ref", last = "last_ref"),
#   method = "osa",
#   max_dist = 1
# )

