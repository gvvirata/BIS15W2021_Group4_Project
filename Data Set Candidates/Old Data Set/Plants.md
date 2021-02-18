---
title: "Title"
author: ""
date: "2021-02-11"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---




```r
#if there's anymore libraries we might need, include them here :)
library(tidyverse)
library(RColorBrewer)
library(paletteer)
library(janitor)
library(here)
library(naniar)
```


```r
plants <- read_csv(here("plantInfo-clean-new.csv")) %>%
  janitor::clean_names()
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   Name = col_character(),
##   alternateName = col_character(),
##   sowInstructions = col_character(),
##   spaceInstructions = col_character(),
##   harvestInstructions = col_character(),
##   compatiblePlants = col_character(),
##   avoidInstructions = col_character(),
##   culinaryHints = col_character(),
##   culinaryPreservation = col_character(),
##   url = col_character()
## )
```

```r
plants
```

```
## # A tibble: 92 x 10
##    name  alternate_name sow_instructions space_instructi… harvest_instruc…
##    <chr> <chr>          <chr>            <chr>            <chr>           
##  1 Amar… Love-lies-ble… Sow in garden~ … Space plants: 2… Harvest in 7-8 …
##  2 Ange… <NA>           Sow in garden~ … Space plants: 1… Harvest in 18 m…
##  3 Arti… <NA>           Sow in garden~ … Space plants: 6… Harvest in 42-5…
##  4 Aspa… <NA>           ~ Plant as crow… Space plants: 8… Harvest in 2-3 …
##  5 Aspa… Winged bean    Sow in garden~ … Space plants: 8… Harvest in 8-11…
##  6 Basil <NA>           Grow in seed tr… Space plants: 8… Harvest in 10-1…
##  7 Beet… Beets          Sow in garden~ … Space plants: 8… Harvest in 7-10…
##  8 Bora… Burrage, Bugl… Sow in garden~ … Space plants: 8… Harvest in 8-10…
##  9 Broa… Fava bean      Sow in garden~ … Space plants: 6… Harvest in 12-2…
## 10 Broc… <NA>           Grow in seed tr… Space plants: 1… Harvest in 10-1…
## # … with 82 more rows, and 5 more variables: compatible_plants <chr>,
## #   avoid_instructions <chr>, culinary_hints <chr>,
## #   culinary_preservation <chr>, url <chr>
```


```r
plants %>%
  separate(alternate_name, into = c("alt_name_1", "alt_name_2", "alt_name_3"), sep = ",") %>%
  separate(space_instructions, into = c("delete", "space"), sep = ":") %>%
  separate(space, into = c("min_space_apart", "max_space_apart"), sep = "-") %>%
  separate(min_space_apart, into = c("delete2", "min_space_apart", "unit_of_mzr_min"), sep = " ") %>%
  separate(max_space_apart, into = c("delete4", "max_space_apart", "unit_of_mzr_max"), sep = " ") %>%
  separate(harvest_instructions, into = c("delete5", "delete6", "time", "unit_of_time"), sep = " ") %>%
  separate(unit_of_time, into = c("unit_of_time", "delete7"), sep = " ") %>%
  separate(time, into = c("min_harvest_time", "max_harvest_time"), sep = "-") %>%
  separate(compatible_plants, into = c("delete9", "compatible_with"), sep = ":") %>%
  separate(avoid_instructions, into = c("delete10", "avoid_growing_near"), sep = ":") %>%
  separate(culinary_hints, into = c("culinary_hints_1", "culinary_hints_2", "culinary_hints_3"), sep = "/") %>%
  separate(culinary_preservation, into = c("cul_pres_1", "cul_pres_2"), sep = "/") %>%
  separate(sow_instructions, into = c("where_to_grow", "sow_step_1", "sow_step_2", "sow_step_3"), sep = "~") %>%
  separate(where_to_grow, into = c("where_to_grow", "delete3"), sep = ",") %>%
  select(-"delete", -"delete2", -"delete3", -"delete4", -"delete5", -"delete6", -"delete7", -"delete9", -"delete10")
```

```
## Warning: Expected 3 pieces. Additional pieces discarded in 1 rows [25].
```

```
## Warning: Expected 3 pieces. Missing pieces filled with `NA` in 47 rows [1, 5, 7,
## 8, 9, 12, 14, 15, 21, 22, 23, 24, 27, 28, 29, 30, 32, 34, 35, 37, ...].
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 27 rows [1, 2, 8,
## 12, 14, 23, 24, 25, 28, 30, 32, 33, 37, 40, 41, 46, 50, 55, 63, 66, ...].
```

```
## Warning: Expected 3 pieces. Additional pieces discarded in 24 rows [1, 2, 8, 12,
## 14, 23, 24, 25, 30, 32, 33, 40, 41, 46, 50, 55, 63, 66, 69, 71, ...].
```

```
## Warning: Expected 3 pieces. Additional pieces discarded in 65 rows [3, 4, 5, 6,
## 7, 9, 10, 11, 13, 15, 16, 17, 18, 19, 20, 21, 22, 26, 27, 29, ...].
```

```
## Warning: Expected 4 pieces. Additional pieces discarded in 91 rows [1, 3, 4, 5,
## 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, ...].
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 92 rows [1, 2, 3,
## 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 10 rows [2, 25,
## 40, 63, 67, 69, 79, 80, 85, 91].
```

```
## Warning: Expected 3 pieces. Additional pieces discarded in 38 rows [3, 4, 6, 9,
## 10, 11, 12, 13, 14, 16, 17, 18, 19, 21, 23, 25, 33, 35, 37, 38, ...].
```

```
## Warning: Expected 3 pieces. Missing pieces filled with `NA` in 20 rows [5, 20,
## 24, 26, 27, 28, 29, 34, 39, 41, 47, 49, 53, 55, 56, 64, 72, 75, 81, 88].
```

```
## Warning: Expected 2 pieces. Additional pieces discarded in 1 rows [9].
```

```
## Warning: Expected 4 pieces. Additional pieces discarded in 1 rows [30].
```

```
## Warning: Expected 4 pieces. Missing pieces filled with `NA` in 88 rows [1, 2, 3,
## 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 88 rows [1, 2, 3,
## 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, ...].
```

```
## # A tibble: 92 x 23
##    name  alt_name_1 alt_name_2 alt_name_3 where_to_grow sow_step_1 sow_step_2
##    <chr> <chr>      <chr>      <chr>      <chr>         <chr>      <chr>     
##  1 Amar… Love-lies…  <NA>      <NA>       "Sow in gard… " Sow see… " Best pl…
##  2 Ange… <NA>        <NA>      <NA>       "Sow in gard… " Sow see… " Best pl…
##  3 Arti… <NA>        <NA>      <NA>       "Sow in gard… " Sow see… " Best pl…
##  4 Aspa… <NA>        <NA>      <NA>       ""            " Plant a… " Best pl…
##  5 Aspa… Winged be…  <NA>      <NA>       "Sow in gard… " Sow see… " Best pl…
##  6 Basil <NA>        <NA>      <NA>       "Grow in see… " Sow see… " Best pl…
##  7 Beet… Beets       <NA>      <NA>       "Sow in gard… " Sow see… " Best pl…
##  8 Bora… Burrage    " Bugloss" <NA>       "Sow in gard… " Sow see… " Best pl…
##  9 Broa… Fava bean   <NA>      <NA>       "Sow in gard… " Sow see… " Best pl…
## 10 Broc… <NA>        <NA>      <NA>       "Grow in see… " Sow see… " Best pl…
## # … with 82 more rows, and 16 more variables: sow_step_3 <chr>,
## #   min_space_apart <chr>, unit_of_mzr_min <chr>, max_space_apart <chr>,
## #   unit_of_mzr_max <chr>, min_harvest_time <chr>, max_harvest_time <chr>,
## #   unit_of_time <chr>, compatible_with <chr>, avoid_growing_near <chr>,
## #   culinary_hints_1 <chr>, culinary_hints_2 <chr>, culinary_hints_3 <chr>,
## #   cul_pres_1 <chr>, cul_pres_2 <chr>, url <chr>
```

