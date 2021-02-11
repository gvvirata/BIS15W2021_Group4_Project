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
plants <- read_csv(here("plantInfo-clean.csv")) %>%
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
##  1 Amar… Love-lies-ble… Sow in garden. … Space plants: 2… Harvest in 7-8 …
##  2 Ange… <NA>           Easy to grow. S… Space plants: 1… Harvest in appr…
##  3 Arti… <NA>           Easy to grow. S… Space plants: 6… Harvest in 42-5…
##  4 Aspa… <NA>           Easy to grow. P… Space plants: 8… Harvest in 2-3 …
##  5 Aspa… Winged bean    Easy to grow. S… Space plants: 8… Harvest in 8-11…
##  6 Basil <NA>           Grow in seed tr… Space plants: 8… Harvest in 10-1…
##  7 Beet… Beets          Easy to grow. S… Space plants: 8… Harvest in 7-10…
##  8 Bora… Burrage, Bugl… Easy to grow. S… Space plants: 8… Harvest in 8-10…
##  9 Broa… Fava bean      Easy to grow. S… Space plants: 6… Harvest in 12-2…
## 10 Broc… <NA>           Easy to grow. G… Space plants: 1… Harvest in 10-1…
## # … with 82 more rows, and 5 more variables: compatible_plants <chr>,
## #   avoid_instructions <chr>, culinary_hints <chr>,
## #   culinary_preservation <chr>, url <chr>
```


```r
plants %>%
  separate(alternate_name, into = c("alt_name_1", "alt_name_2"), sep = ",") %>%
  separate(space_instructions, into = c("delete", "value"), sep = ":") %>%
  separate(value, into = c("min_in_apart", "max_in_apart"), sep = "-") %>%
  separate(min_in_apart, into = c("delete_2", "min_in_apart", "delete_3"), sep = " ") %>%
  separate(max_in_apart, into = c("delete_4", "max_in_apart", "delete_5"), sep = " ") %>%
  separate(harvest_instructions, into = c("delete_6", "delete_7", "harvest_time_wks", "delete_8"), sep = " ") %>%
  select(-"delete", -"delete_2", -"delete_3", -"delete_4", -"delete_5", -"delete_6", -"delete_7") %>%
  separate(harvest_time_wks, into = c("min_harvest_time_wks", "max_harvest_time_wks"), sep = "-")
```

```
## Warning: Expected 2 pieces. Additional pieces discarded in 6 rows [25, 26, 51,
## 75, 77, 78].
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 33 rows [1, 5, 7,
## 9, 12, 22, 24, 29, 35, 37, 42, 43, 46, 50, 52, 55, 56, 57, 64, 65, ...].
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 27 rows [1, 2, 8,
## 12, 14, 23, 24, 25, 28, 30, 32, 33, 37, 40, 41, 46, 50, 55, 63, 66, ...].
```

```
## Warning: Expected 3 pieces. Additional pieces discarded in 26 rows [1, 2, 8, 12,
## 14, 23, 24, 25, 28, 30, 32, 33, 37, 40, 41, 46, 50, 55, 63, 66, ...].
```

```
## Warning: Expected 3 pieces. Additional pieces discarded in 65 rows [3, 4, 5, 6,
## 7, 9, 10, 11, 13, 15, 16, 17, 18, 19, 20, 21, 22, 26, 27, 29, ...].
```

```
## Warning: Expected 4 pieces. Additional pieces discarded in 45 rows [2, 4, 5, 6,
## 8, 9, 10, 11, 15, 21, 22, 23, 25, 31, 32, 33, 34, 35, 38, 40, ...].
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 10 rows [2, 25,
## 40, 63, 67, 69, 79, 80, 85, 91].
```

```
## # A tibble: 92 x 14
##    name  alt_name_1 alt_name_2 sow_instructions min_in_apart max_in_apart
##    <chr> <chr>      <chr>      <chr>            <chr>        <chr>       
##  1 Amar… Love-lies…  <NA>      Sow in garden. … 20           <NA>        
##  2 Ange… <NA>        <NA>      Easy to grow. S… 18           <NA>        
##  3 Arti… <NA>        <NA>      Easy to grow. S… 63           79          
##  4 Aspa… <NA>        <NA>      Easy to grow. P… 8            16          
##  5 Aspa… Winged be…  <NA>      Easy to grow. S… 8            10          
##  6 Basil <NA>        <NA>      Grow in seed tr… 8            10          
##  7 Beet… Beets       <NA>      Easy to grow. S… 8            12          
##  8 Bora… Burrage    " Bugloss" Easy to grow. S… 8            <NA>        
##  9 Broa… Fava bean   <NA>      Easy to grow. S… 6            10          
## 10 Broc… <NA>        <NA>      Easy to grow. G… 14           20          
## # … with 82 more rows, and 8 more variables: min_harvest_time_wks <chr>,
## #   max_harvest_time_wks <chr>, delete_8 <chr>, compatible_plants <chr>,
## #   avoid_instructions <chr>, culinary_hints <chr>,
## #   culinary_preservation <chr>, url <chr>
```


