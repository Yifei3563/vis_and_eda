Vis I
================
Yifei Yu
2024-10-04

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Import the weather data

``` r
weather_df = 
  read_csv("nynoaadat.csv")
```

    ## Rows: 2595176 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): id
    ## dbl  (5): prcp, snow, snwd, tmax, tmin
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
weather_df = 
  read_csv("nynoaadat.csv") |> 
  filter(
    id %in% c("USW00094728", "USW00022534", "USS0023B17S"), 
    date >= "2021-01-01", 
    date <= "2022-12-31"
  ) |> 
  mutate(
    name = case_when(
      id == "USW00094728" ~ "CentralPark_NY",
      id == "USW00022534" ~ "Molokai_HI",
      id == "USS0023B17S" ~ "Waterhole_WA"
    ),
    tmin = tmin / 10,
    tmax = tmax / 10
  ) |> 
  select(name, id, everything())
```

    ## Rows: 2595176 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): id
    ## dbl  (5): prcp, snow, snwd, tmax, tmin
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
weather_df = 
  read_csv("weather_df.csv") |> 
  filter(
    id %in% c("USW00094728", "USW00022534", "USS0023B17S"), 
    
    date >= "2021-01-01", 
    date <= "2022-12-31"
  ) |> 
  mutate(
    name = case_match(
      id, 
      "USW00094728" ~ "CentralPark_NY", 
      "USW00022534" ~ "Molokai_HI",
      "USS0023B17S" ~ "Waterhole_WA"),
    tmin = tmin,
    tmax = tmax) |>
  select(name, id, date, prcp, tmin, tmax) 
```

    ## Rows: 2190 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): name, id
    ## dbl  (3): prcp, tmax, tmin
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Making our first plot.

``` r
ggplot(weather_df, aes(x = tmin, y = tmax)) +
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
weather_df |> 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](vis_1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggp_weather_scatterplot = 
  weather_df |> 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_point()
```
