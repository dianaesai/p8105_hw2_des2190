Homework 2
================
Diana Sanchez

This is my solution to HW1.

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

## Problem 1

Read the Mr. Trashwheel data set.

``` r
trashwheel_df=
  read_xlsx(
    "./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", 
    sheet = "Mr. Trash Wheel", 
    range = cell_cols("A:N")) %>%
  janitor::clean_names() %>%
  drop_na(dumpster) %>%
  mutate(
    sports_balls = round(sports_balls),
    sports_balls = as.integer(sports_balls)
  )
```

Read precipitation data for 2018 and 2017.

``` r
precip_2018 = 
  read_xlsx(
    "./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",
    sheet = "2018 Precipitation",
    skip = 1
  ) %>%
  janitor::clean_names() %>%
  drop_na(month) %>%
  mutate (year = 2018) %>%
  relocate(year)

precip_2017 = 
  read_xlsx(
    "./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",
    sheet = "2017 Precipitation",
    skip = 1
  ) %>%
  janitor::clean_names() %>%
  drop_na(month) %>%
  mutate (year = 2017) %>%
  relocate(year)
```

Combine annual precipitation.

``` r
month_df=
  tibble(
    month = 1:12,
    month_name = month.name
  )

precip_df=
  bind_rows(precip_2018, precip_2017)

left_join(precip_df, month_df, by = "month")
```

    ## # A tibble: 24 x 4
    ##     year month total month_name
    ##    <dbl> <dbl> <dbl> <chr>     
    ##  1  2018     1  0.94 January   
    ##  2  2018     2  4.8  February  
    ##  3  2018     3  2.69 March     
    ##  4  2018     4  4.69 April     
    ##  5  2018     5  9.27 May       
    ##  6  2018     6  4.77 June      
    ##  7  2018     7 10.2  July      
    ##  8  2018     8  6.45 August    
    ##  9  2018     9 10.5  September 
    ## 10  2018    10  2.12 October   
    ## # … with 14 more rows

Summarize Mr. Trash Wheel data

This dataset contains information from the MR. Trashwheel trash
collector in Baltimore. As trash enters the inner harbor, the trashweel
collects that trash, and stores it in a dumpstore. The dataset contains
information on year, month, and trash collected, including some specific
kinds of trash. There are a total of 344 rows in our final document.
Additional data sets include month precipitation data. The median number
of sports balls found in a dumpster in 2017 was 8 sports balls. The
total precipitation in 2018 was 70.33 inches.

## Problem 2

Read NYC Transit data.

``` r
transit_df=
  read_csv("./data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv") %>%
  janitor::clean_names() %>%
  select(line, station_name, route1, route2, route3, route4, route5, route6, route7, route8, route9, route10, route11, station_latitude, station_longitude, entry, vending, entrance_type, ada) %>%
  mutate(entry = recode(entry, "YES" = T, "NO" = F))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Station Latitude` = col_double(),
    ##   `Station Longitude` = col_double(),
    ##   Route8 = col_double(),
    ##   Route9 = col_double(),
    ##   Route10 = col_double(),
    ##   Route11 = col_double(),
    ##   ADA = col_logical(),
    ##   `Free Crossover` = col_logical(),
    ##   `Entrance Latitude` = col_double(),
    ##   `Entrance Longitude` = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
stations = distinct(transit_df, line, station_name, ada)
```

The dataset contains transit data for NYC, specifically related to
entrances and exits of each subway station. It includes the variables
line, station\_name, route1, route2, route3, route4, route5, route6,
route7, route8, route9, route10, route11, station\_latitude,
station\_longitude, entry, vending, entrance\_type, and ada. The
dimensions of the data set are 1868 rows x 19 columns.

This is not tidy data.

There are 465 distinct stations.

``` r
stations2=
 mutate(filter (stations, ada == "TRUE"))
```

There are 84 ada compliant stations.

``` r
no_vending=
  mutate(filter (transit_df, vending == "NO"))
```

``` r
allow_entrance= 
  mutate(filter (no_vending, entry == "TRUE"))
```

The proportion of station entrances / exits without vending allow
entrance is 0.3770492.

## Problem 3

Clean pol-month data

``` r
pols_month = 
  read_csv(
    "./data/pols-month.csv") %>%
  separate(mon, into = c("year", "month", "day")) %>%
  mutate(month= as.integer(month))
```

    ## Parsed with column specification:
    ## cols(
    ##   mon = col_date(format = ""),
    ##   prez_gop = col_double(),
    ##   gov_gop = col_double(),
    ##   sen_gop = col_double(),
    ##   rep_gop = col_double(),
    ##   prez_dem = col_double(),
    ##   gov_dem = col_double(),
    ##   sen_dem = col_double(),
    ##   rep_dem = col_double()
    ## )

``` r
month_df=
  tibble(
    month = 01:12,
    month_name = month.name)

pols_month = left_join(pols_month, month_df, by = "month")

polls_month_mutate = pols_month %>%
  mutate(president = ifelse(prez_dem == 1, "dem", "gop"))

polls_month_mutate2 = 
select(polls_month_mutate, - prez_gop, - prez_dem, - month) %>%
relocate (year, month_name)
```

Clean snp data.

``` r
snp = 
  read_csv(
    "./data/snp.csv") %>%
  separate(date, into = c("month", "day", "year")) %>%
  mutate(month= as.integer(month))
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

``` r
month_df=
  tibble(
    month = 01:12,
    month_name = month.name)

snp2 = 
  left_join(snp, month_df, by = "month") %>%
  select(- month) %>%
  relocate (year, month_name)
```

Clean unemployment data.

``` r
unemployment = 
  read_csv(
    "./data/unemployment.csv") %>%
  pivot_longer(
    Jan:Dec,
    names_to = "month",
    values_to = "unemploy"
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Jan = col_double(),
    ##   Feb = col_double(),
    ##   Mar = col_double(),
    ##   Apr = col_double(),
    ##   May = col_double(),
    ##   Jun = col_double(),
    ##   Jul = col_double(),
    ##   Aug = col_double(),
    ##   Sep = col_double(),
    ##   Oct = col_double(),
    ##   Nov = col_double(),
    ##   Dec = col_double()
    ## )

``` r
month_df2=
  tibble(
    month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    month_name = month.name)

unemploy_tidy = 
  left_join(unemployment, month_df2, by = "month") %>%
  select(- month) %>%
  relocate(Year, month_name) %>%
  janitor::clean_names()
```
