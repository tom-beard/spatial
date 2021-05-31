Building gtfs and tidygtfs objects
================

## Load test GTFS

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.6     v dplyr   1.0.3
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidytransit)
```

    ## Warning: package 'tidytransit' was built under R version 4.0.5

``` r
local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
gtfs <- read_gtfs(local_gtfs_path)
names(gtfs)
```

    ##  [1] "trips"          "stop_times"     "agency"         "calendar"      
    ##  [5] "calendar_dates" "stops"          "routes"         "shapes"        
    ##  [9] "transfers"      "."

## Explore GTFS tables.

The `.` sublist is for storing additional tables (additional to what? anything not strictly required?)

``` r
gtfs$.
```

    ## $dates_services
    ## # A tibble: 3,349 x 2
    ##    date       service_id             
    ##    <date>     <chr>                  
    ##  1 2018-06-24 ASP18GEN-1037-Sunday-00
    ##  2 2018-06-24 ASP18GEN-2048-Sunday-00
    ##  3 2018-06-24 ASP18GEN-3041-Sunday-00
    ##  4 2018-06-24 ASP18GEN-4049-Sunday-00
    ##  5 2018-06-24 ASP18GEN-5048-Sunday-00
    ##  6 2018-06-24 ASP18GEN-6030-Sunday-00
    ##  7 2018-06-24 ASP18GEN-7025-Sunday-00
    ##  8 2018-06-24 BSP18GEN-A051-Sunday-00
    ##  9 2018-06-24 BSP18GEN-C026-Sunday-00
    ## 10 2018-06-24 BSP18GEN-D040-Sunday-00
    ## # ... with 3,339 more rows

``` r
gtfs$routes %>% class()
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

``` r
class(gtfs)
```

    ## [1] "tidygtfs" "gtfs"

These are standard tibbles, not data.table as used by gtfsio for reading. The gtfs object has class `tidygtfs` as well as inheriting from `gtfs`.

## Try creating a new tidygtfs object

``` r
txt_files <- list(
  agency = gtfs$agency,
  stops = gtfs$stops,
  routes = gtfs$routes,
  trips = gtfs$trips,
  stop_times = gtfs$stop_times,
  calendar = gtfs$calendar
)

new_gtfs <- gtfsio::new_gtfs(txt_files)
class(new_gtfs)
```

    ## [1] "gtfs"

This is *just* a `gtfs` object, without the `tidygtfs` class. But it still validates okay:

``` r
validate_gtfs(new_gtfs)
```

    ## # A tibble: 136 x 8
    ##    file  file_spec file_provided_s~ field field_spec field_provided_~
    ##    <chr> <chr>     <lgl>            <chr> <chr>      <lgl>           
    ##  1 agen~ req       TRUE             agen~ opt        TRUE            
    ##  2 agen~ req       TRUE             agen~ req        TRUE            
    ##  3 agen~ req       TRUE             agen~ req        TRUE            
    ##  4 agen~ req       TRUE             agen~ req        TRUE            
    ##  5 agen~ req       TRUE             agen~ opt        TRUE            
    ##  6 agen~ req       TRUE             agen~ opt        TRUE            
    ##  7 agen~ req       TRUE             agen~ opt        FALSE           
    ##  8 agen~ req       TRUE             agen~ opt        FALSE           
    ##  9 stops req       TRUE             stop~ req        TRUE            
    ## 10 stops req       TRUE             stop~ opt        TRUE            
    ## # ... with 126 more rows, and 2 more variables: validation_status <chr>,
    ## #   validation_details <chr>

``` r
read_gtfs
```

    ## function (path, files = NULL, quiet = TRUE) 
    ## {
    ##     g = gtfsio::import_gtfs(path, files = NULL, quiet = quiet)
    ##     validation_result <- validate_gtfs(g)
    ##     g$. <- list()
    ##     g <- convert_times_to_hms(g)
    ##     g <- convert_dates(g)
    ##     g <- set_dates_services(g)
    ##     g[names(g) != "."] <- lapply(g[names(g) != "."], dplyr::as_tibble)
    ##     g <- gtfsio::new_gtfs(g)
    ##     class(g) <- c("tidygtfs", "gtfs")
    ##     attributes(g)$validation_result <- validation_result
    ##     g
    ## }
    ## <bytecode: 0x0000000019934db0>
    ## <environment: namespace:tidytransit>

It seems `tidytransit` doesn't do too much more than gtfsio: just converts to tibble, convert dates and times, adds validation results and an empty `.` slot. However, those conversions rely on the table objects inheriting from `data.table`.

Let's try this on our `gtfs` object.

``` r
g  <- gtfs
validation_result <- validate_gtfs(g)
g$. <- list()
# g <- tidytransit:::convert_times_to_hms(g)
# g <- tidytransit:::convert_dates(g)
# g <- tidytransit:::set_dates_services(g)
# g[names(g) != "."] <- lapply(g[names(g) != "."], 
#     dplyr::as_tibble)
# g <- gtfsio::new_gtfs(g)
# class(g) <- c("tidygtfs", "gtfs")
# attributes(g)$validation_result <- validation_result
# g
```

This fails at `tidytransit:::convert_times_to_hms(g) : inherits(gtfs_obj$stop_times, "data.table") is not TRUE`. Let's try converting to `data.table` first.

``` r
g  <- gtfs
validation_result <- validate_gtfs(g)
g$. <- list()
g[names(g) != "."] <- lapply(g[names(g) != "."],
    data.table::as.data.table)
g <- tidytransit:::convert_times_to_hms(g)
g <- tidytransit:::convert_dates(g)
g <- tidytransit:::set_dates_services(g)
```

    ## Warning in tidytransit:::set_dates_services(g): No valid dates defined in feed

``` r
g[names(g) != "."] <- lapply(g[names(g) != "."],
    dplyr::as_tibble)
g <- gtfsio::new_gtfs(g)
class(g) <- c("tidygtfs", "gtfs")
attributes(g)$validation_result <- validation_result
class(g)
```

    ## [1] "tidygtfs" "gtfs"

This seems to work! Let's make it into a function.

``` r
make_tidygtfs <- function(gtfs_list) {
  g <- gtfsio::new_gtfs(gtfs_list)
  validation_result <- validate_gtfs(g)
  g$. <- list()
  g[names(g) != "."] <- lapply(g[names(g) != "."],
      data.table::as.data.table)
  g <- tidytransit:::convert_times_to_hms(g)
  g <- tidytransit:::convert_dates(g)
  g <- tidytransit:::set_dates_services(g)
  g[names(g) != "."] <- lapply(g[names(g) != "."],
      dplyr::as_tibble)
  g <- gtfsio::new_gtfs(g)
  class(g) <- c("tidygtfs", "gtfs")
  attributes(g)$validation_result <- validation_result
  g
}

test_txt_files <- list(
  agency = gtfs$agency,
  stops = gtfs$stops,
  routes = gtfs$routes,
  trips = gtfs$trips,
  stop_times = gtfs$stop_times,
  calendar = gtfs$calendar
)

test_gtfs <- make_tidygtfs(test_txt_files)
```

    ## Warning in tidytransit:::set_dates_services(g): No valid dates defined in feed

``` r
class(test_gtfs)
```

    ## [1] "tidygtfs" "gtfs"

## Exporting as valid GTFS files

It could be worth doing this to create valid `tidygtfs` objects if we want to use any of the `tidytransit` functions for analysis, but it might be too much for just exporting to GTFS files for OTP use. In that case it might be easier just to adapt a simplified version of the code from `gtfsio::export_gtfs`.

Just as well `tidygtfs` writing seems to work okay:

``` r
# gtfsio::export_gtfs
write_gtfs(test_gtfs, "test_gtfs_export.zip")
gtfs_from_new_file <- read_gtfs("test_gtfs_export.zip")
```

    ## Warning in set_dates_services(g): No valid dates defined in feed

``` r
names(gtfs_from_new_file)
```

    ## [1] "agency"     "stops"      "routes"     "trips"      "stop_times"
    ## [6] "calendar"   "."

``` r
attributes(gtfs_from_new_file)$validation_result
```

    ## # A tibble: 136 x 8
    ##    file  file_spec file_provided_s~ field field_spec field_provided_~
    ##    <chr> <chr>     <lgl>            <chr> <chr>      <lgl>           
    ##  1 agen~ req       TRUE             agen~ opt        TRUE            
    ##  2 agen~ req       TRUE             agen~ req        TRUE            
    ##  3 agen~ req       TRUE             agen~ req        TRUE            
    ##  4 agen~ req       TRUE             agen~ req        TRUE            
    ##  5 agen~ req       TRUE             agen~ opt        TRUE            
    ##  6 agen~ req       TRUE             agen~ opt        TRUE            
    ##  7 agen~ req       TRUE             agen~ opt        FALSE           
    ##  8 agen~ req       TRUE             agen~ opt        FALSE           
    ##  9 stops req       TRUE             stop~ req        TRUE            
    ## 10 stops req       TRUE             stop~ opt        TRUE            
    ## # ... with 126 more rows, and 2 more variables: validation_status <chr>,
    ## #   validation_details <chr>

### Important notes:

-   ~~Dates in GTFS files need to be written as `yyyymmdd` text: beware of databases converting these to Date object-style integers.~~
-   ~~GTFS times are written as `hh:mm:ss` text: beware of conversions to `hms` objects.~~
-   We probably also want shapes to be included in a gtfs database.
