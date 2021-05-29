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

It seems `tidytransit` doesn't do too much more than gtfsio: just converts to tibble, convert dates and times, adds validation results and an empty `.` slot. However, those conversions rely on the table objects inheriting from `data.table`.

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
    ## <bytecode: 0x000000001994a480>
    ## <environment: namespace:tidytransit>

## Exporting as valid GTFS files

It might be quite a bit of work to create valid `tidygtfs` objects. It could be worth it if we want to use any of the `tidytransit` functions for analysis, but for just exporting to GTFS files for OTP use, it might be easier just to adapt a simplified version of the code from `gtfsio::export_gtfs`:

``` r
gtfsio::export_gtfs
```

    ## function (gtfs, path, files = NULL, standard_only = FALSE, compression_level = 9, 
    ##     as_dir = FALSE, overwrite = TRUE, quiet = TRUE) 
    ## {
    ##     gtfs_standards <- get_gtfs_standards()
    ##     if (!inherits(gtfs, "gtfs")) 
    ##         stop("'gtfs' must inherit from the 'gtfs' class.")
    ##     if (!is.character(path) | length(path) != 1) 
    ##         stop("'path' must be a string (a character vector of length 1).")
    ##     if (path == tempdir()) 
    ##         stop(paste0("Please use 'tempfile()' instead of 'tempdir()' to designate ", 
    ##             "temporary directories."))
    ##     if (!is.null(files) & !is.character(files)) 
    ##         stop("'files' must either be a character vector or NULL.")
    ##     if (!is.logical(standard_only) | length(standard_only) != 
    ##         1) 
    ##         stop("'standard_only' must be a logical vector of length 1.")
    ##     if (!is.numeric(compression_level) | length(compression_level) != 
    ##         1) 
    ##         stop("'compression_level' must be a numeric vector of length 1.")
    ##     if (!is.logical(as_dir) | length(as_dir) != 1) 
    ##         stop("'as_dir' must be a logical vector of length 1.")
    ##     if (!is.logical(overwrite) | length(overwrite) != 1) 
    ##         stop("'overwrite' must be a logical vector of length 1.")
    ##     if (!is.logical(quiet) | length(quiet) != 1) 
    ##         stop("'quiet' must be a logical vector of length 1.")
    ##     if (file.exists(path) & !overwrite) 
    ##         stop("'path' points to an existing file/directory, ", 
    ##             "but 'overwrite' is set to FALSE.")
    ##     if (!as_dir & !grepl("\\.zip$", path)) 
    ##         stop("'path' must have '.zip' extension. ", "If you meant to create a directory please set 'as_dir' to TRUE.")
    ##     if (as_dir & grepl("\\.zip$", path)) {
    ##         stop("'path' cannot have '.zip' extension when 'as_dir' is TRUE.")
    ##     }
    ##     extra_files <- setdiff(files, names(gtfs_standards))
    ##     if (standard_only & !is.null(files) & !identical(extra_files, 
    ##         character(0))) 
    ##         stop("Non-standard file specified in 'files', ", "even though 'standard_only' is set to TRUE: ", 
    ##             paste0("'", extra_files, "'", collapse = ", "))
    ##     if (is.null(files)) 
    ##         files <- names(gtfs)
    ##     files <- setdiff(files, ".")
    ##     extra_files <- setdiff(files, names(gtfs_standards))
    ##     if (standard_only) 
    ##         files <- setdiff(files, extra_files)
    ##     missing_files <- setdiff(files, names(gtfs))
    ##     if (!identical(missing_files, character(0))) 
    ##         stop("The provided GTFS object does not contain the following ", 
    ##             "elements specified in 'files': ", paste0("'", missing_files, 
    ##                 "'", collapse = ", "))
    ##     if (as_dir) 
    ##         tmpd <- path
    ##     else tmpd <- tempfile(pattern = "gtfsio")
    ##     unlink(tmpd, recursive = TRUE)
    ##     dir.create(tmpd)
    ##     if (!quiet) 
    ##         message("Writing text files to ", tmpd)
    ##     for (file in files) {
    ##         filename <- paste0(file, ".txt")
    ##         filepath <- file.path(tmpd, filename)
    ##         if (!quiet) 
    ##             message("  - Writing ", filename)
    ##         dt <- gtfs[[file]]
    ##         if (standard_only) {
    ##             file_cols <- names(dt)
    ##             extra_cols <- setdiff(file_cols, names(gtfs_standards[[file]]))
    ##             if (!identical(extra_cols, character(0))) 
    ##                 dt <- dt[, !..extra_cols]
    ##         }
    ##         data.table::fwrite(dt, filepath)
    ##     }
    ##     if (!as_dir) {
    ##         unlink(path, recursive = TRUE)
    ##         filepaths <- file.path(tmpd, paste0(files, ".txt"))
    ##         zip::zip(path, filepaths, compression_level = compression_level, 
    ##             mode = "cherry-pick")
    ##         if (!quiet) 
    ##             message("GTFS object successfully zipped to ", path)
    ##     }
    ##     return(invisible(gtfs))
    ## }
    ## <bytecode: 0x000000002427c7d8>
    ## <environment: namespace:gtfsio>

Might it be easier to use `readr::write_csv()` rather than `data.table::fwrite()`?

### Important notes:

-   Dates in GTFS files need to be written as `yyyymmdd` text: beware of databases converting these to Date object-style integers.
-   GTFS times are written as `hh:mm:ss` text: beware of conversions to `hms` objects.
