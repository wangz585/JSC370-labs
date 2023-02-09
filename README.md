Lab 05 - Data Wrangling
Notice: Please go to lab5.html for html output
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

``` r
library("data.table")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(leaflet)
```

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture.

``` r
#fn <- "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz"
if (!file.exists("met_all.gz"))
  download.file(fn, destfile = "met_all.gz")
met <- data.table::fread("met_all.gz")
met <- merge(x = met, y = stations, all.x = T, all.y = F, 
             by.x = "USAFID", by.y = "USAF")
```

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
station_ave <- met[, .(temp = mean(temp, na.rm = T), 
                   wind.sp = mean(wind.sp, na.rm = T), 
                   atm.press = mean(atm.press, na.rm = T),
                   lat = mean(lat, na.rm=T), 
                   lon = mean(lon, na.rm=T)), 
                    by = .(USAFID, STATE)]
station_ave
```

    ##       USAFID STATE     temp  wind.sp atm.press      lat       lon
    ##    1: 690150    CA 33.18763 3.483560  1010.379 34.29982 -116.1658
    ##    2: 720110    TX 31.22003 2.138348       NaN 30.78400  -98.6620
    ##    3: 720113    MI 23.29317 2.470298       NaN 42.54300  -83.1780
    ##    4: 720120    SC 27.01922 2.504692       NaN 32.21746  -80.6998
    ##    5: 720137    IL 21.88823 1.979335       NaN 41.42500  -88.4190
    ##   ---                                                            
    ## 1591: 726777    MT 19.15492 4.673878  1014.299 46.35792 -104.2501
    ## 1592: 726797    MT 18.78980 2.858586  1014.902 45.78795 -111.1600
    ## 1593: 726798    MT 19.47014 4.445783  1014.072 45.69800 -110.4400
    ## 1594: 726810    ID 25.03549 3.039794  1011.730 43.56700 -116.2390
    ## 1595: 726813    ID 23.47809 2.435372  1012.315 43.64963 -116.6331

``` r
medians <- station_ave[, .(temp_50 = quantile(temp, probs = .5, na.rm = T), 
                           wind.sp_50 = quantile(wind.sp, probs = .5, na.rm = T),
                           atm.press_50 = quantile(atm.press, probs = .5, na.rm = T))]
medians
```

    ##     temp_50 wind.sp_50 atm.press_50
    ## 1: 23.68406   2.461838     1014.691

``` r
station_ave %>% 
  mutate_at(vars(temp), function(x) if_else(between(percent_rank(x), .499, .501), x, NA_real_)) %>%
  subset(!is.na(temp))
```

    ##    USAFID STATE     temp  wind.sp atm.press      lat        lon
    ## 1: 720458    KY 23.68173 1.209682       NaN 37.75100  -82.63700
    ## 2: 724066    MD 23.72338 2.462660  1016.077 39.70602  -77.72999
    ## 3: 725515    NE 23.68639 2.709164       NaN 40.30100  -96.75400
    ## 4: 725835    NV 23.67835 2.652381       NaN 40.61141 -116.89023

``` r
station_ave %>% 
  mutate_at(vars(wind.sp), function(x) if_else(between(percent_rank(x), .499, .501), x, NA_real_)) %>%
  subset(!is.na(wind.sp))
```

    ##    USAFID STATE     temp  wind.sp atm.press      lat       lon
    ## 1: 720929    WI 17.43278 2.461838       NaN 45.50600 -91.98100
    ## 2: 724066    MD 23.72338 2.462660  1016.077 39.70602 -77.72999
    ## 3: 725394    MI 20.78056 2.460641  1015.156 42.74599 -86.09702

``` r
station_ave %>% 
  mutate_at(vars(atm.press), function(x) if_else(between(percent_rank(x), .499, .501), x, NA_real_)) %>%
  subset(!is.na(atm.press))
```

    ##    USAFID STATE     temp  wind.sp atm.press      lat       lon
    ## 1: 722238    AL 26.13978 1.472656  1014.691 31.34990 -85.66667
    ## 2: 723200    GA 25.82436 1.537661  1014.692 34.34823 -85.16164

We printed out weather stations of median temperature, wind speed and
atmosphere pressure. Find that weather station 724066 in Maryland has
both median in wind speed and temperature. And we can also see the
location of median wind speed and temperature are closer to each other
comparing to atmosphere pressure.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
# median temp station
station_ave[, temp_dist := abs(temp - quantile(temp, probs = .5, na.rm = T)), by = STATE]
station_ave[, wind.sp_dist := abs(temp - quantile(wind.sp, probs = .5, na.rm = T)), by = STATE]
station_ave[, atm.press_dist := abs(temp - quantile(atm.press, probs = .5, na.rm = T)), by = STATE]

rep_temp_station_state <- station_ave %>%
  group_by(STATE) %>%
  filter(temp_dist == min(temp_dist)) %>%
  filter(lat == min(lat))

# median wind.sp
rep_wind_station_state <- station_ave %>%
  group_by(STATE) %>%
  filter(wind.sp_dist == min(wind.sp_dist)) %>%
  filter(lat == min(lat))

# median atm.press
rep_atm_station_state <- station_ave %>%
  group_by(STATE) %>%
  filter(atm.press_dist == min(atm.press_dist)) %>%
  filter(lat == min(lat))

rep_temp_station_state
```

    ## # A tibble: 42 × 10
    ## # Groups:   STATE [42]
    ##    USAFID STATE  temp wind.sp atm.press   lat    lon temp_dist wind.sp…¹ atm.p…²
    ##     <int> <chr> <dbl>   <dbl>     <dbl> <dbl>  <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 720202 OR     17.2   1.83       NaN   45.4 -124.    0.817        15.2    998.
    ##  2 720254 WA     19.2   1.27       NaN   46.7 -123.    0            18.0     NA 
    ##  3 720284 MI     20.5   1.98       NaN   42.6  -84.8   0            18.2    994.
    ##  4 720328 WV     21.9   1.62       NaN   39    -80.3   0.00374      20.3    994.
    ##  5 720545 CT     22.4   1.90       NaN   41.4  -72.5   0.0798       20.3    992.
    ##  6 720592 AL     26.3   0.784      NaN   30.5  -87.9   0.0213       24.7    989.
    ##  7 720605 SC     25.9   1.39       NaN   34.7  -80.0   0.0682       24.2    989.
    ##  8 720964 FL     27.6   3.19      1016.  30.0  -85.5   0.00372      24.9    988.
    ##  9 722004 ND     18.6   3.43       NaN   46.2  -96.6   0.0728       14.6     NA 
    ## 10 722041 LA     27.8   1.48       NaN   29.4  -90.3   0.0267       26.3    987.
    ## # … with 32 more rows, and abbreviated variable names ¹​wind.sp_dist,
    ## #   ²​atm.press_dist

We stored information median temperature, wind speed and atmosphere
pressure for each state in corresponding data frames. For example,
rep_temp_station_state.

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

- low: temp \< 20
- Mid: temp \>= 20 and temp \< 25
- High: temp \>= 25

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, examine the
  association between median temperature (y) and median wind speed (x).
  Create a scatterplot of the two variables using ggplot2. Add both a
  linear regression line and a smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.
