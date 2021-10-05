Bellabeat case study
================
Chuks Chinedu
10/5/2021

About the company

Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that
manufactures health-focused smart products. Sršen used her background as
an artist to develop beautifully designed technology that informs and
inspires women around the world. Collecting data on activity, sleep,
stress, and reproductive health has allowed Bellabeat to empower women
with knowledge about their own health and habits. Since it was founded
in 2013, Bellabeat has grown rapidly and quickly positioned itself as a
tech-driven wellness company for women.

**ASK**

Business task: Analyze a smart device fitness data (non-bellabeat
product) to help bellabeat unlock new growth opportunities for the
company.

Key stakeholders

Urška Sršen, Sando Mur, Bellabeat marketing analysis team.

**PREPARE**

Data has been downloaded and stored with appropriate file naming
conventions. The data is being organized in a wide row and column
format. Data was gotten from a public domain in which 30 eligible Fitbit
user consented to the submission of personal tracker data. Since this
data is being used in a case study, we can assume the data source is
credible.

**PROCESS**

For this analysis I’ll be using R. I am using this tool because R can
clean, analyze, and visualize the data.

Load the needed packages.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
```

Import the datasets

``` r
activity <- read_csv("dailyActivity_merged.csv")
```

    ## Rows: 940 Columns: 15

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): ActivityDate
    ## dbl (14): Id, TotalSteps, TotalDistance, TrackerDistance, LoggedActivitiesDi...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sleep <- read_csv("sleepDay_merged.csv")
```

    ## Rows: 413 Columns: 5

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): SleepDay
    ## dbl (4): Id, TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
weight <- read_csv("weightLogInfo_merged.csv")
```

    ## Rows: 67 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Date
    ## dbl (6): Id, WeightKg, WeightPounds, Fat, BMI, LogId
    ## lgl (1): IsManualReport

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(activity)
```

    ## # A tibble: 6 × 15
    ##           Id ActivityDate TotalSteps TotalDistance TrackerDistance LoggedActivitie…
    ##        <dbl> <chr>             <dbl>         <dbl>           <dbl>            <dbl>
    ## 1 1503960366 4/12/2016         13162          8.5             8.5                 0
    ## 2 1503960366 4/13/2016         10735          6.97            6.97                0
    ## 3 1503960366 4/14/2016         10460          6.74            6.74                0
    ## 4 1503960366 4/15/2016          9762          6.28            6.28                0
    ## 5 1503960366 4/16/2016         12669          8.16            8.16                0
    ## 6 1503960366 4/17/2016          9705          6.48            6.48                0
    ## # … with 9 more variables: VeryActiveDistance <dbl>,
    ## #   ModeratelyActiveDistance <dbl>, LightActiveDistance <dbl>,
    ## #   SedentaryActiveDistance <dbl>, VeryActiveMinutes <dbl>,
    ## #   FairlyActiveMinutes <dbl>, LightlyActiveMinutes <dbl>,
    ## #   SedentaryMinutes <dbl>, Calories <dbl>

Fix errors associated with time formatting

``` r
activity <- activity %>% 
  rename (Date = ActivityDate) %>% 
  mutate (Date = as.Date(Date, format = "%m/%d/%y"))
sleep <- sleep %>%
  rename (Date = SleepDay) %>%
  mutate (Date = as.Date(Date, format = "%m/%d/%y"))
weight <- weight %>%
  mutate (Date = as.Date(Date, format = "%m/%d/%y"))
```

``` r
n_distinct(activity$Id)
```

    ## [1] 33

``` r
n_distinct(sleep$Id)
```

    ## [1] 24

``` r
n_distinct(weight$Id)
```

    ## [1] 8

``` r
merged_table <- merge(merge(activity, sleep, by = c('Id', 'Date'), all = TRUE), weight, by = c('Id', 'Date'), all = TRUE)
```

``` r
head(merged_table)
```

    ##           Id       Date TotalSteps TotalDistance TrackerDistance
    ## 1 1503960366 2020-04-12      13162          8.50            8.50
    ## 2 1503960366 2020-04-13      10735          6.97            6.97
    ## 3 1503960366 2020-04-14      10460          6.74            6.74
    ## 4 1503960366 2020-04-15       9762          6.28            6.28
    ## 5 1503960366 2020-04-16      12669          8.16            8.16
    ## 6 1503960366 2020-04-17       9705          6.48            6.48
    ##   LoggedActivitiesDistance VeryActiveDistance ModeratelyActiveDistance
    ## 1                        0               1.88                     0.55
    ## 2                        0               1.57                     0.69
    ## 3                        0               2.44                     0.40
    ## 4                        0               2.14                     1.26
    ## 5                        0               2.71                     0.41
    ## 6                        0               3.19                     0.78
    ##   LightActiveDistance SedentaryActiveDistance VeryActiveMinutes
    ## 1                6.06                       0                25
    ## 2                4.71                       0                21
    ## 3                3.91                       0                30
    ## 4                2.83                       0                29
    ## 5                5.04                       0                36
    ## 6                2.51                       0                38
    ##   FairlyActiveMinutes LightlyActiveMinutes SedentaryMinutes Calories
    ## 1                  13                  328              728     1985
    ## 2                  19                  217              776     1797
    ## 3                  11                  181             1218     1776
    ## 4                  34                  209              726     1745
    ## 5                  10                  221              773     1863
    ## 6                  20                  164              539     1728
    ##   TotalSleepRecords TotalMinutesAsleep TotalTimeInBed WeightKg WeightPounds Fat
    ## 1                 1                327            346       NA           NA  NA
    ## 2                 2                384            407       NA           NA  NA
    ## 3                NA                 NA             NA       NA           NA  NA
    ## 4                 1                412            442       NA           NA  NA
    ## 5                 2                340            367       NA           NA  NA
    ## 6                 1                700            712       NA           NA  NA
    ##   BMI IsManualReport LogId
    ## 1  NA             NA    NA
    ## 2  NA             NA    NA
    ## 3  NA             NA    NA
    ## 4  NA             NA    NA
    ## 5  NA             NA    NA
    ## 6  NA             NA    NA

``` r
summary(merged_table)
```

    ##        Id                 Date              TotalSteps    TotalDistance   
    ##  Min.   :1.504e+09   Min.   :2020-04-12   Min.   :    0   Min.   : 0.000  
    ##  1st Qu.:2.320e+09   1st Qu.:2020-04-19   1st Qu.: 3795   1st Qu.: 2.620  
    ##  Median :4.445e+09   Median :2020-04-26   Median : 7439   Median : 5.260  
    ##  Mean   :4.858e+09   Mean   :2020-04-26   Mean   : 7652   Mean   : 5.503  
    ##  3rd Qu.:6.962e+09   3rd Qu.:2020-05-04   3rd Qu.:10734   3rd Qu.: 7.720  
    ##  Max.   :8.878e+09   Max.   :2020-05-12   Max.   :36019   Max.   :28.030  
    ##                                                                           
    ##  TrackerDistance  LoggedActivitiesDistance VeryActiveDistance
    ##  Min.   : 0.000   Min.   :0.000            Min.   : 0.000    
    ##  1st Qu.: 2.620   1st Qu.:0.000            1st Qu.: 0.000    
    ##  Median : 5.260   Median :0.000            Median : 0.220    
    ##  Mean   : 5.489   Mean   :0.110            Mean   : 1.504    
    ##  3rd Qu.: 7.715   3rd Qu.:0.000            3rd Qu.: 2.065    
    ##  Max.   :28.030   Max.   :4.942            Max.   :21.920    
    ##                                                              
    ##  ModeratelyActiveDistance LightActiveDistance SedentaryActiveDistance
    ##  Min.   :0.0000           Min.   : 0.000      Min.   :0.000000       
    ##  1st Qu.:0.0000           1st Qu.: 1.950      1st Qu.:0.000000       
    ##  Median :0.2400           Median : 3.380      Median :0.000000       
    ##  Mean   :0.5709           Mean   : 3.349      Mean   :0.001601       
    ##  3rd Qu.:0.8050           3rd Qu.: 4.790      3rd Qu.:0.000000       
    ##  Max.   :6.4800           Max.   :10.710      Max.   :0.110000       
    ##                                                                      
    ##  VeryActiveMinutes FairlyActiveMinutes LightlyActiveMinutes SedentaryMinutes
    ##  Min.   :  0.00    Min.   :  0.00      Min.   :  0          Min.   :   0.0  
    ##  1st Qu.:  0.00    1st Qu.:  0.00      1st Qu.:127          1st Qu.: 729.0  
    ##  Median :  4.00    Median :  7.00      Median :199          Median :1057.0  
    ##  Mean   : 21.24    Mean   : 13.63      Mean   :193          Mean   : 990.4  
    ##  3rd Qu.: 32.00    3rd Qu.: 19.00      3rd Qu.:264          3rd Qu.:1229.0  
    ##  Max.   :210.00    Max.   :143.00      Max.   :518          Max.   :1440.0  
    ##                                                                             
    ##     Calories    TotalSleepRecords TotalMinutesAsleep TotalTimeInBed 
    ##  Min.   :   0   Min.   :1.000     Min.   : 58.0      Min.   : 61.0  
    ##  1st Qu.:1830   1st Qu.:1.000     1st Qu.:361.0      1st Qu.:403.0  
    ##  Median :2140   Median :1.000     Median :433.0      Median :463.0  
    ##  Mean   :2308   Mean   :1.119     Mean   :419.5      Mean   :458.6  
    ##  3rd Qu.:2796   3rd Qu.:1.000     3rd Qu.:490.0      3rd Qu.:526.0  
    ##  Max.   :4900   Max.   :3.000     Max.   :796.0      Max.   :961.0  
    ##                 NA's   :530       NA's   :530        NA's   :530    
    ##     WeightKg       WeightPounds        Fat             BMI       
    ##  Min.   : 52.60   Min.   :116.0   Min.   :22.00   Min.   :21.45  
    ##  1st Qu.: 61.40   1st Qu.:135.4   1st Qu.:22.75   1st Qu.:23.96  
    ##  Median : 62.50   Median :137.8   Median :23.50   Median :24.39  
    ##  Mean   : 72.04   Mean   :158.8   Mean   :23.50   Mean   :25.19  
    ##  3rd Qu.: 85.05   3rd Qu.:187.5   3rd Qu.:24.25   3rd Qu.:25.56  
    ##  Max.   :133.50   Max.   :294.3   Max.   :25.00   Max.   :47.54  
    ##  NA's   :876      NA's   :876     NA's   :941     NA's   :876    
    ##  IsManualReport      LogId          
    ##  Mode :logical   Min.   :1.460e+12  
    ##  FALSE:26        1st Qu.:1.461e+12  
    ##  TRUE :41        Median :1.462e+12  
    ##  NA's :876       Mean   :1.462e+12  
    ##                  3rd Qu.:1.462e+12  
    ##                  Max.   :1.463e+12  
    ##                  NA's   :876

**SHARE**

``` r
ggplot(data = merged_table, aes(x = TotalSteps, y = Calories)) +  
  geom_point() + 
  geom_smooth() + 
  labs(title = 'Calories burned by steps', y = 'Calories', x = 'Total Steps')
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](title_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

There’s a positive correlation between number of steps and calories
burnt, which makes sense because the more active one is, the more
calorie is being burnt.

``` r
ggplot(data = merged_table, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='green') + geom_smooth() +
  labs(title="Sleep vs Inactive Lifestyle")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 530 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 530 rows containing missing values (geom_point).

![](title_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The data here shows that those with a sedentary lifestyle spent less
time asleep. There could be several factors affecting sleep cycle but
it’s a point worth noting.

``` r
ggplot(data = merged_table, aes(x = SedentaryMinutes, y = TotalSteps)) + 
  geom_point(color='blue') +
  labs(title = 'Sendentary Minutes vs Total steps', x = 'Sedentary Minutes', y ='Total Steps')
```

![](title_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Sedentary users took less steps.

**ACT**

After analyzing Fitbit Fitness Tracker Data , here are a few insights
which can be helpful to bellabeat in unlocking new growth opportunities.

\*From the reported data, only 8 users reported their weight, bellabeat
could develop a scale that helps track weight progress. (Weight gain or
weight loss progress over time.)

\*Develop app notifications to help alert users when they’re not getting
enough steps per day. According to the CDC, most adults should aim for
10,000 steps day. Sending out notifications throughout the day would
help users achieve that goal.

\*Offer incentives such as (a fraction off the monthly subscription
plan) to more active users to help reduce the sedentary time.
