Homework 5
================

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

``` r
library(faraway)
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
# import and clean data
states = as.data.frame(datasets::state.x77) |> 
  janitor::clean_names() 

# create a column for state names
states$state = rownames(states)

head(states)
```

    ##            population income illiteracy life_exp murder hs_grad frost   area
    ## Alabama          3615   3624        2.1    69.05   15.1    41.3    20  50708
    ## Alaska            365   6315        1.5    69.31   11.3    66.7   152 566432
    ## Arizona          2212   4530        1.8    70.55    7.8    58.1    15 113417
    ## Arkansas         2110   3378        1.9    70.66   10.1    39.9    65  51945
    ## California      21198   5114        1.1    71.71   10.3    62.6    20 156361
    ## Colorado         2541   4884        0.7    72.06    6.8    63.9   166 103766
    ##                 state
    ## Alabama       Alabama
    ## Alaska         Alaska
    ## Arizona       Arizona
    ## Arkansas     Arkansas
    ## California California
    ## Colorado     Colorado

- population: population estimate  
- income: per capita income  
- illiteracy: illiteracy (%)  
- life_exp: life expectancy (years)  
- murder: murder and non-negligent manslaughter rate per 100,000
  population  
- hs_grad: high-school graduates (%)  
- frost: mean number of days with minimum temperature below freezing in
  capital or large city  
- area: land area (sq. miles)

# Problem 1

## part a

Provide descriptive statistics for all variables of interest (continuous
and categorical).

``` r
summary(states)
```

    ##    population        income       illiteracy       life_exp    
    ##  Min.   :  365   Min.   :3098   Min.   :0.500   Min.   :67.96  
    ##  1st Qu.: 1080   1st Qu.:3993   1st Qu.:0.625   1st Qu.:70.12  
    ##  Median : 2838   Median :4519   Median :0.950   Median :70.67  
    ##  Mean   : 4246   Mean   :4436   Mean   :1.170   Mean   :70.88  
    ##  3rd Qu.: 4968   3rd Qu.:4814   3rd Qu.:1.575   3rd Qu.:71.89  
    ##  Max.   :21198   Max.   :6315   Max.   :2.800   Max.   :73.60  
    ##      murder          hs_grad          frost             area       
    ##  Min.   : 1.400   Min.   :37.80   Min.   :  0.00   Min.   :  1049  
    ##  1st Qu.: 4.350   1st Qu.:48.05   1st Qu.: 66.25   1st Qu.: 36985  
    ##  Median : 6.850   Median :53.25   Median :114.50   Median : 54277  
    ##  Mean   : 7.378   Mean   :53.11   Mean   :104.46   Mean   : 70736  
    ##  3rd Qu.:10.675   3rd Qu.:59.15   3rd Qu.:139.75   3rd Qu.: 81162  
    ##  Max.   :15.100   Max.   :67.30   Max.   :188.00   Max.   :566432  
    ##     state          
    ##  Length:50         
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

## part b

Examine exploratory plots, e.g., scatter plots, histograms, box-plots to
get a sense of the data and possible variable transformations. (Be
selective! Even if you create 20 plots, you don’t want to show them
all). If you find a transformation to be necessary or recommended,
perform the transformation and use it through the rest of the problem.

``` r
ggplot(states, aes(x = income)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(
    title = "Histograms of Income", 
    x = "Per Capita Income", 
    y = "Frequency")
```

![](BISTP8130_hw5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(states, aes(x = income, y = life_exp)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Life Expectancy vs. Income", 
    x = "Per Capita Income",
    y = "Life Expectancy")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](BISTP8130_hw5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(states, aes(y = murder)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Boxplot of Murder Rates", 
    y = "Murder Rate (per 100,000)")
```

![](BISTP8130_hw5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
states = states |> 
  mutate(log_population = log(population), 
         log_area = log(area))

ggplot(states, aes(x = log_population, y = life_exp)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
  labs(
    title = "Log Population vs. Life Expectancy", 
    y = "Life Expectancy"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](BISTP8130_hw5_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## part c

Use automatic procedures to find a ‘best subset’ of the full model.
Present the results and comment on the following:

- Do the procedures generate the same model?  
- Are any variables a close call? What was you decision: keep or
  discard? Provide arguments for your choice. (Note: this question might
  have more or less relevance depending on the ‘subset’ you choose).  
- Is there any association between ‘Illiteracy’ and ‘HS graduation
  rate’? Does your ‘subset’ contain both?

## part d

Use criterion-based procedures to guide your selection of the ‘best
subset’. Summarize your results (tabular or graphical).

## part e

Use the LASSO method to perform variable selection. Make sure you choose
the “best lambda” to use and show how you determined this.

## part f

Compare the ‘subsets’ from parts c, d, and e and recommend a ‘final’
model. Using this ‘final’ model do the following:

- Check the model assumptions.  
- Test the model predictive ability using a 10-fold cross-validation.

## part g

In a paragraph, summarize your findings to address the primary quetsion
posed by the investigator (that has limited statistical knowledge).
