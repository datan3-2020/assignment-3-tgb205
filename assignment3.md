Statistical assignment 3
================
\[Toby Baines\] \[670030310\]
\[13/02/20\]

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.2

    ## Warning: package 'ggplot2' was built under R version 3.5.2

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'tidyr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'stringr' was built under R version 3.5.2

    ## Warning: package 'forcats' was built under R version 3.5.2

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.5.2

``` r
# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths
getwd()
```

    ## [1] "/Users/user/assignment-3-tgb205"

``` r
files <- dir(
             # Select the folder where the files are stored.
             "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "/Users/user/assignment-3-tgb205/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
  pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = variable, values_from = value)
  
Long
```

    ## # A tibble: 584,234 x 6
    ##        pidp wave  memorig sex_dv age_dv vote6
    ##       <int> <chr>   <int>  <int>  <int> <int>
    ##  1 68001367 a           1      1     39     3
    ##  2 68001367 b          NA     NA     NA    NA
    ##  3 68001367 c          NA     NA     NA    NA
    ##  4 68001367 d          NA     NA     NA    NA
    ##  5 68001367 e          NA     NA     NA    NA
    ##  6 68001367 f          NA     NA     NA    NA
    ##  7 68001367 g          NA     NA     NA    NA
    ##  8 68004087 a           1      1     59     2
    ##  9 68004087 b           1      1     60     2
    ## 10 68004087 c           1      1     61     2
    ## # … with 584,224 more rows

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig==1) %>%
        mutate(sex_dv = ifelse(sex_dv == 2, "female",
                               ifelse(sex_dv == 1, "male", NA)))
Long <- Long %>%
   mutate(vote6 = recode(vote6,
                         '1' = 1,
                         '2' = 2,
                         '3' = 3,
                         '4' = 4,
                         .default = NA_real_
))
table(Long$vote6)
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

``` r
table(Long$sex_dv)
```

    ## 
    ## female   male 
    ## 117665 100342

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%
  group_by(wave, sex_dv) %>%
  summarise(
    meanVote6 = mean(vote6, na.rm = TRUE)
  )%>%
  na.omit()
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   wave [7]
    ##    wave  sex_dv meanVote6
    ##    <chr> <chr>      <dbl>
    ##  1 a     female      2.84
    ##  2 a     male        2.53
    ##  3 b     female      2.82
    ##  4 b     male        2.51
    ##  5 c     female      2.87
    ##  6 c     male        2.54
    ##  7 d     female      2.89
    ##  8 d     male        2.55
    ##  9 e     female      2.87
    ## 10 e     male        2.51
    ## 11 f     female      2.81
    ## 12 f     male        2.47
    ## 13 g     female      2.73
    ## 14 g     male        2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
meanVote6 %>%
  pivot_wider(names_from = wave, values_from = meanVote6)
```

    ## # A tibble: 2 x 8
    ##   sex_dv     a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

According to the understanding society codebook, a mean closer to 4
implies ‘not interested at all’ in politics, whereas a mean close to 1
indicates a high level of interest in politics.

Looking at men compared to women, men generally have a lower mean across
waves which would indicate a higher level of political interest than
women. Despite this both genders, on the whole, remain inbetween the
‘fairly interest’ to ‘not very interested’ categories.

Moving from a-g (g being more recently), there appears to be an increase
in political interest for both men and women. f represents the period
2014-15 and g represents 2015-2016. This is a period marked by a rise in
populism across not only Britain but the world. Populism engages with a
wider aspect of the population, with events such as Brexit and the
election of Donald Trump the results. This is likely the cause of an
increase in political interest.

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
Polstab <- Long
Polstab <- Polstab  %>% 
  group_by(pidp) %>%
  filter(!is.na(vote6)) %>%
  mutate(delta = order_by(wave, vote6 - lag(vote6))) %>%
  mutate(delta = ifelse(is.na(delta), 0, delta)) %>% 
  filter(n()>= 7) 
  
Polstab$delta<-abs(Polstab$delta)

Polstab<- Polstab %>%
  group_by(pidp) %>%
  mutate(
    sumdelta = sum(delta)
  )

Polstab
```

    ## # A tibble: 103,747 x 8
    ## # Groups:   pidp [14,821]
    ##        pidp wave  memorig sex_dv age_dv vote6 delta sumdelta
    ##       <int> <chr>   <int> <chr>   <int> <dbl> <dbl>    <dbl>
    ##  1 68004087 a           1 male       59     2     0        2
    ##  2 68004087 b           1 male       60     2     0        2
    ##  3 68004087 c           1 male       61     2     0        2
    ##  4 68004087 d           1 male       62     1     1        2
    ##  5 68004087 e           1 male       63     2     1        2
    ##  6 68004087 f           1 male       64     2     0        2
    ##  7 68004087 g           1 male       65     2     0        2
    ##  8 68006127 a           1 female     39     4     0        0
    ##  9 68006127 b           1 female     40     4     0        0
    ## 10 68006127 c           1 female     41     4     0        0
    ## # … with 103,737 more rows

``` r
avPolStabgen <- tapply(Polstab$sumdelta, Polstab$sex_dv, mean, na.rm=TRUE )
avPolStabgen
```

    ##   female     male 
    ## 2.494727 2.527760

``` r
avPolStabage <- Polstab %>%
  filter(wave=="a")%>%
  group_by(age_dv) %>%
  summarise(sumdelta = mean(sumdelta, na.rm=TRUE))
avPolStabage
```

    ## # A tibble: 78 x 2
    ##    age_dv sumdelta
    ##     <int>    <dbl>
    ##  1     15     2   
    ##  2     16     2.78
    ##  3     17     2.55
    ##  4     18     2.83
    ##  5     19     2.82
    ##  6     20     2.39
    ##  7     21     3.08
    ##  8     22     2.59
    ##  9     23     2.42
    ## 10     24     2.15
    ## # … with 68 more rows

## I did not know how to complete the exercise
