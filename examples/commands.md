Commands
================

``` r
library(tidyverse)
library(yingtools2)
library(phyloseq)
library(scales)
```

### Q1

Sort `cid.patients` by `agebmt`

``` r
cid.patients[order(cid.patients$agebmt),]
```

    ## # A tibble: 94 x 47
    ##   Patient_ID agebmt sex    race       admit.day discharge.day firstsampday tstart tstop primary.dx  
    ##   <chr>       <dbl> <chr>  <fct>          <int>         <int>        <int>  <dbl> <dbl> <chr>       
    ## 1 182            19 Male   White            -11            20           -4    -11    20 Leukemia    
    ## 2 232            23 Female White             -8            73           -5     -8    35 Leukemia    
    ## 3 143            25 Male   White             -7            15           -5     -7    15 Lymphoma    
    ## 4 321            26 Male   Black/ Af…        -8            47           -8     -8    35 Lymphoma    
    ## 5 220            28 Female White             -7            19           -7     -7    19 Lymphoma    
    ## 6 190            28 Male   Asian/ Fa…       -10            19          -10    -10    19 Myelodyspla…
    ## 7 305            29 Female Asian/ Fa…        -7            42           -8     -7    35 Lymphoma    
    ## 8 161            30 Male   White            -11            26           -3    -11    26 Myelodyspla…
    ## # … with 86 more rows

``` r
cid.patients %>% arrange(agebmt)
```

    ## # A tibble: 94 x 47
    ##   Patient_ID agebmt sex    race       admit.day discharge.day firstsampday tstart tstop primary.dx  
    ##   <chr>       <dbl> <chr>  <fct>          <int>         <int>        <int>  <dbl> <dbl> <chr>       
    ## 1 182            19 Male   White            -11            20           -4    -11    20 Leukemia    
    ## 2 232            23 Female White             -8            73           -5     -8    35 Leukemia    
    ## 3 143            25 Male   White             -7            15           -5     -7    15 Lymphoma    
    ## 4 321            26 Male   Black/ Af…        -8            47           -8     -8    35 Lymphoma    
    ## 5 220            28 Female White             -7            19           -7     -7    19 Lymphoma    
    ## 6 190            28 Male   Asian/ Fa…       -10            19          -10    -10    19 Myelodyspla…
    ## 7 305            29 Female Asian/ Fa…        -7            42           -8     -7    35 Lymphoma    
    ## 8 161            30 Male   White            -11            26           -3    -11    26 Myelodyspla…
    ## # … with 86 more rows
