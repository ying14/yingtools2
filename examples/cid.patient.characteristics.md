CID Diversity Plot
================

To generate Table 1:

``` r
library(tidyverse)
library(yingtools2)
library(phyloseq)
```

First define age groups:

``` r
pt <- cid.patients %>% 
  mutate(age.groups=cut2(agebmt,lower=c(30,40,50,60)),
         engraft.14d=engraftment & engraftment_day>=14)
pt %>% select(Patient_ID,agebmt,age.groups,engraftment,engraftment_day,engraft.14d)
```

    ## # A tibble: 94 x 6
    ##   Patient_ID agebmt age.groups engraftment engraftment_day engraft.14d
    ##   <chr>       <dbl> <fct>      <lgl>                 <dbl> <lgl>      
    ## 1 301            54 50-59      TRUE                     38 TRUE       
    ## 2 318            57 50-59      FALSE                    NA FALSE      
    ## 3 224            35 30-39      TRUE                      9 FALSE      
    ## 4 138            65 >=60       TRUE                     11 FALSE      
    ## 5 219            58 50-59      TRUE                     16 TRUE       
    ## 6 173            33 30-39      TRUE                     12 FALSE      
    ## 7 178            60 >=60       TRUE                     10 FALSE      
    ## 8 177            54 50-59      TRUE                     12 FALSE      
    ## # … with 86 more rows

Now I could generate the table using `make_table`.

``` r
pt %>% make_table(age.groups,sex,primary.dx,priorabx.14d,intensity,
                  bmt.tcell.depletion,bmt.stemsource,engraft.14d,
                  fever,vanco_iv,fluoroquinolone,betalactam,
                  vre.bsi,gramneg.bsi,other.bsi,no.bsi,vital.status)
```

    ## # A tibble: 44 x 3
    ##   var        value    all          
    ##   <chr>      <chr>    <glue>       
    ## 1 age.groups <=29     7/94 (7.4%)  
    ## 2 age.groups 30-39    13/94 (13.8%)
    ## 3 age.groups 40-49    19/94 (20.2%)
    ## 4 age.groups 50-59    28/94 (29.8%)
    ## 5 age.groups >=60     27/94 (28.7%)
    ## 6 sex        Male     53/94 (56.4%)
    ## 7 sex        Female   41/94 (43.6%)
    ## 8 primary.dx Leukemia 44/94 (46.8%)
    ## # … with 36 more rows
