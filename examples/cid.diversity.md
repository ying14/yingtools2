CID Diversity Plot
================

In this example we will generate a plot of diversity. This is the same plot that was published as [Figure 1 in the Clin Infect Dis (2012) paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3657523/figure/CIS580F1/).

``` r
library(tidyverse)
library(yingtools2)
library(phyloseq)
```

Use `get.samp()` to extract the sample data from the phyloseq object. By specifying `stats=TRUE`, you can calculate alpha diversity for each sample. Note that several types of diversity are calculated.

``` r
s <- get.samp(cid.phy,stats=TRUE)
s
```

    ## # A tibble: 439 x 10
    ##   sample Sample_ID Patient_ID   day day.old Consistency  nseqs Observed Shannon InvSimpson
    ##   <chr>  <chr>     <chr>      <dbl>   <dbl> <chr>        <dbl>    <dbl>   <dbl>      <dbl>
    ## 1 1037   1037      301           -1      -1 liquid        1977       88   2.60        7.23
    ## 2 1039   1039      301           15      15 semi-formed   1238       16   0.261       1.10
    ## 3 1040   1040      301           22      22 semi-formed   5192       31   0.409       1.24
    ## 4 1041   1041      301           28      28 semi-formed   1267       13   0.790       1.98
    ## 5 1042   1042      301           35      35 formed stool  1038        8   0.805       2.02
    ## 6 1038   1038      301            9       9 semi-formed   1181       39   1.62        3.03
    ## 7 1105   1105      318           -6      -6 formed stool  5182       60   2.00        4.03
    ## 8 1106   1106      318            1       1 formed stool  4251       31   0.544       1.24
    ## # … with 431 more rows

With this table you can now plot:

``` r
ggplot(s,aes(x=day,y=Shannon)) + geom_point()
```

![](cid.diversity_files/figure-markdown_github/unnamed-chunk-3-1.png)

We can add a smoothed conditional mean to the plot:

``` r
ggplot(s,aes(x=day,y=Shannon)) + geom_point() + geom_smooth()
```

![](cid.diversity_files/figure-markdown_github/unnamed-chunk-4-1.png)

This is fairly equivalent to Figure 1 in the CID 2012 paper. To make it match that plot even more closely, you could perform the following aesthetic operations: (1) change the X and Y axis titles, using `xlab` and `ylab`, (2) add a verticle line at X=0 to indicate the time of stem cell infusion, (3) change the color of the smoothed mean to black, (4) change the shape and color of the points to match, and specify `alpha` to make them slightly translucent, (5) draw the perimeter of the points in a separate layer (because those are not translucent), (6) change the order of the layers, such that the points are drawn over the smoothed mean.

``` r
ggplot(s,aes(x=day,y=Shannon)) + 
  geom_smooth(color="black") + 
  geom_point(shape=16,color="dark gray",size=3,alpha=0.4) +
  geom_point(shape=1,size=3) + 
  geom_vline(xintercept=0,linetype="longdash") +
  xlab("Day of transplant") +
  ylab("Biodiversity index (Shannon)")
```

![](cid.diversity_files/figure-markdown_github/unnamed-chunk-5-1.png)
