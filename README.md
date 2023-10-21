
# Analysing the ONS ‘GVA by industry by ITL region’ data in R

The ONS produces an [Excel
document](https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry)
with GVA data at three different geographical scales, ITL1 to ITL3.
Wikipedia has an [excellent explanation with
maps](https://en.wikipedia.org/wiki/International_Territorial_Level) of
the ITL regions.

Here, we’ll explore some ways to analyse this data using R. The data has
been processed to make it more useable in R - see the
[process_from_excel.R](process_from_excel.R) script and its comments for
an explanation of how to get from the Excel sheet to the datasets used
here. All of the derived files (and the original ONS excel sheet,
currently used version dated 25th April 2023) are included in this
repository in the [data
folder](https://github.com/DanOlner/regionalGVAbyindustry/tree/master/data).
Test relative data folder link: [data folder](data).

``` r
summary(iris)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

``` r
plot(iris$Sepal.Length, iris$Sepal.Width)
```

![](README_files/figure-gfm/iris-1.png)<!-- -->
