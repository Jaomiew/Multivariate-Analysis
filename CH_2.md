CH_2
================

``` r
#install.packages('DataExplorer')
library(DataExplorer)
library(tidyr)
```

import file

``` r
file = "C:/Users/admin/Downloads/CreditApproval.csv"
data = read.csv(file)
head(data)
```

    ##   A1    A2    A3 A4 A5 A6 A7   A8 A9 A10 A11 A12 A13 A14 A15 A16
    ## 1  b 30.83 0.000  u  g  w  v 1.25  t   t   1   f   g 202   0   +
    ## 2  a 58.67 4.460  u  g  q  h 3.04  t   t   6   f   g  43 560   +
    ## 3  a 24.50 0.500  u  g  q  h 1.50  t   f   0   f   g 280 824   +
    ## 4  b 27.83 1.540  u  g  w  v 3.75  t   t   5   t   g 100   3   +
    ## 5  b 20.17 5.625  u  g  w  v 1.71  t   f   0   f   s 120   0   +
    ## 6  b 32.08 4.000  u  g  m  v 2.50  t   f   0   t   g 360   0   +

show about data

``` r
introduce(data)
```

    ##   rows columns discrete_columns continuous_columns all_missing_columns
    ## 1  690      16               10                  6                   0
    ##   total_missing_values complete_rows total_observations memory_usage
    ## 1                   67           653              11040        86304

view missing data

``` r
plot_missing(data)
```

![](CH_2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
datac = apply(is.na(data),2,which)
datac = drop_na(data)
head(datac)
```

    ##   A1    A2    A3 A4 A5 A6 A7   A8 A9 A10 A11 A12 A13 A14 A15 A16
    ## 1  b 30.83 0.000  u  g  w  v 1.25  t   t   1   f   g 202   0   +
    ## 2  a 58.67 4.460  u  g  q  h 3.04  t   t   6   f   g  43 560   +
    ## 3  a 24.50 0.500  u  g  q  h 1.50  t   f   0   f   g 280 824   +
    ## 4  b 27.83 1.540  u  g  w  v 3.75  t   t   5   t   g 100   3   +
    ## 5  b 20.17 5.625  u  g  w  v 1.71  t   f   0   f   s 120   0   +
    ## 6  b 32.08 4.000  u  g  m  v 2.50  t   f   0   t   g 360   0   +

plot data

``` r
data1 = data[1:200,c('A1','A4','A5')]
plot_bar(data[,c('A1','A4','A5')])
```

![](CH_2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot_scatterplot(data[,c('A2','A3')],by = 'A2')
```

    ## Warning: Removed 12 rows containing missing values (`geom_point()`).

![](CH_2_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
plot_histogram(data[,c('A2')])
```

![](CH_2_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

Summary data show dis

``` r
summary(data)
```

    ##       A1                  A2              A3              A4           
    ##  Length:690         Min.   :13.75   Min.   : 0.000   Length:690        
    ##  Class :character   1st Qu.:22.60   1st Qu.: 1.000   Class :character  
    ##  Mode  :character   Median :28.46   Median : 2.750   Mode  :character  
    ##                     Mean   :31.57   Mean   : 4.759                     
    ##                     3rd Qu.:38.23   3rd Qu.: 7.207                     
    ##                     Max.   :80.25   Max.   :28.000                     
    ##                     NA's   :12                                         
    ##       A5                 A6                 A7                  A8        
    ##  Length:690         Length:690         Length:690         Min.   : 0.000  
    ##  Class :character   Class :character   Class :character   1st Qu.: 0.165  
    ##  Mode  :character   Mode  :character   Mode  :character   Median : 1.000  
    ##                                                           Mean   : 2.223  
    ##                                                           3rd Qu.: 2.625  
    ##                                                           Max.   :28.500  
    ##                                                                           
    ##       A9                A10                 A11           A12           
    ##  Length:690         Length:690         Min.   : 0.0   Length:690        
    ##  Class :character   Class :character   1st Qu.: 0.0   Class :character  
    ##  Mode  :character   Mode  :character   Median : 0.0   Mode  :character  
    ##                                        Mean   : 2.4                     
    ##                                        3rd Qu.: 3.0                     
    ##                                        Max.   :67.0                     
    ##                                                                         
    ##      A13                 A14            A15               A16           
    ##  Length:690         Min.   :   0   Min.   :     0.0   Length:690        
    ##  Class :character   1st Qu.:  75   1st Qu.:     0.0   Class :character  
    ##  Mode  :character   Median : 160   Median :     5.0   Mode  :character  
    ##                     Mean   : 184   Mean   :  1017.4                     
    ##                     3rd Qu.: 276   3rd Qu.:   395.5                     
    ##                     Max.   :2000   Max.   :100000.0                     
    ##                     NA's   :13

Simulation multivariate normal distribution

``` r
#install.packages("MASS")
#install.packages("MVN")
library(MASS)
library(MVN)
library(ggplot2)
```

``` r
n = 100 
mu = c(0,0)
sigma = matrix(c(10,3,3,2),2,2)
set.seed(1)
data2 = mvrnorm(n,mu,sigma)
```

normality test for

H0 : data2 is multivariate is normal distribution

H1 : data2 is multivariate is not normal distribution

for conclustion, we will reject H0 iff p-value \< 0.005

``` r
result = mvn(data2,mvnTest = 'energy')
```

p-value = 0.22 Accept H0, So data2 is multivariate is normal
distribution

``` r
result = mvn(data2,mvnTest = 'energy', univariatePlot = 'qqplot', multivariatePlot = 'qq')
```

![](CH_2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->![](CH_2_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
result = mvn(data2,mvnTest = 'energy', multivariatePlot = 'qq')
```

![](CH_2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

iris data

``` r
library(readxl)
iris = read_excel("C:/Users/admin/Downloads/iris_xcel.xlsx")
data_iris = iris[c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
result_data_iris = mvn(data_iris,mvnTest = 'energy')
result_data_iris = mvn(data_iris,mvnTest = 'energy',univariatePlot = 'qqplot')
```

![](CH_2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
result_data_iris = mvn(data_iris,mvnTest = 'energy',multivariatePlot = 'qq')
```

![](CH_2_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->
