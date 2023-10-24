CH_1
================

Install และเรียกใช้ Libry

``` r
#install.packages("dplyr")
#install.packages("tidyvers")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

สร้าง DATA FRAME สมมติขึ้นมา

``` r
data1 = data.frame(CustomerId = c(1:6), 
                 Product = c("Refrigerator", "Microwave","Toaster","Rice cooker","Dishwasher","Coffee maker"))
data1
```

    ##   CustomerId      Product
    ## 1          1 Refrigerator
    ## 2          2    Microwave
    ## 3          3      Toaster
    ## 4          4  Rice cooker
    ## 5          5   Dishwasher
    ## 6          6 Coffee maker

``` r
data2 = data.frame(CustomerId = c(2, 4, 6, 7, 8), 
                 State = c("New York","Washington","Texas","Nevada","Florida"))
data2
```

    ##   CustomerId      State
    ## 1          2   New York
    ## 2          4 Washington
    ## 3          6      Texas
    ## 4          7     Nevada
    ## 5          8    Florida

``` r
data3 = data.frame(CustomerId = c(10,30,100,50,60,70), 
                 Product = c("Refrigerator", "Microwave","Toaster","Rice cooker","Dishwasher","Coffee maker"))
data3
```

    ##   CustomerId      Product
    ## 1         10 Refrigerator
    ## 2         30    Microwave
    ## 3        100      Toaster
    ## 4         50  Rice cooker
    ## 5         60   Dishwasher
    ## 6         70 Coffee maker

การรวมข้อมูลด้วย Join ประกอดด้วย 4 ประเภทได้แก่ Inner join

``` r
data_Inner1 = inner_join(data1,data2, by ='CustomerId')
data_Inner1
```

    ##   CustomerId      Product      State
    ## 1          2    Microwave   New York
    ## 2          4  Rice cooker Washington
    ## 3          6 Coffee maker      Texas

``` r
data_Inner2= data1 %>% inner_join(data2,by="CustomerId")
data_Inner2
```

    ##   CustomerId      Product      State
    ## 1          2    Microwave   New York
    ## 2          4  Rice cooker Washington
    ## 3          6 Coffee maker      Texas

outer join full_join() keeps all observations in x and y.

``` r
data_outer = full_join(x=data1,y=data2,by="CustomerId") 
data_outer
```

    ##   CustomerId      Product      State
    ## 1          1 Refrigerator       <NA>
    ## 2          2    Microwave   New York
    ## 3          3      Toaster       <NA>
    ## 4          4  Rice cooker Washington
    ## 5          5   Dishwasher       <NA>
    ## 6          6 Coffee maker      Texas
    ## 7          7         <NA>     Nevada
    ## 8          8         <NA>    Florida

left join left_join() keeps all observations in x.

``` r
data_left = left_join(x=data1,y=data2,by="CustomerId") 
data_left
```

    ##   CustomerId      Product      State
    ## 1          1 Refrigerator       <NA>
    ## 2          2    Microwave   New York
    ## 3          3      Toaster       <NA>
    ## 4          4  Rice cooker Washington
    ## 5          5   Dishwasher       <NA>
    ## 6          6 Coffee maker      Texas

right join right_join() keeps all observations in y.

``` r
data_right = right_join(x=data1,y=data2,by="CustomerId") 
data_right
```

    ##   CustomerId      Product      State
    ## 1          2    Microwave   New York
    ## 2          4  Rice cooker Washington
    ## 3          6 Coffee maker      Texas
    ## 4          7         <NA>     Nevada
    ## 5          8         <NA>    Florida

show result

``` r
data_outer %>% glimpse()
```

    ## Rows: 8
    ## Columns: 3
    ## $ CustomerId <dbl> 1, 2, 3, 4, 5, 6, 7, 8
    ## $ Product    <chr> "Refrigerator", "Microwave", "Toaster", "Rice cooker", "Dis…
    ## $ State      <chr> NA, "New York", NA, "Washington", NA, "Texas", "Nevada", "F…

``` r
glimpse(data_outer)
```

    ## Rows: 8
    ## Columns: 3
    ## $ CustomerId <dbl> 1, 2, 3, 4, 5, 6, 7, 8
    ## $ Product    <chr> "Refrigerator", "Microwave", "Toaster", "Rice cooker", "Dis…
    ## $ State      <chr> NA, "New York", NA, "Washington", NA, "Texas", "Nevada", "F…

check missing data in each column

``` r
results = apply(is.na(data_left),2,which)
```

Fillter out the missing values by drop_na

``` r
data_NEW = drop_na(data_outer)
```
