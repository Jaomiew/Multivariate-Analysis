Individual work wanissara
================

``` r
library(DataExplorer)
library(ClusterR)
library(cluster)
library(ggfortify)
```

    ## Loading required package: ggplot2

``` r
library(stats)
library(fpc)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

## import

``` r
file = 'C:/Users/User/Documents/data/Credit Card Customer Data.csv'
data = read.csv(file)
head(data)
```

    ##   Sl_No Customer.Key Avg_Credit_Limit Total_Credit_Cards Total_visits_bank
    ## 1     1        87073           100000                  2                 1
    ## 2     2        38414            50000                  3                 0
    ## 3     3        17341            50000                  7                 1
    ## 4     4        40496            30000                  5                 1
    ## 5     5        47437           100000                  6                 0
    ## 6     6        58634            20000                  3                 0
    ##   Total_visits_online Total_calls_made
    ## 1                   1                0
    ## 2                  10                9
    ## 3                   3                4
    ## 4                   1                4
    ## 5                  12                3
    ## 6                   1                8

``` r
introduce(data)
```

    ##   rows columns discrete_columns continuous_columns all_missing_columns
    ## 1  660       7                0                  7                   0
    ##   total_missing_values complete_rows total_observations memory_usage
    ## 1                    0           660               4620        21016

``` r
new_df <- data[,2:7]
head(new_df)
```

    ##   Customer.Key Avg_Credit_Limit Total_Credit_Cards Total_visits_bank
    ## 1        87073           100000                  2                 1
    ## 2        38414            50000                  3                 0
    ## 3        17341            50000                  7                 1
    ## 4        40496            30000                  5                 1
    ## 5        47437           100000                  6                 0
    ## 6        58634            20000                  3                 0
    ##   Total_visits_online Total_calls_made
    ## 1                   1                0
    ## 2                  10                9
    ## 3                   3                4
    ## 4                   1                4
    ## 5                  12                3
    ## 6                   1                8

``` r
scale_data <- scale(new_df)
head(scale_data)
```

    ##      Customer.Key Avg_Credit_Limit Total_Credit_Cards Total_visits_bank
    ## [1,]    1.2459747        1.7388680         -1.2482780        -0.8597985
    ## [2,]   -0.6527077        0.4099816         -0.7869883        -1.4726139
    ## [3,]   -1.4749797        0.4099816          1.0581707        -0.8597985
    ## [4,]   -0.5714677       -0.1215730          0.1355912        -0.8597985
    ## [5,]   -0.3006287        1.7388680          0.5968810        -1.4726139
    ## [6,]    0.1362801       -0.3873503         -0.7869883        -1.4726139
    ##      Total_visits_online Total_calls_made
    ## [1,]          -0.5470748       -1.2505889
    ## [2,]           2.5186084        1.8904250
    ## [3,]           0.1341882        0.1454173
    ## [4,]          -0.5470748        0.1454173
    ## [5,]           3.1998713       -0.2035842
    ## [6,]          -0.5470748        1.5414235

### PCA

``` r
res.pca = prcomp(scale_data, scale = TRUE)
print(summary(res.pca))
```

    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6
    ## Standard deviation     1.5124 1.3688 1.0005 0.56568 0.52565 0.49181
    ## Proportion of Variance 0.3812 0.3123 0.1668 0.05333 0.04605 0.04031
    ## Cumulative Proportion  0.3812 0.6935 0.8603 0.91364 0.95969 1.00000

``` r
fviz_eig(res.pca,addlabels = TRUE)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

cutting in PC3

## show correlation circle

``` r
var = get_pca_var(res.pca)

fviz_pca_var(res.pca, col.var = "black")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
corrplot(var$cos2, is.corr = FALSE)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB","#E7B880","#FC4E07"))
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
##show Graph of individual
ind <- get_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB","#E7B880","#FC4E07"))
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
pca_data <- res.pca$x[, 1:3]
head(pca_data) # with PCA
```

    ##             PC1         PC2        PC3
    ## [1,] -0.5283993 -0.58718226  1.3056000
    ## [2,]  1.4647551 -3.07138658 -0.7653869
    ## [3,] -0.4963841 -0.74553873 -1.5611019
    ## [4,]  0.3736156 -0.09974765 -0.5924571
    ## [5,] -1.2666920 -3.64178695 -0.5216918
    ## [6,]  1.9910827 -0.67702162  0.1448776

``` r
head(scale_data) # without PCA
```

    ##      Customer.Key Avg_Credit_Limit Total_Credit_Cards Total_visits_bank
    ## [1,]    1.2459747        1.7388680         -1.2482780        -0.8597985
    ## [2,]   -0.6527077        0.4099816         -0.7869883        -1.4726139
    ## [3,]   -1.4749797        0.4099816          1.0581707        -0.8597985
    ## [4,]   -0.5714677       -0.1215730          0.1355912        -0.8597985
    ## [5,]   -0.3006287        1.7388680          0.5968810        -1.4726139
    ## [6,]    0.1362801       -0.3873503         -0.7869883        -1.4726139
    ##      Total_visits_online Total_calls_made
    ## [1,]          -0.5470748       -1.2505889
    ## [2,]           2.5186084        1.8904250
    ## [3,]           0.1341882        0.1454173
    ## [4,]          -0.5470748        0.1454173
    ## [5,]           3.1998713       -0.2035842
    ## [6,]          -0.5470748        1.5414235

## K-means Euclidean without pca

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(scale_data, centers = k)
  sse[k] <- kmeans_model$tot.withinss
}

ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
เลือก k=3

``` r
kmeans_result <- kmeans(scale_data,center = 3)
kmeans_result
```

    ## K-means clustering with 3 clusters of sizes 50, 224, 386
    ## 
    ## Cluster means:
    ##   Customer.Key Avg_Credit_Limit Total_Credit_Cards Total_visits_bank
    ## 1  0.061156937       2.82961800          1.8608149        -1.1049247
    ## 2  0.003839055      -0.59534471         -1.0588197        -0.9008353
    ## 3 -0.010149728      -0.02104581          0.3734064         0.6658895
    ##   Total_visits_online Total_calls_made
    ## 1            2.825177       -0.8736672
    ## 2            0.322752        1.1472387
    ## 3           -0.553252       -0.5525858
    ## 
    ## Clustering vector:
    ##   [1] 3 2 3 3 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [38] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [112] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [149] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [186] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [223] 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [260] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [297] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [334] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [371] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [408] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [445] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [482] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [519] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [556] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [593] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [630] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 168.1350 491.3606 930.9044
    ##  (between_SS / total_SS =  59.8 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
autoplot(kmeans_result,scale_data,frame=TRUE)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## K-means Euclidean with pca

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(pca_data, centers = k)
  sse[k] <- kmeans_model$tot.withinss
}

ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

เลือก k=3

``` r
kmeans_result_p <- kmeans(pca_data,center = 3)
kmeans_result_p
```

    ## K-means clustering with 3 clusters of sizes 386, 50, 224
    ## 
    ## Cluster means:
    ##          PC1        PC2          PC3
    ## 1 -0.6445256  0.8808082  0.021072149
    ## 2 -2.9981375 -3.5181733 -0.177853194
    ## 3  1.7798828 -0.7325147  0.003387546
    ## 
    ## Clustering vector:
    ##   [1] 1 3 1 1 2 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ##  [38] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ##  [75] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [112] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [149] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [186] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [223] 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [260] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [297] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [334] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [371] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [408] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [445] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [482] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [519] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [556] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [593] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [630] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 594.1824 106.9067 340.8401
    ##  (between_SS / total_SS =  69.4 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
autoplot(kmeans_result_p,pca_data,frame=TRUE)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## K-means Manhattan without pca

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(scale_data, centers = k,algorithm = "Lloyd")
  sse[k] <- kmeans_model$tot.withinss
}
```

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

``` r
ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

เลือก k=3

``` r
kmeans_result_m <- kmeans(scale_data,center = 3,algorithm = "Lloyd")
kmeans_result_m
```

    ## K-means clustering with 3 clusters of sizes 386, 50, 224
    ## 
    ## Cluster means:
    ##   Customer.Key Avg_Credit_Limit Total_Credit_Cards Total_visits_bank
    ## 1 -0.010149728      -0.02104581          0.3734064         0.6658895
    ## 2  0.061156937       2.82961800          1.8608149        -1.1049247
    ## 3  0.003839055      -0.59534471         -1.0588197        -0.9008353
    ##   Total_visits_online Total_calls_made
    ## 1           -0.553252       -0.5525858
    ## 2            2.825177       -0.8736672
    ## 3            0.322752        1.1472387
    ## 
    ## Clustering vector:
    ##   [1] 1 3 1 1 2 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ##  [38] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ##  [75] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [112] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [149] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [186] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [223] 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [260] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [297] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [334] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [371] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [408] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [445] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [482] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [519] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [556] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [593] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [630] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 930.9044 168.1350 491.3606
    ##  (between_SS / total_SS =  59.8 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
autoplot(kmeans_result_m,scale_data,frame=TRUE)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## K-means manhattan with pca

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(pca_data, centers = k,algorithm = "Lloyd")
  sse[k] <- kmeans_model$tot.withinss
}
```

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

``` r
ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
เลือก k=4

``` r
kmeans_result_pm <- kmeans(pca_data,center = 4,algorithm = "Lloyd")
```

    ## Warning: did not converge in 10 iterations

``` r
kmeans_result_pm
```

    ## K-means clustering with 4 clusters of sizes 215, 223, 172, 50
    ## 
    ## Cluster means:
    ##          PC1        PC2         PC3
    ## 1 -0.5532431  0.9222451 -0.74195828
    ## 2  1.7855109 -0.7347154  0.01021556
    ## 3 -0.7518302  0.8224855  0.96590476
    ## 4 -2.9981375 -3.5181733 -0.17785319
    ## 
    ## Clustering vector:
    ##   [1] 3 2 1 1 4 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [38] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [112] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [149] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [186] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [223] 2 2 2 2 2 2 1 1 1 3 3 1 1 3 1 1 1 1 1 3 1 3 1 3 3 1 1 1 1 1 1 1 1 1 3 1 1
    ## [260] 1 3 1 3 1 1 1 3 1 3 1 1 1 3 1 1 1 1 1 3 1 1 1 3 1 1 3 1 3 1 3 1 3 1 1 3 1
    ## [297] 1 3 1 3 3 3 1 1 1 1 1 1 1 3 3 3 1 2 1 1 1 3 1 3 1 1 1 1 1 3 1 1 1 1 1 1 1
    ## [334] 1 1 3 3 3 3 1 3 3 1 3 3 1 1 1 3 1 3 3 3 3 1 1 1 3 1 3 1 3 3 3 3 1 1 3 3 3
    ## [371] 3 1 3 3 1 1 1 3 1 1 1 1 3 1 1 1 3 1 1 3 1 3 3 3 3 1 3 3 3 3 1 3 1 1 3 3 1
    ## [408] 1 1 3 1 1 3 3 1 3 1 3 1 3 1 3 1 1 3 3 3 3 1 1 3 1 1 1 1 1 3 3 1 1 1 1 1 3
    ## [445] 1 1 3 3 1 1 1 1 1 3 3 3 1 3 1 1 3 1 3 3 1 1 1 3 3 3 1 1 3 1 1 3 1 3 3 1 3
    ## [482] 3 1 1 1 3 3 1 3 1 1 3 1 1 1 1 3 1 1 3 3 3 3 3 3 3 3 1 1 3 1 3 3 1 1 1 1 1
    ## [519] 3 1 3 3 1 3 3 1 1 1 3 1 1 1 1 1 1 1 3 1 3 1 3 1 3 1 3 3 1 1 3 3 3 1 3 1 3
    ## [556] 1 3 3 3 3 1 1 3 3 3 1 1 3 3 3 3 1 3 3 1 1 3 3 3 3 3 1 3 1 1 3 3 3 1 1 3 3
    ## [593] 3 3 3 1 3 1 1 3 3 1 3 1 3 3 1 3 1 1 1 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ## [630] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 178.5276 336.6870 137.2065 106.9067
    ##  (between_SS / total_SS =  77.7 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## Warning: did *not* converge in specified number of iterations

``` r
autoplot(kmeans_result_pm,pca_data,frame=TRUE)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Hierarchical euclidean without pca

``` r
test = (scale_data[1:50,])
head(test)
```

    ##      Customer.Key Avg_Credit_Limit Total_Credit_Cards Total_visits_bank
    ## [1,]    1.2459747        1.7388680         -1.2482780        -0.8597985
    ## [2,]   -0.6527077        0.4099816         -0.7869883        -1.4726139
    ## [3,]   -1.4749797        0.4099816          1.0581707        -0.8597985
    ## [4,]   -0.5714677       -0.1215730          0.1355912        -0.8597985
    ## [5,]   -0.3006287        1.7388680          0.5968810        -1.4726139
    ## [6,]    0.1362801       -0.3873503         -0.7869883        -1.4726139
    ##      Total_visits_online Total_calls_made
    ## [1,]          -0.5470748       -1.2505889
    ## [2,]           2.5186084        1.8904250
    ## [3,]           0.1341882        0.1454173
    ## [4,]          -0.5470748        0.1454173
    ## [5,]           3.1998713       -0.2035842
    ## [6,]          -0.5470748        1.5414235

``` r
distance_mat <- dist(test,method = 'euclidean')
head(distance_mat)
```

    ## [1] 5.022315 4.111227 3.260102 4.616055 3.759773 4.081853

``` r
Hierar_cl <- hclust(distance_mat)
Hierar_cl
```

    ## 
    ## Call:
    ## hclust(d = distance_mat)
    ## 
    ## Cluster method   : complete 
    ## Distance         : euclidean 
    ## Number of objects: 50

``` r
plot(Hierar_cl)

plot(Hierar_cl)
abline(h=4.75,col = "green")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl,k=3)
fit
```

    ##  [1] 1 2 3 3 2 3 2 1 1 3 1 3 1 3 1 1 1 1 3 3 3 1 3 1 3 1 1 3 1 3 3 3 1 1 3 1 3 1
    ## [39] 3 3 1 3 1 1 1 1 1 1 3 1

``` r
plot(Hierar_cl)
table(fit)
```

    ## fit
    ##  1  2  3 
    ## 26  3 21

``` r
rect.hclust(Hierar_cl,k=3,border = "green")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

### Hierarchical euclidean with pca

``` r
test_pca = (pca_data[1:50,])
head(test_pca)
```

    ##             PC1         PC2        PC3
    ## [1,] -0.5283993 -0.58718226  1.3056000
    ## [2,]  1.4647551 -3.07138658 -0.7653869
    ## [3,] -0.4963841 -0.74553873 -1.5611019
    ## [4,]  0.3736156 -0.09974765 -0.5924571
    ## [5,] -1.2666920 -3.64178695 -0.5216918
    ## [6,]  1.9910827 -0.67702162  0.1448776

``` r
distance_mat_p <- dist(test_pca,method = 'euclidean')
head(distance_mat_p)
```

    ## [1] 3.799069 2.871251 2.157277 3.635200 2.775453 3.312392

``` r
Hierar_cl_p <- hclust(distance_mat_p)
Hierar_cl_p
```

    ## 
    ## Call:
    ## hclust(d = distance_mat_p)
    ## 
    ## Cluster method   : complete 
    ## Distance         : euclidean 
    ## Number of objects: 50

``` r
plot(Hierar_cl_p)

plot(Hierar_cl_p)
abline(h=3.75,col = "green")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl_p,k=3)
fit
```

    ##  [1] 1 2 1 1 2 3 2 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [39] 3 3 3 3 1 3 3 3 3 3 3 3

``` r
plot(Hierar_cl_p)
table(fit)
```

    ## fit
    ##  1  2  3 
    ##  5  3 42

``` r
rect.hclust(Hierar_cl_p,k=3,border = "green")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

## Hierarchical Manhattan without pca

``` r
distance_mat_m <- dist(test,method = 'manhattan')
head(distance_mat_m)
```

    ## [1] 10.508371  8.433559  6.457759  8.798529  7.102030  7.611200

``` r
Hierar_cl_m <- hclust(distance_mat_m)
Hierar_cl_m
```

    ## 
    ## Call:
    ## hclust(d = distance_mat_m)
    ## 
    ## Cluster method   : complete 
    ## Distance         : manhattan 
    ## Number of objects: 50

``` r
plot(Hierar_cl_m)

plot(Hierar_cl_m)
abline(h=9,col = "green")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl_m,k=3)
fit
```

    ##  [1] 1 2 2 2 2 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [39] 3 3 3 3 3 3 3 3 3 3 3 3

``` r
plot(Hierar_cl_m)
table(fit)
```

    ## fit
    ##  1  2  3 
    ##  1  5 44

``` r
rect.hclust(Hierar_cl_m,k=3,border = "green")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

## Hierarchical manhattan with pca

``` r
distance_mat_pm <- dist(test_pca,method = 'manhattan')
head(distance_mat_pm)
```

    ## [1] 6.548346 3.057073 3.287507 5.620189 3.770044 5.115388

``` r
Hierar_cl_pm <- hclust(distance_mat_pm)
Hierar_cl_pm
```

    ## 
    ## Call:
    ## hclust(d = distance_mat_pm)
    ## 
    ## Cluster method   : complete 
    ## Distance         : manhattan 
    ## Number of objects: 50

``` r
plot(Hierar_cl_pm)

plot(Hierar_cl_pm)
abline(h=6,col = "green")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl_pm,k=3)
fit
```

    ##  [1] 1 2 1 3 2 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [39] 3 3 3 3 3 3 3 3 3 3 3 3

``` r
plot(Hierar_cl_pm)
table(fit)
```

    ## fit
    ##  1  2  3 
    ##  2  3 45

``` r
rect.hclust(Hierar_cl_pm,k=3,border = "green")
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

## DBScan euclidean without pca

``` r
dist_matrix_eu <- proxy::dist(scale_data,method = "Euclidean")

Db_cl <- dbscan::dbscan(dist_matrix_eu,eps = 1, minPts = 3)
Db_cl 
```

    ## DBSCAN clustering for 660 objects.
    ## Parameters: eps = 1, minPts = 3
    ## Using Euclidean distances and borderpoints = TRUE
    ## The clustering contains 6 cluster(s) and 34 noise points.
    ## 
    ##   0   1   2   3   4   5   6 
    ##  34 605   3   8   4   3   3 
    ## 
    ## Available fields: cluster, eps, minPts, dist, borderPoints

``` r
Db_cl$cluster
```

    ##   [1] 0 0 0 1 2 1 2 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1
    ##  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [112] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [149] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [186] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [223] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [260] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [297] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [334] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [371] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [408] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [445] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [482] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [519] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [556] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [593] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 0 4 3 0 0 2 0 5 3 0 0 0 0 6 6 0
    ## [630] 0 0 0 0 0 0 0 0 3 4 0 0 6 0 0 5 0 0 0 0 0 3 3 4 5 0 0 0 4 3 3

``` r
plot(scale_data, col = Db_cl$cluster)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## DBScan euclidean with pca

``` r
dist_matrix_p <- proxy::dist(pca_data,method = "Euclidean")

Db_cl_p <- dbscan::dbscan(dist_matrix_p,eps = 1, minPts = 3)
Db_cl_p
```

    ## DBSCAN clustering for 660 objects.
    ## Parameters: eps = 1, minPts = 3
    ## Using Euclidean distances and borderpoints = TRUE
    ## The clustering contains 3 cluster(s) and 1 noise points.
    ## 
    ##   0   1   2   3 
    ##   1 609  46   4 
    ## 
    ## Available fields: cluster, eps, minPts, dist, borderPoints

``` r
Db_cl_p$cluster
```

    ##   [1] 1 0 1 1 2 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [112] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [149] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [186] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [223] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [260] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [297] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [334] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [371] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [408] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [445] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [482] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [519] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [556] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [593] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2
    ## [630] 2 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
plot(pca_data, col = Db_cl_p$cluster)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## DBScan Manhatton without pca

``` r
dist_matrix_m <- proxy::dist(scale_data,method = "Manhattan")

Db_cl_m <- dbscan::dbscan(dist_matrix_m,eps = 1, minPts = 3)
Db_cl_m 
```

    ## DBSCAN clustering for 660 objects.
    ## Parameters: eps = 1, minPts = 3
    ## Using Manhattan distances and borderpoints = TRUE
    ## The clustering contains 41 cluster(s) and 294 noise points.
    ## 
    ##   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19 
    ## 294   5  12  24  43   3   4  12  13   7   3   3   5   3   4   4   5  15  25   3 
    ##  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39 
    ##  28  37  11   8  18   6   3   3   4   3   3   7   5  10   4   4   3   4   3   3 
    ##  40  41 
    ##   3   3 
    ## 
    ## Available fields: cluster, eps, minPts, dist, borderPoints

``` r
Db_cl_m$cluster
```

    ##   [1]  0  0  0  0  0  1  0  0  0  0  0  1  2  4  3  0  0  0  3  0  4  7  4  5  0
    ##  [26]  0  6  0  7  4  4  4  0  0  0  7  0  7  8  3  0  9  0  7 11  8  3  3  4  3
    ##  [51]  0  4  2  0 12  3  0  0  0  8  4  0  0 10  0  5  7  3  0  3  9  3  4  4  4
    ##  [76] 10  2  0  6  9  2  4  5  4 11  0  0  0  4  8  4 14  4  2  0  9  4  7  2  4
    ## [101]  0  2  0  0  2  4  4  3  3  8  0 12  8  0  4  0  0  4 10 12  0  0  0  0  3
    ## [126]  4  8  0  4  4  4  0  4 15  4 13  1  2  4 14  1  7  8  4  8  7  0  3  0  9
    ## [151]  0  0  3  0  0  4 15  3 15  3 14 11  0  6  8  4  0 13  4  4  2  0  0  4  0
    ## [176]  0  3  0  4  0  7 12  1  2  0  0  3  3  7 13  6  4  0 15  7  2  0  9  9  0
    ## [201] 12  0  0  0  4  0  0  0  0  0  0  4  0 14  8  4  4  0  4  0  0  3  8  0  8
    ## [226]  0  3  3 29 16 17 18  0  0  0 19 18 21 20  0 20  0  0  0 24  0 17 21 22 22
    ## [251] 23 21 17 23  0  0 24 21 24 27 17 21 25 20 21 28  0 21  0 26 26  0  0 22  0
    ## [276] 23  0 17 18 22 21  0 24  0  0  0  0 29 20 18 24  0 21 20 18 21  0 19  0 18
    ## [301]  0  0 20 26 27  0 21 21 17 18 17  0 24  3 21  0  0  0 21 20 22  0 21 20 17
    ## [326] 18 27 22 21 24 21 22 24 21 28 22 25  0  0 21 24  0 21  0  0 28 21 16 18  0
    ## [351] 20 29  0 18 21  0 22 25 20 20 21  0 25 20 18 30 30  0  0 18  0 21 22 17  0
    ## [376] 23  0  0 22 16 16 20  0 21 21 28 20 24 25 17  0 17  0 20  0  0 31  0 32 33
    ## [401]  0  0 34 41 32  0 37 21  0 32 20 17 18  0 36  0  0  0  0 31 20 35 24 36  0
    ## [426] 33 32 18  0 21 31  0  0 20 34  0  0 33 16  0 36 23  0 18  0  0  0  0 21 21
    ## [451]  0 21  0  0  0 33  0 37 40  0 18  0  0  0 34 17  0 25 35  0  0 20  0 23  0
    ## [476] 39  0 18 24 20 33  0  0 21 21  0  0 18  0 18  0  0  0  0 20  0  0 34 21  0
    ## [501] 35 24 33  0  0 37 18  0  0  0 21 33  0 23 20 21  0  0  0  0  0  0  0  0  0
    ## [526]  0 20 20  0 23  0 21 20  0  0  0 38 40  0 38  0 18 39  0 33  0 40 17  0 18
    ## [551] 18  0 33  0  0  0 35  0 39 18 24 30 33 31 19  0  0 24  0 31  0  0  0 32 41
    ## [576]  0 24  0  0  0  0  0 31  0  0  0 24  0 41 17 20 37  0  0  0  0 31 18  0  0
    ## [601]  0  0  0 20 24 20  0  0 38  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ## [626]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ## [651]  0  0  0  0  0  0  0  0  0  0

## DBScan Manhatton with pca

``` r
dist_matrix_pm <- proxy::dist(pca_data,method = "Manhattan")

Db_cl_pm <- dbscan::dbscan(dist_matrix_p,eps = 1, minPts = 3)
Db_cl_pm
```

    ## DBSCAN clustering for 660 objects.
    ## Parameters: eps = 1, minPts = 3
    ## Using Euclidean distances and borderpoints = TRUE
    ## The clustering contains 3 cluster(s) and 1 noise points.
    ## 
    ##   0   1   2   3 
    ##   1 609  46   4 
    ## 
    ## Available fields: cluster, eps, minPts, dist, borderPoints

``` r
Db_cl_pm$cluster
```

    ##   [1] 1 0 1 1 2 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [112] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [149] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [186] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [223] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [260] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [297] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [334] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [371] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [408] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [445] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [482] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [519] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [556] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [593] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2
    ## [630] 2 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
plot(pca_data, col = Db_cl_pm$cluster)
```

![](Individual-work-wanissara_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->
