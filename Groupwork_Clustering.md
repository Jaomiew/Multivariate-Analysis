Group Work Clustering
================
Sitthatka Jarutsang ,Wanissara chongchai, Chirat Suwannachote, Student
M.SC.

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
```

# data prepraration

``` r
file = 'C:/Users/User/Documents/data/top-300-youtube-channels.csv'
data = read.csv(file)
head(data)
```

    ##   X Rank               Channel_Name Subscriber_Count  Video_Views Video_Count
    ## 1 0    1                   T-Series        237000000 216496000000       18831
    ## 2 1    2 Cocomelon - Nursery Rhymes        154000000 152639000000         861
    ## 3 2    3                  SET India        152000000 140138000000      105649
    ## 4 3    4                   Sony SAB         77500000  92952274861       65028
    ## 5 4    5          ✿ Kids Diana Show        108000000  88452629066        1070
    ## 6 5    6                Like Nastya        104000000  88060349741         762
    ##              Genre Channel_Started
    ## 1            Music            2006
    ## 2        Education            2006
    ## 3 Film & Animation            2006
    ## 4 Film & Animation            2007
    ## 5   People & Blogs            2015
    ## 6   People & Blogs            2016

``` r
introduce(data)
```

    ##   rows columns discrete_columns continuous_columns all_missing_columns
    ## 1  296       8                2                  6                   0
    ##   total_missing_values complete_rows total_observations memory_usage
    ## 1                    0           296               2368        36568

``` r
new_df <- subset(data, select = c(Subscriber_Count, Video_Views, 
                                  Video_Count, Channel_Started))
head(new_df)
```

    ##   Subscriber_Count  Video_Views Video_Count Channel_Started
    ## 1        237000000 216496000000       18831            2006
    ## 2        154000000 152639000000         861            2006
    ## 3        152000000 140138000000      105649            2006
    ## 4         77500000  92952274861       65028            2007
    ## 5        108000000  88452629066        1070            2015
    ## 6        104000000  88060349741         762            2016

``` r
standardized_data <- scale(new_df)
head(standardized_data)
```

    ##   Subscriber_Count Video_Views  Video_Count Channel_Started
    ## 1         8.311400   10.012628 -0.007451942      -1.3427919
    ## 2         4.952988    6.725341 -0.397487688      -1.3427919
    ## 3         4.872062    6.081804  1.876917752      -1.3427919
    ## 4         1.857584    3.652735  0.995245964      -1.0926557
    ## 5         3.091699    3.421099 -0.392951379       0.9084338
    ## 6         2.929848    3.400904 -0.399636466       1.1585700

# Euclidean Distance

## 1. K-mean

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(standardized_data, centers = k)
  sse[k] <- kmeans_model$tot.withinss
}

ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

เลือก k=8

``` r
kmeans_result <- kmeans(standardized_data,center = 8)
kmeans_result
```

    ## K-means clustering with 8 clusters of sizes 95, 3, 6, 76, 24, 8, 59, 25
    ## 
    ## Cluster means:
    ##   Subscriber_Count Video_Views Video_Count Channel_Started
    ## 1      -0.41125006 -0.31779521  -0.3125464     0.118530042
    ## 2       6.04548323  7.60659090   0.4906594    -1.342791937
    ## 3       0.04687396 -0.06637088   5.1020163    -0.675762085
    ## 4      -0.11210223 -0.15049356  -0.2927622    -1.056451820
    ## 5       1.40524369  0.41747128  -0.3313856     0.064224158
    ## 6       2.20455247  2.65471189   0.3615220     0.001690109
    ## 7      -0.22951436 -0.15239947  -0.3265066     1.383268626
    ## 8      -0.34600359 -0.12235716   1.7673092    -0.242192680
    ## 
    ## Clustering vector:
    ##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
    ##   2   2   2   6   6   6   6   6   6   6   6   5   7   7   3   7   5   8   4   5 
    ##  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
    ##   5   5   5   8   5   8   8   4   5   5   7   5   5   4   4   5   5   5   8   5 
    ##  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
    ##   4   5   5   4   4   8   5   4   7   8   7   4   4   1   4   4   7   5   1   1 
    ##  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80 
    ##   4   4   7   4   4   4   4   1   1   4   4   3   7   7   5   4   7   4   1   7 
    ##  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 
    ##   1   7   1   1   5   1   4   4   1   7   8   7   7   1   4   1   8   4   7   1 
    ## 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 
    ##   7   7   7   1   1   1   7   4   4   4   4   4   7   1   1   5   4   4   1   7 
    ## 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 
    ##   7   1   7   4   4   4   1   1   7   1   1   8   4   7   1   1   4   1   4   7 
    ## 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 
    ##   4   4   1   4   8   7   4   1   4   1   4   7   4   7   1   4   4   4   1   7 
    ## 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 
    ##   1   1   7   1   5   1   1   4   4   1   8   7   1   1   4   1   8   1   3   1 
    ## 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 
    ##   8   4   7   7   1   4   1   4   7   7   1   8   1   1   4   1   4   7   4   1 
    ## 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 
    ##   7   1   1   4   4   1   5   4   1   1   1   1   8   1   8   7   1   1   4   3 
    ## 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 
    ##   1   1   7   4   1   8   7   1   7   1   1   8   7   7   1   1   1   1   7   1 
    ## 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 
    ##   4   4   4   4   7   1   1   1   1   4   4   8   7   4   7   1   1   7   1   4 
    ## 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 
    ##   1   1   4   1   1   8   4   7   7   7   7   4   8   4   7   8   1   1   1   7 
    ## 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 
    ##   7   1   1   7   3   1   1   1   4   1   8   1   1   8   1   3 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 34.62902 19.55406 12.87152 32.83900 31.59040 23.80088 44.09649 44.81533
    ##  (between_SS / total_SS =  79.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
## plot
plot(standardized_data[,c(1,2)],
     col = kmeans_result$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
plot(standardized_data[,c(1,3)],
     col = kmeans_result$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
plot(standardized_data[,c(1,4)],
     col = kmeans_result$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
plot(standardized_data[,c(2,3)],
     col = kmeans_result$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
plot(standardized_data[,c(2,4)],
     col = kmeans_result$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

``` r
plot(standardized_data[,c(3,4)],
     col = kmeans_result$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

``` r
## plot with PCA
autoplot(kmeans_result,standardized_data,frame = TRUE)
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->

## 2. Hierarchical Cluster

``` r
test2 = (standardized_data[1:50,1:4])
distance_mat <- dist(test2, method = 'euclidean')
head(distance_mat)
```

    ## [1] 4.715646 5.552591 9.119646 8.712644 8.893065 9.398825

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
abline(h = 5,col = "green")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl,k=4)
fit
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
    ##  1  2  2  3  4  4  3  4  3  3  4  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3 
    ## 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 
    ##  3  3  3  3  3  3  3  3  3  4  4  3  3  3  3  3  3  3  3  3  3  3  3  3

``` r
plot(Hierar_cl)
table(fit)
```

    ## fit
    ##  1  2  3  4 
    ##  1  2 41  6

``` r
rect.hclust(Hierar_cl,k=4,border = "green")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

## 3. DBSCAN Cluster

``` r
dbscan_cl <- dbscan(standardized_data,eps = 0.5, MinPts = 5)
dbscan_cl
```

    ## dbscan Pts=296 MinPts=5 eps=0.5
    ##         0   1  2
    ## border 72  15  1
    ## seed    0 194 14
    ## total  72 209 15

``` r
## origin is Euclideans


dbscan_cl$cluster
```

    ##   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 1 1 0 0
    ##  [38] 1 0 1 1 0 0 1 1 0 0 1 1 0 2 0 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1
    ##  [75] 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 2 1 1 1 0 1 1 0 1 1 2 1 1 1 1 1 1 1 1
    ## [112] 1 1 1 1 0 1 1 1 1 1 1 2 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 2 1 1 1 1 0 2 1 1
    ## [149] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 0 1 2 2 1
    ## [186] 1 1 1 1 1 1 0 1 1 1 1 1 2 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 2 1 1 1 0 0 1
    ## [223] 2 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 2 1 1 1 1 1 2 1 1 1 1 0 1 0 1 1 2 1 1 1 1
    ## [260] 1 1 1 1 1 1 0 1 1 1 1 2 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 0 1 0

``` r
plot(dbscan_cl,standardized_data[,c(1,2)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(dbscan_cl,standardized_data[,c(1,3)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
plot(dbscan_cl,standardized_data[,c(1,4)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
plot(dbscan_cl,standardized_data[,c(2,3)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
plot(dbscan_cl,standardized_data[,c(2,4)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->

``` r
plot(dbscan_cl,standardized_data[,c(3,4)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-10-6.png)<!-- -->

## Manhattan Distance

## 1. K-mean

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(standardized_data, centers = k,algorithm = "Lloyd")
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

``` r
ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

เลือก k=8

``` r
kmeans_result_manhattan <- kmeans(standardized_data,center = 8,algorithm = "Lloyd")
```

    ## Warning: did not converge in 10 iterations

``` r
kmeans_result_manhattan
```

    ## K-means clustering with 8 clusters of sizes 8, 19, 24, 3, 21, 80, 92, 49
    ## 
    ## Cluster means:
    ##   Subscriber_Count Video_Views Video_Count Channel_Started
    ## 1        2.2045525   2.6547119   0.3615220     0.001690109
    ## 2       -0.2922043  -0.1376747   3.1125644    -0.618713479
    ## 3        1.4605428   0.4005830  -0.2574961    -0.425625890
    ## 4        6.0454832   7.6065909   0.4906594    -1.342791937
    ## 5       -0.5229771  -0.3043632  -0.2897429     2.099558552
    ## 6       -0.5122258  -0.3640849  -0.1847400     0.414414831
    ## 7       -0.2856335  -0.2338768  -0.2719772    -0.924085698
    ## 8        0.2645886   0.1220294  -0.2334138     0.688926542
    ## 
    ## Clustering vector:
    ##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
    ##   4   4   4   1   1   1   1   1   1   1   1   3   8   8   2   8   3   3   7   8 
    ##  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
    ##   8   3   3   8   8   2   8   3   3   8   8   3   3   7   3   3   3   8   2   8 
    ##  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
    ##   3   3   3   7   7   8   3   7   8   2   5   3   7   8   7   3   8   3   8   7 
    ##  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80 
    ##   7   7   8   7   7   7   7   6   6   7   3   2   8   8   3   7   8   7   8   8 
    ##  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 
    ##   8   8   6   8   3   8   7   7   6   8   2   8   5   6   7   8   2   7   8   6 
    ## 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 
    ##   5   8   5   7   8   6   8   7   7   7   7   7   5   8   7   3   7   7   6   8 
    ## 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 
    ##   8   6   5   7   7   7   6   6   6   6   6   5   7   8   8   6   7   7   7   5 
    ## 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 
    ##   7   7   6   7   2   5   7   7   7   6   7   8   7   8   6   7   7   7   6   8 
    ## 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 
    ##   6   6   6   6   3   6   6   7   7   6   2   8   6   6   7   6   6   7   2   6 
    ## 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 
    ##   2   7   5   5   8   7   6   7   6   6   7   7   6   6   7   6   7   5   7   6 
    ## 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 
    ##   6   8   6   7   7   6   8   7   6   7   6   6   2   6   2   5   7   7   7   2 
    ## 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 
    ##   6   6   5   7   6   6   6   6   6   6   6   2   6   6   7   6   7   6   5   7 
    ## 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 
    ##   7   7   7   7   5   7   6   7   8   7   7   6   6   7   5   6   6   5   6   7 
    ## 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 
    ##   6   6   7   6   8   7   7   6   8   6   5   7   2   7   6   6   7   6   6   5 
    ## 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 
    ##   6   6   7   5   2   7   7   6   7   6   2   6   7   6   6   2 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 23.80088 64.61527 37.13176 19.55406  7.45073 34.54298 41.25189 33.59639
    ##  (between_SS / total_SS =  77.8 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## Warning: did *not* converge in specified number of iterations

plot

``` r
plot(standardized_data[,c(1,2)],
     col = kmeans_result_manhattan$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
plot(standardized_data[,c(1,3)],
     col = kmeans_result_manhattan$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
plot(standardized_data[,c(1,4)],
     col = kmeans_result_manhattan$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

``` r
plot(standardized_data[,c(2,3)],
     col = kmeans_result_manhattan$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

``` r
plot(standardized_data[,c(2,4)],
     col = kmeans_result_manhattan$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->

``` r
plot(standardized_data[,c(3,4)],
     col = kmeans_result_manhattan$cluster,
     main = "K-mean with 8 clusters")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-13-6.png)<!-- -->

plot with PCA

``` r
autoplot(kmeans_result_manhattan,standardized_data,frame = TRUE)
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Hierarchical Cluster

``` r
test2 = (standardized_data[1:50,1:4])
distance_mat1 <- dist(test2, method = 'manhattan')
head(distance_mat1)
```

    ## [1]  7.035735  9.254532 14.066542 14.447955 14.886822 14.412377

``` r
Hierar_cl1 <- hclust(distance_mat1)
Hierar_cl1
```

    ## 
    ## Call:
    ## hclust(d = distance_mat1)
    ## 
    ## Cluster method   : complete 
    ## Distance         : manhattan 
    ## Number of objects: 50

``` r
plot(Hierar_cl1)

plot(Hierar_cl1)
abline(h = 5,col = "green")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl1,k=4)
fit
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
    ##  1  2  2  3  4  4  3  4  3  3  4  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3 
    ## 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 
    ##  3  3  3  3  3  3  3  3  3  4  4  3  3  3  3  3  3  3  3  3  3  3  3  3

``` r
plot(Hierar_cl1)
table(fit)
```

    ## fit
    ##  1  2  3  4 
    ##  1  2 41  6

``` r
rect.hclust(Hierar_cl1,k=4,border = "green")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

## DBSCAN Cluster

``` r
manhattan_dist_matrix <- proxy::dist(standardized_data,method = "Manhattan")
dbscan_cl1 <- dbscan(manhattan_dist_matrix,eps = 0.5, MinPts = 5)
dbscan_cl1
```

    ## dbscan Pts=296 MinPts=5 eps=0.5
    ## 
    ##   0 
    ## 296

``` r
dbscan_cl1$cluster
```

    ##   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [38] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [75] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [112] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [149] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [186] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [223] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [260] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

``` r
plot(dbscan_cl1,standardized_data[,c(1,2)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
plot(dbscan_cl1,standardized_data[,c(1,3)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
plot(dbscan_cl1,standardized_data[,c(1,4)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
plot(dbscan_cl1,standardized_data[,c(2,3)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

``` r
plot(dbscan_cl1,standardized_data[,c(2,4)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-16-5.png)<!-- -->

``` r
plot(dbscan_cl1,standardized_data[,c(3,4)],main = "DBScan")
```

![](Group-work-Clustering_files/figure-gfm/unnamed-chunk-16-6.png)<!-- -->
