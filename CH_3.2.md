CH_3.2
================

# One-sample Hotelling’s T-test (สำหรับการทดสอบกรณี 1 กลุ่ม)

``` r
vecmu = matrix(c(-4,4),2,1)
vecmu
```

    ##      [,1]
    ## [1,]   -4
    ## [2,]    4

``` r
Sigma = matrix(c(16,-2,-2,9),2,2)
Sigma
```

    ##      [,1] [,2]
    ## [1,]   16   -2
    ## [2,]   -2    9

ทำการทดสอบ Bivariate normal distribution

``` r
library(MASS)
set.seed(123)
n =15
sample1 = mvrnorm(n,vecmu,Sigma)
head(sample1)
```

    ##           [,1]      [,2]
    ## [1,] -3.132244 -1.610844
    ## [2,] -3.467348  2.359518
    ## [3,] -8.656225 11.158016
    ## [4,] -4.800942  2.100918
    ## [5,] -4.154909  5.464714
    ## [6,] -9.941993  8.793202

กำหนดให้ H0 : vecmu =\[-1 2\]T vs vecmu !=\[-1 2\]T

``` r
muH0 = c(-1,2)
library(DescTools)
HotellingsT2Test(sample1, mu=muH0)
```

    ## 
    ##  Hotelling's one sample T2-test
    ## 
    ## data:  sample1
    ## T.2 = 9.6051, df1 = 2, df2 = 13, p-value = 0.002746
    ## alternative hypothesis: true location is not equal to c(-1,2)

Reject H0 เพราะ p-value \< alpha

กำหนดให้ H0 : vecmu =\[-4 4\]T vs vecmu !=\[-4 4\]T

``` r
muH0 = c(-4,4)
library(DescTools)
HotellingsT2Test(sample1, mu=muH0)
```

    ## 
    ##  Hotelling's one sample T2-test
    ## 
    ## data:  sample1
    ## T.2 = 0.40523, df1 = 2, df2 = 13, p-value = 0.675
    ## alternative hypothesis: true location is not equal to c(-4,4)

Accept H0 เพราะ p-value \> alpha

# Hotelling’s-test for two independent samples (สำหรับการทดสอบกรณี 2 กลุ่มที่เป็นอิสระกัน)

``` r
library(MASS)
vecmu1 = matrix(c(-4,4),2,1)
vecmu1
```

    ##      [,1]
    ## [1,]   -4
    ## [2,]    4

``` r
Sigma = matrix(c(16,-2,-2,9),2,2)
Sigma
```

    ##      [,1] [,2]
    ## [1,]   16   -2
    ## [2,]   -2    9

``` r
n1=15
set.seed(123)
sample1 = mvrnorm(n1,vecmu1,Sigma)
head(sample1)
```

    ##           [,1]      [,2]
    ## [1,] -3.132244 -1.610844
    ## [2,] -3.467348  2.359518
    ## [3,] -8.656225 11.158016
    ## [4,] -4.800942  2.100918
    ## [5,] -4.154909  5.464714
    ## [6,] -9.941993  8.793202

``` r
n2 =18
set.seed(321)
vecmu2 = matrix(c(3,2),2,1)
vecmu2
```

    ##      [,1]
    ## [1,]    3
    ## [2,]    2

``` r
sample2 = mvrnorm(n2,vecmu2,Sigma)
head(sample2)
```

    ##           [,1]         [,2]
    ## [1,] -4.131078  2.154652686
    ## [2,]  5.464656  0.001495897
    ## [3,]  3.407248 -0.870009030
    ## [4,]  3.550146  2.176263288
    ## [5,]  2.748896 -0.909195642
    ## [6,]  2.747029  5.295693290

ค่าเฉลี่ย 2 กลุ่มแตกต่างกันหรือไม่(ทดสอบทางสถิติ)

``` r
library(corpcor)
library(Hotelling)
Htest = hotelling.test(sample1,sample2,ar.equal=TRUE)
Htest
```

    ## Test stat:  31.314 
    ## Numerator df:  2 
    ## Denominator df:  30 
    ## P-value:  2.829e-05

Reject H0 เพราะ p-value \< alpha

# Box’s M-test (การทดสอบความแปรปรวนว่ามีความแตกต่างกันหรือไม่)

``` r
nrow = n1+n2
cl = matrix(0,nrow,1)
cl[1:15]=1
cl[16:nrow]=2
head(cl)
```

    ##      [,1]
    ## [1,]    1
    ## [2,]    1
    ## [3,]    1
    ## [4,]    1
    ## [5,]    1
    ## [6,]    1

ทำการรวมตาราง 1 และ 2

``` r
sample_merge = rbind(sample1,sample2)
```

เพิ่มข้อมูลการ merge

``` r
sample_merge = cbind(sample_merge,cl)
head(sample_merge)
```

    ##           [,1]      [,2] [,3]
    ## [1,] -3.132244 -1.610844    1
    ## [2,] -3.467348  2.359518    1
    ## [3,] -8.656225 11.158016    1
    ## [4,] -4.800942  2.100918    1
    ## [5,] -4.154909  5.464714    1
    ## [6,] -9.941993  8.793202    1

การทดสอบความแปรปรวน(ทดสอบทางสถิติ)

``` r
library(biotools)
```

    ## ---
    ## biotools version 4.2

``` r
results = boxM(data=sample_merge[,1:2],group=sample_merge[,3])
results
```

    ## 
    ##  Box's M-test for Homogeneity of Covariance Matrices
    ## 
    ## data:  sample_merge[, 1:2]
    ## Chi-Sq (approx.) = 0.91463, df = 3, p-value = 0.8219

Accept H0 เพราะ p-value \> alpha

# Hotelling’s T-test for two dependent samples (การทดสอบ 2 กลุ่มที่ไม่เป็นอิสระกัน)

``` r
CLab = matrix(c(6,6,18,8,11,34,28,71,43,33,20,27,23,64,44,30,75,26,124,54,30,14),11,2)
head(CLab)
```

    ##      [,1] [,2]
    ## [1,]    6   27
    ## [2,]    6   23
    ## [3,]   18   64
    ## [4,]    8   44
    ## [5,]   11   30
    ## [6,]   34   75

``` r
SLab = matrix(c(25,28,36,35,15,44,42,54,34,29,39,15,13,22,29,31,64,30,64,56,20,21),11,2)
head(SLab)
```

    ##      [,1] [,2]
    ## [1,]   25   15
    ## [2,]   28   13
    ## [3,]   36   22
    ## [4,]   35   29
    ## [5,]   15   31
    ## [6,]   44   64

สร้างข้อมูลที่นำมาลบกันเรียบร้อยแล้ว

``` r
DLab = SLab-CLab
head(DLab)
```

    ##      [,1] [,2]
    ## [1,]   19  -12
    ## [2,]   22  -10
    ## [3,]   18  -42
    ## [4,]   27  -15
    ## [5,]    4    1
    ## [6,]   10  -11

ทดสอบทางสถิติ

``` r
library(DescTools)
muH0=c(0,0)
Htest = HotellingsT2Test(DLab, mu=muH0)
Htest
```

    ## 
    ##  Hotelling's one sample T2-test
    ## 
    ## data:  DLab
    ## T.2 = 6.1377, df1 = 2, df2 = 9, p-value = 0.02083
    ## alternative hypothesis: true location is not equal to c(0,0)

Reject H0 เพราะ p-value \< alpha

# Multivariate Analysis of Variance (MANOVA) (การทดสอบค่าเฉลี่ยมากกว่า 2 ตัว)

เริ่มด้วยการอ่าน Excel

``` r
library(readxl)
my_data = read_excel("C:/Users/User/Documents/data/iris_xcel.xlsx")
```

กำหนด Factor

``` r
my_data$Species = factor(my_data$Species)
```

ทดสอบทางสถิติ

``` r
res.man = manova(cbind(Sepal.Length, Sepal.Width,Petal.Length,Petal.Width) ~ Species, data = my_data)
summary(res.man)
```

    ##            Df Pillai approx F num Df den Df    Pr(>F)    
    ## Species     2 1.1872   52.949      8    290 < 2.2e-16 ***
    ## Residuals 147                                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Reject H0 เพราะ p-value \< alpha
