CH_4
================

# Comparing Several Multivariate Population Means (One-Way MANOVA)

Example 1

``` r
n1 = 271
n2 = 138
n3 = 107
p = 4
```

``` r
X1bar = matrix(c(2.006,0.48,0.082,0.36),4,1)
X2bar = matrix(c(2.167,0.596,0.124,0.418),4,1)
X3bar = matrix(c(2.273,0.521,0.125,0.383),4,1)

S1=matrix(c(0.291,-0.001,0.002,0.01,-0.001,
            0.011,0.001,0.003,0.002,0.001,
            0.001,0.001,0.01,0.003,0.001,0.01),4,4)
S2=matrix(c(0.561,0.011,0.001,0.037,0.011,
            0.025,0.004,0.007,0.001,0.004,
            0.005,0.002,0.037,0.007,0.002,0.019),4,4)
S3=matrix(c(0.261,0.03,0.003,0.018,0.03,
            0.017,-0.001,0.006,0.003,-0.001,
            0.004,0.001,0.018,0.006,0.001,0.013),4,4)
```

``` r
W = (n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3
print(W)
```

    ##         [,1]  [,2]  [,3]  [,4]
    ## [1,] 183.093 4.417 0.995 9.677
    ## [2,]   4.417 8.197 0.712 2.405
    ## [3,]   0.995 0.712 1.379 0.650
    ## [4,]   9.677 2.405 0.650 6.681

``` r
xbar = (n1*X1bar+n2*X2bar+n3*X3bar)/(n1+n2+n3)
print(xbar)
```

    ##           [,1]
    ## [1,] 2.1044244
    ## [2,] 0.5195252
    ## [3,] 0.1021492
    ## [4,] 0.3802810

``` r
B = (n1*(X1bar - xbar)%*%t(X1bar - xbar)) + (n2*(X2bar - xbar)%*%t(X2bar - xbar)) + (n3*(X3bar - xbar)%*%t(X3bar - xbar))
print(B)
```

    ##           [,1]      [,2]      [,3]      [,4]
    ## [1,] 6.2063401 1.7412510 1.1383043 0.9157195
    ## [2,] 1.7412510 1.2306787 0.4500336 0.6157338
    ## [3,] 1.1383043 0.4500336 0.2317835 0.2311294
    ## [4,] 0.9157195 0.6157338 0.2311294 0.3085943

``` r
lamda = det(W)/det(B+W)
print(lamda)
```

    ## [1] 0.7613785

``` r
comp_val = ((n1 + n2 + n3 - p - 2)/p)*((1 - sqrt(lamda))/sqrt(lamda))
print(comp_val)
```

    ## [1] 18.62007

ค่าที่ได้มีค่า 18.62007 มากกว่า F(8,1020,0.05) 1.95 ฉะนั้นปฎิเสธ H0
สามารถสรุปได้ว่าที่ระดับนัยสำคัญ 0.05ค่าเฉลี่ยของค่าใช้จ่ายขึ้นอยู่กับเจ้าของกิจการ

Example 2

``` r
X1 = matrix(c(21,25,20,24,12,8,12,10),4,2)
X2 = matrix(c(31,23,24,28,9,12,13,10),4,2)
X3 = matrix(c(34,29,35,32,10,14,11,13),4,2)
X4 = matrix(c(33,38,34,35,14,12,13,13),4,2)
```

``` r
X1mean = colMeans(X1)
paste('X1mean[1] = ',X1mean[1],' || X1mean[2] = ',X1mean[2],sep = "")
```

    ## [1] "X1mean[1] = 22.5 || X1mean[2] = 10.5"

``` r
X2mean = colMeans(X2)
paste('X2mean[1] = ',X2mean[1],' || X2mean[2] = ',X2mean[2],sep = "")
```

    ## [1] "X2mean[1] = 26.5 || X2mean[2] = 11"

``` r
X3mean = colMeans(X3)
paste('X3mean[1] = ',X3mean[1],' || X3mean[2] = ',X3mean[2],sep = "")
```

    ## [1] "X3mean[1] = 32.5 || X3mean[2] = 12"

``` r
X4mean = colMeans(X4)
paste('X4mean[1] = ',X4mean[1],' || X4mean[2] = ',X4mean[2],sep = "")
```

    ## [1] "X4mean[1] = 35 || X4mean[2] = 13"

``` r
n1 = length(X1[,1])
n2 = length(X2[,1])
n3 = length(X3[,1])
n4 = length(X4[,1])
p = length(X1[1,])
S1 = cov(X1)
S2 = cov(X2)
S3 = cov(X3)
S4 = cov(X4)
W = (n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3 + (n4-1)*S4
print(W)
```

    ##      [,1] [,2]
    ## [1,]   93  -50
    ## [2,]  -50   33

``` r
xbar = (n1*X1mean + n2*X2mean + n3*X3mean + n4*X4mean)/(n1+n2+n3+n4)
xbar
```

    ## [1] 29.125 11.625

``` r
B = (n1*(X1mean-xbar) %*% t(X1mean-xbar)) + (n2*(X2mean-xbar) %*% t(X2mean-xbar)) +
  (n3*(X3mean-xbar) %*% t(X3mean-xbar)) + (n4*(X4mean-xbar) %*% t(X4mean-xbar))
print(B)
```

    ##        [,1]  [,2]
    ## [1,] 386.75 73.75
    ## [2,]  73.75 14.75

``` r
Lamda = det(W)/det(B+W)
paste("Lamda = ",Lamda)
```

    ## [1] "Lamda =  0.0254654493376298"

``` r
statistic = (((n1+n2+n3+n4)-4-1)/(4-1))*((1-sqrt(Lamda))/sqrt(Lamda))
paste("statistic = ",statistic)
```

    ## [1] "statistic =  19.3104621263056"

``` r
alpha = 0.05
cri_val = qf(1-alpha,6,27)
paste("Critical value = ",cri_val)
```

    ## [1] "Critical value =  2.45910844257833"

จากผลสามารถสรุปได้ว่าค่าคำนวณสถิติ 19.31 มากกว่า 2.459 ฉะนั้น Reject H0

# 2) Simultaneous Confidence Intervals for Treatment Effects

When the hypothesis of equal treatment effects is rejected, those
effects that led to the rejection of the hypothesis are of interest.

For pairwise comparisons, the Bonferroni approach can be used to
construct simultaneous confidence intervals for the components of the
differences τk =τl

Example 3

ต้องการทดสอบว่า

H0: mu1=mu2 ; H1: mu1=!mu2

H0: mu1=mu3 ; H1: mu1=!mu3

H0: mu2=mu3 ; H1: mu2=!mu3

``` r
X1bar = matrix(c(2.006,0.48,0.082,0.36),4,1)
X2bar = matrix(c(2.167,0.596,0.124,0.418),4,1)
X3bar = matrix(c(2.273,0.521,0.125,0.383),4,1)
xbar = (n1*X1bar+n2*X2bar+n3*X3bar)/(n1+n2+n3)

S1=matrix(c(0.291,-0.001,0.002,0.01,-0.001,
            0.011,0.001,0.003,0.002,0.001,
            0.001,0.001,0.01,0.003,0.001,0.01),4,4)
S2=matrix(c(0.561,0.011,0.001,0.037,0.011,
            0.025,0.004,0.007,0.001,0.004,
            0.005,0.002,0.037,0.007,0.002,0.019),4,4)
S3=matrix(c(0.261,0.03,0.003,0.018,0.03,
            0.017,-0.001,0.006,0.003,-0.001,
            0.004,0.001,0.018,0.006,0.001,0.013),4,4)

xbar = (n1*X1bar+n2*X2bar+n3*X3bar)/(n1+n2+n3)
```

``` r
Mu1 = X1bar-xbar
Mu1 = Mu1[3]
paste("Mu1 = ",Mu1)
```

    ## [1] "Mu1 =  -0.0283333333333333"

``` r
Mu2 = X2bar-xbar
Mu2 = Mu2[3]
paste("Mu2 = ",Mu2)
```

    ## [1] "Mu2 =  0.0136666666666667"

``` r
Mu3=X3bar-xbar
Mu3 = Mu3[3]
paste("Mu3 = ",Mu3)
```

    ## [1] "Mu3 =  0.0146666666666667"

``` r
W = (n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3
W = W[3,3]
```

``` r
n1 = 271
n2 = 138
n3 = 107
p = 4
g = 3
alpha = 0.05
n = n1 + n2 + n3
t_value = qt(alpha/(p*g*(g-1)), n-g , lower.tail=FALSE)
```

### ทดสอบว่า Mu1 and Mu2 แตกต่างกันหรือไม่

H0: mu1 = mu2

H1: mu1 =! mu2

``` r
a = t_value * sqrt((W/(n-g))*((1/n1)+(1/n2)))
Confidence_up_13_23 = (Mu1 - Mu2) + a
Confidence_up_13_23
```

    ## [1] -0.03969826

``` r
Confidence_low_13_23 = (Mu1 - Mu2) - a
Confidence_low_13_23
```

    ## [1] -0.04430174

The 95% simultaneous confidence of Mu13 and Mu23 is (-0.05760551 ,
-0.02639449)

### ทดสอบว่า Mu1 and Mu3 แตกต่างกันหรือไม่

H0: mu1=mu3

H1: mu1=!mu3

``` r
b = t_value * sqrt((W/(n-g))*((1/n1)+(1/n3)))
Confidence_up_13_33 = (Mu1 - Mu3) + b
Confidence_up_13_33
```

    ## [1] -0.04048702

``` r
Confidence_low_13_33 = (Mu1 - Mu3) - b
Confidence_low_13_33
```

    ## [1] -0.04551298

The 95% simultaneous confidence of Mu13 and Mu33 is (-0.06003765 ,
-0.02596235)

### ทดสอบว่า Mu2 and Mu3 แตกต่างกันหรือไม่

H0: mu2 = mu3

H1: mu2 =! mu3

``` r
c = t_value * sqrt((W/(n-g))*((1/n2)+(1/n3)))
Confidence_up_23_33 = (Mu2 - Mu3) + c
Confidence_up_23_33
```

    ## [1] 0.001835114

``` r
Confidence_low_23_33 = (Mu2 - Mu3) - c
Confidence_low_23_33
```

    ## [1] -0.003835114

The 95% simultaneous confidence of Mu23 and Mu33 is (-0.02022171 ,
0.01822171)

ดังนั้น ค่าใช้จ่ายนี้จึงมีความแตกต่างระหว่างบ้านพักคนชราเอกชนและ
บ้านพักคนชราที่ไม่แสวงหาผลกำไร

แต่ไม่พบความแตกต่างระหว่างบ้านพักคนชราที่ไม่แสวงหาผลกำไร และบ้านพักข้าราชการ
