CH_3.1
================

คำนวณมือ Example 1

# The Plausibility of as a Value for a Normal

H0 : mu == matrix(c(4,50,10),3,1)

H1 : mu != matrix(c(4,50,10),3,1)

``` r
n = 20
p = 3
alpha = 0.05
```

``` r
Xbar = matrix(c(4.64,45.4,9.965),3,1)
print(Xbar)
```

    ##        [,1]
    ## [1,]  4.640
    ## [2,] 45.400
    ## [3,]  9.965

``` r
S=matrix(c(2.879,10.01,-1.81,10.01,199.788,-5.64,-1.81,-5.64,3.628),3,3)
print(S)
```

    ##        [,1]    [,2]   [,3]
    ## [1,]  2.879  10.010 -1.810
    ## [2,] 10.010 199.788 -5.640
    ## [3,] -1.810  -5.640  3.628

``` r
mu=matrix(c(4,50,10),3,1)
print(mu)
```

    ##      [,1]
    ## [1,]    4
    ## [2,]   50
    ## [3,]   10

``` r
T2 = n*t(Xbar-mu)%*%solve(S)%*%(Xbar-mu)
paste("Hotelling’s T2 is:",T2)
```

    ## [1] "Hotelling’s T2 is: 9.74303756432536"

``` r
T2_cri=(((n-1)*p)/(n-p))*qf(alpha,p,n-p,lower.tail = FALSE)
paste("Critical value: ",T2_cri)
```

    ## [1] "Critical value:  10.7186047019865"

Example 2

H0 : mu == matrix(c(9,5),2,1)

H1 : mu != matrix(c(9,5),2,1)

``` r
n = 3
p = 2
alpha = 0.05
```

``` r
mu = matrix(c(9,5),2,1)
print(mu)
```

    ##      [,1]
    ## [1,]    9
    ## [2,]    5

``` r
X = matrix(c(6,10,8,9,6,3),3,2)
print(X)
```

    ##      [,1] [,2]
    ## [1,]    6    9
    ## [2,]   10    6
    ## [3,]    8    3

``` r
X = matrix(c(6,10,8,9,6,3),3,2)
print(X)
```

    ##      [,1] [,2]
    ## [1,]    6    9
    ## [2,]   10    6
    ## [3,]    8    3

``` r
Xbar = colMeans(X)
print(Xbar)
```

    ## [1] 8 6

``` r
Xbar = matrix(Xbar,2,1) 
print(Xbar)
```

    ##      [,1]
    ## [1,]    8
    ## [2,]    6

``` r
S = cov(X)
print(S)
```

    ##      [,1] [,2]
    ## [1,]    4   -3
    ## [2,]   -3    9

``` r
T2 = n*t(Xbar-mu)%*%solve(S)%*%(Xbar-mu)
paste("Hotelling’s T2:",T2)
```

    ## [1] "Hotelling’s T2: 0.777777777777778"

``` r
Cri_val = (((n-1)*p)/(n-p))*qf(alpha,p,n-p,lower.tail = FALSE)
paste("Critical value: ",Cri_val)
```

    ## [1] "Critical value:  798"

# Confidence Regions and Simultaneous Comparisons of Component Means

Example 3

``` r
n = 20
p = 3
alpha = 0.05
```

``` r
Xbar = matrix(c(4.64,45.4,9.965),3,1)
print(Xbar)
```

    ##        [,1]
    ## [1,]  4.640
    ## [2,] 45.400
    ## [3,]  9.965

``` r
S = matrix(c(2.879,10.01,-1.81,10.01,199.788,-5.64,-1.81,-5.64,3.628),3,3)
print(S)
```

    ##        [,1]    [,2]   [,3]
    ## [1,]  2.879  10.010 -1.810
    ## [2,] 10.010 199.788 -5.640
    ## [3,] -1.810  -5.640  3.628

The Lower bound and The Upper bound for X1 and S11

``` r
L1 = Xbar[1,1]-(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[1,1]/n))
U1 = Xbar[1,1]+(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[1,1]/n))
paste("The lower bound L1: ",L1, "/ The upper bound U1: ",U1)
```

    ## [1] "The lower bound L1:  3.39784737376965 / The upper bound U1:  5.88215262623035"

The Lower bound and The Upper bound for X2 and S22

``` r
L2 = Xbar[2,1]-(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[2,2]/n))
U2 = Xbar[2,1]+(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[2,2]/n))
paste("The lower bound L2: ",L2, "/ The upper bound U2: ",U2)
```

    ## [1] "The lower bound L2:  35.0524191324724 / The upper bound U2:  55.7475808675276"

The Lower bound and The Upper bound for X3 and S33

``` r
L3 = Xbar[3,1]-(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[3,3]/n))
U3 = Xbar[3,1]+(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[3,3]/n))
paste("The lower bound L3: ",L3, "/ The upper bound U3: ",U3)
```

    ## [1] "The lower bound L3:  8.57059873316884 / The upper bound U3:  11.3594012668312"

Example 4

We shall obtain the simultaneous 95% Bonferroni confidence intervals for
the means.

The Lower bound and The Upper bound for X1 and S11

``` r
L1 = Xbar[1,1]-((qt(1-(alpha / (2*p)),n-1))*sqrt(S[1,1]/n))
U1 = Xbar[1,1]+((qt(1-(alpha / (2*p)),n-1))*sqrt(S[1,1]/n))
paste("The lower bound L1: ",L1, "/ The upper bound U1: ",U1)
```

    ## [1] "The lower bound L1:  3.64401530485706 / The upper bound U1:  5.63598469514294"

The Lower bound and The Upper bound for X2 and S22

``` r
L2 = Xbar[2,1]-((qt(1-(alpha / (2*p)),n-1))*sqrt(S[2,2]/n))
U2 = Xbar[2,1]+((qt(1-(alpha / (2*p)),n-1))*sqrt(S[2,2]/n))
paste("The lower bound L2: ",L2, "/ The upper bound U2: ",U2)
```

    ## [1] "The lower bound L2:  37.1030870778836 / The upper bound U2:  53.6969129221164"

The Lower bound and The Upper bound for X3 and S33

``` r
L3 = Xbar[3,1]-((qt(1-(alpha / (2*p)),n-1))*sqrt(S[3,3]/n))
U3 = Xbar[3,1]+((qt(1-(alpha / (2*p)),n-1))*sqrt(S[3,3]/n))
paste("The lower bound L3: ",L3, "/ The upper bound U3: ",U3)
```

    ## [1] "The lower bound L3:  8.84693906986578 / The upper bound U3:  11.0830609301342"

\#Comparing Mean Vectors from Two Populations

Example 5

``` r
n1 = 50
n2 = 50
p = 2
alpha = 0.05
```

``` r
s1 = matrix(c(2,1,1,6),2,2)
print(s1)
```

    ##      [,1] [,2]
    ## [1,]    2    1
    ## [2,]    1    6

``` r
s2 = matrix(c(2,1,1,4),2,2)
print(s2)
```

    ##      [,1] [,2]
    ## [1,]    2    1
    ## [2,]    1    4

``` r
sp = (((n1-1)/(n1+n2-2))*s1)+(((n2-1)/(n1+n2-2))*s2)
print(sp)
```

    ##      [,1] [,2]
    ## [1,]    2    1
    ## [2,]    1    5

``` r
xbar1 = matrix(c(8.3,4.1),2,1)
print(xbar1)
```

    ##      [,1]
    ## [1,]  8.3
    ## [2,]  4.1

``` r
xbar2 = matrix(c(10.2,3.9),2,1)
print(xbar2)
```

    ##      [,1]
    ## [1,] 10.2
    ## [2,]  3.9

``` r
t2 = t(xbar1-xbar2)%*%solve(((1/n1)+(1/n2))*sp)%*%(xbar1-xbar2)
paste("The T2 value: ",t2)
```

    ## [1] "The T2 value:  52.4722222222221"

``` r
cri_val = ((n1+n2-2)*p)/(n1+n2-p-1)*qf(1-alpha,p,n1+n2-p-1)
paste("The critical value: ",cri_val)
```

    ## [1] "The critical value:  6.24408853948819"

Example 6

The Lower bound and The Upper bound for Xbar\[1,1\] and Sp\[1,1\]

``` r
L1 = (xbar1[1,1]-xbar2[1,1])-(sqrt(cri_val)*sqrt(((1/n1)+(1/n2))*sp[1,1]))
U1 = (xbar1[1,1]-xbar2[1,1])+(sqrt(cri_val)*sqrt(((1/n1)+(1/n2))*sp[1,1]))
paste("The lower bound L1: ",L1, "/ The upper bound U1: ",U1)
```

    ## [1] "The lower bound L1:  -2.60677229937162 / The upper bound U1:  -1.19322770062837"

The Lower bound and The Upper bound for Xbar\[2,1\] and Sp\[2,2\]

``` r
L2 = (xbar1[2,1]-xbar2[2,1])-(sqrt(cri_val)*sqrt(((1/n1)+(1/n2))*sp[2,2]))
U2 = (xbar1[2,1]-xbar2[2,1])+(sqrt(cri_val)*sqrt(((1/n1)+(1/n2))*sp[2,2]))
paste("The lower bound L2: ",L2, "/ The upper bound U2: ",U2)
```

    ## [1] "The lower bound L2:  -0.917505126564365 / The upper bound U2:  1.31750512656436"

Example 7

The Bonferroni 100(1-0.05)% simultaneous confidence intervals for the
differences in the mean component

``` r
qt(1-(alpha/(2*p)),n1+n2-2)
```

    ## [1] 2.276362

The Bonferroni 95% simultaneous confidence intervals for the population
difference are

1.  The Lower bound and The Upper bound for Xbar\[1,1\] and Sp\[1,1\]

``` r
L1 = (xbar1[1,1]-xbar2[1,1])-(qt(1-(alpha/(2*p)),n1+n2-2)*sqrt(((1/n1)+(1/n2))*sp[1,1]))
U1 = (xbar1[1,1]-xbar2[1,1])+(qt(1-(alpha/(2*p)),n1+n2-2)*sqrt(((1/n1)+(1/n2))*sp[1,1]))
paste("The lower bound L1: ",L1, "/ The upper bound U1: ",U1)
```

    ## [1] "The lower bound L1:  -2.54385234819145 / The upper bound U1:  -1.25614765180855"

2.The Lower bound and The Upper bound for Xbar\[2,1\] and Sp\[2,2\]

``` r
L2 = (xbar1[2,1]-xbar2[2,1])-(qt(1-(alpha/(2*p)),n1+n2-2)*sqrt(((1/n1)+(1/n2))*sp[2,2]))
U2 = (xbar1[2,1]-xbar2[2,1])+(qt(1-(alpha/(2*p)),n1+n2-2)*sqrt(((1/n1)+(1/n2))*sp[2,2]))
paste("The lower bound L2: ",L2, "/ The upper bound U2: ",U2)
```

    ## [1] "The lower bound L2:  -0.818019948566383 / The upper bound U2:  1.21801994856638"

We conclude that there is a difference in lather and mildness of soap
between the manufactured in each of two ways.

Example 8

The two-sample situation when Σ1 ≠ Σ2

``` r
n1 = 45
n2 = 55
```

``` r
S1 = matrix(c(13825.3,23823.4,23823.4,73107.4),2,2)
print(S1)
```

    ##         [,1]    [,2]
    ## [1,] 13825.3 23823.4
    ## [2,] 23823.4 73107.4

``` r
S2 = matrix(c(8632.0,19616.7,19616.7,55964.5),2,2)
print(S2)
```

    ##         [,1]    [,2]
    ## [1,]  8632.0 19616.7
    ## [2,] 19616.7 55964.5

``` r
S = (1/n1)*S1 + (1/n2)*S2
print(S)
```

    ##          [,1]      [,2]
    ## [1,] 464.1743  886.0762
    ## [2,] 886.0762 2642.1453

``` r
Xbar1 = matrix(c(204.4,556.6),2,1)
Xbar2 = matrix(c(130.0,355.0),2,1)
t(Xbar1-Xbar2)%*%solve(S)%*%(Xbar1-Xbar2)
```

    ##          [,1]
    ## [1,] 15.65853

``` r
qchisq(1-alpha,p)
```

    ## [1] 5.991465

# Testing of Equality of Covariance Matrices

Example 10

``` r
n1 = 50
n2 = 50
g = 2
p = 2
```

``` r
S1 = matrix(c(2,1,1,6),2,2)
S2 = matrix(c(2,1,1,4),2,2)
Sp = ((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
lnM = ((1/2)*(((n1-1)*log(det(S1)))+(n2-1)*log(det(S2)))) - ((1/2)*(n1-1+n2-1)*log(det(Sp)))
paste("lnM : ",lnM)
```

    ## [1] "lnM :  -1.2407714540595"

``` r
c1 = ((1/(n1-1))+(1/(n2-1))-(1/(n1-1+n2-1)))*(((2*p^2)+3*p-1)/(6*(p+1)*(g-1)))
paste("c1 : ",c1)
```

    ## [1] "c1 :  0.022108843537415"

``` r
U = -2*(1-c1)*lnM
cri_val = qchisq(1-alpha,(1/2)*(g-1)*p*(p+1))
```

``` r
if ( U < cri_val){
    paste("Since U =",U," < critical value = ",cri_val,",then do not reject H0.")
} else if(U == cri_val){
    paste("Since U =",U," = critical value = ",cri_val,",then do not reject H0.")
} else {
    paste("Since U =",U," > critical value = ",cri_val,",then reject H0.")
}
```

    ## [1] "Since U = 2.42667886423201  < critical value =  7.81472790325118 ,then do not reject H0."

``` r
c2 = (((1/(n1-1)^2)-(1/(n2-1)^2))-(1/((n1-1)+(n2-1))^2))*((p-1)*(p+2)/(6*(g-1)))
a1 = (1/2)*(g-1)*p*(p+1)
a2 = (a1+2)/abs(c2-c1^2)
b1 = (1-c1-(a1/a2))/a1
b2 = (1-c1+(2/a2))/a2

if (c2 > c1^2){
    F = -2*b1*lnM
} else if(c2 < c1^2){
    F = (-2*a2*b2*lnM)/(a1*(1+(2*b2*lnM)))
} else {
    F = NULL
}

cri_val = qf(1-alpha,a1,a2)

if(is.null(F)==0){
    if(F < cri_val){
        paste("Since F =",F," < critical value = ",cri_val,",then do not reject H0.")
    } else if(F == cri_val){
        paste("Since F =",F," = critical value = ",cri_val,",then do not reject H0.")
    } else {
        paste("Since F =",F," > critical value = ",cri_val,",then reject H0.")
    }
}else{
    print("We cannot conclude the distribution of F.")
}
```

    ## [1] "Since F = 0.809296959926695  < critical value =  2.60590046202348 ,then do not reject H0."

# Paired Comparison

A single sample with two sets of variables measured on each unit

Example 11

``` r
n = 11
```

``` r
X1 = matrix(c(6,6,18,8,11,34,28,71,43,33,20,27,23,64,44,30,75,26,124,54,30,14),11,2)
X2 = matrix(c(25,28,36,35,15,44,42,54,34,29,39,15,13,22,29,31,64,30,64,56,20,21),11,2)
d = X1-X2
dmean = colMeans(d)
dbar = matrix(c(dmean[1],dmean[2]),2,1)
print(dbar)
```

    ##           [,1]
    ## [1,] -9.363636
    ## [2,] 13.272727

``` r
sd = cov(d)
print(sd)
```

    ##           [,1]      [,2]
    ## [1,] 199.25455  88.30909
    ## [2,]  88.30909 418.61818

``` r
T2 = n*t(dbar)%*%solve(sd)%*%dbar
print(T2)
```

    ##          [,1]
    ## [1,] 13.63931

``` r
((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
```

    ## [1] 9.458877

T2 is 13.63913 greater than 9.458877.then reject H0 at level of
significance alpha is 0.05. And conclude that there is a nonzero mean
difference between the measurements of the two laboratories.

the 95% simultaneous confidence intervals for the mean differences
mu(d1) and mu(d2)

``` r
L1 = dbar[1,1]-(sqrt(((n-1)*2/(n-p))*qf(1-alpha,p,n-p))*sqrt(sd[1,1]/n))
U1 = dbar[1,1]+(sqrt(((n-1)*2/(n-p))*qf(1-alpha,p,n-p))*sqrt(sd[1,1]/n))
paste(L1," <= mu_d1 <= ",U1)
```

    ## [1] "-22.4532723477659  <= mu_d1 <=  3.72599962049317"

``` r
L2 = dbar[2,1]-(sqrt(((n-1)*2/(n-p))*qf(1-alpha,p,n-p))*sqrt(sd[2,2]/n))
U2 = dbar[2,1]+(sqrt(((n-1)*2/(n-p))*qf(1-alpha,p,n-p))*sqrt(sd[2,2]/n))
paste(L2," <= mu_d1 <= ",U2)
```

    ## [1] "-5.70011927020871  <= mu_d1 <=  32.2455738156633"

The Bonferroni 100(1-0.05)% simultaneous confidence intervals for the
individual mean difference

``` r
L1 = dbar[1,1]-(qt(1-(alpha/(2*p)),n-1)*sqrt(sd[1,1]/n))
U1 = dbar[1,1]+(qt(1-(alpha/(2*p)),n-1)*sqrt(sd[1,1]/n))
paste(L1," <= mu_d1 <= ",U1)
```

    ## [1] "-20.5731072688261  <= mu_d1 <=  1.84583454155338"

``` r
L2 = dbar[2,1]-(qt(1-(alpha/(2*p)),n-1)*sqrt(sd[2,2]/n))
U2 = dbar[2,1]+(qt(1-(alpha/(2*p)),n-1)*sqrt(sd[2,2]/n))
paste(L2," <= mu_d1 <= ",U2)
```

    ## [1] "-2.97490341536277  <= mu_d1 <=  29.5203579608173"
