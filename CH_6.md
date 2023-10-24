Factor Analysis by R
================

# เริ่มต้น Factor analysis

``` r
library(psych)
library(GPArotation)
```

    ## 
    ## Attaching package: 'GPArotation'

    ## The following objects are masked from 'package:psych':
    ## 
    ##     equamax, varimin

ทำการเรียกข้อมูล

``` r
file = 'C:/Users/User/Documents/data/EFA.csv'
dat = read.csv(file,header = T)
```

# ตรวจสอบด้วย Kaiser Meyer Olkin

``` r
datcorr = cor(dat)
datKMO = KMO(datcorr)
print(datKMO$MSA)
```

    ## [1] 0.614451

# ตรวจสอบด้วย Bartlett’s

``` r
datcorr = cor(dat)
datKMO = KMO(datcorr)
n = 90
datbart = cortest.bartlett(datcorr,n)
print(datbart$p.value)
```

    ## [1] 1.930071e-16

# หาปัจจัยร่วมที่เหมาะสม

``` r
datEig = eigen(datcorr)
datEigmean = mean(datEig$values)
numfac = sum(datEig$value>=1) #or instrad 1 to datEigmean
print(numfac)
```

    ## [1] 5

# สกัดปัจจัย PCA

``` r
datFa = principal(datcorr,nfactors=3,rotate = "none")
datFa
```

    ## Principal Components Analysis
    ## Call: principal(r = datcorr, nfactors = 3, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                       PC1   PC2   PC3   h2   u2 com
    ## Price                0.50 -0.22 -0.33 0.41 0.59 2.1
    ## Safety              -0.11  0.44 -0.31 0.30 0.70 1.9
    ## Exterior_Looks      -0.07  0.36  0.11 0.15 0.85 1.3
    ## Space_comfort        0.36  0.72 -0.14 0.67 0.33 1.6
    ## Technology           0.31  0.37 -0.12 0.25 0.75 2.2
    ## After_Sales_Service  0.52  0.38 -0.19 0.45 0.55 2.2
    ## Resale_Value         0.44 -0.56 -0.29 0.59 0.41 2.4
    ## Fuel_Type            0.32  0.52 -0.37 0.51 0.49 2.5
    ## Fuel_Efficiency      0.74 -0.11  0.09 0.57 0.43 1.1
    ## Color                0.46 -0.33  0.29 0.40 0.60 2.6
    ## Maintenance          0.66 -0.35 -0.09 0.56 0.44 1.6
    ## Test_drive           0.38  0.24  0.46 0.41 0.59 2.5
    ## Product_reviews      0.59  0.04  0.29 0.43 0.57 1.5
    ## Testimonials         0.12  0.31  0.75 0.67 0.33 1.4
    ## 
    ##                        PC1  PC2  PC3
    ## SS loadings           2.76 2.16 1.46
    ## Proportion Var        0.20 0.15 0.10
    ## Cumulative Var        0.20 0.35 0.46
    ## Proportion Explained  0.43 0.34 0.23
    ## Cumulative Proportion 0.43 0.77 1.00
    ## 
    ## Mean item complexity =  1.9
    ## Test of the hypothesis that 3 components are sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.1 
    ## 
    ## Fit based upon off diagonal values = 0.72

# กรณีการหมุ่นแกน

``` r
datFaVarMax = principal(datcorr,nfactors = 4,rotate = "varimax")
datFaVarMax
```

    ## Principal Components Analysis
    ## Call: principal(r = datcorr, nfactors = 4, rotate = "varimax")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                       RC1   RC2   RC3   RC4   h2   u2 com
    ## Price                0.71  0.13 -0.05 -0.13 0.54 0.46 1.2
    ## Safety              -0.37  0.50 -0.27  0.15 0.49 0.51 2.7
    ## Exterior_Looks       0.03  0.05  0.27 -0.78 0.69 0.31 1.2
    ## Space_comfort       -0.03  0.76  0.23 -0.24 0.68 0.32 1.4
    ## Technology           0.04  0.49  0.11  0.01 0.26 0.74 1.1
    ## After_Sales_Service  0.14  0.66  0.12  0.20 0.51 0.49 1.4
    ## Resale_Value         0.78 -0.14 -0.14  0.08 0.65 0.35 1.2
    ## Fuel_Type            0.07  0.70 -0.05 -0.11 0.51 0.49 1.1
    ## Fuel_Efficiency      0.51  0.24  0.36  0.37 0.59 0.41 3.2
    ## Color                0.19 -0.05  0.28  0.76 0.69 0.31 1.4
    ## Maintenance          0.68  0.08  0.13  0.27 0.56 0.44 1.4
    ## Test_drive           0.05  0.15  0.63 -0.01 0.42 0.58 1.1
    ## Product_reviews      0.38  0.14  0.55  0.02 0.47 0.53 2.0
    ## Testimonials        -0.30 -0.03  0.76  0.01 0.67 0.33 1.3
    ## 
    ##                        RC1  RC2  RC3  RC4
    ## SS loadings           2.27 2.13 1.76 1.56
    ## Proportion Var        0.16 0.15 0.13 0.11
    ## Cumulative Var        0.16 0.31 0.44 0.55
    ## Proportion Explained  0.29 0.28 0.23 0.20
    ## Cumulative Proportion 0.29 0.57 0.80 1.00
    ## 
    ## Mean item complexity =  1.5
    ## Test of the hypothesis that 4 components are sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.09 
    ## 
    ## Fit based upon off diagonal values = 0.78
