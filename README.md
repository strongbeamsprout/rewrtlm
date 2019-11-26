# rewrtlm
A rewrite simplified version of "lm/summary.lm" function in R
  
## Getting Started

### Installing

```
# The safest way to install lmcpp is to download the package lmcpp.zip from GitHub:
install.packages("Downloads/lmcpp.zip")
```

## Usage

```
#load the package
library(rewrtlm)

#load test data
data(mtcars)

#run a linear regression and 
fit = lm.rewrt(mpg ~ cyl + hp, data = mtcars, prt = TRUE)

#>Call:
#>lm.rewrt(formula = mpg ~ cyl + hp, data = mtcars, prt = TRUE)
#>
#>Coeffiecients:
#>(Intercept)         cyl          hp 
#>   36.90833    -2.26469    -0.01912 

#summarize the regression output
summ = summary.lm.rewrt(fit, correlation = TRUE, prt = TRUE)

#>Call:
#>lm.rewrt(formula = mpg ~ cyl + hp, data = mtcars, prt = TRUE)
#>
#>Residuals:
#>    Min.  1st Qu.   Median  3rd Qu.     Max. 
#>-4.49475 -2.49006 -0.18283  1.97768  7.29335 
#>
#>Coefficients:
#>            Estimate Std. Error  t value Pr(>|t|)
#>(Intercept) 36.90833    2.19080 16.84698  0.00000
#>cyl         -2.26469    0.57589 -3.93252  0.00048
#>hp          -0.01912    0.01500 -1.27472  0.21253
#>
#>Residual standard erro:3.17 on 29 degrees of freedom
#>Multiple R-squared:	0.7407,	Adjusted R-squared:	0.7228
#>F-statistic: 41.42 on 2 and 29 DF, p-value: 3.161781e-09
#>
#>Correlation of Coefficients:
#>    (Intercept) cyl  
#>cyl -0.79            
#>hp  0.35        -0.83
```

## Versioning

This is the 1st version and might be the last version of lmcpp package.

## Authors

* **strongbeamsprout** - [strongbeamsprout](https://github.com/strongbeamsprout)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* The original code of the "lm/summary.lm" function in R
* Biostatistics 625 class
