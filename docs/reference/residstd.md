# Classification of cases as typical and deviant using the standard deviation of the residuals.

The share of the standard deviation of the residuals is used to
designate cases as typical or deviant.

## Usage

``` r
residstd(lmobject, stdshare = 1)
```

## Arguments

- lmobject:

  Object generated with [`lm`](https://rdrr.io/r/stats/lm.html)

- stdshare:

  Share of standard deviation of residuals distinguishing between
  typical and deviant cases (default is 1).

## Value

A dataframe with the observed outcome, fitted outcome, residual standard
deviation and classification of cases as typical or deviant.

## Details

Proposed by Lieberman, Evan S. (2005): Nested Analysis as a Mixed-Method
Strategy for Comparative Research. *American Political Science Review*
99 (3): 435-452.
[doi:10.1017/S0003055405051762](https://doi.org/10.1017/S0003055405051762)
.

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
residstd(df, stdshare = 1)
#> Error in x$terms: object of type 'closure' is not subsettable
```
