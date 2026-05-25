# Identification of the most deviant case

Identification of the most deviant case (= worst predicted case), based
on regression estimates.

## Usage

``` r
most_deviant(lmobject)
```

## Arguments

- lmobject:

  Object generated with [`lm`](https://rdrr.io/r/stats/lm.html)

## Value

The most deviant case with the largest absolute residual of all cases.

## Details

Proposed by Seawright, Jason and John Gerring (2008): Case Selection
Techniques in Case Study Research: A Menu of Qualitative and
Quantitative Options. *Political Research Quarterly* 61 (2): 294-308.
([doi:10.1177/1065912907313077](https://doi.org/10.1177/1065912907313077)
)

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
most_deviant(df)
#> Toyota Corolla 
#>        6.34844 
```
