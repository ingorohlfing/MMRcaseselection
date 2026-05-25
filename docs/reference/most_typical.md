# Identification of the most typical case

The most typical case (= best predicted case) based on regression
estimates.

## Usage

``` r
most_typical(lmobject)
```

## Arguments

- lmobject:

  Object generated with [`lm`](https://rdrr.io/r/stats/lm.html)

## Value

The most typical case having the smallest absolute residual of all
cases.

## Details

Proposed by Seawright, Jason and John Gerring (2008): Case Selection
Techniques in Case Study Research: A Menu of Qualitative and
Quantitative Options. *Political Research Quarterly* 61 (2): 294-308.
([doi:10.1177/1065912907313077](https://doi.org/10.1177/1065912907313077)
)

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
most_typical(df)
#> Merc 450SE 
#> 0.03421046 
```
