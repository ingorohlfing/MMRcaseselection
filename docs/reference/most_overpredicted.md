# Identification of the most overpredicted case

The case with the largest negative difference between the observed value
and the predicted value on the outcome. Depending on the research
question, there might be a specific interest in the case for which the
model performs worst and yields a larger predicted value.

## Usage

``` r
most_overpredicted(lmobject)
```

## Arguments

- lmobject:

  Object generated with [`lm`](https://rdrr.io/r/stats/lm.html)

## Value

The most overpredicted case with the largest negative residual (the most
negative residual).

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
most_overpredicted(df)
#> Ferrari Dino 
#>     -3.40868 
```
