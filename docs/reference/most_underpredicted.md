# Identification of the most underpredicted case

The case with the largest positive difference between the observed value
and the predicted value on the outcome. Depending on the research
question, there might be a specific interest in the case for which the
model performs worst and yields a smaller predicted value.

## Usage

``` r
most_underpredicted(lmobject)
```

## Arguments

- lmobject:

  Object generated with [`lm`](https://rdrr.io/r/stats/lm.html)

## Value

The most underpredicted case with the largest positive residual (the
most positive residual).

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
most_underpredicted(df)
#> Toyota Corolla 
#>        6.34844 
```
