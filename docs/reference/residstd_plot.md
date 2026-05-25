# Plot of typical and deviant cases based on residuals' standard deviation

Plot of typical and deviant cases based on residuals' standard deviation

## Usage

``` r
residstd_plot(resid_df)
```

## Arguments

- resid_df:

  A dataframe created with
  [`residstd`](https://ingorohlfing.github.io/MMRcaseselection/reference/residstd.md).

## Value

A plot of the observed outcome against the fitted outcome with interval
and case classifications. Created with ggplot2.

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
residstd_status <- residstd(df, stdshare = 1)
#> Error in x$terms: object of type 'closure' is not subsettable
residstd_plot(residstd_status)
#> Error: object 'residstd_status' not found
```
