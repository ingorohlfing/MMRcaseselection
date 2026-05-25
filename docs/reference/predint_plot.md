# Plot of typical and deviant cases with prediction intervals

Presented in Rohlfing, Ingo and Peter Starke (2013): Building on Solid
Ground: Robust Case Selection in Multi-Method Research. *Swiss Political
Science Review* 19 (4): 492-512.
([doi:10.1111/spsr.12052](https://doi.org/10.1111/spsr.12052) )

## Usage

``` r
predint_plot(pred_df)
```

## Arguments

- pred_df:

  A dataframe created with
  [`predint`](https://ingorohlfing.github.io/MMRcaseselection/reference/predint.md).

## Value

A plot of the observed outcome against the fitted outcome with
prediction intervals and case classifications. Created with ggplot2.

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
predint_status <- predint(df, piwidth = 0.9)
predint_plot(predint_status)

```
