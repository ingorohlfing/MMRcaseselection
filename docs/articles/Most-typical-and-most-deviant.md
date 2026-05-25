# Most typical and most deviant

``` r

library(MMRcaseselection)
```

One can use four functions to choose four *types of cases* without
classifying all cases as either typical or deviant. The *most typical*
and *most deviant* cases are proposed by Seawright and Gerring
([2008](https://doi.org/10.1177/1065912907313077)). The most typical
case has the smallest residual of all cases. The most deviant case has
the largest residual of all cases. The two functions
[`most_typical()`](https://ingorohlfing.github.io/MMRcaseselection/reference/most_typical.md)
and
[`most_deviant()`](https://ingorohlfing.github.io/MMRcaseselection/reference/most_deviant.md)
work in the same way and show you the case with its residual. The input
into the function is an `lm` object.

``` r

df <- lm(mpg ~ disp + wt, data = mtcars)
most_typical(df)
#> Merc 450SE 
#> 0.03421046
most_deviant(df)
#> Toyota Corolla 
#>        6.34844
```

The most deviant case does not distinguish between cases that have a
large negative and a large positive residual. Cases with a negative
residual are *overpredicted* because the predicted outcome is higher
than the observed outcome. Cases with a positive residual are
*underpredicted* because the predicted outcome is lower than the
observed outcome. It might not matter whether a case is overpredicted or
underpredicted because both subtypes of outliers can have the same type
of deviance. However, one might be interested in knowing whether a case
has a positive or negative residual and what the most overpredicted and
underpredicted cases are. This is what the functions
[`most_overpredicted()`](https://ingorohlfing.github.io/MMRcaseselection/reference/most_overpredicted.md)
and
[`most_underpredicted()`](https://ingorohlfing.github.io/MMRcaseselection/reference/most_underpredicted.md)
achieve, each taking an `lm` object as input.

``` r

# largest positive residual
most_underpredicted(df)
#> Toyota Corolla 
#>        6.34844
# largest negative residual
most_overpredicted(df)
#> Ferrari Dino 
#>     -3.40868
```

The package does not include functions for plotting the cases. There are
multiple, very useful packages such as the
[olsrr](https://olsrr.rsquaredacademy.com/articles/residual_diagnostics.html)
package that can be used for the easy visualization of residuals.
