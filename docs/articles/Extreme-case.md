# Extreme case

``` r

library(MMRcaseselection)
```

The extreme case is selected with regard to the cases’ values on an
independent variable or the outcome. It is defined by the absolute
difference between the case value on the chosen variable and the
variable’s mean. For example, for the outcome the *extremeness* of a
case is defined as $`|Y_i-\hat{Y}|`$, with $`i`$ being the case index.
The extreme case is the case with the maximum absolute difference. (see
Seawright ([2016](https://doi.org/10.1177/0049124116643556))) Depending
on the research question or substantive interest, one might be
interested in the extreme case in the lower or upper range of a
variable. Extremeness is then calculated with $`Y_i-\hat{Y}`$.

The
[`extreme_on_x()`](https://ingorohlfing.github.io/MMRcaseselection/reference/extreme_on_x.md)
and
[`extreme_on_y()`](https://ingorohlfing.github.io/MMRcaseselection/reference/extreme_on_y.md)
functions take an `lm` object as input and calculate the extremeness of
all cases. For extremeness on an independent variable, one additionally
needs to specify the variable of interest as a *character*. The output
is a dataframe and cases are ordered by absolute extremeness in
decreasing order. The dataframe also presents the extremeness values
that show whether the case is extreme in the lower range of the variable
(negative values) or the positive range (positive values).

``` r

df <- lm(mpg ~ disp + wt, data = mtcars)
extreme_on_x(df, "wt")
#>                      mpg  disp    wt abs. extremeness extremeness
#> Lincoln Continental 10.4 460.0 5.424          2.20675     2.20675
#> Chrysler Imperial   14.7 440.0 5.345          2.12775     2.12775
#> Cadillac Fleetwood  10.4 472.0 5.250          2.03275     2.03275
#> Lotus Europa        30.4  95.1 1.513          1.70425    -1.70425
#> Honda Civic         30.4  75.7 1.615          1.60225    -1.60225
#> Toyota Corolla      33.9  71.1 1.835          1.38225    -1.38225
#> Fiat X1-9           27.3  79.0 1.935          1.28225    -1.28225
#> Porsche 914-2       26.0 120.3 2.140          1.07725    -1.07725
#> Fiat 128            32.4  78.7 2.200          1.01725    -1.01725
#> Datsun 710          22.8 108.0 2.320          0.89725    -0.89725
#> Merc 450SE          16.4 275.8 4.070          0.85275     0.85275
#> Toyota Corona       21.5 120.1 2.465          0.75225    -0.75225
#> Pontiac Firebird    19.2 400.0 3.845          0.62775     0.62775
#> Camaro Z28          13.3 350.0 3.840          0.62275     0.62275
#> Mazda RX4           21.0 160.0 2.620          0.59725    -0.59725
#> Merc 450SLC         15.2 275.8 3.780          0.56275     0.56275
#> Merc 450SL          17.3 275.8 3.730          0.51275     0.51275
#> Ferrari Dino        19.7 145.0 2.770          0.44725    -0.44725
#> Volvo 142E          21.4 121.0 2.780          0.43725    -0.43725
#> Duster 360          14.3 360.0 3.570          0.35275     0.35275
#> Maserati Bora       15.0 301.0 3.570          0.35275     0.35275
#> Mazda RX4 Wag       21.0 160.0 2.875          0.34225    -0.34225
#> Dodge Challenger    15.5 318.0 3.520          0.30275     0.30275
#> Valiant             18.1 225.0 3.460          0.24275     0.24275
#> Hornet Sportabout   18.7 360.0 3.440          0.22275     0.22275
#> Merc 280            19.2 167.6 3.440          0.22275     0.22275
#> Merc 280C           17.8 167.6 3.440          0.22275     0.22275
#> AMC Javelin         15.2 304.0 3.435          0.21775     0.21775
#> Merc 230            22.8 140.8 3.150          0.06725    -0.06725
#> Ford Pantera L      15.8 351.0 3.170          0.04725    -0.04725
#> Merc 240D           24.4 146.7 3.190          0.02725    -0.02725
#> Hornet 4 Drive      21.4 258.0 3.215          0.00225    -0.00225
```

The calculation of extremeness on the outcome only requires an `lm`
object as input.

``` r

df <- lm(mpg ~ disp + wt, data = mtcars)
Y_extreme <- extreme_on_y(df)
head(Y_extreme)
#>                      mpg  disp    wt abs. extremeness extremeness
#> Toyota Corolla      33.9  71.1 1.835        13.809375   13.809375
#> Fiat 128            32.4  78.7 2.200        12.309375   12.309375
#> Honda Civic         30.4  75.7 1.615        10.309375   10.309375
#> Lotus Europa        30.4  95.1 1.513        10.309375   10.309375
#> Cadillac Fleetwood  10.4 472.0 5.250         9.690625   -9.690625
#> Lincoln Continental 10.4 460.0 5.424         9.690625   -9.690625
```
