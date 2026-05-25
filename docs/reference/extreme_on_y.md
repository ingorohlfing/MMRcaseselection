# Extremeness of cases on the dependent variable

Extremeness of a case is calculated by the difference between a case's
value on the dependent variable and the variable's mean value.

## Usage

``` r
extreme_on_y(lmobject)
```

## Arguments

- lmobject:

  Object generated with [`lm`](https://rdrr.io/r/stats/lm.html)

## Value

A dataframe with

\- all variables in the linear model,

\- absolute extremeness (absolute value of difference between variable
score and mean value of variable),

\- extremeness (difference between variable score and mean value of
variable), which can be useful when the direction of extremeness is
relevant.

The rows are ordered in decreasing order of the absolute extreme values.

## Details

Calculating the absolute value of the difference between the cases'
values and the variable's mean value is proposed by Seawright, Jason
(2016): The Case for Selecting Cases That Are Deviant or Extreme on the
Independent Variable. *Sociological Methods & Research* 45 (3): 493-525.
([doi:10.1177/0049124116643556](https://doi.org/10.1177/0049124116643556)
)

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
extreme_on_y(df)
#>                      mpg  disp    wt abs. extremeness extremeness
#> Toyota Corolla      33.9  71.1 1.835        13.809375   13.809375
#> Fiat 128            32.4  78.7 2.200        12.309375   12.309375
#> Honda Civic         30.4  75.7 1.615        10.309375   10.309375
#> Lotus Europa        30.4  95.1 1.513        10.309375   10.309375
#> Cadillac Fleetwood  10.4 472.0 5.250         9.690625   -9.690625
#> Lincoln Continental 10.4 460.0 5.424         9.690625   -9.690625
#> Fiat X1-9           27.3  79.0 1.935         7.209375    7.209375
#> Camaro Z28          13.3 350.0 3.840         6.790625   -6.790625
#> Porsche 914-2       26.0 120.3 2.140         5.909375    5.909375
#> Duster 360          14.3 360.0 3.570         5.790625   -5.790625
#> Chrysler Imperial   14.7 440.0 5.345         5.390625   -5.390625
#> Maserati Bora       15.0 301.0 3.570         5.090625   -5.090625
#> Merc 450SLC         15.2 275.8 3.780         4.890625   -4.890625
#> AMC Javelin         15.2 304.0 3.435         4.890625   -4.890625
#> Dodge Challenger    15.5 318.0 3.520         4.590625   -4.590625
#> Merc 240D           24.4 146.7 3.190         4.309375    4.309375
#> Ford Pantera L      15.8 351.0 3.170         4.290625   -4.290625
#> Merc 450SE          16.4 275.8 4.070         3.690625   -3.690625
#> Merc 450SL          17.3 275.8 3.730         2.790625   -2.790625
#> Datsun 710          22.8 108.0 2.320         2.709375    2.709375
#> Merc 230            22.8 140.8 3.150         2.709375    2.709375
#> Merc 280C           17.8 167.6 3.440         2.290625   -2.290625
#> Valiant             18.1 225.0 3.460         1.990625   -1.990625
#> Toyota Corona       21.5 120.1 2.465         1.409375    1.409375
#> Hornet Sportabout   18.7 360.0 3.440         1.390625   -1.390625
#> Hornet 4 Drive      21.4 258.0 3.215         1.309375    1.309375
#> Volvo 142E          21.4 121.0 2.780         1.309375    1.309375
#> Mazda RX4           21.0 160.0 2.620         0.909375    0.909375
#> Mazda RX4 Wag       21.0 160.0 2.875         0.909375    0.909375
#> Merc 280            19.2 167.6 3.440         0.890625   -0.890625
#> Pontiac Firebird    19.2 400.0 3.845         0.890625   -0.890625
#> Ferrari Dino        19.7 145.0 2.770         0.390625   -0.390625
```
