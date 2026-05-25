# Classification of cases as typical and deviant using a prediction interval.

Case are designated as typical (= well predicted) and deviant (= badly
predicted) based on the prediction interval. The x% prediction interval
represents the range that we expect to include x% of outcome values in
repeated samples. For example, a 95% prediction interval ranging from
0-5 conveys that 95% of future outcome values will be in the range of
0-5. If the observed outcome is inside the prediction interval, the case
is classified (or designated) as typical and as deviant otherwise.

## Usage

``` r
predint(lmobject, piwidth = 0.95)
```

## Arguments

- lmobject:

  Object generated with [`lm`](https://rdrr.io/r/stats/lm.html)

- piwidth:

  Width of the prediction interval (default is 0.95).

## Value

A dataframe with the observed outcome, fitted outcome, upper and lower
bound of the % prediction interval and classification of cases as
typical or deviant.

## Details

Proposed by Rohlfing, Ingo and Peter Starke (2013): Building on Solid
Ground: Robust Case Selection in Multi-Method Research. \*Swiss
Political Science Review\* 19 (4): 492-512.
([doi:10.1111/spsr.12052](https://doi.org/10.1111/spsr.12052) )

## Examples

``` r
df <- lm(mpg ~ disp + wt, data = mtcars)
predint(df, piwidth = 0.9)
#>                           fit       lwr      upr outcome  status
#> Mazda RX4           23.345433 18.283456 28.40741    21.0 typical
#> Mazda RX4 Wag       22.490972 17.423867 27.55808    21.0 typical
#> Datsun 710          25.272367 20.162824 30.38191    22.8 typical
#> Hornet 4 Drive      19.614667 14.563903 24.66543    21.4 typical
#> Hornet Sportabout   17.052807 11.759861 22.34575    18.7 typical
#> Valiant             19.378631 14.315024 24.44224    18.1 typical
#> Duster 360          16.617200 11.384029 21.85037    14.3 typical
#> Merc 240D           21.671201 16.482348 26.86005    24.4 typical
#> Merc 230            21.909810 16.715283 27.10434    22.8 typical
#> Merc 280            20.463048 15.241708 25.68439    19.2 typical
#> Merc 280C           20.463048 15.241708 25.68439    17.8 typical
#> Merc 450SE          16.434210 11.280831 21.58759    16.4 typical
#> Merc 450SL          17.573491 12.515664 22.63132    17.3 typical
#> Merc 450SLC         17.405950 12.339554 22.47235    15.2 typical
#> Cadillac Fleetwood   9.002642  3.637621 14.36766    10.4 typical
#> Lincoln Continental  8.632295  3.206218 14.05837    10.4 typical
#> Chrysler Imperial    9.251505  3.839323 14.66369    14.7 deviant
#> Fiat 128            26.193801 21.043418 31.34418    32.4 deviant
#> Honda Civic         28.207208 22.953215 33.46120    30.4 typical
#> Toyota Corolla      27.551560 22.364024 32.73910    33.9 deviant
#> Toyota Corona       24.572028 19.477070 29.66699    21.5 typical
#> Dodge Challenger    17.529180 12.421144 22.63722    15.5 typical
#> AMC Javelin         18.062147 12.968523 23.15577    15.2 typical
#> Camaro Z28          15.889724 10.767485 21.01196    13.3 typical
#> Pontiac Firebird    14.986733  9.692819 20.28065    19.2 typical
#> Fiat X1-9           27.076452 21.909290 32.24361    27.3 typical
#> Porsche 914-2       25.657501 20.527806 30.78720    26.0 typical
#> Lotus Europa        28.205132 22.867032 33.54323    30.4 typical
#> Ford Pantera L      18.117052 12.715793 23.51831    15.8 typical
#> Ferrari Dino        23.108680 18.029629 28.18773    19.7 typical
#> Maserati Bora       17.662960 12.597715 22.72820    15.0 typical
#> Volvo 142E          23.500566 18.364678 28.63645    21.4 typical
```
