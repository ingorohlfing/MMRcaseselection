# Pathway case

Calculation of pathway values, defined as the difference between
residuals of full model and reduced model lacking the pathway variable.
The larger the difference, the more a case qualifies as a pathway case
suitable for the analysis of mechanisms.

## Usage

``` r
pathway(full_model, reduced_model)
```

## Arguments

- full_model:

  Full model including covariate of interest (= pathway variable)

- reduced_model:

  Reduced model excluding covariate of interest

## Value

A dataframe with

\- all full model variables,

\- full model residuals (`full_resid`),

\- reduced model residuals (`reduced_resid`),

\- pathway values following Weller/Barnes (`pathway_wb`),

\- pathway values following Gerring (`pathway_gvalue`),

\- variable showing whether Gerring's criterion for a pathway case is
met (`pathway_gstatus`)

## Details

The difference between the absolute residuals of the full and reduced
model follows the approach developed by Weller and Barnes (2014):
*Finding Pathways: Mixed-Method Research for Studying Causal
Mechanisms.* Cambridge: Cambridge University Press.
[doi:10.1017/CBO9781139644501](https://doi.org/10.1017/CBO9781139644501)
).

The calculation of the absolute difference between the full-model and
reduced-model residuals, given a case's reduced-model residual is larger
than its full-model residual, follows the proposal by Gerring (2007): Is
There a (Viable) Crucial-Case Method? *Comparative Political Studies* 40
(3): 231-253.
[doi:10.1177/0010414006290784](https://doi.org/10.1177/0010414006290784)
)

## Examples

``` r
df_full <- lm(mpg ~ disp + wt, data = mtcars)
df_reduced <- lm(mpg ~ wt, data = mtcars)
pathway(df_full, df_reduced)
#>                      mpg  disp    wt  full_resid reduced_resid  pathway_wb
#> Mazda RX4           21.0 160.0 2.620 -2.34543258    -2.2826106 -0.06282193
#> Mazda RX4 Wag       21.0 160.0 2.875 -1.49097212    -0.9197704 -0.57120172
#> Datsun 710          22.8 108.0 2.320 -2.47236688    -2.0859521 -0.38641476
#> Hornet 4 Drive      21.4 258.0 3.215  1.78533343     1.2973499 -0.48798349
#> Hornet Sportabout   18.7 360.0 3.440  1.64719305    -0.2001440 -1.44704909
#> Valiant             18.1 225.0 3.460 -1.27863092    -0.6932545 -0.58537640
#> Duster 360          14.3 360.0 3.570 -2.31719966    -3.9053627  1.58816299
#> Merc 240D           24.4 146.7 3.190  2.72879876     4.1637381  1.43493939
#> Merc 230            22.8 140.8 3.150  0.89018976     2.3499593  1.45976953
#> Merc 280            19.2 167.6 3.440 -1.26304775     0.2998560 -0.96319171
#> Merc 280C           17.8 167.6 3.440 -2.66304775    -1.1001440 -1.56290379
#> Merc 450SE          16.4 275.8 4.070 -0.03421046     0.8668731  0.83266267
#> Merc 450SL          17.3 275.8 3.730 -0.27349107    -0.0502472 -0.22324387
#> Merc 450SLC         15.2 275.8 3.780 -2.20594981    -1.8830236 -0.32292618
#> Cadillac Fleetwood  10.4 472.0 5.250  1.39735826     1.1733496 -0.22400867
#> Lincoln Continental 10.4 460.0 5.424  1.76770494     2.1032876  0.33558271
#> Chrysler Imperial   14.7 440.0 5.345  5.44849485     5.9810744  0.53257954
#> Fiat 128            32.4  78.7 2.200  6.20619907     6.8727113  0.66651222
#> Honda Civic         30.4  75.7 1.615  2.19279202     1.7461954 -0.44659660
#> Toyota Corolla      33.9  71.1 1.835  6.34843977     6.4219792  0.07353940
#> Toyota Corona       21.5 120.1 2.465 -3.07202780    -2.6110037 -0.46102406
#> Dodge Challenger    15.5 318.0 3.520 -2.02918018    -2.9725862  0.94340605
#> AMC Javelin         15.2 304.0 3.435 -2.86214676    -3.7268663  0.86471956
#> Camaro Z28          13.3 350.0 3.840 -2.58972426    -3.4623553  0.87263107
#> Pontiac Firebird    19.2 400.0 3.845  4.21326708     2.4643670 -1.74890005
#> Fiat X1-9           27.3  79.0 1.935  0.22354778     0.3564263  0.13287854
#> Porsche 914-2       26.0 120.3 2.140  0.34249891     0.1520430 -0.19045591
#> Lotus Europa        30.4  95.1 1.513  2.19486787     1.2010593 -0.99380855
#> Ford Pantera L      15.8 351.0 3.170 -2.31705249    -4.5431513  2.22609879
#> Ferrari Dino        19.7 145.0 2.770 -3.40867994    -2.7809399 -0.62774003
#> Maserati Bora       15.0 301.0 3.570 -2.66295957    -3.2053627  0.54240308
#> Volvo 142E          21.4 121.0 2.780 -2.10056555    -1.0274952 -1.07307036
#>                     pathway_gvalue pathway_gtype
#> Mazda RX4               0.06282193            no
#> Mazda RX4 Wag           0.57120172            no
#> Datsun 710              0.38641476            no
#> Hornet 4 Drive          0.48798349            no
#> Hornet Sportabout       1.84733701            no
#> Valiant                 0.58537640            no
#> Duster 360              1.58816299           yes
#> Merc 240D               1.43493939           yes
#> Merc 230                1.45976953           yes
#> Merc 280                1.56290379            no
#> Merc 280C               1.56290379            no
#> Merc 450SE              0.90108359           yes
#> Merc 450SL              0.22324387            no
#> Merc 450SLC             0.32292618            no
#> Cadillac Fleetwood      0.22400867            no
#> Lincoln Continental     0.33558271           yes
#> Chrysler Imperial       0.53257954           yes
#> Fiat 128                0.66651222           yes
#> Honda Civic             0.44659660            no
#> Toyota Corolla          0.07353940           yes
#> Toyota Corona           0.46102406            no
#> Dodge Challenger        0.94340605           yes
#> AMC Javelin             0.86471956           yes
#> Camaro Z28              0.87263107           yes
#> Pontiac Firebird        1.74890005            no
#> Fiat X1-9               0.13287854           yes
#> Porsche 914-2           0.19045591            no
#> Lotus Europa            0.99380855            no
#> Ford Pantera L          2.22609879           yes
#> Ferrari Dino            0.62774003            no
#> Maserati Bora           0.54240308           yes
#> Volvo 142E              1.07307036            no
```
