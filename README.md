
# Analysing the ONS ‘GVA by industry by ITL region’ data in R

The ONS produces an [Excel
document](https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry)
with GVA data at three different geographical scales, ITL1 to ITL3.
Wikipedia has an [excellent explanation with
maps](https://en.wikipedia.org/wiki/International_Territorial_Level) of
the ITL regions.

IT2 level data includes several zones that match existing mayoral
authorities. ITL3 data has zones matching **local** authorities - but
neither perfectly. Some are grouped. For example, ITL3 zones in South
Yorkshire separate Sheffield from the other three local authorities in
South Yorkshire grouped into one zone (Rotherham, Barnsley, Doncaster).

Here, we’ll explore some ways to analyse this data using R. The data has
been processed to make it more useable in R - see the
[process_from_excel.R](process_from_excel.R) script and its comments for
an explanation of how to get from the Excel sheet to the datasets used
here. All of the derived files (and the original ONS excel sheet
downloaded from the above page, current version dated 25th April 2023)
are included in this repository in the [data folder](data).

We’ll look at the **current prices** data, not the chained volume data.
We’ll be making **location quotients**, which require being able to sum
different regions in different ways; chained volume measures can’t be
summed. Current price data (prices at the time point of the data) can’t
be used to measure nominal growth as it’s not inflation adjusted, but as
long as we’re working with proportional change over time (which as we’ll
see, LQs are), they’re fine.

First, load some libraries and get the ITL2 level data. (If you haven’t
already, install the libraries/packages with
e.g. `install.packages("tidyverse")` before loading here.) Also, load
some functions that include an LQ function.

``` r
library(tidyverse)
source('functions/misc_functions.R')

itl2.cp <- read_csv('data/ITL2currentprices_long.csv')
```

In this dataframe, we have: ITL2 regions, SIC sectors and year - ranging
from 1998 to 2021 in the current data - and finally the current price
GVA value.

Then we’ll find the **location quotients** for the whole dataset. The
[Excel sheet
here](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/locationquotientdataandindustrialspecialisationforlocalauthorities)
from the ONS has an excellent explanation of location quotients in its
notes, I won’t repeat all of that here. But a quick word on what the
location quotient is showing for this data:

- The location quotient gives a **measure of concentration** for sectors
  within regions, when compared to a larger geography (the UK in this
  case).
- The LQ is found easily: it’s the ratio of two ratios - the proportion
  of a sector in region x, over the proportion of that sector in the UK
  as a whole.
- If the LQ \> 1, that industry is *relatively more concentrated* in the
  region, compared to the UK.
- If the LQ \< 1, that industry is *relatively less concentrated* in the
  region, compared to the UK.
- We’re looking at raw GVA values here - the total GVA value of a
  particular sector in a region tells us something about that region’s
  economic structure. But it can’t directly be used to say anything
  definitive about productivity, since we don’t know e.g. if that GVA
  value is due to high productivity workers, or just a very large but
  lower productivity sector.
- LQs are good for getting a structural overview, but their biggest
  weakness is that a regional sector can be *proportionally larger* than
  the UK, but itself quite small. So a region’s top LQ sector may still
  be a tiny part of its overall economy. We’ll look at a way to overcome
  that weakness below.
- Note, as the ONS Excel sheet on LQs make really clear, because
  (A/B)/(C/D) is equivalent to (A/C)/(B/D), the LQ actually captures two
  related measures. Here, we’ll only look at **concentration** (whether
  a region’s industries have a relatively higher concentration of GVA
  than the UK as a whole).

The LQ function takes in a dataframe, the name of the region column, the
name of the sector column and the name of the value column to find the
LQ for. It returns the same dataframe with the LQ and region and total
proportions added (we’ll need those proportions later), as well the LQ
logged, which will help with plotting (as the log makes plus/minus 1
values symmetric).

First, here’s the function working on a single year in the data, to
illustrate what the function takes in.

``` r
lq1998 <- add_location_quotient_and_proportions(
  df = itl2.cp %>% filter(year == 1998),
  regionvar = ITL_region_name,
  lq_var = SIC07_description,
  valuevar = value
)
```

Let’s repeat that for all years and replace the original dataframe with
the result.

``` r
itl2.cp <- itl2.cp %>% 
  split(.$year) %>% 
  map(add_location_quotient_and_proportions, 
      regionvar = ITL_region_name,
      lq_var = SIC07_description,
      valuevar = value) %>% 
  bind_rows()
```

``` r
itl2.cp %>% filter(
  ITL_region_name == 'South Yorkshire',
  year == 2021,
  LQ > 1.5
  ) %>% 
  mutate(regional_percent = sector_regional_proportion *100) %>% 
  select(SIC07_description,regional_percent, LQ) %>% 
  arrange(-LQ)
```

    # A tibble: 10 × 3
       SIC07_description                                  regional_percent    LQ
       <chr>                                                         <dbl> <dbl>
     1 Manufacture of basic metals                                   1.28   5.14
     2 Manufacture of furniture                                      0.780  3.17
     3 Manufacture of fabricated metal products                      2.21   2.79
     4 Other manufacturing                                           0.694  2.53
     5 Manufacture of other non-metallic mineral products            0.835  2.39
     6 Manufacture of rubber and plastic products                    1.02   2.39
     7 Manufacture of electrical equipment                           0.576  1.95
     8 Motor trades                                                  2.52   1.80
     9 Education                                                    10.8    1.70
    10 Telecommunications                                            2.69   1.64

The next function adds in some ordinary least squares slopes for LQ
change over time, to get a numerical sense of the growth trends in LQ
for each region’s SIC sectors. LQ_log is used so that slope scale is the
same for different size sectors, so their trends are comparable.

``` r
LQ_slopes <- compute_slope_or_zero(data = itl2.cp, ITL_region_name, SIC07_description, y = LQ_log, x = year)
```
