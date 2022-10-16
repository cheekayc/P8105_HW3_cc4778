P8105_HW3_cc4778
================
Chee Kay Cheong

# Problem 1

The `instacart` dataset contains 15 variables and 1384617 observations,
where each row in the dataset is a product from an order. There is a
single order per user, and there are 131209 unique users in this
dataset.

### Questions

1.  How many aisles are there, and which aisles are the most items
    ordered from?

``` r
instacart %>% 
  select(aisle_id, aisle) %>% 
  distinct() 
```

    ## # A tibble: 134 × 2
    ##    aisle_id aisle                        
    ##       <int> <chr>                        
    ##  1      120 yogurt                       
    ##  2      108 other creams cheeses         
    ##  3       83 fresh vegetables             
    ##  4       95 canned meat seafood          
    ##  5       24 fresh fruits                 
    ##  6       21 packaged cheese              
    ##  7        2 specialty cheeses            
    ##  8      115 water seltzer sparkling water
    ##  9       53 cream                        
    ## 10      123 packaged vegetables fruits   
    ## # … with 124 more rows

``` r
# There are 134 distinct aisles.
instacart %>% 
  group_by(aisle) %>% 
  summarize(
    n_order = n()) %>% 
  arrange(desc(n_order))
```

    ## # A tibble: 134 × 2
    ##    aisle                         n_order
    ##    <chr>                           <int>
    ##  1 fresh vegetables               150609
    ##  2 fresh fruits                   150473
    ##  3 packaged vegetables fruits      78493
    ##  4 yogurt                          55240
    ##  5 packaged cheese                 41699
    ##  6 water seltzer sparkling water   36617
    ##  7 milk                            32644
    ##  8 chips pretzels                  31269
    ##  9 soy lactosefree                 26240
    ## 10 bread                           23635
    ## # … with 124 more rows

``` r
# Most items are ordered from the "Fresh vegetables" aisle.
```

2.  Make a plot that shows the number of items ordered in each aisle,
    limiting this to aisles with more than 10000 items ordered. Arrange
    aisles sensibly, and organize your plot so others can read it.

``` r
instacart %>% 
  group_by(aisle) %>% 
  summarize(
    n_order = n()) %>% 
  filter(n_order > 10000) %>% 
  ggplot(aes(x = aisle, y = n_order, fill = aisle)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n_order),
            position = position_stack(vjust = 1)) +
  labs(
    title = "Number of items ordered in each aisle",
    y = "Number of items ordered") +
  theme(legend.key.size = unit(1.2, 'cm'))
```

![](HW3_files/figure-gfm/Q2-1.png)<!-- -->

3.  Make a table showing the three most popular items in each of the
    aisles “baking ingredients”, “dog food care”, and “packaged
    vegetables fruits”. Include the number of times each item is ordered
    in your table.

# Problem 2

``` r
accel = 
  read_csv("Data/accel_data.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "minute",
    names_prefix = "activity_",
    values_to = "activity_count") %>% 
  mutate(
    week_ = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
    minute = as.numeric(minute))
```

After tidying the `accel` dataset, it now has 6 variables and 50400
observations.

The variables are:

-   `week` : numeric

-   `day_id` : numeric

-   `day` : character

-   `minute` : numeric

-   `activity_count` : numeric

-   `week_` : character

``` r
accel %>% 
  mutate(day = fct_relevel(day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  group_by(week, day) %>% 
  summarize(
    total_activity_per_day = sum(activity_count)) %>% 
 knitr::kable(digits = 1)
```

| week | day       | total_activity_per_day |
|-----:|:----------|-----------------------:|
|    1 | Monday    |                78828.1 |
|    1 | Tuesday   |               307094.2 |
|    1 | Wednesday |               340115.0 |
|    1 | Thursday  |               355923.6 |
|    1 | Friday    |               480542.6 |
|    1 | Saturday  |               376254.0 |
|    1 | Sunday    |               631105.0 |
|    2 | Monday    |               295431.0 |
|    2 | Tuesday   |               423245.0 |
|    2 | Wednesday |               440962.0 |
|    2 | Thursday  |               474048.0 |
|    2 | Friday    |               568839.0 |
|    2 | Saturday  |               607175.0 |
|    2 | Sunday    |               422018.0 |
|    3 | Monday    |               685910.0 |
|    3 | Tuesday   |               381507.0 |
|    3 | Wednesday |               468869.0 |
|    3 | Thursday  |               371230.0 |
|    3 | Friday    |               467420.0 |
|    3 | Saturday  |               382928.0 |
|    3 | Sunday    |               467052.0 |
|    4 | Monday    |               409450.0 |
|    4 | Tuesday   |               319568.0 |
|    4 | Wednesday |               434460.0 |
|    4 | Thursday  |               340291.0 |
|    4 | Friday    |               154049.0 |
|    4 | Saturday  |                 1440.0 |
|    4 | Sunday    |               260617.0 |
|    5 | Monday    |               389080.0 |
|    5 | Tuesday   |               367824.0 |
|    5 | Wednesday |               445366.0 |
|    5 | Thursday  |               549658.0 |
|    5 | Friday    |               620860.0 |
|    5 | Saturday  |                 1440.0 |
|    5 | Sunday    |               138421.0 |

There are 5 weeks and 35 days, but the `day_id` does not seem to be
consistent with the sequence of day in a week, so I rearranged the days
in each week to a normal, reasonable sequence. It seems like this
individual would typically have more than 100,000 activities counted
each day, except for the first Monday and two Saturdays (one on the 24th
day, another one on the 31st day), in which the activity count on both
days are 1440.

``` r
accel %>% 
  mutate(day = fct_relevel(day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  group_by(day) %>% 
  ggplot(aes(x = minute, y = activity_count, color = day)) +
  geom_smooth(se = FALSE) + 
  facet_grid(. ~ day) + 
  labs(
    title = "Activity count per minute for 24 hour",
    x = "Minute (24 hour)",
    y = "Activity count",
    caption = "Accelerometer data")
```

![](HW3_files/figure-gfm/activity%20vs%20time%20graph-1.png)<!-- -->
Based on the graph, in most days of a week, the highest number of
activity count occurred after 1000 minute, which is around 5pm. However,
on Sunday, the highest number of activity count occurred at about 600
minute, which is around 10am. In conclusion, this individual typically
has highest number of activity count during the late afternoon to early
evening every day, except for Sunday.

# Problem 3

``` r
data("ny_noaa")

noaa =
  ny_noaa %>%  
  janitor::clean_names() %>% 
  mutate(
    tmax = as.numeric(tmax),
    tmin = as.numeric(tmin)) %>% 
  separate(date, into = c("year", "month", "day")) %>% 
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day)) %>% 
  mutate(
    prcp = prcp / 10,
    tmax = tmax / 10,
    tmin = tmin / 10)
```

After loading and cleaning the `noaa` dataset, it now contains 9
variables and 2595176 observations. All observations for temperatures,
precipitation, and snowfall are also in reasonable units (mm). The data
size is massive, and there is a lot of missing variables across
different columns. We cannot simply remove those missing values because
we are not sure they can be safely removed without interfering with the
analysis. Also, these missing values might impact our mathematical
computation, and we need to exclude them during computation.

``` r
noaa %>% 
  group_by(snow) %>% 
  summarize(
    n_times_appear = n()) %>% 
    arrange(desc(n_times_appear))
```

    ## # A tibble: 282 × 2
    ##     snow n_times_appear
    ##    <int>          <int>
    ##  1     0        2008508
    ##  2    NA         381221
    ##  3    25          31022
    ##  4    13          23095
    ##  5    51          18274
    ##  6    76          10173
    ##  7     8           9962
    ##  8     5           9748
    ##  9    38           9197
    ## 10     3           8790
    ## # … with 272 more rows

For snowfall, the most commonly observed value is **0**, meaning 0 mm of
snowfall. I believe that is because snowfall only occurs during winter,
and it doesn’t occur every day during the entire winter season. This
dataset contains daily snowfall information which spans across decades
from many different weather stations, so it would be normal to see 0 mm
of snowfall most commonly across this dataset.

``` r
noaa_tmax_jan =
  noaa %>% 
  filter(month == 1) %>% 
  group_by(id, year) %>% 
  summarize(
    average_tmax = round(mean(tmax, na.rm = TRUE), 2)) %>% 
  filter(!is.na(average_tmax))
  
noaa_tmax_july = 
  noaa %>% 
  filter(month == 7) %>% 
  group_by(id, year) %>% 
  summarize(
    average_tmax = round(mean(tmax, na.rm = TRUE), 2)) %>% 
  filter(!is.na(average_tmax))
```

``` r
tmax_jan_plot =
  noaa_tmax_jan %>% 
  ggplot(aes(x = year, y = average_tmax, group = id)) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Years",
    y = "Average maximum temperature",
    color = "Station",
    title = "Average maximum temperature in January",
    caption = "Data come from the ny_noaa") +
  scale_x_continuous(breaks = seq(1980, 2010, 1)) +
  theme_gray() +
  theme(legend.position = "none")

tmax_july_plot = 
  noaa_tmax_july %>% 
  ggplot(aes(x = year, y = average_tmax, group = id)) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Years",
    y = "Average maximum temperature",
    title = "Average maximum temperature in July",
    caption = "Data come from the ny_noaa") +
  scale_x_continuous(breaks = seq(1980, 2010, 1)) +
  theme_gray()

tmax_jan_plot / tmax_july_plot
```

![](HW3_files/figure-gfm/two%20panel%20plot-1.png)<!-- --> Based on the
above two graphs, we can see that the average maximum temperatures in
both January and July from each station followed a normal distribution,
with some outliers, in each year. In addition, if we compared the
average maximum temperatures of January and July for the year 1981 and
2010, we can see that there is an increase in average maximum
temperature, suggesting the evidence of global warming.

``` r
temp_plot =
  noaa %>% 
  ggplot(aes(x = tmin, y = tmax)) +
  geom_hex(na.rm = TRUE) +
  labs(
    x = "Minimum temperature",
    y = "Maximum temperature",
    title = "Maximum temperature vs. Minimum temperature",
    caption = "Data comes from 'ny_noaa'")


snow_plot =
  noaa %>% 
  filter((snow > 0) & (snow < 100)) %>%
  ggplot(aes(x = year, y = snow, group = year)) + 
  geom_boxplot() +
  labs(
    x = "Year",
    y = "Snowfall (mm)",
    title = "Snowfall Distribution of each year",
    caption = "Data comes from 'ny_noaa'") +
  scale_x_continuous(breaks = seq(1980, 2010, 1)) +
  theme_gray()

temp_plot / snow_plot
```

![](HW3_files/figure-gfm/temp%20&%20snow-1.png)<!-- -->
