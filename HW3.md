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
  group_by(day_id, day) %>% 
  summarize(
    total_activity_per_day = sum(activity_count)) %>% 
 knitr::kable(digits = 1)
```

| day_id | day       | total_activity_per_day |
|-------:|:----------|-----------------------:|
|      1 | Friday    |               480542.6 |
|      2 | Monday    |                78828.1 |
|      3 | Saturday  |               376254.0 |
|      4 | Sunday    |               631105.0 |
|      5 | Thursday  |               355923.6 |
|      6 | Tuesday   |               307094.2 |
|      7 | Wednesday |               340115.0 |
|      8 | Friday    |               568839.0 |
|      9 | Monday    |               295431.0 |
|     10 | Saturday  |               607175.0 |
|     11 | Sunday    |               422018.0 |
|     12 | Thursday  |               474048.0 |
|     13 | Tuesday   |               423245.0 |
|     14 | Wednesday |               440962.0 |
|     15 | Friday    |               467420.0 |
|     16 | Monday    |               685910.0 |
|     17 | Saturday  |               382928.0 |
|     18 | Sunday    |               467052.0 |
|     19 | Thursday  |               371230.0 |
|     20 | Tuesday   |               381507.0 |
|     21 | Wednesday |               468869.0 |
|     22 | Friday    |               154049.0 |
|     23 | Monday    |               409450.0 |
|     24 | Saturday  |                 1440.0 |
|     25 | Sunday    |               260617.0 |
|     26 | Thursday  |               340291.0 |
|     27 | Tuesday   |               319568.0 |
|     28 | Wednesday |               434460.0 |
|     29 | Friday    |               620860.0 |
|     30 | Monday    |               389080.0 |
|     31 | Saturday  |                 1440.0 |
|     32 | Sunday    |               138421.0 |
|     33 | Thursday  |               549658.0 |
|     34 | Tuesday   |               367824.0 |
|     35 | Wednesday |               445366.0 |

It seems like this individual would typically have more than 100000
activities counted every day, except for two Saturdays (one on the 24th
day, another one on the 31st day), in which the activity count on both
days are 1440.
