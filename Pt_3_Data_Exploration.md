Untitled
================

We will look at specific statistics and the correlations among them.

### Aggregated data

Aggregated/summarized data is great for visualization. We can start at
the “macroscopic” level of the league and teams. We use the `ggplot2`
package for plotting because this tends to be the easiest way when
dealing with grouping. The default theme is questionable, so add
`theme_bw` to the `theme_set` function

``` r
suppressMessages(library(dplyr))
library(ggplot2)
theme_set(theme_bw())

source('Finished_Functions.R')

url <- fg_url(Stats = 'bat', Type = 8, 
              Qual = '20', # arbitrary; a little under half of avg PA
              Month = 1000, 
              Max.results = 1000,
              Start.date = '2019-06-01',
              End.date = '2019-06-14')

df <- fg_table(url)
```

We start by looking at a histogram of the data to see the frequencies of
batting averages, along with a formal test for normality.

``` r
ggplot(df) + 
  geom_histogram(aes(AVG))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Pt_3_Data_Exploration_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
shapiro.test(df$AVG)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$AVG
    ## W = 0.99707, p-value = 0.8417

The data looks like it is normally-distributed across the league. This
is backed by the Shapiro-Wilk test of normality (any p-value above 0.05
is typically considered normal).

There’s a pretty big drop in the number of players around a certain
average around 0.350. We will consider this the “exceptional” range
relative to the rest of the league.

``` r
df.top <- df %>%
  filter(AVG > .350)

nrow(df.top) 
```

    ## [1] 26

We can add text to the scatterplot to visualize how well the players are
performing in these categories relative to each other. A lower number of
plate appearances leaves less chances for a bad game or 2 (.000 batting
avg.) to deflate your batting average. So we also want to consider the
number of PA’s a player had in this span of time. To do this on our
graph, we add a transparency gradient.

``` r
library(ggrepel)

ggplot(df.top) +
  geom_point(aes(x = SLG, y = AVG)) +
  geom_text_repel(aes(x = SLG, y = AVG, 
                   label = Name, alpha = PA)) +
  theme(
    panel.grid.minor = element_blank() # for cleaner look
    )
```

![](Pt_3_Data_Exploration_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The graph is a little cluttered, but it displays the information we
want: finding the top players in terms of both power and batting
averages.

Fun fact: we are looking at the ISO statistic in the above graph. ISO
stands for “isolated power” and is usually calculated as SLG - AVG.

### 2\. Team performance

##### Q1: top 5 AVG/SLG

We also want to look at performance of each team. What 5 teams have the
best batting average for the first 2 weeks of June?

``` r
df.team <- df %>%
  group_by(Team) %>%
  summarize(Mn.AVG = mean(AVG),
            Mn.SLG = mean(SLG)) %>%
  arrange(desc(Mn.AVG))

head(df.team, 5)
```

    ## # A tibble: 5 x 3
    ##   Team    Mn.AVG Mn.SLG
    ##   <chr>    <dbl>  <dbl>
    ## 1 Pirates  0.301  0.489
    ## 2 Twins    0.290  0.557
    ## 3 Rockies  0.286  0.458
    ## 4 Red Sox  0.286  0.499
    ## 5 Marlins  0.275  0.417

##### Q2: distribution of averages

One way to look at how well teams are performing is to look how they’re
doing relative to the rest of the league. There are metrics out there
(e.g. WRC+) that account for this, but we will do this manually to look
at average alone.

There’s a handful of options we can use to contrast players that are
performing well vs. poorly: mean-centering, z-scores, percentiles, etc.
Both mean-centering and percentiles are guaranteed to give us a handful
of data points after performance distinction. However, mean-centering is
not very helpful to find players below or above a cutoff, and with
z-scores, it’s possible that we won’t even find an outlier (typically z
\> 1.96).

Taking these into account, we choose to separate the data using
percentiles.

``` r
p <- ggplot(df.team) + 
  geom_histogram(aes(Mn.AVG),
                 color = 'black', bins = 60,
                 position = position_dodge()) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = 'Team Batting Average',
       y = 'Count')

### make df of 25, 50, 75th percentiles for plotting
vec.nms <- c('twen5th', 'med', 'sev5th') 
perc.vals <- as.vector(summary(df.team$Mn.AVG)[c(2, 3, 5)])
names(perc.vals) <- vec.nms

df.perc <- as.data.frame(t(perc.vals))

### graph with percentiles
p + geom_vline(data = df.perc,
               aes(xintercept = twen5th),
               linetype = 'dashed',
               color = 'red') +
  geom_vline(data = df.perc,
               aes(xintercept = med),
               linetype = 'dashed',
             color = 'black') +
  geom_vline(data = df.perc,
               aes(xintercept = sev5th),
               linetype = 'dashed',
             color = 'red')
```

![](Pt_3_Data_Exploration_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
The teams that we want to look at are below the 25th percentile and
above the 75th percentile. This will help us arbitrarily label and
contrast “good” vs. “bad” performance. So now we filter the data to keep
only those teams.

``` r
df.teamfilt <- df.team[df.team$Mn.AVG < df.perc$twen5th |
                         df.team$Mn.AVG > df.perc$sev5th, ]

nrow(df.teamfilt)
```

    ## [1] 16

Now we’re down to 16 teams. We label them as “top” teams or “bottom”
teams.

``` r
df.teamfilt$TopBot <- ifelse(df.teamfilt$Mn.AVG < df.perc$twen5th, 'Bottom', 'Top')

head(df.teamfilt)
```

    ## # A tibble: 6 x 4
    ##   Team    Mn.AVG Mn.SLG TopBot
    ##   <chr>    <dbl>  <dbl> <chr> 
    ## 1 Pirates  0.301  0.489 Top   
    ## 2 Twins    0.290  0.557 Top   
    ## 3 Rockies  0.286  0.458 Top   
    ## 4 Red Sox  0.286  0.499 Top   
    ## 5 Marlins  0.275  0.417 Top   
    ## 6 Brewers  0.274  0.487 Top

simple stats: avg vs. slg on team runs per game

##### Q3:

On first thought, one might use a boxplot by team to look at how batting
averages are dispersed within teams.

``` r
ggplot(df) +
  geom_boxplot(aes(x = Team, y = AVG)) +
  geom_point(aes(x = Team, y = AVG))
```

![](Pt_3_Data_Exploration_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

On second thought, it’s really hard to objectively describe the
dispersion of data points using just a boxplot. Instead, we can find the
standard deviation for each group. - \[move this somewhere higher\] -

#### 

To start, we’ll look at the within-team effect on batting average.

``` r
#by(xx[,2:3], xx$group, function(x) {cor(x$a, x$b)}
```

##### Q2:

  - look at bar graphs with values inside to see how correlations behave
    within teams

  - 
### 3\. What teams have
