FanGraphs Functions Pt.2: Data Acquisition and Cleaning
================

## Introduction

In part 1, we made a function to construct an url based on your desired
parameters. We now have 2 options on how to get this data into R: using
the download link or scraping the table manually. Since a download link
is not always available, we will go with the latter method. <BR> Let’s
start by loading the required functions. <BR>

``` r
source('Finished Functions.R') # function from pt.1 
suppressMessages(library(rvest)) # for scraping
suppressMessages(library(dplyr))

url <- fg_url(Stats = 'bat', Type = 8)
```

<BR> The “Inspect Element” tool on Firefox and Chrome allows us to get
identifiers for different page elements. This is especially useful in
cases where the page doesn’t render the table as an actual table, or
when there are a lot of tables on the page. We can find the css selector
of our main table by using the “pick element” tool and clicking on the
area of the table wrapper. The table’s parts are easy to find; typically
labeled as ‘th’, ‘tr’, or ‘td’. <BR> ![table id](Images/Table%20id.PNG)
<BR> Now we can retrieve the data. <BR>

``` r
pg <- read_html(url) # get page
tab.obj <- html_nodes(pg, '#LeaderBoard1_dg1_ctl00') # give table id
main.tab <- html_table(tab.obj, fill = TRUE) # extract table from page 

### There's only 1 table so we can extract the data frame from the list easily
df <- main.tab[[1]]
```

The data frame is returned with a couple rows of non-useful html
information. We can filter this out by removing rows that contain the
text ‘Page size’ in the first column (but any column works in this
case).

``` r
df <- df[ grep("Page size", df[,1], invert = TRUE) , ]

head(df, 4)
```

    ##   X1               X2      X3 X4  X5 X6 X7  X8 X9    X10    X11  X12   X13
    ## 2  #             Name    Team  G  PA HR  R RBI SB    BB%     K%  ISO BABIP
    ## 4  1   Cody Bellinger Dodgers 68 293 22 53  57  8 15.0 % 14.7 % .339  .355
    ## 5  2 Christian Yelich Brewers 63 279 26 56  56 16 14.7 % 17.2 % .403  .331
    ## 6  3       Mike Trout  Angels 67 298 18 53  44  7 21.1 % 17.8 % .321  .290
    ##    X14  X15  X16  X17  X18  X19  X20  X21 X22
    ## 2  AVG  OBP  SLG wOBA wRC+  BsR  Off  Def WAR
    ## 4 .355 .451 .694 .459  191 -0.7 34.2  5.4 4.8
    ## 5 .342 .444 .745 .469  192  3.2 36.9 -0.7 4.4
    ## 6 .281 .453 .603 .429  177  3.5 32.5  1.8 4.4

This reveals header names in the first row, so let’s make them our data
frame’s column names then remove that row.

``` r
colnames(df) <- df[1,]
df <- df[-1,]

head(df, 4)
```

    ##   #             Name    Team  G  PA HR  R RBI SB    BB%     K%  ISO BABIP
    ## 4 1   Cody Bellinger Dodgers 68 293 22 53  57  8 15.0 % 14.7 % .339  .355
    ## 5 2 Christian Yelich Brewers 63 279 26 56  56 16 14.7 % 17.2 % .403  .331
    ## 6 3       Mike Trout  Angels 67 298 18 53  44  7 21.1 % 17.8 % .321  .290
    ## 7 4     Alex Bregman  Astros 69 307 19 47  48  3 16.9 % 12.4 % .272  .247
    ##    AVG  OBP  SLG wOBA wRC+  BsR  Off  Def WAR
    ## 4 .355 .451 .694 .459  191 -0.7 34.2  5.4 4.8
    ## 5 .342 .444 .745 .469  192  3.2 36.9 -0.7 4.4
    ## 6 .281 .453 .603 .429  177  3.5 32.5  1.8 4.4
    ## 7 .272 .401 .545 .395  155 -0.3 21.0  1.4 3.3

Now we have our table\! Since rvest trims leading and trailing
whitespaces by default, we have to convert columns with numbers in them
from “character” type to “numeric”. Additionally, we remove symbols that
are difficult to call in R, such as the % or + sign. These symbols are
meaningful, so we want to replace them with something that represents
them.

``` r
### Format column names
colnames(df) <- gsub('\\#', 'num', colnames(df))
colnames(df) <- gsub('\\+', 'plus', colnames(df))
colnames(df) <- gsub('\\%', 'perc', colnames(df))
### Format cells
df[4:length(df)] <- sapply(df[4:length(df)], function(x) as.numeric(gsub('[^0-9\\.\\-]', '', x)))
```

The `sapply` command is used to go over each specified column (4 to the
end here) and format it as numeric. We add `\\.` and `\\-` at the end of
our regex expression to indicate that decimal points and negative signs
should be kept during conversion. <BR> <BR> \#\# Functions Since data
you find online is almost always messy, we should make a couple
formatting functions to make things faster in the future.

``` r
### Format numbers in cells
format_nums <- function(x) { as.numeric(gsub('[^0-9\\.\\-]', '', x)) }

### Format column names
format_colnames <- function(x) {
  x <- gsub('\\#', 'num', x)
  x <- gsub('\\+', 'plus', x)
  x <- gsub('\\%', 'perc', x)
  
  return(x)
}
```

Now let’s write a function that retrieves a table using any url
parameters we might want. In this case, we can use piping from the
`dplyr` package to make the code more readable.

``` r
fg_table <- function(Url){
  
  ### Get table
  df <- Url %>%
    read_html() %>%
    html_nodes('#LeaderBoard1_dg1_ctl00') %>%
    html_table(fill = TRUE)
  df <- df[[1]]
  
  ### Remove page size rows
  df <- df[ grep("Page size", df[,1], invert = TRUE) , ]
  
  ### Get and use column names
  colnames(df) <- df[1,]
  df <- df[-1,]
  
  ### Formatting
  # column names
  colnames(df) <- format_colnames(colnames(df))
  # cells
  df[4:length(df)] <- sapply(df[4:length(df)], format_nums)
  
  return(df)
  
}

head(fg_table(Url = url), 4)
```

    ##   num             Name    Team  G  PA HR  R RBI SB BBperc Kperc   ISO
    ## 4   1   Cody Bellinger Dodgers 68 293 22 53  57  8   15.0  14.7 0.339
    ## 5   2 Christian Yelich Brewers 63 279 26 56  56 16   14.7  17.2 0.403
    ## 6   3       Mike Trout  Angels 67 298 18 53  44  7   21.1  17.8 0.321
    ## 7   4     Alex Bregman  Astros 69 307 19 47  48  3   16.9  12.4 0.272
    ##   BABIP   AVG   OBP   SLG  wOBA wRCplus  BsR  Off  Def WAR
    ## 4 0.355 0.355 0.451 0.694 0.459     191 -0.7 34.2  5.4 4.8
    ## 5 0.331 0.342 0.444 0.745 0.469     192  3.2 36.9 -0.7 4.4
    ## 6 0.290 0.281 0.453 0.603 0.429     177  3.5 32.5  1.8 4.4
    ## 7 0.247 0.272 0.401 0.545 0.395     155 -0.3 21.0  1.4 3.3
