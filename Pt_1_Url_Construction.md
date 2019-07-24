FanGraphs Functions Pt.1: Url Construction
================

## Introduction

MLB data has become widely available over the past decade. R packages
such as the ‘baseballr’ and ‘Lahman’ packages allow for quick collection
of both raw and summarized data. However, these packages do not provide
many of the advanced statistics available on Fangraphs, and most - if
not all - packages for this purpose are not up to date. To solve this,
we can write a function to acquire all of our desired metrics;
optionally with splits.

## Link editing

We start with the default query for batters in 2019, which looks like:
<BR> ![default query](Images/Default%20Query.PNG) <BR> This provides us
with the link
<url><https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_50></url>
<BR> <BR> The fact that each parameter in the link is labeled makes it
relatively easy to write a function that allows us to specify queries.
Let’s start by dissecting the link. Each of these assumptions about the
parameter meanings can be verified by manually changing options on the
webpage. <BR> \* `pos` defines the player’s position <BR> \* `stats`
says that we want stats for batting vs. pitching vs. fielding <BR> \*
`lg` separates National League and American League <BR> \* `qual`is the
number of plate appearances required for counting a player in the query
<BR> \* `type` is a numeric entry defining the type of statistics to
return (standard, advanced, etc.) <BR> \* `season` and `season1` are the
start and end seasons of our query <BR> \* `month` ironically, this
actually defines the type of split to return (home, away, last 7 days,
etc.) <BR> \* `ind` determines whether you want to split seasons or not
<BR> \* `team` allows you to specify a team of interest <BR> \* `rost`
is a binary value representing whether the players should be active on
the roster <BR> \* `age` is the age range of the player <BR> \* `filter`
can add customer filters seen at the bottom of the page <BR> \*
`players` can add customer players from the box at the bottom of the
page <BR> \* `page` gives the range of players for the current page.
Change this to 2000+ to get all players for most queries. <BR> \*
`startdate` and `enddate` are for specifying a range of dates to gather
aggregated data for. `month` must be set to 1000

#### Starting the function

We won’t mess around with all of these url parameters, but they will be
included of the function to demonstrate the beauty of the `sprintf`
function. We start with writing a skeleton of our function that returns
the url of our desired query. The less significant parameters can be
pre-stated by using `parameter =
value`.

``` r
fg_url <- function(Stats, Type, Pos = 'all', Lg = 'all', Qual = 'y', Season = 2019, Season1 = 2019, Team = 0, Rost = 0, Age = 0, Players = 0, Month = 0, Ind = 0, Max.results = 2000, Start.date = '', End.date = ''){
  
  ### Construct url
  url <- sprintf('https://www.fangraphs.com/leaders.aspx?pos=%s&stats=%s&lg=%s&qual=%s&type=%d&season=%d&month=%d&season1=%d&ind=%d&team=%d&rost=%d&age=%d&filter=&players=%s&startdate=%s&enddate=%s&page=1_%d',
                 Pos, Stats, Lg, Qual, Type, Season, Month, Season1, Ind, Team, Rost, Age, Players, Start.date, End.date, Max.results)
  
  ### Return url
  return(url)
  
}
```

#### Explanation

We start by “copying” the url parameters to our function inputs. Note
that the inputs with pre-specified values must come after the
user-entered ones. Using the `sprintf` function, we replace
string/character values in the url with `%s` and numeric values with
`%d`. The parameters for our function can then be stated at the end of
our `sprintf` in the order that they are listed in the url. This
“pastes” our parameters into the url. <BR> <BR> Now, for example, if
we wanted to construct a link to get <i>advanced</i> batting statistics
under the same parameters as earlier, we can simply specify new values
using our
    function.

``` r
fg_url(Stats = 'bat', Type = 12)
```

    ## [1] "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=12&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&page=1_2000"

Or we can construct a list of urls for multiple split types with lapply.

``` r
lapply('bat', FUN = fg_url, Type = 10:12)
```

    ## [[1]]
    ## [1] "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=10&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&page=1_2000"
    ## [2] "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=11&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&page=1_2000"
    ## [3] "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=12&season=2019&month=0&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=&enddate=&page=1_2000"

#### Second function

We can also make a function that scrapes the newer splits leaderboard.
This offers fewer options, but allows us to scrape day-by-day.
Additionally, we use the `paste0` function this time to allow “adding”
NULL objects into our url (it pastes nothing) if the arguments are not
specified by the user. Unfortunately, this page’s data must be
downloaded via a javascript download link and will not be discussed
further. This function can also be accessed via the `Finished
Functions.R` file. <BR> <BR> If you are truly interested in taking this
approach, check out the `RSelenium` package to “drive” your browser
through R and download the data.

``` r
fg_splits_url <- function(Pos, Start.date, End.date, Split.type = NULL, 
                          Pitch.type = NULL, Split.teams = 'false', Stat.type = 'player',
                          Stat.group = 1, Players = NULL, Filt = NULL){
  
  url <- paste0('https://www.fangraphs.com/leaders/splits-leaderboards?splitArr=', Split.type,
  '&splitArrPitch=', Pitch.type,
  '&position=', Pos,
  '&autoPt=true&splitTeams=', Split.teams,
  '&statType=', Stat.type,
  '&statgroup=', Stat.group,
  '&startDate=', Start.date,
  '&endDate=', End.date,
  '&players=', Players,
  '&filter=', Filt)
  
  ### Return url
  return(url)
  
}

fg_splits_url(Pos = 'B', Start.date = '2019-05-11', End.date = '2019-05-18')
```

    ## [1] "https://www.fangraphs.com/leaders/splits-leaderboards?splitArr=&splitArrPitch=&position=B&autoPt=true&splitTeams=false&statType=player&statgroup=1&startDate=2019-05-11&endDate=2019-05-18&players=&filter="
