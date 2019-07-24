suppressMessages(library(rvest)) # for scraping
suppressMessages(library(dplyr))

##### From Pt. 1: Url Construction ---------------

### construct url for gathering data
fg_url <- function(Stats, Type, Pos = 'all', Lg = 'all', 
                   Qual = 'y', Season = 2019, Season1 = 2019, 
                   Team = 0, Rost = 0, Age = 0, Players = 0, 
                   Month = 0, Ind = 0, Max.results = 2000, 
                   Start.date = '', End.date = ''){
  
  ### Construct url
  url <- sprintf('https://www.fangraphs.com/leaders.aspx?pos=%s&stats=%s&lg=%s&qual=%s&type=%d&season=%d&month=%d&season1=%d&ind=%d&team=%d&rost=%d&age=%d&filter=&players=%s&startdate=%s&enddate=%s&page=1_%d',
                 Pos, Stats, Lg, Qual, Type, Season, Month, Season1, Ind, Team, Rost, Age, Players, Start.date, End.date, Max.results)
  
  ### Return url
  return(url)
  
}

### construct url for gathering splits and repeated-measures data
fg_splits_url <- function(Pos, Start.date, End.date, Split.type = NULL, 
                          Pitch.type = NULL, Split.teams = 'false', Stat.type = 'player',
                          Stat.group = 1, Players = NULL, Filt = NULL){
  
  # Construct url
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
  
  # Return url
  return(url)
  
}


##### From Pt. 2: Data Acquisition and Cleaning  ---------------

### Format numbers in cells
format_nums <- function(x) { as.numeric(gsub('[^0-9\\.\\-]', '', x)) }

### Format column names
format_colnames <- function(x) {
  x <- gsub('\\#', 'num', x)
  x <- gsub('\\+', 'plus', x)
  x <- gsub('\\%', 'perc', x)
  
  return(x)
}

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
  colnames(df) <- gsub('\\#', 'num', colnames(df))
  colnames(df) <- gsub('\\+', 'plus', colnames(df))
  colnames(df) <- gsub('\\%', 'perc', colnames(df))
  # cells
  df[4:length(df)] <- sapply(df[4:length(df)], function(x) as.numeric(gsub('[^0-9\\.\\-]', '', x))) 
  
  return(df)
  
}
