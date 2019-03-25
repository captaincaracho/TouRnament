#'Create a league table
#'
#'Create a league table from individually defined criteria from a results dataset.
#'
#'Mandatory parameters are a dataset with match results as well the names of the variables for home and away team and their respective scored goals.
#'
#'Optional parameters are the point distribution that will be awarded (win, draw, lose), the order of ranking criteria to use (Pts = Points, GD = Goal difference, GF = Goals for),
#'the display of additional columns with separate home and away tables and the dates and matchdays to be used for calculation.
#'
#'If a date range and a matchday range are set, the subset of matches that fit both selection criteria will be used for calculation.
#'
#'
#'@param dataset A dataset with the results.
#'@param home The name of the home team column in the dataset as a character string.
#'@param away The name of the away team column in the dataset  as a character string.
#'@param score_home The name of the home team goals column in the dataset as a character string.
#'@param score_away The name of the away team goals column in the dataset as a character string.
#'@param date The name of the date column in the dataset as a character string (optional).
#'@param date_start The earliest date to include if not the earliest date in the dataset as a character string in the format "YY-mm-dd" (optional).
#'@param date_end The last date to include if not the last date in the dataset as character string in the format "YY-mm-dd" (optional).
#'@param matchday The name of the matchday column in the dataset as a character string (optional).
#'@param matchday_start The earliest matchday to include if not the earliest in the dataset as an integer (optional).
#'@param matchday_end The last matchday to include if not the last in the dataset as an integer (optional).
#'@param points A vector of integers of length three containing the points awarded for wins, draws and losses. Defaults to c(3,1,0).
#'@param HA_table Logical value to indicate, whether home and away results should be displayed in the table. Defaults to FALSE.
#'@param rank_by A character vector with the order of arguments to sort the league table. Possible arguments are "Pts", "GD", "GF", "GA", "W","D","L", Defaults to c("Pts","GD","GF").
#'@return A data frame in the form of a league table.
#'@examples
#'#create league table for La Liga 1994/95 with three points reward, only games in 1995 with home and away results
#'library(engsoccerdata)
#'leaguetable(dataset=engsoccerdata::spain[which(engsoccerdata::spain$Season==1994),], home="home", away="visitor", score_home="hgoal", score_away="vgoal", date="Date", date_start="1995-01-01", date_end="1995-06-30", points = c(3,1,0), HA_tables = TRUE)
#'@export



leaguetable <- function(dataset, home, away, score_home, score_away, date, date_start, date_end, matchday, matchday_start, matchday_end, points = c(3,1,0), rank_by = c("Pts","GD","GF"), HA_tables = FALSE) {

  #get vector of columns to include for calculation
  include       <- c(home, away, score_home, score_away)
  include_names <- c("home", "away", "score_home", "score_away")

  #add optional arguments to include
  optionals <- vector()

  #name vector for individual team results
  names_itr <- c("Team","GF","GA")

  #set date range
  if(missing(date)==FALSE) {
    include       <- append(include, date)
    include_names <- append(include_names, "date")
    optionals     <- append(optionals, "date")
    names_itr     <- append(names_itr,"Date")
    dataset$Date  <- as.Date(dataset$Date)

    #if date is provided, get start and end date, if not provided set to min respectively max
    if(missing(date_start)==FALSE) {
      date_start <- as.Date(date_start)
    }else {
      date_start <- min(as.Date(dataset$Date))
    }

    if(missing(date_end)==FALSE) {
      date_end <- as.Date(date_end)
    }else {
      date_end <- max(as.Date(dataset$Date))
    }
  }

  #set matchday range
  if(missing(matchday)==FALSE) {
    include       <- append(include, matchday)
    include_names <- append(include_names, "matchday")
    optionals     <- append(optionals, "matchday")
    names_itr     <- append(names_itr,"Matchday")

    #if matchday is provided, get start and end matchday, if not provided set to min respectively max
    if(missing(matchday_start)==FALSE) {
      matchday_start <- matchday_start
    }else {
      matchday_start <- min(dataset$Matchday)
    }

    if(missing(matchday_end)==FALSE) {
      matchday_end <- matchday_end
    }else {
      matchday_end <- max(dataset$Matchday)
    }
  }


  #reduce to relevant dataset
  ds        <- dataset[,include]
  names(ds) <- include_names

  #create dataset of inidividual team results
  hs <- ds[,c("home","score_home","score_away",optionals)]
  names(hs) <- names_itr
  hs$location <- "H"
  as <- ds[,c("away","score_away","score_home",optionals)]
  names(as) <- names_itr
  as$location <- "A"
  ds2 <- rbind(hs,as)


  #create lookup table for direct comparison feature, which is not yet implemented
  if(FALSE){

    #get inverted dataset for direct comparison lookup
    dsinv            <- ds
    dsinv$home       <- ds$away
    dsinv$away       <- ds$home
    dsinv$score_home <- ds$score_away
    dsinv$score_away <- ds$score_home

    #bind original and inverted dataset
    ds3 <- rbind(ds,dsinv)

    #aggregate
    dclookup <- aggregate(list(ds3$score_home,ds3$score_away), by=list(ds3$home, ds3$away), FUN=sum)
    names(dclookup) <- c("Team1", "Team2", "Score1", "Score2")
  }



  #reduce to relevant timeframe if date is provided
  if(missing(date)==FALSE) {
    ds2     <- ds2[which(ds2$Date >= date_start & ds2$Date <= date_end),]
  }

  #reduce to relevant matchdays if date is provided
  if(missing(matchday)==FALSE) {
    ds2     <- ds2[which(ds2$Matchday >= matchday_start & ds2$Matchday <= matchday_end),]
  }

  #calculate points
  ds2$Pts <- ifelse(ds2$GF>ds2$GA,points[1],ifelse(ds2$GF<ds2$GA,points[3],points[2]))

  #calculate games played, wins, draws, losses ###needs to be worked over, because equal points for two outcomes would distort calculation
  ds2$W   <- ifelse(ds2$Pts == points[1],1,0)
  ds2$D   <- ifelse(ds2$Pts == points[2],1,0)
  ds2$L   <- ifelse(ds2$Pts == points[3],1,0)
  ds2$P   <- 1

  #create table by aggregating all desired statistics by team
  table                 <- aggregate(list(ds2$P, ds2$W, ds2$D, ds2$L, ds2$Pts, ds2$GF, ds2$GA), by=list(ds2$Team), FUN=sum)
  names(table)          <- c("Team","P","W","D","L","Pts","GF","GA")

  #compute goal difference
  table$GD              <- table$GF - table$GA

  #create home and away tables if HA_tables = TRUE
  if (HA_tables == TRUE) {

    #aggregate
    tableHA               <- aggregate(list(ds2$P, ds2$W, ds2$D, ds2$L, ds2$Pts, ds2$GF, ds2$GA), by=list(ds2$Team,ds2$location), FUN=sum)
    names(tableHA)        <- c("Team","Location","P","W","D","L","Pts","GF","GA")

    #create home table
    tableH                <- tableHA[which(tableHA$Location=="H"),]
    names(tableH)         <- c("Team","Location","P_H","W_H","D_H","L_H","Pts_H","GF_H","GA_H")
    tableH$GD_H           <- tableH$GF_H - tableH$GA_H
    tableH$Location       <- NULL

    #create away tablöe
    tableA                <- tableHA[which(tableHA$Location=="A"),]
    names(tableA)         <- c("Team","Location","P_A","W_A","D_A","L_A","Pts_A","GF_A","GA_A")
    tableA$GD_A           <- tableA$GF_A - tableA$GA_A
    tableA$Location       <- NULL

    #merge with main table
    tableHA               <- merge(tableH, tableA, by="Team")
    table                 <- merge(table, tableHA, by="Team")
  }

  #order table
  for (by in 1:length(rank_by)) {

      by <- by - 1
      table <- table[order(-table[,rank_by[length(rank_by)-by]]),]

  }

  #add position
  row.names(table) <- NULL
  Pos              <- 1:nrow(table)
  table            <- cbind(Pos, table)

  #check for same rank by pasting relevant ranking columns
  table$rank_comp  <- apply(table[,rank_by],1,paste,collapse = " ")

  #set min position for all teams of same rank
  for (i in 1:length(unique(table$rank_comp))) {

    table[which(table$rank_comp==unique(table$rank_comp)[i]),"Pos"] <- min(table[which(table$rank_comp==unique(table$rank_comp)[i]),"Pos"])

  }

  #remove help column
  table$rank_comp <- NULL


  return(table)

}


