#'Create a league table
#'
#'Create a league table by individually defined criteria from a results dataset.
#'
#'Mandatory parameters are a dataset with match results as well the names of the variables for home and away team and their respective scored goals.
#'
#'List of abbreviations:
#'
#'A   = Away (used only as appendix "_A")
#'D   = (Matches) Drawn
#'DC  = Direct comparison (used only as appendix "_DC")
#'GA  = Goals against
#'GD  = Goal difference
#'GF  = Goals for
#'H   = Home (used only as appendix "_H")
#'L   = (Matches) Lost
#'P   = (Matches) Played
#'Pos = Position
#'Pts = Points
#'W   = (Matches) Won
#'
#'
#'Possible ranking criteria:
#'
#'D   = (Matches) Drawn
#'GA  = Goals against
#'GD  = Goal difference
#'GF  = Goals for
#'L   = (Matches) Lost
#'P   = (Matches) Played
#'Pts = Points (Automatically set as most important ranking criterium, doesn't need to be set)
#'W   = (Matches) Won
#'
#'as well as any of the above with the appendix "_DC", for example Pts_DC, GD_DC, which will applied as ranking criteria for teams with an equal amount of points (Pts).
#'
#'Please be aware that ranking for all criteria is done with descending order. So GA or L can technically be used for ranking, but will result in nonsensical results.
#'
#'Furteher optional parameters are the point rewards for wins, draws and losses,the display of additional columns with separate home and away tables and the dates and matchdays to be used for calculation.
#'
#'If a date range and a matchday range are set, the subset of matches that fit both selection criteria will be used for calculation.
#'
#'
#'@param dataset A dataset with the results.
#'@param home The name of the home team column in the dataset as a character string.
#'@param away The name of the away team column in the dataset  as a character string.
#'@param score_home The name of the home team goals column in the dataset as a character string.
#'@param score_away The name of the away team goals column in the dataset as a character string.
#'@param rank_by A character vector with the order of arguments to sort the league table following "Pts". Defaults to c("GD","GF").
#'@param points A vector of integers of length three containing the points awarded for wins, draws and losses. Defaults to c(3,1,0).
#'@param date The name of the date column in the dataset as a character string (optional).
#'@param date_start The earliest date to include if not the earliest date in the dataset as a character string in the format "YY-mm-dd" (optional).
#'@param date_end The last date to include if not the last date in the dataset as character string in the format "YY-mm-dd" (optional).
#'@param matchday The name of the matchday column in the dataset as a character string (optional).
#'@param matchday_start The earliest matchday to include if not the earliest in the dataset as an integer (optional).
#'@param matchday_end The last matchday to include if not the last in the dataset as an integer (optional).
#'@param HA_display Logical value to indicate whether home and away results should be displayed in the table. Defaults to FALSE.
#'@param DC_display Logical value to indicate whether direct comparison variables from ranking vector should be displayed in the table. Defaults to FALSE.
#'@return A data frame in the form of a league table.
#'@examples
#'#create league table for La Liga 1994/95 with two points rewards with home and away results
#'library(engsoccerdata)
#'leaguetable(dataset=engsoccerdata::spain[which(engsoccerdata::spain$Season==1994),], home="home", away="visitor", score_home="hgoal", score_away="vgoal", date="Date", points = c(2,1,0), rank_by = c("Pts_DC","GD_DC","GF_DC","GD","GF"), DC_display = TRUE)
#'@export



leaguetable <- function(dataset, home, away, score_home, score_away, date, date_start, date_end, matchday, matchday_start, matchday_end, points = c(3,1,0), rank_by = c("GD","GF"), HA_display = FALSE, DC_display=FALSE) {


  #set ranking vector
  rank_crit     <- c("Pts",rank_by)

  #check whether there are direct comparison ranking criteria involved
  if(any(grepl("DC",rank_crit))){
    #set DC to TRUE
    DC            <- TRUE
    #create vector of relevant variables for direct comparison
    DC_vars       <- rank_crit[grep("DC",rank_crit)]
  }

  #get vector of columns to include for calculation
  include       <- c(home, away, score_home, score_away)
  include_names <- c("home", "away", "score_home", "score_away")

  #add optional arguments to include
  optionals <- vector()

  #name vector for individual team results
  names_itr <- c("Team","Opponent","GF","GA")

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


  #reduce to relevant data
  ds        <- dataset[,include]
  names(ds) <- include_names

  #create dataset of individual team results
  hs <- ds[,c("home","away","score_home","score_away",optionals)]
  names(hs) <- names_itr
  hs$location <- "H"
  as <- ds[,c("away","home","score_away","score_home",optionals)]
  names(as) <- names_itr
  as$location <- "A"
  ds2 <- rbind(hs,as)

  #reduce to relevant timeframe if date is provided
  if(missing(date)==FALSE) {
    ds2     <- ds2[which(ds2$Date >= date_start & ds2$Date <= date_end),]
  }

  #reduce to relevant matchdays if date is provided
  if(missing(matchday)==FALSE) {
    ds2     <- ds2[which(ds2$Matchday >= matchday_start & ds2$Matchday <= matchday_end),]
  }

  #calculate games played, wins, draws, losses
  ds2$R   <- ifelse(ds2$GF>ds2$GA,"W",ifelse(ds2$GF<ds2$GA,"L","D"))
  ds2$W   <- ifelse(ds2$R == "W",1,0)
  ds2$D   <- ifelse(ds2$R == "D",1,0)
  ds2$L   <- ifelse(ds2$R == "L",1,0)
  ds2$P   <- 1

  #calculate points
  ds2$Pts <- ifelse(ds2$R=="W",points[1],ifelse(ds2$R == "L",points[3], points[2]))


  #create table by aggregating all statistics by team
  table                 <- aggregate(list(ds2$P, ds2$W, ds2$D, ds2$L, ds2$Pts, ds2$GF, ds2$GA), by=list(ds2$Team), FUN=sum)
  names(table)          <- c("Team","P","W","D","L","Pts","GF","GA")

  #compute goal difference
  table$GD              <- table$GF - table$GA

  #create home and away tables if HA_tables = TRUE
  if (HA_display==TRUE) {

    #aggregate
    tableHA               <- aggregate(list(ds2$P, ds2$W, ds2$D, ds2$L, ds2$Pts, ds2$GF, ds2$GA), by=list(ds2$Team,ds2$location), FUN=sum)
    names(tableHA)        <- c("Team","Location","P","W","D","L","Pts","GF","GA")

    #create home table
    tableH                <- tableHA[which(tableHA$Location=="H"),]
    names(tableH)         <- c("Team","Location","P_H","W_H","D_H","L_H","Pts_H","GF_H","GA_H")
    tableH$GD_H           <- tableH$GF_H - tableH$GA_H
    tableH$Location       <- NULL

    #create away table
    tableA                <- tableHA[which(tableHA$Location=="A"),]
    names(tableA)         <- c("Team","Location","P_A","W_A","D_A","L_A","Pts_A","GF_A","GA_A")
    tableA$GD_A           <- tableA$GF_A - tableA$GA_A
    tableA$Location       <- NULL

    #merge with main table
    tableHA               <- merge(tableH, tableA, by="Team")
    table                 <- merge(table, tableHA, by="Team")
  }

  #separate all teams with equal points and rank them if direct comparison is selected
  if(DC==TRUE){

    #find counts of points with more than one team
    same_pts <- aggregate(table$Pts, by=list(table$Pts), FUN = length)
    uni_pts  <- same_pts[which(same_pts[,2]>1), 1]

    #create empty variables for direct comparison ranking criteria
    for (rc in 1:length(DC_vars)){
       table[,DC_vars[rc]] <- NA
    }

    #only execute when at least two teams have the same amount of points
    if(length(uni_pts>0)){

      #iterate over all counts of points with more than one team
      for(eq in 1:length(uni_pts)){

        #get teams with equal points
        eq_teams <- table[which(table$Pts == uni_pts[eq]),"Team"]

        #create subset of games with teams of equal points
        ds2_sub  <- ds2[which(is.element(ds2$Team, eq_teams) & is.element(ds2$Opponent, eq_teams)),]

        #create sub table for teams with equal points
        sub_table    <- aggregate(list(ds2_sub$P, ds2_sub$W, ds2_sub$D, ds2_sub$L, ds2_sub$Pts, ds2_sub$GF, ds2_sub$GA), by=list(ds2_sub$Team), FUN=sum)
        names(sub_table) <- c("Team","P_DC","W_DC","D_DC","L_DC","Pts_DC","GF_DC","GA_DC")

        #compute goal difference
        sub_table$GD_DC              <- sub_table$GF_DC - sub_table$GA_DC

        #iterate over teams in subtable
        for (team in 1:nrow(sub_table)) {

          #iterate over DC_vars
          for (rc in 1:length(DC_vars)){

            #fill direct comparison variables
            table[which(table$Team == sub_table[team, "Team"]), DC_vars[rc]] <- sub_table[team, DC_vars[rc]]

          }
        }
      }
    }
  }

  #Order table with ranking vector for final table
  for (by in length(rank_crit):1) {
      table <- table[order(-table[,rank_crit[by]]),]
  }

  #add position
  row.names(table) <- NULL
  Pos              <- 1:nrow(table)
  table            <- cbind(Pos, table)

  #correct position, if all ranking criteria are equal

  #check for same rank by pasting relevant ranking columns
  table$rank_comp  <- apply(table[,rank_crit],1,paste,collapse = " ")

  #iterate over all groups  of teams with equal rank
  for (i in 1:length(unique(table$rank_comp))) {

    #set min position for all teams of same rank
    table[which(table$rank_comp==unique(table$rank_comp)[i]),"Pos"] <- min(table[which(table$rank_comp==unique(table$rank_comp)[i]),"Pos"])

  }

  #remove help column
  table$rank_comp <- NULL

  if (DC_display==FALSE) {
    #remove DC_variables
    table[,DC_vars] <- NULL
  }

  return(table)


}


