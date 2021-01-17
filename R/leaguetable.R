#'Create a league table
#'
#'Create a table for sports leagues with three way results (e.g. soccer, handball) by individually defined criteria from a results dataset.
#'
#'@param dataset A dataset with the results.
#'@param home Name of the home team variable in the dataset as a character string.
#'@param away Name of the away team variable in the dataset as a character string.
#'@param score_home Name of the home team goals variable in the dataset as a character string.
#'@param score_away Name of the away team goals variable in the dataset as a character string.
#'@param rank_by Character vector with the order of arguments to sort the league table following "Pts". Defaults to c("GD","GF").
#'@param points Vector of integers of length three containing the points awarded for wins, draws and losses. Defaults to c(3,1,0).
#'@param date Name of the date variable in the dataset as a character string (optional).
#'@param date_start Earliest date to include if not the earliest date in the dataset as a character string in the format "YY-mm-dd" (optional).
#'@param date_end Last date to include if not the last date in the dataset as character string in the format "YY-mm-dd" (optional).
#'@param matchday Name of the matchday variable in the dataset as a character string (optional).
#'@param matchday_start Earliest matchday to include if not the earliest in the dataset as an integer (optional).
#'@param matchday_end Last matchday to include if not the last in the dataset as an integer (optional).
#'@param HA_display Logical value to indicate whether home and away results should be displayed in the table. Defaults to FALSE.
#'@param DC_display Logical value to indicate whether direct comparison variables from ranking vector should be displayed in the table. Defaults to FALSE.
#'@details Mandatory input is a dataset with match results and the names of the variables for home and away team and their respective scored goals.
#'
#'List of abbreviations:
#'
#'\itemize{
#'\item A   = Away (used only as appendix "_A"),
#'\item D   = (Matches) Drawn,
#'\item DC  = Direct comparison (used only as appendix "_DC"),
#'\item GA  = Goals against,
#'\item GD  = Goal difference,
#'\item GF  = Goals for,
#'\item H   = Home (used only as appendix "_H"),
#'\item L   = (Matches) Lost,
#'\item P   = (Matches) Played,
#'\item Pos = Position,
#'\item Pts = Points,
#'\item W   = (Matches) Won.
#'}
#'
#'
#'
#'Possible ranking criteria are:
#'
#'\itemize{
#'\item D   = (Matches) Drawn,
#'\item GA  = Goals against,
#'\item GD  = Goal difference,
#'\item GF  = Goals for,
#'\item L   = (Matches) Lost,
#'\item P   = (Matches) Played,
#'\item Pts = Points (Automatically set as most important ranking criterium, doesn't need to be set),
#'\item W   = (Matches) Won
#'}
#'
#'as well as any of the above with the appendix "_DC", for example Pts_DC or GD_DC, which will applied as ranking criteria for teams with an equal number of points (Pts).
#'
#'Please be aware that ranking for all criteria is done with descending order. So GA or L can technically be used for ranking, but will result in nonsensical results.
#'
#'Further optional parameters are the point rewards for wins, draws and losses,the display of additional columns with separate home and away tables and the dates and matchdays to be used for calculation.
#'
#'If a date range and a matchday range are set, the subset of matches that fit both selection criteria will be used for calculation.
#'
#'
#'@return League table in the form of a data.frame.
#'@examples
#'#league table for La Liga 94/95 with three point rewards (instead of two) and home and away results
#'require(engsoccerdata)
#'leaguetable(dataset=engsoccerdata::spain[which(engsoccerdata::spain$Season==1994),],
#'home="home", away="visitor",score_home="hgoal", score_away="vgoal", date="Date",
#'points = c(3,1,0), rank_by = c("Pts_DC","GD_DC","GF_DC","GD","GF"), DC_display = TRUE)
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
  ds        <- setNames(dataset[,include],include_names)

  #create dataset of individual team results
  hs <- setNames(ds[,c("home","away","score_home","score_away",optionals)],names_itr)
  hs$location <- "H"

  as <- setNames(ds[,c("away","home","score_away","score_home",optionals)],names_itr)
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
  tab                 <- setNames(aggregate(list(ds2$P, ds2$W, ds2$D, ds2$L, ds2$Pts, ds2$GF, ds2$GA), by=list(ds2$Team), FUN=sum),c("Team","P","W","D","L","Pts","GF","GA"))

  #compute goal difference
  tab$GD              <- tab$GF - tab$GA

  #create home and away tables if HA_tables = TRUE
  if (HA_display==TRUE) {

    #aggregate
    tabHA               <- setNames(aggregate(list(ds2$P, ds2$W, ds2$D, ds2$L, ds2$Pts, ds2$GF, ds2$GA), by=list(ds2$Team,ds2$location), FUN=sum),c("Team","Location","P","W","D","L","Pts","GF","GA"))

    #create home table
    tabH                <- setNames(tabHA[which(tabHA$Location=="H"),],c("Team","Location","P_H","W_H","D_H","L_H","Pts_H","GF_H","GA_H"))
    tabH$GD_H           <- tabH$GF_H - tabH$GA_H
    tabH$Location       <- NULL

    #create away table
    tabA                <- setNames(tabHA[which(tabHA$Location=="A"),],c("Team","Location","P_A","W_A","D_A","L_A","Pts_A","GF_A","GA_A"))
    tabA$GD_A           <- tabA$GF_A - tabA$GA_A
    tabA$Location       <- NULL

    #merge with main table
    tabHA               <- merge(tabH, tabA, by="Team")
    tab                 <- merge(tab, tabHA, by="Team")
  }

  #separate all teams with equal points and rank them if direct comparison is selected
  if(DC==TRUE){

    #find counts of points with more than one team
    same_pts <- aggregate(tab$Pts, by=list(tab$Pts), FUN = length)
    uni_pts  <- same_pts[which(same_pts[,2]>1), 1]

    #create empty variables for direct comparison ranking criteria
    for (rc in 1:length(DC_vars)){
       tab[,DC_vars[rc]] <- 0
    }

    #only execute when at least two teams have the same amount of points
    if(length(uni_pts>0)){

      #iterate over all counts of points with more than one team
      for(eq in 1:length(uni_pts)){

        #get teams with equal points
        eq_teams <- tab[which(tab$Pts == uni_pts[eq]),"Team"]

        #create subset of games with teams of equal points
        ds2_sub  <- ds2[which(is.element(ds2$Team, eq_teams) & is.element(ds2$Opponent, eq_teams)),]

        #create sub table for teams with equal points
        sub_tab    <- setNames(aggregate(list(ds2_sub$P, ds2_sub$W, ds2_sub$D, ds2_sub$L, ds2_sub$Pts, ds2_sub$GF, ds2_sub$GA), by=list(ds2_sub$Team), FUN=sum),c("Team","P_DC","W_DC","D_DC","L_DC","Pts_DC","GF_DC","GA_DC"))

        #compute goal difference
        sub_tab$GD_DC              <- sub_tab$GF_DC - sub_tab$GA_DC

        #iterate over teams in subtable
        for (team in 1:nrow(sub_tab)) {

          #iterate over DC_vars
          for (rc in 1:length(DC_vars)){

            #fill direct comparison variables
            tab[which(tab$Team == sub_tab[team, "Team"]), DC_vars[rc]] <- sub_tab[team, DC_vars[rc]]

          }
        }
      }
    }
  }

  #Order table with ranking vector for final table
  for (by in length(rank_crit):1) {
      tab <- tab[order(-tab[,rank_crit[by]]),]
  }

  #add position
  row.names(tab) <- NULL
  Pos            <- 1:nrow(tab)
  tab            <- cbind(Pos, tab)

  #correct position, if all ranking criteria are equal

  #check for same rank by pasting relevant ranking columns
  tab$rank_comp  <- apply(tab[,rank_crit],1,paste,collapse = " ")

  #iterate over all groups  of teams with equal rank
  for (i in 1:length(unique(tab$rank_comp))) {

    #set min position for all teams of same rank
    tab[which(tab$rank_comp==unique(tab$rank_comp)[i]),"Pos"] <- min(tab[which(tab$rank_comp==unique(tab$rank_comp)[i]),"Pos"])

  }

  #remove help column
  tab$rank_comp <- NULL

  if (DC_display==FALSE) {
    #remove DC_variables
    tab[,DC_vars] <- NULL
  }

  #remove NA
  tab[is.na(tab)] <- 0

  return(tab)


}


