#'Create a match schedule
#'
#'Create a match schedule according to the DFB's (German Soccer Association) "harmonischer Schlüssel-Plan 1-L" which is used as a blueprint for german football leagues.
#'
#'@param teamvector A character vector of teams.
#'@param second_round A logical value, indicating whether a second round with changed home and away team should be planned. Defaults to TRUE.
#'@param match_free A logical value, indicating whether match free teams should be deleted from the schedule. Defaults to TRUE.
#'@param randomize A logical value, indicating whether the team vector should be ordered randomly. Defaults to TRUE.
#'@param seed A user defined integer to replicate the randomization process if randomize = TRUE.
#'@return A data frame containing a match schedule including variables for matchday, home and away team.
#'@references \url{https://portal.dfbnet.org/fileadmin/content/downloads/faq/211111_SZ_DFBnet_extern_mit_Gegenueberstellung4.pdf}
#'@examples
#'#replicable schedule
#'roundrobin(teamvector=LETTERS[1:18],second_round=TRUE,match_free=TRUE,randomize=TRUE,seed=1234)
#'#non replicable schedule
#'roundrobin(teamvector=LETTERS[1:18],second_round=TRUE,match_free=TRUE,randomize=TRUE)
#'@export

roundrobin <- function(teamvector,second_round = TRUE,match_free = TRUE,randomize = TRUE , seed) {

  try(if(length(teamvector)<5) stop("number of teams has to be at least 5"))

  try(if(typeof(second_round)!="logical") stop("second_round has to be logical"))

  try(if(typeof(match_free)!="logical") stop("match_free has to be logical"))

  try(if(typeof(randomize)!="logical") stop("randomize has to be logical"))

  if(missing(seed)==FALSE) {try(if(typeof(seed)!="double") stop("seed has to be an integer"))}

  #compensate for odd teams numbers
  if(length(teamvector)%%2==1) {teamvector <- append(teamvector, "free")}

  if(randomize==TRUE) {

    if(missing(seed)==FALSE) {
      set.seed(seed)
      teamvector <- sample(teamvector)
    }


    if(missing(seed)==TRUE) {
      teamvector <- sample(teamvector)
    }

  }


  teams <- as.integer(1:length(teamvector))

  teamid <- cbind(teamvector,teams)

  for(day in 1:(length(teams)-1)){

    if (day == 1){
      #initialize dataset
      schedule  <- data.frame(Team1=integer(),Team2=integer(),Matchday=integer(), stringsAsFactors = FALSE)

      #initialize positions
      up     <- teams[2:((length(teams)/2))]
      down   <- teams[(length(teams)-1):((length(teams)/2)+1)]
      left1  <- teams[1]
      left2  <- length(teams)

      #save first matches
      schedule[1:length(up),"Team1"]                          <- up
      schedule[1:length(down),"Team2"]                        <- down
      schedule[length(teams)/2, "Team1"]                      <- left1
      schedule[length(teams)/2, "Team2"]                      <- left2
      schedule[1:(length(teams)/2),"Matchday"]                <- day
  }



    if(day > 1 ) {

      #rotate lower side
      left2old <- left2
      left2    <- up[1]
      up       <- append(up[2:length(up)],down[length(down)])
      down     <- append(left2old,down[1:(length(down)-1)])

      #save matches
      rows <- nrow(schedule)

      schedule[(rows+1):(rows+length(up)),"Team1"]            <- up
      schedule[(rows+1):(rows+length(down)),"Team2"]          <- down
      schedule[(rows+length(up)+1), "Team1"]                  <- left1
      schedule[(rows+length(up)+1), "Team2"]                  <- left2
      schedule[(rows+1):(rows+(length(teams)/2)),"Matchday"]  <- day

    }

  }

  #give home right to teams
  schedule$HA <- ifelse(schedule$Matchday%%2==0, ifelse((schedule$Team1%%2==1 & schedule$Team1>schedule$Matchday)|(schedule$Team1%%2==0 & schedule$Team1<=schedule$Matchday),1,2), ifelse((schedule$Team1%%2==0 & schedule$Team1>schedule$Matchday)|(schedule$Team1%%2==1 & schedule$Team1<schedule$Matchday),1,2))
  schedule$HA <- ifelse(schedule$Matchday == 1, ifelse((schedule$Team1%%2==0),1,2), schedule$HA)
  schedule$HA <- ifelse(schedule$Matchday == 2, ifelse((schedule$Team1%%2==1),1,2), schedule$HA)

  #calculate home and away
  schedule$HomeID <- ifelse(schedule$HA == 1, schedule$Team1, schedule$Team2)
  schedule$AwayID <- ifelse(schedule$HA == 1, schedule$Team2, schedule$Team1)

  #merge names
  home     <- merge(schedule, teamid, by.x="HomeID", by.y="teams", all.x=TRUE)
  schedule <- merge(home, teamid, by.x="AwayID", by.y="teams", all.x=TRUE)

  rm(home)

  #remove old columns
  schedule$HA     <- NULL
  schedule$Team1  <- NULL
  schedule$Team2  <- NULL
  schedule$HomeID <- NULL
  schedule$AwayID <- NULL

  names(schedule) <- c("Matchday","Home","Away")

  #create second round matches
  if(second_round == TRUE) {

    #duplicate
    schedule2          <- schedule
    schedule2$Matchday <- schedule$Matchday+(length(teams)-1)
    schedule2$Home     <- schedule$Away
    schedule2$Away     <- schedule$Home

    schedule <- rbind(schedule, schedule2)

  }

  #delete match free pairings
  if(match_free == TRUE) {

    schedule <- schedule[which(schedule$Home != "free" & schedule$Away != "free"),]

  }

  #order by matchday
  schedule <- schedule[order(schedule$Matchday),]

  #get row names straight
  row.names(schedule) <- 1:nrow(schedule)

  return(schedule)
}



