#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 24 May 2021
# File: SSMutilities.R
# More details: https://gigadom.in/
#
#########################################################################################################
library(dplyr)

# Run this function first. This is imporant to create the data sets
getSSMBatsmen <- function(dir="."){

  teams <-c("Auckland", "Canterbury", "Central Districts",
            "Northern Districts", "Otago", "Wellington")

  cwd=getwd()
  print("T20batsmen")
  cat("ssmbats=",cwd)
  battingDF <- NULL
  for(team in teams){
    battingDetails <- NULL
    val <- paste(dir,"/",team,"-BattingDetails.RData",sep="")
    print(val)
    tryCatch(load(val),
             error = function(e) {
               print("No data1")
               setNext=TRUE
             }


    )
    details <- battingDetails

    # Save individual team batsmen separately
    df1 <- select(details,batsman,runs,strikeRate)
    a1 <- df1 %>% distinct(batsman)
    a1$batsman = as.character(a1$batsman)
    teamBatsmen <- a1[order(a1$batsman),]
    teamFile = paste(dir,"/",team,"-batsmen.rds",sep="")
    saveRDS(teamBatsmen, file=teamFile)


    # Create DF of all batsmen
    battingDF <- rbind(battingDF,details)
  }

  df <- select(battingDF,batsman,runs,strikeRate)
  a <- df %>% distinct(batsman)
  a$batsman = as.character(a$batsman)
  ssmBatsmen <- a[order(a$batsman),]

  # Create all batsmen from all teams for display in UI
  batsmenFile=  paste(dir,"/","SSMbatsmen.RData",sep="")
  cat("T20Bfile=",batsmenFile)
  save(ssmBatsmen, file=batsmenFile)
  cat("t20lba=",getwd())
}

# Get the team indices of T20 Men teams for which the bowler as played
getSSMTeamIndex <- function(batsman,dir="."){
  teams <-c("Auckland", "Canterbury", "Central Districts",
            "Northern Districts", "Otago", "Wellington")


  cwd=getwd()

  cat("Entering teamIndex Men",getwd(),"\n")
  cat("cwd=",cwd,"\n")
  cat(getwd(),"\n")


  for(team in teams){
     # Set the name of the data frame
     val <- paste(team,"-batsmen",sep="")
     batsmen <- paste(cwd,'/',dir,"/",team,"-batsmen.rds",sep="")
     cat("tibat=",batsmen,"\n")
     df <-readRDS(batsmen)
     #Assign a name to the dataframe
     m <-assign(val,df)

  }


  #Load add dataframes

  teamsBatsmen = list(`Auckland-batsmen`,`Canterbury-batsmen`,`Central Districts-batsmen`,
                      `Northern Districts-batsmen`,`Otago-batsmen`,`Wellington-batsmen`)


  b <- NULL
  for (i in 1:length(teamsBatsmen)){
    a <- which(teamsBatsmen[[i]] == batsman)

    if(length(a) != 0)
      b <- c(b,i)
  }
  cat("Exiting teamindex=",getwd(),"\n")
  b

}


getSSMBowlers <- function(dir="."){
  teams <-c("Auckland", "Canterbury", "Central Districts",
            "Northern Districts", "Otago", "Wellington")

  cwd=getwd()
  cat("bowlers=",cwd,"\n")
  print(dir)
  bowlingDF <- NULL
  for(team in teams){
    bowlingDetails <- NULL
    val <- paste(dir,"/",team,"-BowlingDetails.RData",sep="")
    print(val)
    tryCatch(load(val),
             error = function(e) {
               print("No data1")
               setNext=TRUE
             }


    )
    details <- bowlingDetails
    # Save individual team batsmen separately
    df1 <- select(details,bowler,economyRate)
    print(dim(df1))
    a1 <- df1 %>% distinct(bowler)
    a1$bowler = as.character(a1$bowler)
    teamBowlers <- a1[order(a1$bowler),]
    teamFile = paste(dir,"/",team,"-bowlers.rds",sep="")
    saveRDS(teamBowlers, file=teamFile)


    # Create DF of all batsmen
    bowlingDF <- rbind(bowlingDF,details)
  }

  df <- select(bowlingDF,bowler,economyRate)
  a <- df %>% distinct(bowler)
  a$bowler = as.character(a$bowler)
  ssmBowlers <- a[order(a$bowler),]

  # Create all batsmen from all teams for display in UI
  bowlersFile=  paste(dir,"/","SSMbowlers.RData",sep="")
  save(ssmBowlers, file=bowlersFile)
  cat("bowlers=",getwd(),"\n")
}

# Get the team indices of T20 Men teams for which the bowler as played
getSSMTeamIndex_bowler <- function(bowler,dir="."){
  teams <-c("Auckland", "Canterbury", "Central Districts",
            "Northern Districts", "Otago", "Wellington")

  cwd=getwd()
  cat("Entering teamIndex_bowlers",getwd(),"\n")
  cat("cwd=",cwd,"\n")

  cat(getwd(),"\n")
  for(team in teams){
    # Set the name of the data frame
    val <- paste(team,"-bowlers",sep="")
    bowlers <- paste(dir,"/",team,"-bowlers.rds",sep="")
    df <-readRDS(bowlers)
    #Assign a name to the dataframe
    m <-assign(val,df)

  }

  cat("bowlerindex=",getwd(),"\n")
  #Load add dataframes
  teamsBowlers = list(`Auckland-bowlers`,`Canterbury-bowlers`,`Central Districts-bowlers`,
                    `Northern Districts-bowlers`,`Otago-bowlers`,`Wellington-bowlers`)

  b <- NULL
  for (i in 1:length(teamsBowlers)){
    a <- which(teamsBowlers[[i]] == bowler)

    if(length(a) != 0)
      b <- c(b,i)
  }
  cat("Exiting teamindex_bowlers=",getwd(),"\n")
  b
}

# Get the list of the IPL team names from the indices passed
getSSMTeams <- function(x){

  l <- NULL
  # Get the teams passed in as indexes
  for (i in seq_along(x)){

    l <- c(l, SSMTeamNames[[x[i]]])

  }
  l
}
