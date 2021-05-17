#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 27 Jan 2021
# File: CPLutilities.R
# More details: https://gigadom.in/
#
#########################################################################################################
library(dplyr)

# Run this function first. This is imporant to create the data sets
getCPLBatsmen <- function(dir="."){
    teams <-c("Antigua Hawksbills","Barbados Tridents","Guyana Amazon Warriors","Jamaica Tallawahs",
              "St Kitts and Nevis Patriots","St Lucia Zouks","Trinbago Knight Riders")


    cwd=getwd()
    print("CPLbatsmen")
    cat("CPLbats=",cwd)
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
    cplBatsmen <- a[order(a$batsman),]

    # Create all batsmen from all teams for display in UI
    batsmenFile=  paste(dir,"/","CPLBatsmen.RData",sep="")
    save(cplBatsmen, file=batsmenFile)
    cat("cplba=",getwd())
}

# Get the team indices of CPL teams for which the bowler as played
getCPLTeamIndex <- function(batsman,dir="."){
    teams <-c("Antigua Hawksbills","Barbados Tridents","Guyana Amazon Warriors","Jamaica Tallawahs",
              "St Kitts and Nevis Patriots","St Lucia Zouks","Trinbago Knight Riders")


    cwd=getwd()

    cat("Entering CPL teamIndex",getwd(),"\n")
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

    teamsBatsmen <- list(`Antigua Hawksbills-batsmen`,`Barbados Tridents-batsmen`,`Guyana Amazon Warriors-batsmen`,`Jamaica Tallawahs-batsmen`,
        `St Kitts and Nevis Patriots-batsmen`,`St Lucia Zouks-batsmen`,`Trinbago Knight Riders-batsmen`)

    b <- NULL
    for (i in 1:length(teamsBatsmen)){
        a <- which(teamsBatsmen[[i]] == batsman)

        if(length(a) != 0)
            b <- c(b,i)
    }
    cat("Exiting teamindex=",getwd(),"\n")
    b

}


getCPLBowlers <- function(dir="."){
    teams <-c("Antigua Hawksbills","Barbados Tridents","Guyana Amazon Warriors","Jamaica Tallawahs",
              "St Kitts and Nevis Patriots","St Lucia Zouks","Trinbago Knight Riders")

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
    cplBowlers <- a[order(a$bowler),]

    # Create all batsmen from all teams for display in UI
    bowlersFile=  paste(dir,"/","CPLBowlers.RData",sep="")
    save(cplBowlers, file=bowlersFile)
    cat("bowlers=",getwd(),"\n")
}

# Get the team indices of CPL teams for which the bowler as played
getCPLTeamIndex_bowler <- function(bowler,dir="."){
    teams <-c("Antigua Hawksbills","Barbados Tridents","Guyana Amazon Warriors","Jamaica Tallawahs",
              "St Kitts and Nevis Patriots","St Lucia Zouks","Trinbago Knight Riders")

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
    teamsBowlers = list(`Antigua Hawksbills-bowlers`,`Barbados Tridents-bowlers`,`Guyana Amazon Warriors-bowlers`,`Jamaica Tallawahs-bowlers`,
                        `St Kitts and Nevis Patriots-bowlers`,`St Lucia Zouks-bowlers`,`Trinbago Knight Riders-bowlers`)


    b <- NULL
    for (i in 1:length(teamsBowlers)){
        a <- which(teamsBowlers[[i]] == bowler)

        if(length(a) != 0)
            b <- c(b,i)
    }
    cat("Exiting teamindex_bowlers=",getwd(),"\n")
    b
}

# Get the list of the CPL team names from the indices passed
getCPLTeams <- function(x){

    l <- NULL
    # Get the teams passed in as indexes
    for (i in seq_along(x)){

        l <- c(l, CPLTeamNames[[x[i]]])

    }
    l
}
