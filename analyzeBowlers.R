#########################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 28 Jun 2020
# File: analyzeBowlers.R
# More details: https://gigadom.in/
#
#########################################################################################################
# Analyze IPL bowlers
analyzeBowlers <- function(bowler,func, t20type="IPL",staticIntv1=1) {

  # Return when name is NULL at start
  if(is.null(bowler))
    return()
  cat("bowler=",bowler,"\n")
    # Check and get the team indices of IPL teams in which the bowler has played
    cat("analBow=",getwd())


    if(t20type == "IPL"){
      dir1="./ipl/iplBattingBowlingDetails/"
      dir2="./ipl/iplMatches"
      # Check and get the team indices of IPL teams in which the batsman has played
      i <- getTeamIndex_bowler(bowler, dir1)
      # Get the team names
      teamNames <- getTeams(i)
    }
    else if (t20type == "T20M"){
      dir1="./t20/t20BattingBowlingDetails/"
      dir2="./t20/t20MenMatches"
      i <- getT20MTeamIndex_bowler(bowler, dir1)
      print(i)
      # Get the team names
      teamNames <- getT20MTeams(i)
      print(teamNames)
    } else if (t20type == "T20W"){
      dir1="./t20/t20WomenBattingBowlingDetails/"
      dir2="./t20/t20WomenMatches"
      i <- getT20WTeamIndex_bowler(bowler, dir1)
      print(i)
      # Get the team names
      teamNames <- getT20WTeams(i)
      print(teamNames)
    } else if (t20type == "BBL"){
      dir1="./bbl/bblBattingBowlingDetails/"
      dir2="./bbl/bblMatches"
      i <- getBBLTeamIndex_bowler(bowler, dir1)
      print(i)
      # Get the team names
      teamNames <- getBBLTeams(i)
      print(teamNames)
    } else if (t20type == "NTB"){
      dir1="./ntb/ntbBattingBowlingDetails/"
      dir2="./ntb/ntbMatches"
      i <- getNTBTeamIndex_bowler(bowler, dir1)
      print(i)
      # Get the team names
      teamNames <- getNTBTeams(i)
      print(teamNames)
    } else if (t20type == "PSL"){
      dir1="./psl/pslBattingBowlingDetails/"
      dir2="./psl/pslMatches"
      i <- getPSLTeamIndex_bowler(bowler, dir1)
      print(i)
      # Get the team names
      teamNames <- getPSLTeams(i)
      print(teamNames)
    } else if (t20type == "WBB"){
      dir1="./wbb/wbbBattingBowlingDetails/"
      dir2="./wbb/wbbMatches"
      i <- getWBBTeamIndex_bowler(bowler, dir1)
      print(i)
      # Get the team names
      teamNames <- getWBBTeams(i)
      print(teamNames)
    } else if (t20type == "ODIM"){
      dir1="./odi/odiBattingBowlingDetails/"
      dir2="./odi/odiMenMatches"
      i <- getODIMTeamIndex_bowler(bowler, dir1)
      print(i)
      # Get the team names
      teamNames <- getODIMTeams(i)
      print(teamNames)
    } else if (t20type == "ODIW"){
      dir1="./odi/odiWomenBattingBowlingDetails/"
      dir2="./odi/odiWomenMatches"
      i <- getODIWTeamIndex_bowler(bowler, dir1)
      print(i)
      # Get the team names
      teamNames <- getODIWTeams(i)
      print(teamNames)
    } else if(t20type == "CPL"){
      dir1="./cpl/cplBattingBowlingDetails/"
      dir2="./cpl/cplMatches"
      # Check and get the team indices of IPL teams in which the batsman has played
      i <- getCPLTeamIndex_bowler(bowler, dir1)
      # Get the team names
      teamNames <- getCPLTeams(i)
    } else if(t20type == "SSM"){
      dir1="./ssm/ssmBattingBowlingDetails/"
      dir2="./ssm/ssmMatches"
      # Check and get the team indices of IPL teams in which the batsman has played
      i <- getSSMTeamIndex_bowler(bowler, dir1)
      # Get the team names
      teamNames <- getSSMTeams(i)
    }



    bowlerDF <- NULL

    # Create a consolidated Data frame of bowlers for teams played
    for (i in seq_along(teamNames)){
      tryCatch(df <- getBowlerWicketDetails(team=teamNames[i],name=bowler,dir=dir1),
              error = function(e) {
                print("Error!")
                return
              }
          )
          df <- getBowlerWicketDetails(team=teamNames[i],name=bowler,dir=dir1)
          bowlerDF <- rbind(bowlerDF,df)
    }

    print(dim(bowlerDF))
    # Call the necessary function
    if(func == "Mean Economy Rate of bowler"){
        bowlerMeanEconomyRate(bowlerDF,bowler,staticIntv1)
    } else if (func == "Mean runs conceded by bowler"){
        bowlerMeanRunsConceded(bowlerDF,bowler,staticIntv1)
    }     else if (func == "Bowler's Moving Average"){
        bowlerMovingAverage(bowlerDF,bowler,staticIntv1)
    } else if (func == "Bowler's Cumulative Avg. Wickets"){
        bowlerCumulativeAvgWickets(bowlerDF,bowler,staticIntv1)
    } else if (func == "Bowler's Cumulative Avg. Economy Rate"){
        bowlerCumulativeAvgEconRate(bowlerDF,bowler,staticIntv1)
    } else if (func == "Bowler's Wicket Plot"){
        bowlerWicketPlot(bowlerDF,bowler,staticIntv1)
    } else if (func == "Bowler's Wickets against opposition"){
        bowlerWicketsAgainstOpposition(bowlerDF,bowler,staticIntv1)
    } else if (func == "Bowler's Wickets at Venues"){
        bowlerWicketsVenue(bowlerDF,bowler,staticIntv1)
    } else if (func == "Bowler's wickets prediction"){
      # This is for the function wicket predict
      bowlerDF1 <- NULL
      # Create a consolidated Data frame of batsman for all IPL teams played
      for (i in seq_along(teamNames)){
          # The below 2 lines for Bowler's wicket prediction
          print(teamNames[i])

          df1 <- getDeliveryWickets(team=teamNames[i],dir=dir2,name=bowler,save=FALSE)
          bowlerDF1 <- rbind(bowlerDF1,df1)
      }
      bowlerWktsPredict(bowlerDF1,bowler)

  }

}

