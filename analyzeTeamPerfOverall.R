#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 28 Jun 2020
# File: analyzeTeamPerfOverall.R
# More details: https://gigadom.in/
#
#########################################################################################################
# Analyze an IPL team's performance in all matches
analyzeTeamPerfOverall <- function(matches,matchFunc,team,rankV,plotOrTable2,repType2,t20type="IPL") {

  if(t20type == "IPL"){
    # Check and get the team indices of IPL teams in which the bowler has played
    dir1="./ipl/iplAllMatchesAllTeams/"
    IPLmatch <- paste("./ipl/iplAllMatchesAllTeams/", matches,".RData",sep="")
    load(IPLmatch)
    matchesDF <- matches
    print(repType2)
  } else if (t20type == "T20M"){
    dir1="./t20/t20AllMatchesAllTeams/"
    T20Mmatch <- paste("./t20/t20AllMatchesAllTeams/", matches,".RData",sep="")
    load(T20Mmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "T20W"){
    dir1="./t20/t20WomenAllMatchesAllTeams/"
    T20Wmatch <- paste("./t20/t20WomenAllMatchesAllTeams/", matches,".RData",sep="")
    load(T20Wmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "BBL"){
    dir1="./bbl/bblAllMatchesAllTeams/"
    BBLmatch <- paste("./bbl/bblAllMatchesAllTeams/", matches,".RData",sep="")
    load(BBLmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "NTB"){
    dir1="./ntb/ntbAllMatchesAllTeams/"
    NTBmatch <- paste("./ntb/ntbAllMatchesAllTeams/", matches,".RData",sep="")
    load(NTBmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "NTB"){
    dir1="./ntb/ntbAllMatchesAllTeams/"
    NTBmatch <- paste("./ntb/ntbAllMatchesAllTeams/", matches,".RData",sep="")
    load(NTBmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "PSL"){
    dir1="./psl/pslAllMatchesAllTeams/"
    PSLmatch <- paste("./psl/pslAllMatchesAllTeams/", matches,".RData",sep="")
    load(PSLmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "WBB"){
    dir1="./wbb/wbbAllMatchesAllTeams/"
    WBBmatch <- paste("./wbb/wbbAllMatchesAllTeams/", matches,".RData",sep="")
    load(WBBmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "ODIM"){
    dir1="./odi/odiAllMatchesAllTeams/"
    ODIMmatch <- paste("./odi/odiAllMatchesAllTeams/", matches,".RData",sep="")
    load(ODIMmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "ODIW"){
    dir1="./odi/odiWomenAllMatchesAllTeams/"
    cat("dir=","\n")
    cat(dir("./odi/odiWomenAllMatchesAllTeams/"))
    ODIWmatch <- paste("./odi/odiWomenAllMatchesAllTeams/", matches,".RData",sep="")
    load(ODIWmatch)
    matchesDF <- matches
    print(repType2)

  } else if (t20type == "CPL"){
    dir1="./cpl/cplAllMatchesAllTeams/"
    # Check and get the team indices of CPL teams in which the bowler has played
    CPLmatch <- paste("./cpl/cplAllMatchesAllTeams/", matches,".RData",sep="")
    load(CPLmatch)
    matchesDF <- matches
    print(repType2)
  } else if (t20type == "SSM"){
    dir1="./ssm/ssmAllMatchesAllTeams/"
    # Check and get the team indices of SSM teams in which the bowler has played
    SSMmatch <- paste("./ssm/ssmAllMatchesAllTeams/", matches,".RData",sep="")
    load(SSMmatch)
    matchesDF <- matches
    print(repType2)
  }

    if(plotOrTable2 == 1 || plotOrTable2 == 2 ){
        val3=TRUE
    } else {
        val3= FALSE
    }

    # Call the correct function
    if(matchFunc == "Team Batting Scorecard Overall"){
        teamBattingScorecardAllOppnAllMatches(matchesDF,team)
    } else if (matchFunc == "Team Batsmen Partnerships Overall"){
        if(val3 == TRUE){
            teamBatsmenPartnershipAllOppnAllMatchesPlot(matchesDF,team,main=team,plot=plotOrTable2)
        } else if(val3 == FALSE){
            if(repType2 ==1){
                teamBatsmenPartnershipAllOppnAllMatches(matchesDF,team,report="summary")
            } else if(repType2 ==2){
                teamBatsmenPartnershipAllOppnAllMatches(matchesDF,team,report="detailed")
            }
        }


    } else if (matchFunc == "Team Batsmen vs Bowlers Overall"){
        if(val3 == TRUE){
           df <- teamBatsmenVsBowlersAllOppnAllMatchesRept(matchesDF,team,rank=as.integer(rankV),dispRows = 20)
           teamBatsmenVsBowlersAllOppnAllMatchesPlot(df,plot=plotOrTable2)
        } else {
            teamBatsmenVsBowlersAllOppnAllMatchesRept(matchesDF,team,rank=as.integer(rankV))
        }

    } else if(matchFunc == "Team Bowling Scorecard Overall"){
        teamBowlingScorecardAllOppnAllMatchesMain(matchesDF,theTeam=team)

    } else if (matchFunc == "Team Bowler vs Batsmen Overall"){
        if(val3 == TRUE){
           df <- teamBowlersVsBatsmenAllOppnAllMatchesRept(matchesDF,team,rank=as.integer(rankV))
           teamBowlersVsBatsmenAllOppnAllMatchesPlot(df,team,team,plot=plotOrTable2)
        } else {
            teamBowlersVsBatsmenAllOppnAllMatchesRept(matchesDF,team,rank=as.integer(rankV))
        }


    } else if (matchFunc == "Team Bowler Wicket Kind Overall"){
        teamBowlingWicketKindAllOppnAllMatches(matchesDF,team,"All",plot=plotOrTable2)

    } else if (matchFunc == "Win Loss Team vs All Opposition"){
      plotWinLossTeamVsAllTeams(team,dir1,plot=plotOrTable2)

    }

}
