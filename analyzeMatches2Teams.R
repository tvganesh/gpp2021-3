#########################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 28 Jun 2020
# File: analyzeMatches2Teams.R
# More details: https://gigadom.in/
#
#########################################################################################################
#Analyze all IPL matches between 2 IPL teams
analyzeMatches2Teams <- function(match2,matchFunc,plotOrTable1,repType, team,opposition,t20type) {


    # Check and get the team indices of IPL teams in which the bowler has played
    print("Entering matches 2 analyze..........")
    cat("match2=",match2,"matchFunc=",matchFunc,"plotTbl=",plotOrTable1,"rep=",repType,
        "team=",team,"oppn=",opposition,"type=",t20type,"\n")
    if(t20type == "IPL"){
        dir1="./ipl/iplMatches2Teams"
        IPLmatch <- paste("./ipl/iplMatches2Teams/", match2,".RData",sep="")
        cat("IPL2=",getwd(),"\n")
        load(IPLmatch)
        matchesDF <- matches
    } else if (t20type == "T20M"){
        dir1="./t20/t20Matches2Teams"
        T20Mmatch <- paste("./t20/t20Matches2Teams/", match2,".RData",sep="")
        cat("t20m2=",getwd(),"\n")
        load(T20Mmatch)
        matchesDF <- matches

    } else if (t20type == "T20W"){
        dir1="./t20/t20WomenMatches2Teams"
        T20Wmatch <- paste("./t20/t20WomenMatches2Teams/", match2,".RData",sep="")
        cat("T20W2=",getwd(),"\n")
        load(T20Wmatch)
        matchesDF <- matches

    } else if (t20type == "BBL"){
        dir1="./bbl/bblMatches2Teams"
        BBLmatch <- paste("./bbl/bblMatches2Teams/", match2,".RData",sep="")
        cat("BBL2=",getwd(),"\n")
        load(BBLmatch)
        matchesDF <- matches

    } else if (t20type == "NTB"){
        dir1="./ntb/ntbMatches2Teams"
        NTBmatch <- paste("./ntb/ntbMatches2Teams/", match2,".RData",sep="")
        cat("NTB2=",getwd(),"\n")
        load(NTBmatch)
        matchesDF <- matches

    } else if (t20type == "PSL"){
        dir1="./psl/pslMatches2Teams"
        PSLmatch <- paste("./psl/pslMatches2Teams/", match2,".RData",sep="")
        cat("PSL2=",getwd(),"\n")
        load(PSLmatch)
        matchesDF <- matches

    } else if (t20type == "WBB"){
        dir1="./wbb/wbbMatches2Teams"
        WBBmatch <- paste("./wbb/wbbMatches2Teams/", match2,".RData",sep="")
        cat("WBB2=",getwd(),"\n")
        load(WBBmatch)
        matchesDF <- matches

    } else if (t20type == "ODIM"){
        dir1="./odi/odiMenMatches2Teams"
        ODIMmatch <- paste("./odi/odiMatches2Teams/", match2,".RData",sep="")
        cat("ODIM2=",getwd(),"\n")
        load(ODIMmatch)
        matchesDF <- matches

    } else if (t20type == "ODIW"){
        dir1="./odi/odiWomenMatches2Teams"
        ODIWmatch <- paste("./odi/odiWomenMatches2Teams/", match2,".RData",sep="")
        cat("ODIW2=",getwd(),"\n")
        load(ODIWmatch)
        matchesDF <- matches

    } else if (t20type == "CPL"){
        dir1="./cpl/cplMatches2Teams"
        CPLmatch <- paste("./cpl/cplMatches2Teams/", match2,".RData",sep="")
        cat("CPL2=",getwd(),"\n")
        load(CPLmatch)
        matchesDF <- matches
    }

    cat("dim1=",dim(matchesDF),"\n")

    if(plotOrTable1 == 1){
        val1=TRUE
    } else {
        val1= FALSE
    }

    cat("reptype=",repType,"\n")
    if(repType == 1){
        val2="summary"
    } else {
        val2= "detailed"
    }

    # Call the correct function
    if(matchFunc == "Team Batting Scorecard All Matches"){
        cat("dim2=",dim(matchesDF),"team=",team,"oppn=",opposition,"\n")
        teamBattingScorecardOppnAllMatches(matchesDF,team, opposition)
    } else if (matchFunc == "Team Batsmen Batting Partnerships All Matches"){
        cat("dim3=",dim(matchesDF),"team=",team,"oppn=",opposition,"\n")
        if(val1 == TRUE){
            teamBatsmenPartnershipOppnAllMatchesChart(matchesDF,team,opposition,plot=val1)
        } else if(val1 == FALSE){
            if(repType ==1){
                teamBatsmenPartnershiOppnAllMatches(matchesDF,team,report="summary")
            } else if(repType ==2){
                teamBatsmenPartnershiOppnAllMatches(matchesDF,team,report="detailed")
            }
        }
    } else if (matchFunc == "Team Batsmen vs Bowlers all Matches"){
        teamBatsmenVsBowlersOppnAllMatches(matchesDF,team,opposition,plot=val1)
    }  else if (matchFunc == "Team Bowling Scorecard All Matches"){
        teamBowlingPerfOppnAllMatches(matchesDF,team, opposition)
    } else if (matchFunc == "Team Wickets Opposition All Matches"){
        teamBowlersWicketsOppnAllMatches(matchesDF,team,opposition,plot=val1)
    } else if (matchFunc == "Team Bowler vs Batsmen All Matches"){
        teamBowlersVsBatsmenOppnAllMatches(matchesDF,team,opposition,plot=val1)
    } else if (matchFunc == "Team Bowlers Wicket Kind All Matches"){
        teamBowlersWicketKindOppnAllMatches(matchesDF,team,opposition,plot=val1)
    } else if (matchFunc == "Team Bowler Wicket Runs All Matches"){
        teamBowlersWicketRunsOppnAllMatches(matchesDF,team,opposition,plot=val1)
    } else if (matchFunc == "Win Loss Head-to-head All Matches"){
        plotWinLossBetweenTeams(team,opposition,dir1)
    }



}

