#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 07 Jan 2021
# File: rankPlayers.R
# More details: https://gigadom.in/
#
#########################################################################################################


rankPlayers <- function(input,output,type="IPL",player="batsmen") {
  cat("Entering rank Players\n")

  if (type == "IPL"){
    output$Mode <- renderUI({
        selectInput('runsOverSR', 'Mode',choices=runsVsSR,selected=input$runsOverSR)
    })

    output$Mode1 <- renderUI({
      selectInput('wicketsOverER', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverER)
    })
  } else if (type == "T20M"){
    print("T20M")
      output$ModeT20M <- renderUI({
        selectInput('runsOverSRT20M', 'Mode',choices=runsVsSR,selected=input$runsOverSRT20M)
      })

      output$Mode1T20M <- renderUI({
        selectInput('wicketsOverERT20M', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERT20M)
      })

  } else if (type == "T20W"){
    print("Here111")
    output$ModeT20W <- renderUI({
      selectInput('runsOverSRT20W', 'Mode',choices=runsVsSR,selected=input$runsOverSRT20W)
    })

    output$Mode1T20W <- renderUI({
      selectInput('wicketsOverERT20W', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERT20W)
    })

  } else if (type == "BBL"){
    print("BBL")
    output$ModeBBL <- renderUI({
      selectInput('runsOverSRBBL', 'Mode',choices=runsVsSR,selected=input$runsOverSRBBL)
    })

    output$Mode1BBL <- renderUI({
      selectInput('wicketsOverERBBL', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERBBL)
    })

  } else if (type == "NTB"){
    print("NTB")
    output$ModeNTB <- renderUI({
      selectInput('runsOverSRNTB', 'Mode',choices=runsVsSR,selected=input$runsOverSRNTB)
    })

    output$Mode1NTB <- renderUI({
      selectInput('wicketsOverERNTB', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERNTB)
    })

  } else if (type == "PSL"){
    print("PSL")
    output$ModePSL <- renderUI({
      selectInput('runsOverSRPSL', 'Mode',choices=runsVsSR,selected=input$runsOverSRPSL)
    })

    output$Mode1PSL <- renderUI({
      selectInput('wicketsOverERPSL', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERPSL)
    })

  } else if (type == "WBB"){
    print("WBB")
    output$ModeWBB <- renderUI({
      selectInput('runsOverSRWBB', 'Mode',choices=runsVsSR,selected=input$runsOverSRWBB)
    })

    output$Mode1WBB <- renderUI({
      selectInput('wicketsOverERWBB', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERWBB)
    })

  } else if (type == "CPL"){
    print("CPL")
    output$ModeCPL <- renderUI({
      selectInput('runsOverSRCPL', 'Mode',choices=runsVsSR,selected=input$runsOverSRCPL)
    })

    output$Mode1CPL <- renderUI({
      selectInput('wicketsOverERCPL', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERCPL)
    })

  } else if (type == "SSM"){
    print("SSM")
    output$ModeSSM <- renderUI({
      selectInput('runsOverSRSSM', 'Mode',choices=runsVsSR,selected=input$runsOverSRSSM)
    })

    output$Mode1SSM <- renderUI({
      selectInput('wicketsOverERSSM', 'Mode1',choices=wicketsVsER,selected=input$wicketsOverERSSM)
    })

  }

  cat("Rank players year selected=",input$yearSelected1,"input$minMatches1=", input$minMatches1,"input$wicketsOverER=",input$wicketsOverER)

  if(type == "IPL"){
    if(player=="batsmen"){
      cat("year=",input$yearSelected," minMatches=",input$minMatches, "\n")
      a <-rankT20Batsmen(IPLTeamNames,"./ipl/iplBattingBowlingDetails",input$minMatches, input$yearSelected,input$runsOverSR)
    } else if (player =="bowlers"){
      cat("year1=",input$yearSelected1," minMatches1=",input$minMatches1, "\n")
      a <- rankT20Bowlers(IPLTeamNames,"./ipl/iplBattingBowlingDetails",input$minMatches1, input$yearSelected1,input$wicketsOverER)

    }
  } else if (type ==  "T20M"){
    if(player=="batsmen"){
      a <- rankT20Batsmen(T20MTeamNames,"./t20/t20BattingBowlingDetails",input$minMatchesT20M,input$yearSelectedT20M,input$runsOverSRT20M)
    } else if (player =="bowlers"){
      a <-rankT20Bowlers(T20MTeamNames,"./t20/t20BattingBowlingDetails",input$minMatches1T20M, input$yearSelected1T20M,input$wicketsOverERT20M)

    }
  } else if (type ==  "T20W"){
    if(player=="batsmen"){
      cat("runs vs SR T20WW=",input$runsOverSRT20W,"\n")
      a <- rankT20Batsmen(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails",input$minMatchesT20W,input$yearSelectedT20W,input$runsOverSRT20W)
    } else if (player =="bowlers"){
      a <-rankT20Bowlers(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails",input$minMatches1T20W, input$yearSelected1T20W,input$wicketsOverERT20W)

    }
  } else if (type ==  "BBL"){
    if(player=="batsmen"){
      a <- rankT20Batsmen(BBLTeamNames,"./bbl/bblBattingBowlingDetails",input$minMatchesBBL,input$yearSelectedBBL,input$runsOverSRBBL)
    } else if (player =="bowlers"){
      a <-rankT20Bowlers(BBLTeamNames,"./bbl/bblBattingBowlingDetails",input$minMatches1BBL, input$yearSelected1BBL,input$wicketsOverERBBL)

    }
  } else if (type ==  "NTB"){
    if(player=="batsmen"){
      a <- rankT20Batsmen(NTBTeamNames,"./ntb/ntbBattingBowlingDetails",input$minMatchesNTB,input$yearSelectedNTB,input$runsOverSRNTB)
    } else if (player =="bowlers"){
      a <-rankT20Bowlers(NTBTeamNames,"./ntb/ntbBattingBowlingDetails",input$minMatches1NTB, input$yearSelected1NTB,input$wicketsOverERNTB)

    }
  } else if (type ==  "PSL"){
    if(player=="batsmen"){
      a <- rankT20Batsmen(PSLTeamNames,"./psl/pslBattingBowlingDetails",input$minMatchesPSL,input$yearSelectedPSL,input$runsOverSRPSL)
    } else if (player =="bowlers"){
      a <-rankT20Bowlers(PSLTeamNames,"./psl/pslBattingBowlingDetails",input$minMatches1PSL, input$yearSelected1PSL,input$wicketsOverERPSL)

    }
  } else if (type ==  "WBB"){
    if(player=="batsmen"){
      a <- rankT20Batsmen(WBBTeamNames,"./wbb/wbbBattingBowlingDetails",input$minMatchesWBB,input$yearSelectedWBB,input$runsOverSRWBB)
    } else if (player =="bowlers"){
      a <-rankT20Bowlers(WBBTeamNames,"./wbb/wbbBattingBowlingDetails",input$minMatches1WBB, input$yearSelected1WBB,input$wicketsOverERWBB)

    }
  } else if (type ==  "CPL"){
    if(player=="batsmen"){
      a <- rankT20Batsmen(CPLTeamNames,"./cpl/cplBattingBowlingDetails",input$minMatchesCPL,input$yearSelectedCPL,input$runsOverSRCPL)
    } else if (player =="bowlers"){
      a <-rankT20Bowlers(CPLTeamNames,"./cpl/cplBattingBowlingDetails",input$minMatches1CPL, input$yearSelected1CPL,input$wicketsOverERCPL)

    }
  } else if (type ==  "SSM"){
    if(player=="batsmen"){
      a <- rankT20Batsmen(SSMTeamNames,"./ssm/ssmBattingBowlingDetails",input$minMatchesSSM,input$yearSelectedSSM,input$runsOverSRSSM)
    } else if (player =="bowlers"){
      a <-rankT20Bowlers(SSMTeamNames,"./ssm/ssmBattingBowlingDetails",input$minMatches1SSM, input$yearSelected1SSM,input$wicketsOverERSSM)

    }
  }
  a
}
