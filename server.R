#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 07 Jan 2021
# File: server.R
# More details: https://gigadom.in/
#
#########################################################################################################
library(shiny)
library(yorkr)
library(rpart)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rpart.plot)
library(ggthemes)
library(plotly)
library(shinycssloaders)

# Source files

source("definitions.R")
source("IPLutilities.R")
source("T20Mutilities.R")
source("T20Wutilities.R")
source("BBLutilities.R")
source("NTButilities.R")
source("PSLutilities.R")
source("WBButilities.R")
source("CPLutilities.R")
#source("ODIMutilities.R")
#source("ODIWutilities.R")
source("analyzeBatsmen.R")
source("analyzeBowlers.R")
source("analyzeMatches.R")
source("analyzeMatches2Teams.R")
source("analyzeTeamPerfOverall.R")
source("printOrPlotMatch.R")
source('printOrPlotMatch2Teams.R')
source('printOrPlotTeamPerfOverall.R')
source("rankPlayers.R")
shinyServer(function(input, output,session) {


    #############
    # IPL Batsmen
     output$batsmanPlotsIPL <- renderPlot({
       analyzeBatsmen(input$batsmanIPL,input$batsmanFuncIPL, "IPL",input$staticIntv)

     })

     output$batsmanPlotlyIPL <- renderPlotly({
       analyzeBatsmen(input$batsmanIPL,input$batsmanFuncIPL, "IPL",input$staticIntv)

     })


     # Analyze and display batsmen plots
     output$batsmanPlotIPL <- renderUI({
       if(input$staticIntv == 1){
            plotOutput("batsmanPlotsIPL")
       }
       else{
          #Plotly does not support polar coordinates required for dismissals, hence this will be normal ggplot (Kludge!!)
          if(input$batsmanFuncIPL =="Dismissals of batsman" || input$batsmanFuncIPL == "Predict Runs of batsman")
              plotOutput("batsmanPlotsIPL")
          else
              plotlyOutput("batsmanPlotlyIPL")
       }

     })

     ########################
     #IPL bowlers
     output$bowlerPlotsIPL <- renderPlot({
       analyzeBowlers(input$bowlerIPL,input$bowlerFuncIPL,"IPL",input$staticIntv1)
     })

     output$bowlerPlotlyIPL <- renderPlotly({
       analyzeBowlers(input$bowlerIPL,input$bowlerFuncIPL, "IPL",input$staticIntv1)
     })

     output$bowlerPlotIPL <- renderUI({
       if(input$staticIntv1 == 1){
           plotOutput("bowlerPlotsIPL")
       }   else{
           if(input$bowlerFuncIPL == "Bowler's wickets prediction")
                  plotOutput("bowlerPlotsIPL")
           else
                  plotlyOutput("bowlerPlotlyIPL")
       }

     })






    ######################################## IPL Match  #############################################
    # Analyze and display IPL Match plot
    output$IPLMatchPlots <- renderPlot({
        printOrPlotMatch(input, output,"IPL")

    })

    output$IPLMatchPlotly <- renderPlotly({
       printOrPlotMatch(input, output,"IPL")

    })

    # Analyze and display IPL Match table
    output$IPLMatchPrint <- renderTable({
        a <- printOrPlotMatch(input, output,"IPL")
        a

    })
    # Output either a table or a plot
    output$plotOrPrintIPLMatch <-  renderUI({
        # Check if output is a dataframe. If so, print
        if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"IPL"))){
            tableOutput("IPLMatchPrint")
        }
        else{ #Else plot
          if(input$plotOrTable == 1){
            plotOutput("IPLMatchPlots")
          } else{
            plotlyOutput("IPLMatchPlotly")
          }

        }

    })

    #################################### IPL Matches between 2 teams ######################
    # Analyze Head to head confrontation of IPL teams

    # Analyze and display IPL Matches between 2 teams plot
    output$IPLMatch2TeamsPlots <- renderPlot({
        printOrPlotMatch2Teams(input, output)

    })

    output$IPLMatch2TeamsPlotly <- renderPlotly({
      printOrPlotMatch2Teams(input, output)

    })

    # Analyze and display IPL Match table
    output$IPLMatch2TeamsPrint <- renderTable({
        a <- printOrPlotMatch2Teams(input, output)
        a
    })

    # Output either a table or a plot
    output$plotOrPrintIPLMatch2teams <-  renderUI({

      if(input$matches2TeamFunc == "Win Loss Head-to-head All Matches" && input$plotOrTable1 == 3){
           plotlyOutput("IPLMatch2TeamsPlotly")
       }
        # Check if output is a dataframe. If so, print
        else if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output))){
            tableOutput("IPLMatch2TeamsPrint")
        }
        else{ #Else plot
          if(input$plotOrTable1 == 1){
            plotOutput("IPLMatch2TeamsPlots")
          } else if(input$plotOrTable1 == 2){
            plotlyOutput("IPLMatch2TeamsPlotly")
          }
        }

    })



    ################################ IPL Teams's overall performance ##############################
    # Analyze overall IPL team performance plots
    output$IPLTeamPerfOverallPlot <- renderPlot({
        printOrPlotTeamPerfOverall(input, output)

    })

    # Analyze and display IPL Match table
    output$IPLTeamPerfOverallPrint <- renderTable({
        a <- printOrPlotTeamPerfOverall(input, output)
        a

    })
    # Output either a table or a plot
    output$printOrPlotIPLTeamPerfoverall <-  renderUI({
        # Check if output is a dataframe. If so, print
        if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output))){
            tableOutput("IPLTeamPerfOverallPrint")
        }
        else{ #Else plot
            plotOutput("IPLTeamPerfOverallPlot")
        }
    })


    ################################ Rank IPL ##############################
    # Rank IPL Batsmen

    observeEvent(input$yearSelected,{
      updateSliderInput(session, "minMatches", max = helper1(IPLTeamNames,input$yearSelected,"./ipl/iplBattingBowlingDetails")[[4]],value = helper1(IPLTeamNames,input$yearSelected,"./ipl/iplBattingBowlingDetails")[[4]]- 25)
    })

    # Analyze and display IPL Match table
    output$IPLRankBatsmenPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"IPL","batsmen")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankIPLBatsmen <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(a <- rankPlayers(input, output,"IPL","batsmen"))){
        tableOutput("IPLRankBatsmenPrint")

      }
    })

    ########################################
    # Rank IPL Bowlers
    observeEvent(input$yearSelected1,{
      updateSliderInput(session, "minMatches1", max = helper3(IPLTeamNames,input$yearSelected1,"./ipl/iplBattingBowlingDetails")[[4]],value = helper3(IPLTeamNames,input$yearSelected1,"./ipl/iplBattingBowlingDetails")[[4]]- 15)
    })

    # Analyze and display IPL Match table
    output$IPLRankBowlersPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"IPL","bowlers")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankIPLBowlers <-  renderUI({
      # Check if output is a dataframe. If so, print

      if(is.data.frame(a <- rankPlayers(input, output,"IPL","bowlers"))){
        tableOutput("IPLRankBowlersPrint")

      }
    })







##########################################################################################
# T20 Men

    # Analyze and display batsmen plots
    output$batsmanPlotT20M <- renderPlot({
      analyzeBatsmen(input$batsmanT20M,input$batsmanFuncT20M, "T20M")

    })


    # Analyze and display bowler plots
    output$bowlerPlotT20M <- renderPlot({
          analyzeBowlers(input$bowlerT20M,input$bowlerFuncT20M, "T20M")
    })


    ######################################## T20 Men's Match  #############################################
    # Analyze and display T20 Match plot
    output$T20MMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"T20M")

    })

    # Analyze and display T20 Match table
    output$T20MMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"T20M")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintT20MMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"T20M"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("T20MMatchPrint")
      }
      else{ #Else plot
        plotOutput("T20MMatchPlot")
      }

    })

    #################################### T20 Men's Matches between 2 teams ######################
    # Analyze Head to head confrontation of T20 Mens teams

    # Analyze and display T20 Men Matches between 2 teams plot
    output$T20MMatch2TeamsPlot <- renderPlot({
      print("plot")
      printOrPlotMatch2Teams(input, output,"T20M")

    })

    # Analyze and display IPL Match table
    output$T20MMatch2TeamsPrint <- renderTable({
      print("table")
      a <- printOrPlotMatch2Teams(input, output,"T20M")
      a
      #a
    })

    # Output either a table or a plot
    output$plotOrPrintT20MMatch2teams <-  renderUI({
      print("Server ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"T20M"))){
        tableOutput("T20MMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("T20MMatch2TeamsPlot")
      }

    })



    ################################ T20 Men's Teams's overall performance ##############################
    # Analyze overall T20 Mens team performance plots
    output$T20MTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"T20M")

    })

    # Analyze and display IPL Match table
    output$T20MTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"T20M")
      a

    })
    # Output either a table or a plot
    output$printOrPlotT20MTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"T20M"))){
        tableOutput("T20MTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("T20MTeamPerfOverallPlot")
      }
    })


    ################################ Rank T20 Men ##############################
    # Rank T20M batsmen performance


    observeEvent(input$yearSelectedT20M,{
      updateSliderInput(session, "minMatchesT20M", max = helper1(T20MTeamNames,input$yearSelectedT20M,"./t20/t20BattingBowlingDetails")[[4]],value = helper1(T20MTeamNames,input$yearSelectedT20M,"./t20/t20BattingBowlingDetails")[[4]]- 8)
    })

    # Analyze and display T20M Match table
    output$T20MRankBatsmenPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"T20M","batsmen")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankT20MBatsmen <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(a <- rankPlayers(input, output,"T20M","batsmen"))){
        tableOutput("T20MRankBatsmenPrint")

      }
    })

    ########################################
    # Rank T20M Bowlers
    observeEvent(input$yearSelected1T20M,{
      updateSliderInput(session, "minMatches1T20M", max = helper3(T20MTeamNames,input$yearSelected1T20M,"./t20/t20BattingBowlingDetails")[[4]],value = helper3(T20MTeamNames,input$yearSelected1T20M,"./t20/t20BattingBowlingDetails")[[4]]- 12)
    })

    # Analyze and display T20M Match table
    output$T20MRankBowlersPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"T20M","bowlers")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankT20MBowlers <-  renderUI({
      # Check if output is a dataframe. If so, print

      if(is.data.frame(a <- rankPlayers(input, output,"T20M","bowlers"))){
        tableOutput("T20MRankBowlersPrint")

      }
    })

    ##########################################################################################
    # T20 Women

    # Analyze and display batsmen plots
    output$batsmanPlotT20W <- renderPlot({
      analyzeBatsmen(input$batsmanT20W,input$batsmanFuncT20W, "T20W")

    })


    # Analyze and display bowler plots
    output$bowlerPlotT20W <- renderPlot({
      analyzeBowlers(input$bowlerT20W,input$bowlerFuncT20W, "T20W")

    })

    ######################################## T20 Women's Match  #############################################
    # Analyze and display T20 Match plot
    output$T20WMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"T20W")

    })

    # Analyze and display T20 Match table
    output$T20WMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"T20W")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintT20WMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"T20W"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("T20WMatchPrint")
      }
      else{ #Else plot
        plotOutput("T20WMatchPlot")
      }

    })

    #################################### T20 Women's Matches between 2 teams ######################
    # Analyze Head to head confrontation of T20 Womens teams

    # Analyze and display T20 Women Matches between 2 teams plot
    output$T20WMatch2TeamsPlot <- renderPlot({
      print("Women plot")
      printOrPlotMatch2Teams(input, output,"T20W")

    })

    # Analyze and display IPL Match table
    output$T20WMatch2TeamsPrint <- renderTable({
      print("Women table")
      a <- printOrPlotMatch2Teams(input, output,"T20W")
      a
    })

    # Output either a table or a plot
    output$plotOrPrintT20WMatch2teams <-  renderUI({
      print("Women's match ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"T20W"))){
        tableOutput("T20WMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("T20WMatch2TeamsPlot")
      }

    })

    ################################ T20 Women's Teams's overall performance ##############################
    # Analyze overall T20 Womens team performance plots
    output$T20WTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"T20W")

    })

    # Analyze and display T20 Men table
    output$T20WTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"T20W")
      a

    })
    # Output either a table or a plot
    output$printOrPlotT20WTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"T20W"))){
        tableOutput("T20WTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("T20WTeamPerfOverallPlot")
      }
    })


    ################################ Rank T20 Women ##############################
    # Rank T20 WoMen performance


    observeEvent(input$yearSelectedT20W,{
      updateSliderInput(session, "minMatchesT20W", max = helper1(T20WTeamNames,input$yearSelectedT20W,"./t20/t20WomenBattingBowlingDetails")[[4]],value = helper1(T20WTeamNames,input$yearSelectedT20W,"./t20/t20WomenBattingBowlingDetails")[[4]]- 5)
    })

    # Analyze and display T20W Match table
    output$T20WRankBatsmenPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"T20W","batsmen")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankT20WBatsmen <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(a <- rankPlayers(input, output,"T20W","batsmen"))){
        tableOutput("T20WRankBatsmenPrint")

      }
    })

    ########################################
    # Rank T20 Women Bowlers
    observeEvent(input$yearSelected1T20W,{
      updateSliderInput(session, "minMatches1T20W", max = helper3(T20WTeamNames,input$yearSelected1T20W,"./t20/t20WomenBattingBowlingDetails")[[4]],value = helper3(T20WTeamNames,input$yearSelected1T20W,"./t20/t20WomenBattingBowlingDetails")[[4]]- 5)
    })

    # Analyze and display T20W Match table
    output$T20WRankBowlersPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"T20W","bowlers")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankT20WBowlers <-  renderUI({
      # Check if output is a dataframe. If so, print

      if(is.data.frame(a <- rankPlayers(input, output,"T20W","bowlers"))){
        tableOutput("T20WRankBowlersPrint")

      }
    })

    ##########################################################################################
    # Big Bash League

    # Analyze and display batsmen plots
    output$batsmanPlotBBL <- renderPlot({
      analyzeBatsmen(input$batsmanBBL,input$batsmanFuncBBL, "BBL")

    })

    # Analyze and display bowler plots
    output$bowlerPlotBBL <- renderPlot({
      analyzeBowlers(input$bowlerBBL,input$bowlerFuncBBL, "BBL")

    })

    ########################################  BBL T20 Match  #############################################
    # Analyze and display T20 Match plot
    output$BBLMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"BBL")

    })

    # Analyze and display T20 Match table
    output$BBLMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"BBL")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintBBLMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"BBL"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("BBLMatchPrint")
      }
      else{ #Else plot
        plotOutput("BBLMatchPlot")
      }

    })

    #################################### BBL  Matches between 2 teams ######################
    # Analyze Head to head confrontation of BBL T20  teams

    # Analyze and display BBL T20  Matches between 2 teams plot
    output$BBLMatch2TeamsPlot <- renderPlot({
      print("Women plot")
      printOrPlotMatch2Teams(input, output,"BBL")

    })

    # Analyze and display BBL Match table
    output$BBLMatch2TeamsPrint <- renderTable({
      print("Women table")
      a <- printOrPlotMatch2Teams(input, output,"BBL")
      a
    })

    # Output either a table or a plot
    output$plotOrPrintBBLMatch2teams <-  renderUI({
      print("Women's match ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"BBL"))){
        tableOutput("BBLMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("BBLMatch2TeamsPlot")
      }

    })

    ################################ BBL T20  Teams's overall performance ##############################
    # Analyze overall BBL  team performance plots
    output$BBLTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"BBL")

    })

    # Analyze and display IPL Match table
    output$BBLTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"BBL")
      a

    })
    # Output either a table or a plot
    output$printOrPlotBBLTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"BBL"))){
        tableOutput("BBLTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("BBLTeamPerfOverallPlot")
      }

   })


    ################################ Rank BBL ##############################


    # Display ranks
    observeEvent(input$yearSelectedBBL,{
      updateSliderInput(session, "minMatchesBBL", max = helper1(BBLTeamNames,input$yearSelectedBBL,"./bbl/bblBattingBowlingDetails")[[4]],value = helper1(BBLTeamNames,input$yearSelectedBBL,"./bbl/bblBattingBowlingDetails")[[4]]- 8)
    })

    # Analyze and display BBL Match table
    output$BBLRankBatsmenPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"BBL","batsmen")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankBBLBatsmen <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(a <- rankPlayers(input, output,"BBL","batsmen"))){
        tableOutput("BBLRankBatsmenPrint")

      }
    })

    ########################################
    # Rank BBL Bowlers
    observeEvent(input$yearSelected1BBL,{
      updateSliderInput(session, "minMatches1BBL", max = helper3(BBLTeamNames,input$yearSelected1BBL,"./bbl/bblBattingBowlingDetails")[[4]],value = helper3(BBLTeamNames,input$yearSelected1BBL,"./bbl/bblBattingBowlingDetails")[[4]]- 8)
    })

    # Analyze and display BBL Match table
    output$BBLRankBowlersPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"BBL","bowlers")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankBBLBowlers <-  renderUI({
      # Check if output is a dataframe. If so, print

      if(is.data.frame(a <- rankPlayers(input, output,"BBL","bowlers"))){
        tableOutput("BBLRankBowlersPrint")

      }
    })
    ##########################################################################################
    # Natwest T20

    # Analyze and display batsmen plots
    output$batsmanPlotNTB <- renderPlot({
      analyzeBatsmen(input$batsmanNTB,input$batsmanFuncNTB, "NTB")
    })

    # Analyze and display bowler plots
    output$bowlerPlotNTB <- renderPlot({
      analyzeBowlers(input$bowlerNTB,input$bowlerFuncNTB, "NTB")
    })

    ########################################  NTB T20 Match  #############################################
    # Analyze and display T20 Match plot
    output$NTBMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"NTB")

    })

    # Analyze and display T20 Match table
    output$NTBMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"NTB")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintNTBMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"NTB"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("NTBMatchPrint")
      }
      else{ #Else plot
        plotOutput("NTBMatchPlot")
      }

    })

    #################################### NTB  Matches between 2 teams ######################
    # Analyze Head to head confrontation of NTB T20  teams

    # Analyze and display NTB T20  Matches between 2 teams plot
    output$NTBMatch2TeamsPlot <- renderPlot({
      print("Women plot")
      printOrPlotMatch2Teams(input, output,"NTB")

    })

    # Analyze and display NTB Match table
    output$NTBMatch2TeamsPrint <- renderTable({
      print("Women table")
      a <- printOrPlotMatch2Teams(input, output,"NTB")
      a
    })

    # Output either a table or a plot
    output$plotOrPrintNTBMatch2teams <-  renderUI({
      print("Women's match ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"NTB"))){
        tableOutput("NTBMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("NTBMatch2TeamsPlot")
      }

    })

    ################################ NTB T20  Teams's overall performance ##############################
    # Analyze overall NTB  team performance plots
    output$NTBTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"NTB")

    })

    # Analyze and display NTB Match table
    output$NTBTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"NTB")
      a

    })
    # Output either a table or a plot
    output$printOrPlotNTBTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"NTB"))){
        tableOutput("NTBTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("NTBTeamPerfOverallPlot")
      }

    })

    ################################ Rank NTB ##############################
    # Analyze overall NTB team performance plots


    # Display ranks
    observeEvent(input$yearSelectedNTB,{
      updateSliderInput(session, "minMatchesNTB", max = helper1(NTBTeamNames,input$yearSelectedNTB,"./ntb/ntbBattingBowlingDetails")[[4]],value = helper1(NTBTeamNames,input$yearSelectedNTB,"./ntb/ntbBattingBowlingDetails")[[4]]- 3)
    })

    # Analyze and display NTB Match table
    output$NTBRankBatsmenPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"NTB","batsmen")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankNTBBatsmen <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(a <- rankPlayers(input, output,"NTB","batsmen"))){
        tableOutput("NTBRankBatsmenPrint")

      }
    })

    ########################################
    # Rank NTB Bowlers
    observeEvent(input$yearSelected1NTB,{
      updateSliderInput(session, "minMatches1NTB", max = helper3(NTBTeamNames,input$yearSelected1NTB,"./ntb/ntbBattingBowlingDetails")[[4]],value = helper3(NTBTeamNames,input$yearSelected1NTB,"./ntb/ntbBattingBowlingDetails")[[4]]- 3)
    })

    # Analyze and display NTB Match table
    output$NTBRankBowlersPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"NTB","bowlers")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankNTBBowlers <-  renderUI({
      # Check if output is a dataframe. If so, print

      if(is.data.frame(a <- rankPlayers(input, output,"NTB","bowlers"))){
        tableOutput("NTBRankBowlersPrint")

      }
    })

    ##########################################################################################
    # PSL T20

    # Analyze and display batsmen plots
    output$batsmanPlotPSL <- renderPlot({
      analyzeBatsmen(input$batsmanPSL,input$batsmanFuncPSL, "PSL")

    })

    # Analyze and display bowler plots
    output$bowlerPlotPSL <- renderPlot({
      analyzeBowlers(input$bowlerPSL,input$bowlerFuncPSL, "PSL")
    })

    ########################################  PSL T20 Match  #############################################
    # Analyze and display T20 Match plot
    output$PSLMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"PSL")

    })

    # Analyze and display T20 Match table
    output$PSLMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"PSL")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintPSLMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"PSL"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("PSLMatchPrint")
      }
      else{ #Else plot
        plotOutput("PSLMatchPlot")
      }

    })

    ################################ Rank PSL ##############################
    # Analyze overall PSL team performance plots



    # Display ranks
    observeEvent(input$yearSelectedPSL,{
      updateSliderInput(session, "minMatchesPSL", max = helper1(PSLTeamNames,input$yearSelectedPSL,"./psl/pslBattingBowlingDetails")[[4]],value = helper1(PSLTeamNames,input$yearSelectedPSL,"./psl/pslBattingBowlingDetails")[[4]]- 8)
    })

    # Analyze and display PSL Match table
    output$PSLRankBatsmenPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"PSL","batsmen")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankPSLBatsmen <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(a <- rankPlayers(input, output,"PSL","batsmen"))){
        tableOutput("PSLRankBatsmenPrint")

      }
    })

    ########################################
    # Rank PSL Bowlers
    observeEvent(input$yearSelected1PSL,{
      updateSliderInput(session, "minMatches1PSL", max = helper3(PSLTeamNames,input$yearSelected1PSL,"./psl/pslBattingBowlingDetails")[[4]],value = helper3(PSLTeamNames,input$yearSelected1PSL,"./psl/pslBattingBowlingDetails")[[4]]- 8)
    })

    # Analyze and display PSL Match table
    output$PSLRankBowlersPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"PSL","bowlers")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankPSLBowlers <-  renderUI({
      # Check if output is a dataframe. If so, print

      if(is.data.frame(a <- rankPlayers(input, output,"PSL","bowlers"))){
        tableOutput("PSLRankBowlersPrint")

      }
    })

    #################################### PSL  Matches between 2 teams ######################
    # Analyze Head to head confrontation of PSL T20  teams

    # Analyze and display PSL T20  Matches between 2 teams plot
    output$PSLMatch2TeamsPlot <- renderPlot({
      print("Women plot")
      printOrPlotMatch2Teams(input, output,"PSL")

    })

    # Analyze and display PSL Match table
    output$PSLMatch2TeamsPrint <- renderTable({
      print("Women table")
      a <- printOrPlotMatch2Teams(input, output,"PSL")
      a
    })

    # Output either a table or a plot
    output$plotOrPrintPSLMatch2teams <-  renderUI({
      print("Women's match ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"PSL"))){
        tableOutput("PSLMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("PSLMatch2TeamsPlot")
      }

    })

    ################################ PSL T20  Teams's overall performance ##############################
    # Analyze overall PSL  team performance plots
    output$PSLTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"PSL")

    })

    # Analyze and display PSL Match table
    output$PSLTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"PSL")
      a

    })
    # Output either a table or a plot
    output$printOrPlotPSLTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"PSL"))){
        tableOutput("PSLTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("PSLTeamPerfOverallPlot")
      }

    })

    ##########################################################################################
    # WBBL T20

    # Analyze and display batsmen plots
    output$batsmanPlotWBB <- renderPlot({
      analyzeBatsmen(input$batsmanWBB,input$batsmanFuncWBB, "WBB")

    })

    # Analyze and display bowler plots
    output$bowlerPlotWBB <- renderPlot({
      analyzeBowlers(input$bowlerWBB,input$bowlerFuncWBB, "WBB")
    })

    ########################################  WBBL T20 Match  #############################################
    # Analyze and display T20 Match plot
    output$WBBMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"WBB")

    })

    # Analyze and display T20 Match table
    output$WBBMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"WBB")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintWBBMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"WBB"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("WBBMatchPrint")
      }
      else{ #Else plot
        plotOutput("WBBMatchPlot")
      }

    })

    #################################### WBB  Matches between 2 teams ######################
    # Analyze Head to head confrontation of WBB T20  teams

    # Analyze and display WBB T20  Matches between 2 teams plot
    output$WBBMatch2TeamsPlot <- renderPlot({
      print("Women plot")
      printOrPlotMatch2Teams(input, output,"WBB")

    })

    # Analyze and display WBB Match table
    output$WBBMatch2TeamsPrint <- renderTable({
      print("Women table")
      a <- printOrPlotMatch2Teams(input, output,"WBB")
      a
    })

    # Output either a table or a plot
    output$plotOrPrintWBBMatch2teams <-  renderUI({
      print("Women's match ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"WBB"))){
        tableOutput("WBBMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("WBBMatch2TeamsPlot")
      }

    })

    ################################ WBB T20  Teams's overall performance ##############################
    # Analyze overall WBB  team performance plots
    output$WBBTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"WBB")

    })

    # Analyze and display WBB Match table
    output$WBBTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"WBB")
      a

    })
    # Output either a table or a plot
    output$printOrPlotWBBTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"WBB"))){
        tableOutput("WBBTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("WBBTeamPerfOverallPlot")
      }

    })

    ################################ Rank WBB ##############################
    # Analyze overall WBB team performance plots

    # Display ranks
    observeEvent(input$yearSelectedWBB,{
      updateSliderInput(session, "minMatchesWBB", max = helper1(WBBTeamNames,input$yearSelectedWBB,"./wbb/wbbBattingBowlingDetails")[[4]],value = helper1(WBBTeamNames,input$yearSelectedWBB,"./wbb/wbbBattingBowlingDetails")[[4]]- 5)
    })

    # Analyze and display WBB Match table
    output$WBBRankBatsmenPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"WBB","batsmen")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankWBBBatsmen <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(a <- rankPlayers(input, output,"WBB","batsmen"))){
        tableOutput("WBBRankBatsmenPrint")

      }
    })
    observeEvent(input$yearSelected1WBB,{
      updateSliderInput(session, "minMatches1WBB", max = helper3(WBBTeamNames,input$yearSelected1WBB,"./wbb/wbbBattingBowlingDetails")[[4]],value = helper3(WBBTeamNames,input$yearSelected1WBB,"./wbb/wbbBattingBowlingDetails")[[4]]- 5)
    })

    # Analyze and display WBB Match table
    output$WBBRankBowlersPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"WBB","bowlers")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankWBBBowlers <-  renderUI({
      # Check if output is a dataframe. If so, print

      if(is.data.frame(a <- rankPlayers(input, output,"WBB","bowlers"))){
        tableOutput("WBBRankBowlersPrint")

      }
    })

    ##########################################################################################

    ##########################################################################################
    # ODI Men

    # Analyze and display batsmen plots
    output$batsmanPlotODIM <- renderPlot({
      analyzeBatsmen(input$batsmanODIM,input$batsmanFuncODIM, "ODIM")

    })

    # Analyze and display bowler plots
    output$bowlerPlotODIM <- renderPlot({
      analyzeBowlers(input$bowlerODIM,input$bowlerFuncODIM, "ODIM")
    })

    ########################################  ODI Men Match  #############################################
    # Analyze and display ODI Men Match plot
    output$ODIMMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"ODIM")

    })

    # Analyze and display ODI Men Match table
    output$ODIMMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"ODIM")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintODIMMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"ODIM"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("ODIMMatchPrint")
      }
      else{ #Else plot
        plotOutput("ODIMMatchPlot")
      }

    })

    # Analyze Head to head confrontation of ODI Men  teams

    # Analyze and display ODI Men  Matches between 2 teams plot
    output$ODIMMatch2TeamsPlot <- renderPlot({
      print("Women plot")
      printOrPlotMatch2Teams(input, output,"ODIM")

    })

    # Analyze and display ODIM Match table
    output$ODIMMatch2TeamsPrint <- renderTable({
      print("Women table")
      a <- printOrPlotMatch2Teams(input, output,"ODIM")
      a
    })

    # Output either a table or a plot
    output$plotOrPrintODIMMatch2teams <-  renderUI({
      print("Women's match ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"ODIM"))){
        tableOutput("ODIMMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("ODIMMatch2TeamsPlot")
      }

    })

    ################################ ODI Men  Teams's overall performance ##############################
    # Analyze overall ODIM  team performance plots
    output$ODIMTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"ODIM")

    })

    # Analyze and display ODIM Match table
    output$ODIMTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"ODIM")
      a

    })
    # Output either a table or a plot
    output$printOrPlotODIMTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"ODIM"))){
        tableOutput("ODIMTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("ODIMTeamPerfOverallPlot")
      }

    })


    ##########################################################################################
    # ODI Women

    # Analyze and display batsmen plots
    output$batsmanPlotODIW <- renderPlot({
      analyzeBatsmen(input$batsmanODIW,input$batsmanFuncODIW, "ODIW")

    })

    # Analyze and display bowler plots
    output$bowlerPlotODIW <- renderPlot({
      analyzeBowlers(input$bowlerODIW,input$bowlerFuncODIW, "ODIW")
    })

    ########################################  ODI Women Match  #############################################
    # Analyze and display ODI Women Match plot
    output$ODIWMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"ODIW")

    })

    # Analyze and display ODI Women Match table
    output$ODIWMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"ODIW")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintODIWMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"ODIW"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("ODIWMatchPrint")
      }
      else{ #Else plot
        plotOutput("ODIWMatchPlot")
      }

    })


    ##########################################################################################

    # Analyze Head to head confrontation of ODI Women  teams

    # Analyze and display ODI Women  Matches between 2 teams plot
    output$ODIWMatch2TeamsPlot <- renderPlot({
      print("Women plot")
      printOrPlotMatch2Teams(input, output,"ODIW")

    })

    # Analyze and display ODIW Match table
    output$ODIWMatch2TeamsPrint <- renderTable({
      print("Women table")
      a <- printOrPlotMatch2Teams(input, output,"ODIW")
      a
    })

    # Output either a table or a plot
    output$plotOrPrintODIWMatch2teams <-  renderUI({
      print("Women's match ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"ODIW"))){
        tableOutput("ODIWMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("ODIWMatch2TeamsPlot")
      }

    })

    ################################ ODI Women  Teams's overall performance ##############################
    # Analyze overall ODI Women  team performance plots
    output$ODIWTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"ODIW")

    })

    # Analyze and display ODIW Match table
    output$ODIWTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"ODIW")
      a

    })
    # Output either a table or a plot
    output$printOrPlotODIWTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"ODIW"))){
        tableOutput("ODIWTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("ODIWTeamPerfOverallPlot")
      }

    })


    ############################################################Caribbean Premier League ##############################


    # Analyze and display batsmen plots
    output$batsmanPlotCPL <- renderPlot({
      analyzeBatsmen(input$batsmanCPL,input$batsmanFuncCPL, "CPL")

    })

    # Analyze and display bowler plots
    output$bowlerPlotCPL <- renderPlot({
      analyzeBowlers(input$bowlerCPL,input$bowlerFuncCPL, "CPL")

    })

    ########################################  CPL T20 Match  #############################################
    # Analyze and display T20 Match plot
    output$CPLMatchPlot <- renderPlot({
      print("t20 plot")
      printOrPlotMatch(input, output,"CPL")

    })

    # Analyze and display T20 Match table
    output$CPLMatchPrint <- renderTable({
      print("t20 print")
      a <- printOrPlotMatch(input, output,"CPL")
      head(a)
      a

    })
    # Output either a table or a plot
    output$plotOrPrintCPLMatch <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch(input, output,"CPL"))){
        print("Hello&&&&&&&&&&&&&&&")
        tableOutput("CPLMatchPrint")
      }
      else{ #Else plot
        plotOutput("CPLMatchPlot")
      }

    })

    #################################### CPL  Matches between 2 teams ######################
    # Analyze Head to head confrontation of CPL T20  teams

    # Analyze and display CPL T20  Matches between 2 teams plot
    output$CPLMatch2TeamsPlot <- renderPlot({
      print("Women plot")
      printOrPlotMatch2Teams(input, output,"CPL")

    })

    # Analyze and display CPL Match table
    output$CPLMatch2TeamsPrint <- renderTable({
      print("Women table")
      a <- printOrPlotMatch2Teams(input, output,"CPL")
      a
    })

    # Output either a table or a plot
    output$plotOrPrintCPLMatch2teams <-  renderUI({
      print("Women's match ")
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotMatch2Teams(input, output,"CPL"))){
        tableOutput("CPLMatch2TeamsPrint")
      }
      else{ #Else plot
        plotOutput("CPLMatch2TeamsPlot")
      }

    })

    ################################ CPL T20  Teams's overall performance ##############################
    # Analyze overall CPL  team performance plots
    output$CPLTeamPerfOverallPlot <- renderPlot({
      printOrPlotTeamPerfOverall(input, output,"CPL")

    })

    # Analyze and display IPL Match table
    output$CPLTeamPerfOverallPrint <- renderTable({
      a <- printOrPlotTeamPerfOverall(input, output,"CPL")
      a

    })
    # Output either a table or a plot
    output$printOrPlotCPLTeamPerfoverall <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(scorecard <- printOrPlotTeamPerfOverall(input, output,"CPL"))){
        tableOutput("CPLTeamPerfOverallPrint")
      }
      else{ #Else plot
        plotOutput("CPLTeamPerfOverallPlot")
      }

    })


    ################################ Rank CPL ##############################


    # Display ranks
    observeEvent(input$yearSelectedCPL,{
      updateSliderInput(session, "minMatchesCPL", max = helper1(CPLTeamNames,input$yearSelectedCPL,"./cpl/cplBattingBowlingDetails")[[4]],value = helper1(CPLTeamNames,input$yearSelectedCPL,"./cpl/cplBattingBowlingDetails")[[4]]- 8)
    })

    # Analyze and display CPL Match table
    output$CPLRankBatsmenPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"CPL","batsmen")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankCPLBatsmen <-  renderUI({
      # Check if output is a dataframe. If so, print
      if(is.data.frame(a <- rankPlayers(input, output,"CPL","batsmen"))){
        tableOutput("CPLRankBatsmenPrint")

      }
    })

    ########################################
    # Rank CPL Bowlers
    observeEvent(input$yearSelected1CPL,{
      updateSliderInput(session, "minMatches1CPL", max = helper3(CPLTeamNames,input$yearSelected1CPL,"./cpl/cplBattingBowlingDetails")[[4]],value = helper3(CPLTeamNames,input$yearSelected1CPL,"./cpl/cplBattingBowlingDetails")[[4]]- 8)
    })

    # Analyze and display CPL Match table
    output$CPLRankBowlersPrint <- renderTable({
      Sys.sleep(1.5)
      plot(runif(10))
      a <- rankPlayers(input, output,"CPL","bowlers")
      head(a,20)
    })

    # Output either a table or a plot
    output$rankCPLBowlers <-  renderUI({
      # Check if output is a dataframe. If so, print

      if(is.data.frame(a <- rankPlayers(input, output,"CPL","bowlers"))){
        tableOutput("CPLRankBowlersPrint")

      }
    })


})




