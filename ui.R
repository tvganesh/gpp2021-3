#########################################################################################################
#
# Title :  GooglyPlusPLus2021 - An interactive app to analyze T20
# Designed and developed by: Tinniam V Ganesh
# Date : 06 Feb 2021
# File: ui.R
# More details: https://gigadom.in/
#
#########################################################################################################
source("data.R")
source("definitions.R")
library(shinybusy)
library(lubridate)
library(shinythemes)
library(yorkr)
library(ggthemes)
library(plotly)



shinyUI(fluidPage(theme = shinytheme("readable"),

  ## Title and headings
  titlePanel("GooglyPlusPlus 2021-IPL 2021!!!"),
  h5("GooglyPlusPlus"),

  ## Beginning of navlistPanel
  navlistPanel(
    widths = c(2, 10),
    "Match formats",
    tabPanel("IPL T20", navbarPage("GooglyPlusPlus - Indian Premier League (IPL)",
                                 # Batsman tab
                               tabsetPanel(id = "tabs",
                                 tabPanel("IPL batsman", value = "IPL",
                                          h4('Analyze IPL batsman performances'),
                                          sidebarPanel(
                                            selectInput('batsmanFuncIPL', 'Select function', batsmanFuncs),
                                            selectInput('batsmanIPL', 'Select batsman', IPLBatsmen,selectize=FALSE, size=20),
                                            radioButtons("staticIntv", label = h4("Plot type"),
                                                         choices = c("static" = 1, "interactive" = 2),
                                                         selected = 1,inline=T)
                                          ),
                                          mainPanel(
                                                  uiOutput('batsmanPlotIPL'),

                                                   column(12, offset=6,
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                   tags$h5((tags$i("Feb 6, 2021"))),
                                                   tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                   tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                            )
                                          )

                                 ),
                                 # Bowlers tab
                                 tabPanel("IPL bowlers",

                                          h4('Analyze IPL bowler performances'),

                                          sidebarPanel(
                                            selectInput('bowlerFuncIPL', 'Select function', bowlerFuncs),
                                            selectInput('bowlerIPL', 'Select IPL bowler', IPLBowlers,selectize=FALSE, size=20),
                                            radioButtons("staticIntv1", label = h4("Plot type"),
                                                         choices = c("static" = 1, "interactive" = 2),
                                                         selected = 1,inline=T)


                                          ),
                                          mainPanel(
                                                   uiOutput('bowlerPlotIPL'),
                                                   column(12, offset=6,
                                                          br(),
                                                          br(),
                                                          br(),
                                                          br(),
                                                          br(),
                                                   tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                   tags$h5((tags$i("Feb 6, 2021"))),
                                                   tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                   tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                            )
                                          )

                                 ),
                                 # Analyze IPL matches
                                 tabPanel("IPL Match",
                                          h4('Analyze an IPL match'),
                                          sidebarPanel(
                                            selectInput('matchFunc', 'Select match function', matchFuncs),
                                            selectInput('match', 'Select IPL match ', IPLMatches,selectize=FALSE, size=15),
                                            uiOutput("selectTeam"),
                                            radioButtons("plotOrTable", label = h4("Plot(static,interactive) or table"),
                                                         choices = c("Plot(static)" = 1, "Plot(interactive)" = 2, "Table" = 3),
                                                         selected = 1,inline=T)

                                          ),
                                          mainPanel(
                                            uiOutput("plotOrPrintIPLMatch"),
                                            column(7, offset=4,
                                                   tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                   tags$h5((tags$i("Feb 6, 2021"))),
                                                   tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                   tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                            )
                                          )

                                 ),
                                 # Analyze 2 Teams IPL matches
                                 tabPanel("Head to head",
                                          h4('Head-to-head between 2 IPL teams'),
                                          sidebarPanel(
                                            selectInput('matches2TeamFunc', 'Select function', matches2TeamsFuncs),
                                            selectInput('match2', 'Select matches', IPLMatches2Teams,selectize=FALSE, size=13),
                                            uiOutput("selectTeam2"),
                                            radioButtons("plotOrTable1", label = h4("Plot(static,interactive) or table"),
                                                         choices = c("Plot(static)" = 1, "Plot(interactive)" = 2, "Table" = 3),
                                                         selected = 1,inline=T),
                                            radioButtons("repType", label = h4("Report Type"),
                                                         choices = c("Summary" = 1, "Detailed" = 2),
                                                         selected = 1,inline=T)

                                          ),
                                          mainPanel(
                                            uiOutput("plotOrPrintIPLMatch2teams"),
                                            column(12, offset=6,
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                   tags$h5((tags$i("Feb 6, 2021"))),
                                                   tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                   tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                            )
                                          )

                                 ),
                                 # Analyze IPL Team Overall Perf
                                 tabPanel("Overall Performance",
                                          h4("Analyze IPL team's overall performance"),
                                          sidebarPanel(
                                            selectInput('overallperfFunc', 'Select function', teamOverallPerfFunc),
                                            selectInput('teamMatches', 'Select the team', IPLTeamsAll,selectize=FALSE, size=13),
                                            uiOutput("Rank"),
                                            radioButtons("plotOrTable2", label = h4("Plot or table"),
                                                         choices = c("Plot(static)" = 1, "Plot(interactive)" = 2, "Table" = 3),
                                                         selected = 1,inline=T),
                                            radioButtons("repType2", label = h4("Report Type"),
                                                         choices = c("Summary" = 1, "Detailed" = 2),
                                                         selected = 1,inline=T)
                                          ),
                                          mainPanel(
                                            uiOutput('printOrPlotIPLTeamPerfoverall'),
                                            column(12, offset=6,
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),                                                 tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                   tags$h5((tags$i("Feb 6, 2021"))),
                                                   tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                   tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                            )
                                          )

                                        ),
                                 # Rank IPL Batsmen tab
                                 tabPanel("Rank IPL Batsmen",

                                          h4('Rank IPL Batsmen'),

                                          sidebarPanel(

                                            tags$head( tags$style( type = "text/css", '
                                              .js-irs-0 .irs-line-mid{
                                                background: #428bca ;
                                                border: 1px solid #428bca ;
                                              }
                                              .js-irs-0 .irs-line-right{
                                                background: #428bca ;
                                              }
                                              .js-irs-0 .irs-bar {
                                                background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                border-top: 1px solid #CCC ;
                                                border-bottom: 1px solid #CCC ;
                                              }
                                              .js-irs-0 .irs-bar-edge {
                                                background: inherit ;
                                                border: inherit ;
                                              }

                                            ')),
                                            sliderInput("yearSelected", "Since year",min = (helper(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[1]])-1, max = (helper(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[2]]), value = (helper(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[1]])),
                                            sliderInput("minMatches", "Matches played",min = (helper(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[3]]), max = (helper(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[4]]), value = 0),
                                            uiOutput("Mode")

                                          ),
                                          mainPanel(
                                            shinycssloaders::withSpinner(
                                              uiOutput('rankIPLBatsmen'),
                                            ),

                                            column(7, offset=4,
                                                   tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                   tags$h5((tags$i("Feb 6, 2021"))),
                                                   tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                   tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                            )
                                          )
                                 ),
                                 # Rank IPL Bowlers tab
                                 tabPanel("Rank IPL Bowlers",

                                          h4('Rank IPL Bowlers'),

                                          sidebarPanel(


                                            tags$head( tags$style( type = "text/css", '
                                              .js-irs-2 .irs-line-mid{
                                                background: #428bca ;
                                                border: 1px solid #428bca ;
                                              }
                                              .js-irs-2 .irs-line-right{
                                                background: #428bca ;
                                              }
                                              .js-irs-2 .irs-bar {
                                                background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                border-top: 1px solid #CCC ;
                                                border-bottom: 1px solid #CCC ;
                                              }
                                              .js-irs-2 .irs-bar-edge {
                                                background: inherit ;
                                                border: inherit ;
                                              }
                                            ')),

                                            sliderInput("yearSelected1", "Since year",min = (helper2(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[1]])-1, max = (helper2(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[2]]), value = (helper2(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[1]])),
                                            sliderInput("minMatches1", "Matches played",min = (helper2(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[3]]), max = (helper2(IPLTeamNames,"./ipl/iplBattingBowlingDetails")[[4]]), value = 0),
                                            #selectInput('runsOverSR', 'Mode', runsVsSR),
                                            uiOutput("Mode1")

                                          ),
                                          mainPanel(
                                            shinycssloaders::withSpinner(
                                              uiOutput('rankIPLBowlers'),
                                            ),

                                            column(7, offset=4,
                                                   tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                   tags$h5((tags$i("Feb 6, 2021"))),
                                                   tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                   tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                            )
                                          )
                                 )


                                 ))),

    ############################# International T20 (Men) ################################
    tabPanel("Intl. T20 (men)",navbarPage("GooglyPlusPlus - International T20 (men)",
                                         # Batsman tab
                                         tabPanel("Intl T20 batsman",
                                                  h4('Analyze Intl T20 batsman performances'),
                                                  sidebarPanel(
                                                    selectInput('batsmanFuncT20M', 'Select function', batsmanFuncs),
                                                    selectInput('batsmanT20M', 'Select batsman', T20MBatsmen,selectize=FALSE, size=20),
                                                  ),
                                                  mainPanel(
                                                    plotOutput('batsmanPlotT20M'),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                         ),
                                         # Bowlers tab
                                         tabPanel("Intl. T20 bowlers",

                                                  h4('Analyze Intl. T20 bowler performances'),

                                                  sidebarPanel(
                                                    selectInput('bowlerFuncT20M', 'Select function', bowlerFuncs),
                                                    selectInput('bowlerT20M', 'Select bowler', T20MBowlers,selectize=FALSE, size=20)


                                                  ),
                                                  mainPanel(
                                                    plotOutput('bowlerPlotT20M'),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )

                                              ),
                                         tabPanel("Intl T20 Match",
                                                  h4('Analyze an Intl T20 match'),
                                                  sidebarPanel(
                                                    selectInput('matchFuncT20M', 'Select match function', matchFuncs),
                                                    selectInput('matchT20M', 'Select T20 match ', T20MMatches,selectize=FALSE, size=15),
                                                    uiOutput("selectTeamT20M"),
                                                    radioButtons("plotOrTableT20M", label = h4("Plot or table"),
                                                                 choices = c("Plot" = 1, "Table" = 2),
                                                                 selected = 1,inline=T)

                                                  ),
                                                  mainPanel(
                                                    uiOutput("plotOrPrintT20MMatch"),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                            ),

                                         # Analyze 2 T20 Teams men's  matches
                                         tabPanel("Head to head",
                                                  h4('Head-to-head between 2 T20 (mens) teams'),
                                                  sidebarPanel(
                                                    selectInput('matches2TeamFuncT20M', 'Select function', matches2TeamsFuncs),
                                                    selectInput('match2T20M', 'Select matches', T20MMatches2Teams,selectize=FALSE, size=13),
                                                    uiOutput("selectTeam2T20M"),
                                                    radioButtons("plotOrTable1T20M", label = h4("Plot or table"),
                                                                 choices = c("Plot" = 1, "Table" = 2),
                                                                 selected = 1,inline=T),
                                                    radioButtons("repTypeT20M", label = h4("Report Type"),
                                                                 choices = c("Summary" = 1, "Detailed" = 2),
                                                                 selected = 1,inline=T)

                                                  ),
                                                  mainPanel(
                                                    uiOutput("plotOrPrintT20MMatch2teams"),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )

                                         ),
                                         # Analyze T20Mens Team Overall Perf
                                         tabPanel("Overall Performance",
                                                  h4("Analyze T20M team's overall performance"),
                                                  sidebarPanel(
                                                    selectInput('overallperfFuncT20M', 'Select function', teamOverallPerfFunc),
                                                    selectInput('teamMatchesT20M', 'Select the team', T20MTeamsAll,selectize=FALSE, size=13),
                                                    uiOutput("RankT20M"),
                                                    radioButtons("plotOrTable2T20M", label = h4("Plot or table"),
                                                                 choices = c("Plot" = 1, "Table" = 2),
                                                                 selected = 1,inline=T),
                                                    radioButtons("repType2T20M", label = h4("Report Type"),
                                                                 choices = c("Summary" = 1, "Detailed" = 2),
                                                                 selected = 1,inline=T)
                                                  ),
                                                  mainPanel(
                                                    uiOutput('printOrPlotT20MTeamPerfoverall'),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Dec 14, 2020"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )

                                         ),
                                         # Rank T20 Men  tab
                                         tabPanel("Rank Intl. T20 Batsmen (men)",

                                            h4('Rank Intl. T20 Batsmen (men)'),

                                            sidebarPanel(
                                            tags$head( tags$style( type = "text/css", '
                                              .js-irs-4 .irs-line-mid{
                                                background: #428bca ;
                                                border: 1px solid #428bca ;
                                              }
                                              .js-irs-4 .irs-line-right{
                                                background: #428bca ;
                                              }
                                              .js-irs-4 .irs-bar {
                                                background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                border-top: 1px solid #CCC ;
                                                border-bottom: 1px solid #CCC ;
                                              }
                                              .js-irs-4 .irs-bar-edge {
                                                background: inherit ;
                                                border: inherit ;
                                              }

                                            ')),
                                                    #uiOutput("yearsPlayed"),
                                                    #uiOutput("MinMatches"),
                                                    sliderInput("yearSelectedT20M", "Since year",min = (helper(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[1]])-1, max = (helper(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[2]]), value = (helper(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[1]])),
                                                    sliderInput("minMatchesT20M", "Matches played",min = (helper(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[3]]), max = (helper(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[4]]), value = 0),
                                                    uiOutput("ModeT20M")

                                                  ),
                                                  mainPanel(
                                                    shinycssloaders::withSpinner(
                                                      uiOutput('rankT20MBatsmen'),
                                                    ),

                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                         ),
                                         # Rank T20 Bowlers(men) tab
                                         tabPanel("Rank Intl. T20 Bowlers (men)",

                                              h4('Rank Intl. T20 Bowlers (men)'),

                                              sidebarPanel(
                                              tags$head( tags$style( type = "text/css", '
                                              .js-irs-6 .irs-line-mid{
                                                background: #428bca ;
                                                border: 1px solid #428bca ;
                                              }
                                              .js-irs-6 .irs-line-right{
                                                background: #428bca ;
                                              }
                                              .js-irs-6 .irs-bar {
                                                background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                border-top: 1px solid #CCC ;
                                                border-bottom: 1px solid #CCC ;
                                              }
                                              .js-irs-6 .irs-bar-edge {
                                                background: inherit ;
                                                border: inherit ;
                                              }
                                            ')),

                                              sliderInput("yearSelected1T20M", "Since year",min = (helper2(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[1]])-1, max = (helper2(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[2]]), value = (helper2(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[1]])),
                                              sliderInput("minMatches1T20M", "Matches played",min = (helper2(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[3]]), max = (helper2(T20MTeamNames,"./t20/t20BattingBowlingDetails")[[4]]), value = 0),
                                              uiOutput("Mode1T20M")
                                            ),
                                              mainPanel(
                                                shinycssloaders::withSpinner(
                                                  uiOutput('rankT20MBowlers'),
                                                ),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                         )

                                      )),
    ############################# International T20 (Women) ################################
    tabPanel("Intl. T20 (women)",navbarPage("GooglyPlusPlus - International T20 (women)",
                                         # Batsman tab
                                         tabPanel("Intl T20 batsman",
                                                  h4('Analyze Intl T20 batsman performances'),
                                                  sidebarPanel(
                                                    selectInput('batsmanFuncT20W', 'Select function', batsmanFuncs),
                                                    selectInput('batsmanT20W', 'Select batsman', T20WBatsmen,selectize=FALSE, size=20),
                                                  ),
                                                  mainPanel(
                                                    plotOutput('batsmanPlotT20W'),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                         ),
                                         # Bowlers tab
                                         tabPanel("Intl T20 Women bowlers",

                                                  h4('Analyze T20 Women bowler performances'),

                                                  sidebarPanel(
                                                    selectInput('bowlerFuncT20W', 'Select function', bowlerFuncs),
                                                    selectInput('bowlerT20W', 'Select T20 bowler', T20WBowlers,selectize=FALSE, size=20)


                                                  ),
                                                  mainPanel(
                                                    plotOutput('bowlerPlotT20W'),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                         ),
                                         tabPanel("Intl T20 Women's Match",
                                                  h4('Analyze an Intl T20 match'),
                                                  sidebarPanel(
                                                    selectInput('matchFuncT20W', 'Select match function', matchFuncs),
                                                    selectInput('matchT20W', 'Select T20 match ', T20WMatches,selectize=FALSE, size=15),
                                                    uiOutput("selectTeamT20W"),
                                                    radioButtons("plotOrTableT20W", label = h4("Plot or table"),
                                                                 choices = c("Plot" = 1, "Table" = 2),
                                                                 selected = 1,inline=T)

                                                  ),
                                                  mainPanel(
                                                    uiOutput("plotOrPrintT20WMatch"),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                         ),
                                         # Analyze 2 T20 Teams Women's  matches
                                         tabPanel("Head to head",
                                                  h4('Head-to-head between 2 T20 (Womens) teams'),
                                                  sidebarPanel(
                                                    selectInput('matches2TeamFuncT20W', 'Select function', matches2TeamsFuncs),
                                                    selectInput('match2T20W', 'Select matches', T20WMatches2Teams,selectize=FALSE, size=13),
                                                    uiOutput("selectTeam2T20W"),
                                                    radioButtons("plotOrTable1T20W", label = h4("Plot or table"),
                                                                 choices = c("Plot" = 1, "Table" = 2),
                                                                 selected = 1,inline=T),
                                                    radioButtons("repTypeT20W", label = h4("Report Type"),
                                                                 choices = c("Summary" = 1, "Detailed" = 2),
                                                                 selected = 1,inline=T)

                                                  ),
                                                  mainPanel(
                                                    uiOutput("plotOrPrintT20WMatch2teams"),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )

                                         ),
                                         # Analyze T20 Womens Team Overall Perf
                                         tabPanel("Overall Performance",
                                                  h4("Analyze T20 Womens team's overall performance"),
                                                  sidebarPanel(
                                                    selectInput('overallperfFuncT20W', 'Select function', teamOverallPerfFunc),
                                                    selectInput('teamMatchesT20W', 'Select the team', T20WTeamsAll,selectize=FALSE, size=13),
                                                    uiOutput("RankT20W"),
                                                    radioButtons("plotOrTable2T20W", label = h4("Plot or table"),
                                                                 choices = c("Plot" = 1, "Table" = 2),
                                                                 selected = 1,inline=T),
                                                    radioButtons("repType2T20W", label = h4("Report Type"),
                                                                 choices = c("Summary" = 1, "Detailed" = 2),
                                                                 selected = 1,inline=T)
                                                  ),
                                                  mainPanel(
                                                    uiOutput('printOrPlotT20WTeamPerfoverall'),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )

                                         ),

                                         # Rank T20 Women  tab
                                         tabPanel("Rank Intl. T20 Batsmen (women)",

                                                h4('Rank Intl. T20 Batsmen (women)'),

                                                sidebarPanel(
                                                  tags$head( tags$style( type = "text/css", '
                                                .js-irs-8 .irs-line-mid{
                                                  background: #428bca ;
                                                  border: 1px solid #428bca ;
                                                }
                                                .js-irs-8 .irs-line-right{
                                                  background: #428bca ;
                                                }
                                                .js-irs-8 .irs-bar {
                                                  background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                  border-top: 1px solid #CCC ;
                                                  border-bottom: 1px solid #CCC ;
                                                }
                                                .js-irs-8 .irs-bar-edge {
                                                  background: inherit ;
                                                  border: inherit ;
                                                }

                                              ')),

                                                    sliderInput("yearSelectedT20W", "Since year",min = (helper(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[1]])-1, max = (helper(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[2]]), value = (helper(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[1]])),
                                                    sliderInput("minMatchesT20W", "Matches played",min = (helper(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[3]]), max = (helper(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[4]]), value = 0),
                                                    uiOutput("ModeT20W")

                                                  ),
                                                  mainPanel(
                                                    shinycssloaders::withSpinner(
                                                      uiOutput('rankT20WBatsmen'),
                                                    ),

                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                         ),
                                         # Rank T20 Bowlers(women) tab
                                         tabPanel("Rank Intl. T20 Bowlers (women)",

                                                  h4('Rank Intl. T20 Bowlers (women)'),

                                                  sidebarPanel(
                                                    tags$head( tags$style( type = "text/css", '
                                                    .js-irs-10 .irs-line-mid{
                                                      background: #428bca ;
                                                      border: 1px solid #428bca ;
                                                    }
                                                    .js-irs-10 .irs-line-right{
                                                      background: #428bca ;
                                                    }
                                                    .js-irs-10 .irs-bar {
                                                      background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                      border-top: 1px solid #CCC ;
                                                      border-bottom: 1px solid #CCC ;
                                                    }
                                                    .js-irs-10 .irs-bar-edge {
                                                      background: inherit ;
                                                      border: inherit ;
                                                    }
                                                  ')),

                                                    sliderInput("yearSelected1T20W", "Since year",min = (helper2(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[1]])-1, max = (helper2(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[2]]), value = (helper2(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[1]])),
                                                    sliderInput("minMatches1T20W", "Matches played",min = (helper2(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[3]]), max = (helper2(T20WTeamNames,"./t20/t20WomenBattingBowlingDetails")[[4]]), value = 0),
                                                    uiOutput("Mode1T20W")
                                                  ),
                                                  mainPanel(
                                                    shinycssloaders::withSpinner(
                                                      uiOutput('rankT20WBowlers'),
                                                    ),
                                                    column(7, offset=4,
                                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                           tags$h5((tags$i("Feb 6, 2021"))),
                                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                    )
                                                  )
                                         )

                               )),
    #############################Big Bash League T20 ################################
    tabPanel("BBL T20",navbarPage("GooglyPlusPlus - Big Bash League T20",
                                           # Batsman tab
                                           tabPanel("BBL T20 batsman",
                                                    h4('Analyze BBL T20 batsman performances'),
                                                    sidebarPanel(
                                                      selectInput('batsmanFuncBBL', 'Select function', batsmanFuncs),
                                                      selectInput('batsmanBBL', 'Select batsman', BBLBatsmen,selectize=FALSE, size=20),
                                                    ),
                                                    mainPanel(
                                                      plotOutput('batsmanPlotBBL'),
                                                      column(7, offset=4,
                                                             tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                             tags$h5((tags$i("Feb 6, 2021"))),
                                                             tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                             tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                                      )
                                                    )
                                           ),

                                         # Bowlers tab
                                         tabPanel("BBL bowlers",

                                           h4('BBL bowler performances'),

                                           sidebarPanel(
                                             selectInput('bowlerFuncBBL', 'Select function', bowlerFuncs),
                                             selectInput('bowlerBBL', 'Select T20 bowler', BBLBowlers,selectize=FALSE, size=20)


                                           ),
                                           mainPanel(
                                             plotOutput('bowlerPlotBBL'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                         ),
                                  tabPanel("BBL  Match",
                                           h4('Analyze BBL T20 match'),
                                           sidebarPanel(
                                             selectInput('matchFuncBBL', 'Select match function', matchFuncs),
                                             selectInput('matchBBL', 'Select BBL match ', BBLMatches,selectize=FALSE, size=15),
                                             uiOutput("selectTeamBBL"),
                                             radioButtons("plotOrTableBBL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintBBLMatch"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Analyze Head-to-Head BBL  matches
                                  tabPanel("Head to head",
                                           h4('Head-to-head between BBL teams'),
                                           sidebarPanel(
                                             selectInput('matches2TeamFuncBBL', 'Select function', matches2TeamsFuncs),
                                             selectInput('match2BBL', 'Select matches', BBLMatches2Teams,selectize=FALSE, size=13),
                                             uiOutput("selectTeam2BBL"),
                                             radioButtons("plotOrTable1BBL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repTypeBBL", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintBBLMatch2teams"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Analyze BBL T20 Team Overall Perf
                                  tabPanel("Overall Performance",
                                           h4("Analyze BBL team's overall performance"),
                                           sidebarPanel(
                                             selectInput('overallperfFuncBBL', 'Select function', teamOverallPerfFunc),
                                             selectInput('teamMatchesBBL', 'Select the team', BBLTeamsAll,selectize=FALSE, size=13),
                                             uiOutput("RankBBL"),
                                             radioButtons("plotOrTable2BBL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repType2BBL", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)
                                           ),
                                           mainPanel(
                                             uiOutput('printOrPlotBBLTeamPerfoverall'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Rank BBL Players tab
                                  tabPanel("Rank  BBL Batsmen",

                                           h4('Rank  BBL Batsmen '),

                                           sidebarPanel(
                                             tags$head( tags$style( type = "text/css", '
                                                .js-irs-12 .irs-line-mid{
                                                  background: #428bca ;
                                                  border: 1px solid #428bca ;
                                                }
                                                .js-irs-12 .irs-line-right{
                                                  background: #428bca ;
                                                }
                                                .js-irs-12 .irs-bar {
                                                  background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                  border-top: 1px solid #CCC ;
                                                  border-bottom: 1px solid #CCC ;
                                                }
                                                .js-irs-12 .irs-bar-edge {
                                                  background: inherit ;
                                                  border: inherit ;
                                                }

                                              ')),

                                             sliderInput("yearSelectedBBL", "Since year",min = (helper(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[1]])-1, max = (helper(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[2]]), value = (helper(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[1]])),
                                             sliderInput("minMatchesBBL", "Matches played",min = (helper(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[3]]), max = (helper(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[4]]), value = 0),
                                             uiOutput("ModeBBL")

                                           ),
                                           mainPanel(
                                             shinycssloaders::withSpinner(
                                               uiOutput('rankBBLBatsmen'),
                                             ),

                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Rank BBL Bowlers tab
                                  tabPanel("Rank BBL Bowlers",

                                           h4('Rank BBL Bowlers '),

                                           sidebarPanel(
                                             tags$head( tags$style( type = "text/css", '
                                                    .js-irs-14 .irs-line-mid{
                                                      background: #428bca ;
                                                      border: 1px solid #428bca ;
                                                    }
                                                    .js-irs-14 .irs-line-right{
                                                      background: #428bca ;
                                                    }
                                                    .js-irs-14 .irs-bar {
                                                      background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                      border-top: 1px solid #CCC ;
                                                      border-bottom: 1px solid #CCC ;
                                                    }
                                                    .js-irs-14 .irs-bar-edge {
                                                      background: inherit ;
                                                      border: inherit ;
                                                    }
                                                  ')),

                                             sliderInput("yearSelected1BBL", "Since year",min = (helper2(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[1]])-1, max = (helper2(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[2]]), value = (helper2(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[1]])),
                                             sliderInput("minMatches1BBL", "Matches played",min = (helper2(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[3]]), max = (helper2(BBLTeamNames,"./bbl/bblBattingBowlingDetails")[[4]]), value = 0),
                                             uiOutput("Mode1BBL")
                                           ),
                                           mainPanel(
                                             shinycssloaders::withSpinner(
                                               uiOutput('rankBBLBowlers'),
                                             ),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  )

                            )),
    ############################# Natwest T20 ################################
    tabPanel("NTB T20",navbarPage("GooglyPlusPlus - Natwest T20",
                                  # Batsman tab
                                  tabPanel("NTB T20 batsman",
                                           h4('Analyze NTB T20 batsman performances'),
                                           sidebarPanel(
                                             selectInput('batsmanFuncNTB', 'Select function', batsmanFuncs),
                                             selectInput('batsmanNTB', 'Select batsman', NTBBatsmen,selectize=FALSE, size=20),
                                           ),
                                           mainPanel(
                                             plotOutput('batsmanPlotNTB'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                         ),
                                    # Bowlers tab
                                    tabPanel("NTB bowlers",

                                           h4('NTB bowler performances'),

                                           sidebarPanel(
                                             selectInput('bowlerFuncNTB', 'Select function', bowlerFuncs),
                                             selectInput('bowlerNTB', 'Select T20 bowler', NTBBowlers,selectize=FALSE, size=20)


                                           ),
                                           mainPanel(
                                             plotOutput('bowlerPlotNTB'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  tabPanel("NTB  Match",
                                           h4('Analyze NTB T20 match'),
                                           sidebarPanel(
                                             selectInput('matchFuncNTB', 'Select match function', matchFuncs),
                                             selectInput('matchNTB', 'Select NTB match ', NTBMatches,selectize=FALSE, size=15),
                                             uiOutput("selectTeamNTB"),
                                             radioButtons("plotOrTableNTB", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintNTBMatch"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Analyze Head-to-Head NTB  matches
                                  tabPanel("Head to head",
                                           h4('Head-to-head between NTB teams'),
                                           sidebarPanel(
                                             selectInput('matches2TeamFuncNTB', 'Select function', matches2TeamsFuncs),
                                             selectInput('match2NTB', 'Select matches', NTBMatches2Teams,selectize=FALSE, size=13),
                                             uiOutput("selectTeam2NTB"),
                                             radioButtons("plotOrTable1NTB", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repTypeNTB", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintNTBMatch2teams"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Analyze NTB T20 Team Overall Perf
                                  tabPanel("Overall Performance",
                                           h4("Analyze NTB team's overall performance"),
                                           sidebarPanel(
                                             selectInput('overallperfFuncNTB', 'Select function', teamOverallPerfFunc),
                                             selectInput('teamMatchesNTB', 'Select the team', NTBTeamsAll,selectize=FALSE, size=13),
                                             uiOutput("RankNTB"),
                                             radioButtons("plotOrTable2NTB", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repType2NTB", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)
                                           ),
                                           mainPanel(
                                             uiOutput('printOrPlotNTBTeamPerfoverall'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Rank NTB Players tab
                                  tabPanel("Rank  NTB Batsmen",

                                           h4('Rank  NTB Batsmen '),

                                           sidebarPanel(
                                             tags$head( tags$style( type = "text/css", '
                                                .js-irs-16 .irs-line-mid{
                                                  background: #428bca ;
                                                  border: 1px solid #428bca ;
                                                }
                                                .js-irs-16 .irs-line-right{
                                                  background: #428bca ;
                                                }
                                                .js-irs-16 .irs-bar {
                                                  background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                  border-top: 1px solid #CCC ;
                                                  border-bottom: 1px solid #CCC ;
                                                }
                                                .js-irs-16 .irs-bar-edge {
                                                  background: inherit ;
                                                  border: inherit ;
                                                }

                                              ')),

                                             sliderInput("yearSelectedNTB", "Since year",min = (helper(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[1]])-1, max = (helper(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[2]]), value = (helper(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[1]])),
                                             sliderInput("minMatchesNTB", "Matches played",min = (helper(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[3]]), max = (helper(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[4]]), value = 0),
                                             uiOutput("ModeNTB")

                                           ),
                                           mainPanel(
                                             shinycssloaders::withSpinner(
                                               uiOutput('rankNTBBatsmen'),
                                             ),

                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Rank NTB Bowlers tab
                                  tabPanel("Rank NTB Bowlers",

                                           h4('Rank NTB Bowlers '),

                                           sidebarPanel(
                                             tags$head( tags$style( type = "text/css", '
                                                    .js-irs-18 .irs-line-mid{
                                                      background: #428bca ;
                                                      border: 1px solid #428bca ;
                                                    }
                                                    .js-irs-18 .irs-line-right{
                                                      background: #428bca ;
                                                    }
                                                    .js-irs-18 .irs-bar {
                                                      background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                      border-top: 1px solid #CCC ;
                                                      border-bottom: 1px solid #CCC ;
                                                    }
                                                    .js-irs-18 .irs-bar-edge {
                                                      background: inherit ;
                                                      border: inherit ;
                                                    }
                                                  ')),

                                             sliderInput("yearSelected1NTB", "Since year",min = (helper2(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[1]])-1, max = (helper2(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[2]]), value = (helper2(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[1]])),
                                             sliderInput("minMatches1NTB", "Matches played",min = (helper2(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[3]]), max = (helper2(NTBTeamNames,"./ntb/ntbBattingBowlingDetails")[[4]]), value = 0),
                                             uiOutput("Mode1NTB")
                                           ),
                                           mainPanel(
                                             shinycssloaders::withSpinner(
                                               uiOutput('rankNTBBowlers'),
                                             ),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  )

                       )),
    ############################# PSL T20 ################################
    tabPanel("PSL T20",navbarPage("GooglyPlusPlus - Pakistan Super League",
                                  # Batsman tab
                                  tabPanel("PSL T20 batsman",
                                           h4('Analyze PSL T20 batsman performances'),
                                           sidebarPanel(
                                             selectInput('batsmanFuncPSL', 'Select function', batsmanFuncs),
                                             selectInput('batsmanPSL', 'Select batsman', PSLBatsmen,selectize=FALSE, size=20),
                                           ),
                                           mainPanel(
                                             plotOutput('batsmanPlotPSL'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Bowlers tab
                                  tabPanel("PSL bowlers",

                                           h4('PSL bowler performances'),

                                           sidebarPanel(
                                             selectInput('bowlerFuncPSL', 'Select function', bowlerFuncs),
                                             selectInput('bowlerPSL', 'Select T20 bowler', PSLBowlers,selectize=FALSE, size=20)


                                           ),
                                           mainPanel(
                                             plotOutput('bowlerPlotPSL'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  tabPanel("PSL  Match",
                                           h4('Analyze PSL T20 match'),
                                           sidebarPanel(
                                             selectInput('matchFuncPSL', 'Select match function', matchFuncs),
                                             selectInput('matchPSL', 'Select PSL match ', PSLMatches,selectize=FALSE, size=15),
                                             uiOutput("selectTeamPSL"),
                                             radioButtons("plotOrTablePSL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintPSLMatch"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Analyze Head-to-Head PSL  matches
                                  tabPanel("Head to head",
                                           h4('Head-to-head between PSL teams'),
                                           sidebarPanel(
                                             selectInput('matches2TeamFuncPSL', 'Select function', matches2TeamsFuncs),
                                             selectInput('match2PSL', 'Select matches', PSLMatches2Teams,selectize=FALSE, size=13),
                                             uiOutput("selectTeam2PSL"),
                                             radioButtons("plotOrTable1PSL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repTypePSL", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintPSLMatch2teams"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Analyze PSL T20 Team Overall Perf
                                  tabPanel("Overall Performance",
                                           h4("Analyze PSL team's overall performance"),
                                           sidebarPanel(
                                             selectInput('overallperfFuncPSL', 'Select function', teamOverallPerfFunc),
                                             selectInput('teamMatchesPSL', 'Select the team', PSLTeamsAll,selectize=FALSE, size=13),
                                             uiOutput("RankPSL"),
                                             radioButtons("plotOrTable2PSL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repType2PSL", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)
                                           ),
                                           mainPanel(
                                             uiOutput('printOrPlotPSLTeamPerfoverall'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Rank PSL Players tab
                                  tabPanel("Rank  PSL Batsmen",

                                           h4('Rank  PSL Batsmen '),

                                           sidebarPanel(
                                             tags$head( tags$style( type = "text/css", '
                                                .js-irs-20 .irs-line-mid{
                                                  background: #428bca ;
                                                  border: 1px solid #428bca ;
                                                }
                                                .js-irs-20 .irs-line-right{
                                                  background: #428bca ;
                                                }
                                                .js-irs-20 .irs-bar {
                                                  background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                  border-top: 1px solid #CCC ;
                                                  border-bottom: 1px solid #CCC ;
                                                }
                                                .js-irs-20 .irs-bar-edge {
                                                  background: inherit ;
                                                  border: inherit ;
                                                }

                                              ')),

                                             sliderInput("yearSelectedPSL", "Since year",min = (helper(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[1]])-1, max = (helper(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[2]]), value = (helper(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[1]])),
                                             sliderInput("minMatchesPSL", "Matches played",min = (helper(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[3]]), max = (helper(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[4]]), value = 0),
                                             uiOutput("ModePSL")

                                           ),
                                           mainPanel(
                                             shinycssloaders::withSpinner(
                                               uiOutput('rankPSLBatsmen'),
                                             ),

                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Rank PSL Bowlers tab
                                  tabPanel("Rank PSL Bowlers",

                                           h4('Rank PSL Bowlers '),

                                           sidebarPanel(
                                             tags$head( tags$style( type = "text/css", '
                                                    .js-irs-22 .irs-line-mid{
                                                      background: #428bca ;
                                                      border: 1px solid #428bca ;
                                                    }
                                                    .js-irs-22 .irs-line-right{
                                                      background: #428bca ;
                                                    }
                                                    .js-irs-22 .irs-bar {
                                                      background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                      border-top: 1px solid #CCC ;
                                                      border-bottom: 1px solid #CCC ;
                                                    }
                                                    .js-irs-22 .irs-bar-edge {
                                                      background: inherit ;
                                                      border: inherit ;
                                                    }
                                                  ')),

                                             sliderInput("yearSelected1PSL", "Since year",min = (helper2(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[1]])-1, max = (helper2(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[2]]), value = (helper2(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[1]])),
                                             sliderInput("minMatches1PSL", "Matches played",min = (helper2(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[3]]), max = (helper2(PSLTeamNames,"./psl/pslBattingBowlingDetails")[[4]]), value = 0),
                                             uiOutput("Mode1PSL")
                                           ),
                                           mainPanel(
                                             shinycssloaders::withSpinner(
                                               uiOutput('rankPSLBowlers'),
                                             ),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  )

                      )),
    ############################# WBB T20 ################################
    tabPanel("WBBL T20",navbarPage("GooglyPlusPlus - Women's Big Bash League",
                                  # Batsman tab
                                  tabPanel("WBB T20 batsman",
                                           h4('Analyze WBBL T20 batsman performances'),
                                           sidebarPanel(
                                             selectInput('batsmanFuncWBB', 'Select function', batsmanFuncs),
                                             selectInput('batsmanWBB', 'Select batsman', WBBBatsmen,selectize=FALSE, size=20),
                                           ),
                                           mainPanel(
                                             plotOutput('batsmanPlotWBB'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),

                                  # Bowlers tab
                                  tabPanel("WBB bowlers",

                                           h4('WBB bowler performances'),

                                           sidebarPanel(
                                             selectInput('bowlerFuncWBB', 'Select function', bowlerFuncs),
                                             selectInput('bowlerWBB', 'Select T20 bowler', WBBBowlers,selectize=FALSE, size=20)


                                           ),
                                           mainPanel(
                                             plotOutput('bowlerPlotWBB'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  tabPanel("WBB  Match",
                                           h4('Analyze WBBL T20 match'),
                                           sidebarPanel(
                                             selectInput('matchFuncWBB', 'Select match function', matchFuncs),
                                             selectInput('matchWBB', 'Select WBB match ', WBBMatches,selectize=FALSE, size=15),
                                             uiOutput("selectTeamWBB"),
                                             radioButtons("plotOrTableWBB", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintWBBMatch"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Analyze Head-to-Head WBBL  matches
                                  tabPanel("Head to head",
                                           h4('Head-to-head between Womens BBL teams'),
                                           sidebarPanel(
                                             selectInput('matches2TeamFuncWBB', 'Select function', matches2TeamsFuncs),
                                             selectInput('match2WBB', 'Select matches', WBBMatches2Teams,selectize=FALSE, size=13),
                                             uiOutput("selectTeam2WBB"),
                                             radioButtons("plotOrTable1WBB", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repTypeWBB", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintWBBMatch2teams"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Analyze WBBL T20 Team Overall Perf
                                  tabPanel("Overall Performance",
                                           h4("Analyze WBB team's overall performance"),
                                           sidebarPanel(
                                             selectInput('overallperfFuncWBB', 'Select function', teamOverallPerfFunc),
                                             selectInput('teamMatchesWBB', 'Select the team', WBBTeamsAll,selectize=FALSE, size=13),
                                             uiOutput("RankWBB"),
                                             radioButtons("plotOrTable2WBB", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repType2WBB", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)
                                           ),
                                           mainPanel(
                                             uiOutput('printOrPlotWBBTeamPerfoverall'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Rank WBB PlayersL tab
                              tabPanel("Rank WBB Batsmen",
                                  h4('Rank  WBB Batsmen '),

                                  sidebarPanel(
                                    tags$head( tags$style( type = "text/css", '
                                                .js-irs-24 .irs-line-mid{
                                                  background: #428bca ;
                                                  border: 1px solid #428bca ;
                                                }
                                                .js-irs-24 .irs-line-right{
                                                  background: #428bca ;
                                                }
                                                .js-irs-24 .irs-bar {
                                                  background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                  border-top: 1px solid #CCC ;
                                                  border-bottom: 1px solid #CCC ;
                                                }
                                                .js-irs-24 .irs-bar-edge {
                                                  background: inherit ;
                                                  border: inherit ;
                                                }

                                              ')),

                                    sliderInput("yearSelectedWBB", "Since year",min = (helper(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[1]])-1, max = (helper(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[2]]), value = (helper(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[1]])),
                                    sliderInput("minMatchesWBB", "Matches played",min = (helper(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[3]]), max = (helper(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[4]]), value = 0),
                                    uiOutput("ModeWBB")

                                  ),
                                  mainPanel(
                                    shinycssloaders::withSpinner(
                                      uiOutput('rankWBBBatsmen'),
                                    ),

                                    column(7, offset=4,
                                           tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                           tags$h5((tags$i("Feb 6, 2021"))),
                                           tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                           tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                    )
                                  )
                            ),
                            # Rank WBB Bowlers tab
                            tabPanel("Rank WBB Bowlers",

                                     h4('Rank WBB Bowlers '),

                                     sidebarPanel(
                                       tags$head( tags$style( type = "text/css", '
                                                                            .js-irs-26 .irs-line-mid{
                                                                              background: #428bca ;
                                                                              border: 1px solid #428bca ;
                                                                            }
                                                                            .js-irs-26 .irs-line-right{
                                                                              background: #428bca ;
                                                                            }
                                                                            .js-irs-26 .irs-bar {
                                                                              background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                                              border-top: 1px solid #CCC ;
                                                                              border-bottom: 1px solid #CCC ;
                                                                            }
                                                                            .js-irs-26 .irs-bar-edge {
                                                                              background: inherit ;
                                                                              border: inherit ;
                                                                            }
                                                                        ')),

                                       sliderInput("yearSelected1WBB", "Since year",min = (helper2(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[1]])-1, max = (helper2(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[2]]), value = (helper2(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[1]])),
                                       sliderInput("minMatches1WBB", "Matches played",min = (helper2(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[3]]), max = (helper2(WBBTeamNames,"./wbb/wbbBattingBowlingDetails")[[4]]), value = 0),
                                       uiOutput("Mode1WBB")
                                     ),
                                     mainPanel(
                                       shinycssloaders::withSpinner(
                                         uiOutput('rankWBBBowlers'),
                                       ),
                                       column(7, offset=4,
                                              tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                              tags$h5((tags$i("Feb 6, 2021"))),
                                              tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                              tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                       )
                                     )
                            )

                        )),

    #############################Carribean Premier League T20 ################################
    tabPanel("CPL T20",navbarPage("GooglyPlusPlus - Big Bash League T20",
                                  # Batsman tab
                                  tabPanel("CPL T20 batsman",
                                           h4('Analyze CPL T20 batsman performances'),
                                           sidebarPanel(
                                             selectInput('batsmanFuncCPL', 'Select function', batsmanFuncs),
                                             selectInput('batsmanCPL', 'Select batsman', CPLBatsmen,selectize=FALSE, size=20),
                                           ),
                                           mainPanel(
                                             plotOutput('batsmanPlotCPL'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),

                                  # Bowlers tab
                                  tabPanel("CPL bowlers",

                                           h4('CPL bowler performances'),

                                           sidebarPanel(
                                             selectInput('bowlerFuncCPL', 'Select function', bowlerFuncs),
                                             selectInput('bowlerCPL', 'Select T20 bowler', CPLBowlers,selectize=FALSE, size=20)


                                           ),
                                           mainPanel(
                                             plotOutput('bowlerPlotCPL'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  tabPanel("CPL  Match",
                                           h4('Analyze CPL T20 match'),
                                           sidebarPanel(
                                             selectInput('matchFuncCPL', 'Select match function', matchFuncs),
                                             selectInput('matchCPL', 'Select CPL match ', CPLMatches,selectize=FALSE, size=15),
                                             uiOutput("selectTeamCPL"),
                                             radioButtons("plotOrTableCPL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintCPLMatch"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Analyze Head-to-Head CPL  matches
                                  tabPanel("Head to head",
                                           h4('Head-to-head between CPL teams'),
                                           sidebarPanel(
                                             selectInput('matches2TeamFuncCPL', 'Select function', matches2TeamsFuncs),
                                             selectInput('match2CPL', 'Select matches', CPLMatches2Teams,selectize=FALSE, size=13),
                                             uiOutput("selectTeam2CPL"),
                                             radioButtons("plotOrTable1CPL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repTypeCPL", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)

                                           ),
                                           mainPanel(
                                             uiOutput("plotOrPrintCPLMatch2teams"),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Analyze CPL T20 Team Overall Perf
                                  tabPanel("Overall Performance",
                                           h4("Analyze CPL team's overall performance"),
                                           sidebarPanel(
                                             selectInput('overallperfFuncCPL', 'Select function', teamOverallPerfFunc),
                                             selectInput('teamMatchesCPL', 'Select the team', CPLTeamsAll,selectize=FALSE, size=13),
                                             uiOutput("RankCPL"),
                                             radioButtons("plotOrTable2CPL", label = h4("Plot or table"),
                                                          choices = c("Plot" = 1, "Table" = 2),
                                                          selected = 1,inline=T),
                                             radioButtons("repType2CPL", label = h4("Report Type"),
                                                          choices = c("Summary" = 1, "Detailed" = 2),
                                                          selected = 1,inline=T)
                                           ),
                                           mainPanel(
                                             uiOutput('printOrPlotCPLTeamPerfoverall'),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )

                                  ),
                                  # Rank CPL Players tab
                                  tabPanel("Rank  CPL Batsmen",

                                           h4('Rank  CPL Batsmen '),

                                           sidebarPanel(
                                             tags$head( tags$style( type = "text/css", '
                                                .js-irs-28 .irs-line-mid{
                                                  background: #428bca ;
                                                  border: 1px solid #428bca ;
                                                }
                                                .js-irs-28 .irs-line-right{
                                                  background: #428bca ;
                                                }
                                                .js-irs-28 .irs-bar {
                                                  background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                  border-top: 1px solid #CCC ;
                                                  border-bottom: 1px solid #CCC ;
                                                }
                                                .js-irs-28 .irs-bar-edge {
                                                  background: inherit ;
                                                  border: inherit ;
                                                }

                                              ')),

                                             sliderInput("yearSelectedCPL", "Since year",min = (helper(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[1]])-1, max = (helper(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[2]]), value = (helper(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[1]])),
                                             sliderInput("minMatchesCPL", "Matches played",min = (helper(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[3]]), max = (helper(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[4]]), value = 0),
                                             uiOutput("ModeCPL")

                                           ),
                                           mainPanel(
                                             shinycssloaders::withSpinner(
                                               uiOutput('rankCPLBatsmen'),
                                             ),

                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  ),
                                  # Rank CPL Bowlers tab
                                  tabPanel("Rank CPL Bowlers",

                                           h4('Rank CPL Bowlers '),

                                           sidebarPanel(
                                             tags$head( tags$style( type = "text/css", '
                                                    .js-irs-30 .irs-line-mid{
                                                      background: #428bca ;
                                                      border: 1px solid #428bca ;
                                                    }
                                                    .js-irs-30 .irs-line-right{
                                                      background: #428bca ;
                                                    }
                                                    .js-irs-30 .irs-bar {
                                                      background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                                      border-top: 1px solid #CCC ;
                                                      border-bottom: 1px solid #CCC ;
                                                    }
                                                    .js-irs-30 .irs-bar-edge {
                                                      background: inherit ;
                                                      border: inherit ;
                                                    }
                                                  ')),

                                             sliderInput("yearSelected1CPL", "Since year",min = (helper2(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[1]])-1, max = (helper2(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[2]]), value = (helper2(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[1]])),
                                             sliderInput("minMatches1CPL", "Matches played",min = (helper2(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[3]]), max = (helper2(CPLTeamNames,"./cpl/cplBattingBowlingDetails")[[4]]), value = 0),
                                             uiOutput("Mode1CPL")
                                           ),
                                           mainPanel(
                                             shinycssloaders::withSpinner(
                                               uiOutput('rankCPLBowlers'),
                                             ),
                                             column(7, offset=4,
                                                    tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
                                                    tags$h5((tags$i("Feb 6, 2021"))),
                                                    tags$h6("Data source Cricsheet: http://cricsheet.org/"),
                                                    tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
                                             )
                                           )
                                  )
                      )),

    ############################# ODI Men ################################
    # tabPanel("ODI Men",navbarPage("GooglyPlusPlus - One Day International (ODI) Men",
    #                                # Batsman tab
    #                                tabPanel("ODI Men batsman",
    #                                         h4('Analyze ODI batsman performances'),
    #                                         sidebarPanel(
    #                                           selectInput('batsmanFuncODIM', 'Select function', batsmanFuncs),
    #                                           selectInput('batsmanODIM', 'Select batsman', ODIMBatsmen,selectize=FALSE, size=20),
    #                                         ),
    #                                         mainPanel(
    #                                           plotOutput('batsmanPlotODIM'),
    #                                           column(7, offset=4,
    #                                                  tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                                  tags$h5((tags$i("Feb 6, 2021"))),
    #                                                  tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                                  tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                           )
    #                                         )
    #                                ),
    #                               # Bowlers tab
    #                               tabPanel("ODI Men bowlers",
    #
    #                                        h4('ODI  bowler performances'),
    #
    #                                        sidebarPanel(
    #                                          selectInput('bowlerFuncODIM', 'Select function', bowlerFuncs),
    #                                          selectInput('bowlerODIM', 'Select T20 bowler', ODIMBowlers,selectize=FALSE, size=20)
    #
    #
    #                                        ),
    #                                        mainPanel(
    #                                          plotOutput('bowlerPlotODIM'),
    #                                          column(7, offset=4,
    #                                                 tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                                 tags$h5((tags$i("Feb 6, 2021"))),
    #                                                 tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                                 tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                          )
    #                                        )
    #
    #                               ),
    #                               tabPanel("ODI Mens  Match",
    #                                        h4('Analyze ODI Mens match'),
    #                                        sidebarPanel(
    #                                          selectInput('matchFuncODIM', 'Select match function', matchFuncs),
    #                                          selectInput('matchODIM', 'Select ODIM match ', ODIMMatches,selectize=FALSE, size=15),
    #                                          uiOutput("selectTeamODIM"),
    #                                          radioButtons("plotOrTableODIM", label = h4("Plot or table"),
    #                                                       choices = c("Plot" = 1, "Table" = 2),
    #                                                       selected = 1,inline=T)
    #
    #                                        ),
    #                                        mainPanel(
    #                                          uiOutput("plotOrPrintODIMMatch"),
    #                                          column(7, offset=4,
    #                                                 tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                                 tags$h5((tags$i("Feb 6, 2021"))),
    #                                                 tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                                 tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                          )
    #                                        )
    #                               ),
    #                               # Analyze Head-to-Head Mens ODI  matches
    #                               tabPanel("Head to head",
    #                                        h4('Head-to-head between Mens ODI teams'),
    #                                        sidebarPanel(
    #                                          selectInput('matches2TeamFuncODIM', 'Select function', matches2TeamsFuncs),
    #                                          selectInput('match2ODIM', 'Select matches', ODIMMatches2Teams,selectize=FALSE, size=13),
    #                                          uiOutput("selectTeam2ODIM"),
    #                                          radioButtons("plotOrTable1ODIM", label = h4("Plot or table"),
    #                                                       choices = c("Plot" = 1, "Table" = 2),
    #                                                       selected = 1,inline=T),
    #                                          radioButtons("repTypeODIM", label = h4("Report Type"),
    #                                                       choices = c("Summary" = 1, "Detailed" = 2),
    #                                                       selected = 1,inline=T)
    #
    #                                        ),
    #                                        mainPanel(
    #                                          uiOutput("plotOrPrintODIMMatch2teams"),
    #                                          column(7, offset=4,
    #                                                 tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                                 tags$h5((tags$i("Feb 6, 2021"))),
    #                                                 tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                                 tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                          )
    #                                        )
    #
    #                               ),
    #                               # Analyze ODI Men Team Overall Perf
    #                               tabPanel("Overall Performance",
    #                                        h4("Analyze ODI Men team's overall performance"),
    #                                        sidebarPanel(
    #                                          selectInput('overallperfFuncODIM', 'Select function', teamOverallPerfFunc),
    #                                          selectInput('teamMatchesODIM', 'Select the team', ODIMTeamsAll,selectize=FALSE, size=13),
    #                                          uiOutput("RankODIM"),
    #                                          radioButtons("plotOrTable2ODIM", label = h4("Plot or table"),
    #                                                       choices = c("Plot" = 1, "Table" = 2),
    #                                                       selected = 1,inline=T),
    #                                          radioButtons("repType2ODIM", label = h4("Report Type"),
    #                                                       choices = c("Summary" = 1, "Detailed" = 2),
    #                                                       selected = 1,inline=T)
    #                                        ),
    #                                        mainPanel(
    #                                          uiOutput('printOrPlotODIMTeamPerfoverall'),
    #                                          column(7, offset=4,
    #                                                 tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                                 tags$h5((tags$i("Feb 6, 2021"))),
    #                                                 tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                                 tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                          )
    #                                        )
    #
    #                               )
    #
    #                  )),

    ############################# ODI Women ################################
    # tabPanel("ODI Women",navbarPage("GooglyPlusPlus - One Day International (ODI) Women",
    #                           # Batsman tab
    #                           tabPanel("ODI Women batsman",
    #                                    h4('Analyze ODI batsman performances'),
    #                                    sidebarPanel(
    #                                      selectInput('batsmanFuncODIW', 'Select function', batsmanFuncs),
    #                                      selectInput('batsmanODIW', 'Select batsman', ODIWBatsmen,selectize=FALSE, size=20),
    #                                    ),
    #                                    mainPanel(
    #                                      plotOutput('batsmanPlotODIW'),
    #                                      column(7, offset=4,
    #                                             tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                             tags$h5((tags$i("Feb 6, 2021"))),
    #                                             tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                             tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                      )
    #                                    )
    #                           ),
    #                           # Bowlers tab
    #                           tabPanel("ODI Women bowlers",
    #
    #                                    h4('ODI  bowler performances'),
    #
    #                                    sidebarPanel(
    #                                      selectInput('bowlerFuncODIW', 'Select function', bowlerFuncs),
    #                                      selectInput('bowlerODIW', 'Select T20 bowler', ODIWBowlers,selectize=FALSE, size=20)
    #
    #
    #                                    ),
    #                                    mainPanel(
    #                                      plotOutput('bowlerPlotODIW'),
    #                                      column(7, offset=4,
    #                                             tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                             tags$h5((tags$i("Feb 6, 2021"))),
    #                                             tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                             tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                      )
    #                                    )
    #                           ),
    #                           tabPanel("ODI Womens  Match",
    #                                    h4('Analyze ODI Womens match'),
    #                                    sidebarPanel(
    #                                      selectInput('matchFuncODIW', 'Select match function', matchFuncs),
    #                                      selectInput('matchODIW', 'Select ODIW match ', ODIWMatches,selectize=FALSE, size=15),
    #                                      uiOutput("selectTeamODIW"),
    #                                      radioButtons("plotOrTableODIW", label = h4("Plot or table"),
    #                                                   choices = c("Plot" = 1, "Table" = 2),
    #                                                   selected = 1,inline=T)
    #
    #                                    ),
    #                                    mainPanel(
    #                                      uiOutput("plotOrPrintODIWMatch"),
    #                                      column(7, offset=4,
    #                                             tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                             tags$h5((tags$i("Feb 6, 2021"))),
    #                                             tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),,
    #                                             tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                      )
    #                                    )
    #                           ),
    #                           # Analyze Head-to-Head Womens ODI  matches
    #                           tabPanel("Head to head",
    #                                    h4('Head-to-head between Womens ODI teams'),
    #                                    sidebarPanel(
    #                                      selectInput('matches2TeamFuncODIW', 'Select function', matches2TeamsFuncs),
    #                                      selectInput('match2ODIW', 'Select matches', ODIWMatches2Teams,selectize=FALSE, size=13),
    #                                      uiOutput("selectTeam2ODIW"),
    #                                      radioButtons("plotOrTable1ODIW", label = h4("Plot or table"),
    #                                                   choices = c("Plot" = 1, "Table" = 2),
    #                                                   selected = 1,inline=T),
    #                                      radioButtons("repTypeODIW", label = h4("Report Type"),
    #                                                   choices = c("Summary" = 1, "Detailed" = 2),
    #                                                   selected = 1,inline=T)
    #
    #                                    ),
    #                                    mainPanel(
    #                                      uiOutput("plotOrPrintODIWMatch2teams"),
    #                                      column(7, offset=4,
    #                                             tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                             tags$h5((tags$i("Feb 6, 2021"))),
    #                                             tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                             tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                      )
    #                                    )
    #
    #                           ),
    #                           # Analyze ODI Women Team Overall Perf
    #                           tabPanel("Overall Performance",
    #                                    h4("Analyze ODI Women team's overall performance"),
    #                                    sidebarPanel(
    #                                      selectInput('overallperfFuncODIW', 'Select function', teamOverallPerfFunc),
    #                                      selectInput('teamMatchesODIW', 'Select the team', ODIWTeamsAll,selectize=FALSE, size=13),
    #                                      uiOutput("RankODIW"),
    #                                      radioButtons("plotOrTable2ODIW", label = h4("Plot or table"),
    #                                                   choices = c("Plot" = 1, "Table" = 2),
    #                                                   selected = 1,inline=T),
    #                                      radioButtons("repType2ODIW", label = h4("Report Type"),
    #                                                   choices = c("Summary" = 1, "Detailed" = 2),
    #                                                   selected = 1,inline=T)
    #                                    ),
    #                                    mainPanel(
    #                                      uiOutput('printOrPlotODIWTeamPerfoverall'),
    #                                      column(7, offset=4,
    #                                             tags$h5((tags$i("Designed and developed by Tinniam V Ganesh"))),
    #                                             tags$h5((tags$i("Feb 6, 2021"))),
    #                                             tags$a(href="https://cricsheet.org/", " Data source: Cricsheet"),
    #                                             tags$a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr")
    #                                      )
    #                                    )
    #
    #                            )
    #
    #
    # )),

    tabPanel("About GooglyPlusPlus 2021",h3("GooglyPlusPlus 2021 - Analyzing ODI,T20 Players, teams and matches with plots and tables"),
             p("This Shiny app is based on my R package 'yorkr'. In this Shiny app, I use  the
                    yorkr package to analyze the performances of cricketers,teams, matches."),
             p("GooglyPlusPlus an handle ODI (men),ODI (women), Intl T20 (men), Intl T20 (women), IPL, BBL, NTB, PSL,CPL and Women BBL"),
             p("The R package 'yorkr' has been authored by Tinniam V Ganesh for analyzing  performances of cricketers
                    teams, individudal match, head-to-head and overall team performances"),
             p("This Shiny app 'GooglyPlusPlus2021' has been designed and developed by  Tinniam V Ganesh,Jan 07,2021"),
             p("This data includes the all the latest data including IPL 2020, CPL 2020, NTB 2020, BBL 2020-21,PSL 2021"),
             p("Daily updates from IPL 2021"),
             p("The data for this Shiny app has been taken from Cricsheet - http://http://cricsheet.org/"),
             p("More details about this app and for other posts, see my blog
                    https://gigadom.in/"),
             a(href="https://cran.r-project.org/web/packages/yorkr/index.html", " Based on R package yorkr"),
             h5("Please see"),
             a(href="https://gigadom.in/2020/06/28/introducing-googlyplusplus/","How to use GooglyPlusPlus?"))


  )))
