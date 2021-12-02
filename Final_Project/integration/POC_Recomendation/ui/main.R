hidden(fluidRow(
  id = "main",
  column(
    12,
    tags$button(
      id = "submit_sign_out",
      type = "button",
      "Sign Out",
      class = "btn-danger pull-right",
      style = "color: white;"
    )
  ),
  column(
    12,
    div(
      dashboardPage(
        dashboardHeader(title = "POC Recommender"),
        dashboardSidebar(
         
          sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon=icon("line-chart"), selected=TRUE),
            menuItem("Data Extraction", tabName = "dataExtraction", icon=icon("table")),
            menuItem("Survey Q&A", tabName = "survey_qa", icon=icon("mortar-board")),
            menuItem("Codes",  icon = icon("file-text-o"),
                     menuSubItem("global.R", tabName = "global", icon = icon("angle-right")),
                     menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                     menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
                     menuSubItem("main.R", tabName = "mainUI", icon = icon("angle-right"))
            ),
            menuItem("About", tabName = "about", icon = icon("question"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem("dashboard",
                    fluidRow(
                      column(5,
                             uiOutput("jobLocationOutput")    
                      ),
                      column(4,
                             uiOutput("jobTitleOutput")    
                      )
                    ),
                    # Show a plot of the generated distribution
                    tabsetPanel(
                      tabPanel(title = "Jobs By State",
                               plotOutput("distPlot")
                      ),tabPanel(title = "Jobs by Industry",
                                 plotOutput("barPlot")
                      ),
                      tabPanel(title = "Filter Jobs By Location",
                               tableOutput("joblocation_table")
                      ),
                      tabPanel(title = "Filter Jobs By Title",
                               tableOutput("jobtitle_table")
                      ),
                      tabPanel(title = "Graph Video",
                               uiOutput("graph_video")
                      )
                    )
            ),
            tabItem("survey_qa",
                    fluidRow(
                      column(12,
                             uiOutput("questionOutput")   
                      )),
                   
                    plotOutput("qaPlot")
            ),
            
            tabItem("dataExtraction",
                    fluidRow(
                      column(12,
                             uiOutput("resumeUrlOutput")   
                      )),
                    
                    plotOutput("resumeKeywordPlot")
            ),
            #############################################################
            tabItem(tabName = "global",
                    box(width = NULL, status = "primary", solidHeader = TRUE, title= "global.R",
                        downloadButton('downloadData1', 'Download'),
                        br(),br(),
                        pre(includeText("global.R"))
                        
                    )
            ),
            tabItem(tabName = "ui",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                         downloadButton('downloadData2', 'Download'),
                         br(),br(),
                         pre(includeText("ui.R"))
                    )
            ),
            tabItem(tabName = "server",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                         downloadButton('downloadData3', 'Download'),
                         br(),br(),
                         pre(includeText("server.R"))
                    )
            ),
            tabItem(tabName = "mainUI",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="main.R",
                         downloadButton('downloadData4', 'Download'),
                         br(),br(),
                         pre(includeText("ui/main.R"))
                    )
            ),
            tabItem(tabName = "about",
                    fluidPage(
                      
                     # HTML("<iframe width=\"960\" height=\"680\" 
                     #             src=\"https://rpubs.com/bikrambarua/Final_Project_Layout\" 
                     #             frameborder=\"0\"></iframe>")
                      tags$iframe(src = "Final_Project_Layout.html",
                                  width = '100%', height = '800px', 
                                 frameborder = 0, scrolling = 'auto'
                       )
                    )
            )
            ########################################
          )
        )
      )
    )
  )
))