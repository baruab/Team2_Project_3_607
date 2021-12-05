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
            menuItem("Resume Data Extraction", tabName = "dataExtraction", icon=icon("table")),
            menuItem("Data Visualization", tabName = "dataVisualization", icon=icon("table")),
            menuItem("Neo4J Graph", tabName = "neo4jGraph", icon=icon("table")),
            menuItem("Survey Q&A", tabName = "survey_qa", icon=icon("mortar-board")),
            menuItem("Source Code",  icon = icon("file-text-o"),
                     menuSubItem("global.R", tabName = "global", icon = icon("angle-right")),
                     menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                     menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
                     menuSubItem("main.R", tabName = "mainUI", icon = icon("angle-right")),
                     menuSubItem("neo4j_data_schema.txt", tabName = "neo4jSchema", icon = icon("angle-right"))
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
            tabItem("neo4jGraph",
                    fluidRow(
                      column(2, wellPanel(
                        radioButtons("neo4j_pics", "Industry/Companies/Jobs:",
                                     c("all_rels", "company_city","industry_companies")),
                        radioButtons("picture", "Jobs/Companies By City:",
                                     c("sf_jobs", "rc_jobs"))
                      )),
                      column(6,
                             uiOutput("image1"),
                             uiOutput("image2")
                      )
                    )
            ),
            tabItem("dataExtraction",
                    fluidRow(
                      column(12,
                             uiOutput("resumeUrlOutput")   
                      )),
                    tabsetPanel(
                      tabPanel(title = "Skills Table",
                               DT::dataTableOutput("resume_table")
                      ),tabPanel(title = "Skills Plot",
                                 plotOutput("plot")
                      )
                    )
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
            tabItem(tabName = "neo4jSchema",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="Neo4J Schema",
                         downloadButton('downloadData5', 'Download'),
                         br(),br(),
                         pre(includeText("neo4j_data_schema.txt"))
                    )
            ),
            tabItem(tabName = "dataVisualization",
                    fluidPage(
                      tags$iframe(src = "Final_Project_Visualizations.html",
                                  width = '100%', height = '800px', 
                                  frameborder = 0, scrolling = 'auto'
                      )
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