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
        dashboardHeader(title = "DS Recommender"),
        dashboardSidebar(
         
          sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"),
            menuItem("Data Extraction", tabName = "dataExtraction"),
            menuItem("Survey Q&A", tabName = "survey_qa")
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
            )
          )
        )
      )
    )
  )
))