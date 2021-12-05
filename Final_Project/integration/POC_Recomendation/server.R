
#glassdoorfile<-"https://raw.githubusercontent.com/baruab/Team2_Project_3_607/main/Data_Job/glassdoor_datascience.csv"
#glassdoorfile<-"https://raw.githubusercontent.com/baruab/Team2_Project_3_607/main/glassdoor_datascience_updated.csv"

# glassdoorfile<-"https://raw.githubusercontent.com/baruab/Team2_Project_3_607/main/job_posting_updated.csv"
glassdoorfile<-"https://raw.githubusercontent.com/baruab/Team2_Project_3_607/main/job_posting.csv"
glassdoor_raw<-read_csv(url(glassdoorfile))
glassdoor_df  <- as.data.frame(glassdoor_raw)

sel_glassdoor_df  <-  glassdoor_df  %>% select(job_title, city, state, max_salary, company_name, company_industry, company_rating, bachelors, masters, phd) 
sel_glassdoor_df$job_title <- trimws(sel_glassdoor_df$job_title)

###########################################################################

# Load the data from csv
url<-"https://raw.githubusercontent.com/baruab/msdsrepo/main/Project_3_607/kaggle-survey-2018/multipleChoiceResponses.csv"
survey_tib <- read.csv(file=url(url), sep=",")

df = data.frame(id =c('Q11','Q12','Q13','Q14','Q15','Q16','Q17','Q18','Q19','Q20','Q21','Q22','Q23','Q24','Q25','Q26','Q27','Q28','Q29','Q30','Q31','Q32','Q33','Q34','Q35','Q36','Q37','Q38','Q39','Q40','Q41','Q42','Q43','Q44','Q45','Q46','Q47','Q48','Q49','Q50'),
                desc = c( " Q11	Select any activities that make up an important part of your role at work",
                          " Q12	What is the primary tool that you use at work or school to analyze data?",
                          " Q13	Which of the following integrated development environments",
                          " Q14	Which of the following hosted notebooks have you used at work or school in the last 5 years?",
                          " Q15	Which of the following cloud computing services have you used at work or school in the last 5 years?",
                          " Q16	What programming languages do you use on a regular basis?",
                          " Q17	What specific programming language do you use most often? - Selected Choice",
                          
                          " Q18	What programming language would you recommend an aspiring data scientist to learn first? - Selected Choice",
                          " Q19	What machine learning frameworks have you used in the past 5 years?",
                          " Q20	Of the choices that you selected in the previous question, which ML library have you used the most? - Selected Choice",
                          
                          " Q21	What data visualization libraries or tools have you used in the past 5 years?",
                          " Q22	Of the choices that you selected in the previous question, which specific data visualization library or tool have you used the most? - Selected Choice",
                          " Q23	Approximately what percent of your time at work or school is spent actively coding?",
                          " Q24	How long have you been writing code to analyze data?",
                          " Q25	For how many years have you used machine learning methods",
                          " Q26	Do you consider yourself to be a data scientist?",
                          " Q27	Which of the following cloud computing products have you used at work or school in the last 5 years",
                          " Q28	Which of the following machine learning products have you used at work or school in the last 5 years?",
                          " Q29	Which of the following relational database products have you used at work or school in the last 5 years?",
                          " Q30	Which of the following big data and analytics products have you used at work or school in the last 5 years?",
                          " Q31	Which types of data do you currently interact with most often at work or school?",
                          " Q32	What is the type of data that you currently interact with most often at work or school? - Selected Choice",
                          " Q33	Where do you find public datasets?",
                          " Q34	During a typical data science project at work or school, approximately what proportion of your time is devoted to the following?",
                          " Q35	What percentage of your current machine learning/data science training falls under each category?",
                          " Q36	On which online platforms have you begun or completed data science courses?",
                          " Q37	On which online platform have you spent the most amount of time? - Selected Choice",
                          " Q38	Who/what are your favorite media sources that report on data science topics?",
                          " Q39	How do you perceive the quality of online learning platforms and in-person bootcamps as compared to the quality of the education provided by traditional brick and mortar institutions? - Online learning platforms and MOOCs:",
                          " Q40	Which better demonstrates expertise in data science: academic achievements or independent projects? - Your views:",
                          " Q41	How do you perceive the importance of the following topics? - Fairness and bias in ML algorithms:",
                          " Q42	What metrics do you or your organization use to determine whether or not your models were successful?",
                          " Q43	Approximately what percent of your data projects involved exploring unfair bias in the dataset and/or algorithm?",
                          " Q44	What do you find most difficult about ensuring that your algorithms are fair and unbiased?",
                          " Q45	In what circumstances would you explore model insights and interpret your model's predictions?",
                          " Q46	Approximately what percent of your data projects involve exploring model insights?",
                          " Q47	What methods do you prefer for explaining and/or interpreting decisions that are made by ML models?",
                          " Q48	Do you consider ML models to be black boxes with outputs that are difficult or impossible to explain?",
                          " Q49	What tools and methods do you use to make your work easy to reproduce?",
                          " Q50	What barriers prevent you from making your work even easier to reuse and reproduce?") )


datasciencetools<- function(ds,question){
  QuestionCode<-c('Q11','Q12','Q13','Q14','Q15','Q16','Q17','Q18','Q19','Q20','Q21','Q22','Q23','Q24','Q25','Q26','Q27','Q28','Q29','Q30','Q31','Q32','Q33','Q34','Q35','Q36','Q37','Q38','Q39','Q40','Q41','Q42','Q43','Q44','Q45','Q46','Q47','Q48','Q49','Q50')
  if (question %in% QuestionCode){
    names(ds) <- paste(names(ds),ds[1,],sep="_")
    #Remove row 1
    ds <- ds[-c(1),]
    ### Select only Data Scientist in the United States of America
    ds <- ds %>%
      filter((`Q3_In which country do you currently reside?`=='United States of America')&(`Q6_Select the title most similar to your current role (or most recent title if retired): - Selected Choice`=='Data Scientist'))
    ds_tool<- ds %>% select(starts_with(question),-contains("OTHER_TEXT")) %>%
      pivot_longer(starts_with(question),names_to="ToolName",values_to="Tool")
    ds_tool_tib <- ds_tool %>%
      count(ds_tool$Tool) %>% rename("VisualTools" = "ds_tool$Tool","Count"="n")
    ds_tool_tib <- ds_tool_tib%>%filter(!(VisualTools==""))%>%
      arrange(desc(Count))
    prop_n_count<-ds_tool_tib %>%
      mutate(proportion = round((Count /sum(Count))*100,2))%>% arrange(desc(proportion))
    # print(prop_n_count)
    prop_n_count_view <- prop_n_count %>% ggplot(aes(reorder(VisualTools,proportion),proportion)) +
      geom_col()+coord_flip()+geom_col(fill="#A7ADBE")+
      geom_text(aes(label=proportion),color="red") +labs(x="Tools/Technique")+theme_bw()
    return(prop_n_count_view)
  }else{
    print("Choose correct question code")
  }
}
############################################################################
# Create the data frame.


#Read in csv
skill_income <- read.csv("https://raw.githubusercontent.com/catfoodlover/Data607/main/skills_income.csv", stringsAsFactors = FALSE)



resume_url_df <- data.frame(
  name = c("https://www.postjobfree.com/resume/adol8d/data-scientist-new-york-ny",
           "https://www.postjobfree.com/resume/adktqz/senior-data-scientist-brooklyn-ny",
           "https://www.postjobfree.com/resume/adk07o/data-science-new-york-ny",
           "https://www.postjobfree.com/resume/adol8d/data-scientist-new-york-ny",
           "https://www.postjobfree.com/resume/adost3/data-scientist-new-york-ny",
           "https://www.postjobfree.com/resume/adonl3/data-scientist-charlotte-nc",
           "https://www.postjobfree.com/resume/ado61j/data-scientist-arlington-va"),
  
  stringsAsFactors = FALSE
)


url<-"https://www.postjobfree.com/resume/adol8d/data-scientist-new-york-ny"
#url<-"https://www.postjobfree.com/resume/ador9o/data-scientist-san-francisco-ca"

resume<-function(obj){
  key_clean<-c( "jupyter/ipython", "jupyter",  "rstudio",   "pycharm",  "visual studio code",  "nteract", "atom", "matlab", "visual studio",   "notepad++","sublime text", "vim", "intellij", "spyder", "kaggle kernels", "google colab", "azure notebook",  "domino datalab","google cloud datalab", "paperspace", "floydhub", "crestle", "jupyterhub/binder", "google cloud platform (gcp)",   "amazon web services (aws)",   "microsoft azure",   "ibm cloud",   "alibaba cloud", "python", "r","sql","bash","java", "javascript/typescript", "visual basic/vba", "c/c++", "scala","go", "c#/.net", "php",  "ruby", "sas/stata",  "scikit-learn",  "tensorflow",  "keras", "pytorch","spark mllib", "h20", "fastai", "mxnet", "caret",  "xgboost", "mlr", "prophet", "randomforest", "lightgbm", "cntk", "caffe", "ggplot2",  "matplotlib", "altair", "shiny", "d3", "plotly", "bokeh", "seaborn", "geoplotlib", "leaflet",  "lattice",  "aws elastic compute cloud (ec2)",  "google compute engine", "aws elastic beanstalk", "google app engine", "google kubernetes engine", "aws lambda", "google cloud functions","aws batch",  "azure virtual machines", "azure container service",  "azure functions",  "azure event grid",  "azure batch", "azure kubernetes service", "ibm cloud virtual servers", "ibm cloud container registry",   "ibm cloud kubernetes service", "ibm cloud foundry",  "amazon transcribe", "google cloud speech-to-text api",  "amazon rekognition", "google cloud vision api", "amazon comprehend", "google cloud natural language api", "amazon translate","google cloud translation api", "amazon lex", "google dialogflow enterprise edition",  "amazon rekognition video", "google cloud video intelligence api",  "google cloud automl", "amazon sagemaker","google cloud machine learning engine",  "datarobot",   "h20 driverless ai", "sas", "dataiku",  "rapidminer",  "instabase", "algorithmia", "dataversity", "cloudera", "azure machine learning studio", "azure machine learning workbench", "azure cortana intelligence suite", "azure bing speech api",  "azure speaker recognition api","azure computer vision api",  "azure face api", "azure video api", "ibm watson studio", "ibm watson knowledge catalog",  "ibm watson assistant", "ibm watson discovery", "ibm watson text to speech", "ibm watson visual recognition",  "ibm watson machine learning", "azure cognitive services",  "aws relational database service", "aws aurora",  "google cloud sql","google cloud spanner", "aws dynamodb",  "google cloud datastore", "google cloud bigtable", "aws simpledb", "microsoft sql server", "mysql","postgressql", "sqlite",  "oracle database","ingres", "microsoft access", "nexusdb",  "sap iq", "google fusion tables", "azure database for mysql",   "azure cosmos db", "azure sql database","azure database for postgresql",  "postgresql", "ibm cloud compose",  "ibm cloud compose for mysql",  "ibm cloud compose for postgresql", "ibm cloud db2",  "aws elastic mapreduce", "google cloud dataproc", "google cloud dataflow",   "google cloud dataprep", "aws kinesis", "google cloud pub/sub", "aws athena", "aws redshift", "google bigquery", "teradata",  "microsoft analysis services",  "oracle exadata",  "oracle warehouse builder", "snowflake","databricks",     "azure sql data warehouse",  "azure hdinsight", "azure stream analytics","ibm infosphere datastorage","ibm cloud analytics engine", "ibm cloud streaming analytics","audio data", "categorical data", "genetic data", "geospatial data", "image data", "numerical data", "sensor data","tabular data", "text data", "time series data", "video data", "government websites", "university research group websites", "non-profit research group websites", "dataset aggregator/platform (socrata, kaggle public datasets platform, etc.)", "i collect my own data (web-scraping, etc.)",  "publicly released data from private companies", "google search",  "google dataset search", "github","git")
  obj <-as_tibble(obj) %>%
    mutate(resume = row_number())
  wd<-obj %>% unnest_tokens(word, value,token = stringr::str_split, pattern = "[,;]")
  #wd
  pt<-wd$word
  tools<-as_tibble(pt)
  res_tools<-tools%>%mutate(tool = trimws(str_replace_all(str_replace_all(unlist(tools),"[&():>]",""),"and","")))
  resume_tools<-trimws(gsub(".*:","",res_tools$value))
  resume_tools<-trimws(gsub("[.*:]","",resume_tools))
  resume_tools<-trimws(gsub("and","",resume_tools))
  resume_tools<-str_replace_all(resume_tools,"[()]","")
  resume_tools<-str_replace_all(resume_tools,"aws","amazon web services (aws)")
  resume_tools<-str_replace_all(resume_tools,"r shiny","shiny")
  resume_tools<-unique(str_replace_all(resume_tools,"r-programing","r"))
  df = list(ResumeTools=resume_tools, DataSciTools=key_clean)
  
  attributes(df) = list(names = names(df),
                        row.names=1:max(length(resume_tools), length(key_clean)), class='data.frame')
  
  df<-df%>%
    dplyr::mutate(flag=as.integer(df$DataSciTools %in% df$ResumeTools),TotalTool=sum(flag))
  print(unique(df$TotalTool))
  
  vec<-c(unique(df$TotalTool), length(key_clean))
#  barplot(vec)
  
#  t<-subset(df,flag==1)%>%select(DataSciTools)
  t <- subset(df, flag == 1) %>% select(DataSciTools) %>% as.data.frame()
  row.names(t) <- NULL
# print(t)
#  resume_df <- df%>% select(c(DataSciTools,flag))
#  res<-resume_df%>%
#    pivot_wider(names_from = DataSciTools, values_from = flag)
# res_view <- res %>% ggplot(aes(res))
#  return (res)
  return(t)
  
}


###########################################################################

function(input, output, session) {

  ##### Switch Views ------------------
  # if user click link to register, go to register view
  observeEvent(input$go_to_register, {
    shinyjs::show("register_panel", anim = TRUE, animType = "fade")
    shinyjs::hide("sign_in_panel")
  }, ignoreInit = TRUE)

  observeEvent(input$go_to_sign_in, {
    shinyjs::hide("register_panel")
    shinyjs::show("sign_in_panel", anim = TRUE, animType = "fade")
  }, ignoreInit = TRUE)

  # switch between auth sign in/registration and app for signed in user
  observeEvent(session$userData$current_user(), {
    current_user <- session$userData$current_user()

    if (is.null(current_user)) {
      shinyjs::show("sign_in_panel")
      shinyjs::hide("main")
      shinyjs::hide("verify_email_view")
    } else {
      shinyjs::hide("sign_in_panel")
      shinyjs::hide("register_panel")

      if (current_user$emailVerified == TRUE) {
       shinyjs::show("main")
      } else {
        shinyjs::show("verify_email_view")
      }
    }

  }, ignoreNULL = FALSE)



  # Signed in user --------------------
  # the `session$userData$current_user()` reactiveVal will hold information about the user
  # that has signed in through Firebase.  A value of NULL will be used if the user is not
  # signed in
  session$userData$current_user <- reactiveVal(NULL)

  # input$sof_auth_user comes from front end js in "www/sof-auth.js"
  observeEvent(input$sof_auth_user, {

    # set the signed in user
    session$userData$current_user(input$sof_auth_user)

  }, ignoreNULL = FALSE)



  ##### App for signed in user
  signed_in_user_df <- reactive({
    req(session$userData$current_user())

    out <- session$userData$current_user()
    out <- unlist(out)

    data.frame(
      name = names(out),
      value = unname(out)
    )
  })


  output$user_out <- DT::renderDT({
    datatable(
      signed_in_user_df(),
      rownames = FALSE,
      options = list(
        dom = "tp",
        scrollX = TRUE
      )
    )
  })
  
  
  
  output$jobTitleOutput <- renderUI({
    selectInput("jobTitleInput", "Job Title",
                sort(unique(trimws(sel_glassdoor_df$job_title)) ),
                selected = "")
  })
  
  output$jobLocationOutput <- renderUI({
    selectInput("jobLocationInput", "Job Location",
                sort(unique(trimws(sel_glassdoor_df$city)) ),
                selected = "")
  })

  output$resumeUrlOutput <- renderUI({
    selectInput("resumeUrlInput", "Resume URL",
                sort(resume_url_df$name ), 
                width="80%",
                selected = "")
  })
  
  newman <- reactive({
      web <- read_html(input$resumeUrlInput)
      raw_resume<-web %>%html_nodes(".normalText")%>%html_text()
      resume(raw_resume)
  })
  
  
  ### Plot tab (State) ####
  output$distPlot <- renderPlot({
    # Location based
    ggplot(sel_glassdoor_df, aes(x = state)) +
      theme(axis.text.x=element_text(angle=90,hjust=1)) + 
      geom_bar() 
  })
  
  ### Bar Chart tab (company_industry) ####
  output$barPlot <- renderPlot({
    # Job Title based
    ggplot(sel_glassdoor_df, aes(x = company_industry)) +
      theme(axis.text.x=element_text(angle=90,hjust=1)) + 
      geom_bar() 
  })
  
  ### Filter Job Location tab ####
  output$joblocation_table <- renderTable({
    sel_glassdoor_df  %>% filter(
      city == trimws(input$jobLocationInput))
  })
  
  ### Table tab ####
  output$jobtitle_table <- renderTable({
    sel_glassdoor_df  %>% filter(
      job_title == trimws(input$jobTitleInput))
  })
  
  
  
  ### Resume Table tab ####
  output$resume_table <-  DT::renderDataTable({
 #   res <- resume(read_html(input$resumeUrlInput) %>%html_nodes(".normalText")%>%html_text())
 #   DT::datatable(res)
    DT::datatable(newman())
  })

  ### Graph tab ####
  output$graph_video <- renderUI({ 
    tags$video(id="video2", type = "video/mp4",src = "graph_video.mp4",  width = "1080px", height = "480px", controls = "controls") 
  })
  
  #### Survey Q & A ##########
  
  
  output$questionOutput <- renderUI({
    selectInput("questionInput", "Question",
                setNames(df$id, df$desc), width="70%",
                selected = NULL)
  }) 
  
  output$qaPlot <- renderPlot({
    datasciencetools(survey_tib,input$questionInput)
   
  })
  
  
  # image2 sends pre-rendered images
  output$image1 <- renderUI({
    
    if (is.null(input$neo4j_pics))
      return(NULL)
    
    if (input$neo4j_pics == "all_rels") {
      tags$img(src= "images/AutoDesk_Relationships.png")
    } else if (input$neo4j_pics == "company_city") {
      tags$img(src= "images/COMPANY_OFFICE_IN.png")
    } else if (input$neo4j_pics == "industry_companies") {
      tags$img(src= "images/INDUSTRY_COMPANY_MATCH.jpg")
    }
    })
    

  # image2 sends pre-rendered images
  output$image2 <- renderUI({
    if (is.null(input$picture))
      return(NULL)
    
    if (input$picture == "sf_jobs") {
        tags$img(src= "images/sf_jobs.png")
    } else if (input$picture == "rc_jobs") {
      tags$img(src= "images/rc_jobs.png")
    }})
  #################
  
  
  show_plot <- reactive({
#    inFile <- input$target_upload
#    if (is.null(inFile))
#      return(NULL)
    
    resume2 <- function(obj){
      key_clean <-
        c(
          "jupyter/ipython",
          "jupyter",
          "rstudio",
          "pycharm",
          "visual studio code",
          "nteract",
          "atom",
          "matlab",
          "visual studio",
          "notepad++",
          "sublime text",
          "vim",
          "intellij",
          "spyder",
          "kaggle kernels",
          "google colab",
          "azure notebook",
          "domino datalab",
          "google cloud datalab",
          "paperspace",
          "floydhub",
          "crestle",
          "jupyterhub/binder",
          "google cloud platform (gcp)",
          "amazon web services (aws)",
          "microsoft azure",
          "ibm cloud",
          "alibaba cloud",
          "python",
          "r",
          "sql",
          "bash",
          "java",
          "javascript/typescript",
          "visual basic/vba",
          "c/c++",
          "scala",
          "go",
          "c#/.net",
          "php",
          "ruby",
          "sas/stata",
          "scikit-learn",
          "tensorflow",
          "keras",
          "pytorch",
          "spark mllib",
          "h20",
          "fastai",
          "mxnet",
          "caret",
          "xgboost",
          "mlr",
          "prophet",
          "randomforest",
          "lightgbm",
          "cntk",
          "caffe",
          "ggplot2",
          "matplotlib",
          "altair",
          "shiny",
          "d3",
          "plotly",
          "bokeh",
          "seaborn",
          "geoplotlib",
          "leaflet",
          "lattice",
          "aws elastic compute cloud (ec2)",
          "google compute engine",
          "aws elastic beanstalk",
          "google app engine",
          "google kubernetes engine",
          "aws lambda",
          "google cloud functions",
          "aws batch",
          "azure virtual machines",
          "azure container service",
          "azure functions",
          "azure event grid",
          "azure batch",
          "azure kubernetes service",
          "ibm cloud virtual servers",
          "ibm cloud container registry",
          "ibm cloud kubernetes service",
          "ibm cloud foundry",
          "amazon transcribe",
          "google cloud speech-to-text api",
          "amazon rekognition",
          "google cloud vision api",
          "amazon comprehend",
          "google cloud natural language api",
          "amazon translate",
          "google cloud translation api",
          "amazon lex",
          "google dialogflow enterprise edition",
          "amazon rekognition video",
          "google cloud video intelligence api",
          "google cloud automl",
          "amazon sagemaker",
          "google cloud machine learning engine",
          "datarobot",
          "h20 driverless ai",
          "sas",
          "dataiku",
          "rapidminer",
          "instabase",
          "algorithmia",
          "dataversity",
          "cloudera",
          "azure machine learning studio",
          "azure machine learning workbench",
          "azure cortana intelligence suite",
          "azure bing speech api",
          "azure speaker recognition api",
          "azure computer vision api",
          "azure face api",
          "azure video api",
          "ibm watson studio",
          "ibm watson knowledge catalog",
          "ibm watson assistant",
          "ibm watson discovery",
          "ibm watson text to speech",
          "ibm watson visual recognition",
          "ibm watson machine learning",
          "azure cognitive services",
          "aws relational database service",
          "aws aurora",
          "google cloud sql",
          "google cloud spanner",
          "aws dynamodb",
          "google cloud datastore",
          "google cloud bigtable",
          "aws simpledb",
          "microsoft sql server",
          "mysql",
          "postgressql",
          "sqlite",
          "oracle database",
          "ingres",
          "microsoft access",
          "nexusdb",
          "sap iq",
          "google fusion tables",
          "azure database for mysql",
          "azure cosmos db",
          "azure sql database",
          "azure database for postgresql",
          "postgresql",
          "ibm cloud compose",
          "ibm cloud compose for mysql",
          "ibm cloud compose for postgresql",
          "ibm cloud db2",
          "aws elastic mapreduce",
          "google cloud dataproc",
          "google cloud dataflow",
          "google cloud dataprep",
          "aws kinesis",
          "google cloud pub/sub",
          "aws athena",
          "aws redshift",
          "google bigquery",
          "teradata",
          "microsoft analysis services",
          "oracle exadata",
          "oracle warehouse builder",
          "snowflake",
          "databricks",
          "azure sql data warehouse",
          "azure hdinsight",
          "azure stream analytics",
          "ibm infosphere datastorage",
          "ibm cloud analytics engine",
          "ibm cloud streaming analytics",
          "audio data",
          "categorical data",
          "genetic data",
          "geospatial data",
          "image data",
          "numerical data",
          "sensor data",
          "tabular data",
          "text data",
          "time series data",
          "video data",
          "government websites",
          "university research group websites",
          "non-profit research group websites",
          "dataset aggregator/platform (socrata, kaggle public datasets platform, etc.)",
          "i collect my own data (web-scraping, etc.)",
          "publicly released data from private companies",
          "google search",
          "google dataset search",
          "github",
          "git"
        )
      obj <- as_tibble(obj) %>%
        mutate(resume = row_number())
      wd <-
        obj %>% unnest_tokens(word,
                              value,
                              token = stringr::str_split,
                              pattern = "[,;]")
      pt <- wd$word
      tools <- as_tibble(pt)
      res_tools <-
        tools %>% mutate(tool = trimws(str_replace_all(
          str_replace_all(unlist(tools), "[&():>]", ""), "and", ""
        )))
      resume_tools <- trimws(gsub(".*:", "", res_tools$value))
      resume_tools <- trimws(gsub("[.*:]", "", resume_tools))
      resume_tools <- trimws(gsub("and", "", resume_tools))
      resume_tools <- str_replace_all(resume_tools, "[()]", "")
      resume_tools <-
        str_replace_all(resume_tools, "aws", "amazon web services (aws)")
      resume_tools <- str_replace_all(resume_tools, "r shiny", "shiny")
      resume_tools <-
        unique(str_replace_all(resume_tools, "r-programing", "r"))
      df = list(ResumeTools = resume_tools, DataSciTools = key_clean)
      attributes(df) = list(
        names = names(df),
        row.names = 1:max(length(resume_tools), length(key_clean)),
        class = 'data.frame'
      )
      df <- df %>%
        dplyr::mutate(
          flag = as.integer(df$DataSciTools %in% df$ResumeTools),
          TotalTool = sum(flag)
        )
      t <- df$TotalTool %>% unique()
      return(t)
    }
    
#    obj <- read.csv(inFile$datapath)
#    obj <- unlist(obj[2])
    web <- read_html(input$resumeUrlInput)
    raw_resume<-web %>%html_nodes(".normalText")%>%html_text()
    
    obj <- resume2(raw_resume)
    salary <- 123887.3 + 721.8*obj
    title <- paste0('With ',obj,' skills expect to make $',salary)
    return(ggplot2::ggplot(skill_income, aes(x = num_skills, y = income)) +
             geom_point() +
             geom_smooth(method = 'lm') +
             geom_point(aes(x = obj, y = (123887.3 + 721.8 * obj)), colour =
                          "red") +
             labs(title = title) +
             theme(plot.title = element_text(face = "bold")))
  })
  
  output$plot <- renderPlot({
    show_plot()
  })
  
}
