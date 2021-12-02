
#glassdoorfile<-"https://raw.githubusercontent.com/baruab/Team2_Project_3_607/main/Data_Job/glassdoor_datascience.csv"
#glassdoorfile<-"https://raw.githubusercontent.com/baruab/Team2_Project_3_607/main/glassdoor_datascience_updated.csv"

glassdoorfile<-"https://raw.githubusercontent.com/baruab/Team2_Project_3_607/main/job_posting_updated.csv"
glassdoor_raw<-read_csv(url(glassdoorfile))
glassdoor_df  <- as.data.frame(glassdoor_raw)

sel_glassdoor_df  <-  glassdoor_df  %>% select(job_title, city, state, max_salary, company_name, company_industry, company_rating, bachelors, masters, phd) 
sel_glassdoor_df$job_title <- trimws(sel_glassdoor_df$job_title)

###########################################################################

# Load the data from csv
#url<-"https://raw.githubusercontent.com/baruab/msdsrepo/main/Project_3_607/kaggle-survey-2018/multipleChoiceResponses.csv"
#survey_tib <- read.csv(file=url(url), sep=",")

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

  
  filtered <- reactive({ 
    if (is.null(input$jobTitleInput)) {
      return(NULL)
    }
    
    sel_glassdoor_df  %>% filter(
      job_title == trimws(input$jobTitleInput),
      city == trimws(input$jobLocationInput)
    )
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
  
  ### Graph tab ####
  output$graph_video <- renderUI({ 
    tags$video(id="video2", type = "video/mp4",src = "graph_video.mp4",  width = "640px", height = "480px", controls = "controls") 
  })
  
  #### Survey Q & A ##########
  
  
  output$questionOutput <- renderUI({
    selectInput("questionInput", "Question",
                setNames(df$id, df$desc) ,
                selected = NULL)
  }) 
  
  output$qaPlot <- renderPlot({
    datasciencetools(survey_tib,input$questionInput)
   
  })
  
  #################
}
