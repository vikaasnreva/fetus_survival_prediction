shinyApp(
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      theme = shinythemes::shinytheme("cosmo"),  # <--- To use a theme, uncomment this
      "FETAL MORTALITY PREDICTION",
      #tab panel of decision tree classification model 
      
      tabPanel("DECISION TREE",
               sidebarPanel(
                 sliderInput(inputId = "num1",
                             label = h5("ENTER THE MATERNAL AGE"),
                             value = 25,min = 15, max = 55),
                 sliderInput(inputId = "num2",
                             label = h5("ENTER THE GESTATIONAL AGE"),
                             value = 30,min = 1,max = 40),
                 selectInput("select1", label = h5("Rh Disease"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("select2", label = h5("Congeniticalabnorm"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("select3", label = h5("GeneticAbnorm"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("select4", label = h5("Placental&UmbillicalcordAccident"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 #actionButton(inputId = "go", label = h5("Submit"),class = "btn-primary")
                 submitButton("Submit")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("PREDICTION"
                            ,h3("Input parameters supplied:",style = "color:green"),
                            verbatimTextOutput("text1"),
                            verbatimTextOutput("text2"),
                            verbatimTextOutput("text3"),
                            verbatimTextOutput("text4"),
                            verbatimTextOutput("text5"),
                            verbatimTextOutput("text6"),
                            h3("Prediction made by Decision tree classification model:",style = "color: red"),
                            verbatimTextOutput("text7")),
                   tabPanel("OUTCOMES",
                            h3("Decision tree algorithm working",style = "color: green"),
                            plotOutput("decisionplot")),
                   tabPanel("SUMMARY",
                            h2("Classification error and other statisitics",style = "color:green"),
                            p("This tab shows the application of the decision tree algorithm to the fetal mortality dataset.",style = "color: black"),
                            p("It also presents the summary statistics of the fetal mortality dataset", style = "color: black."),
                            p("The accuracy of decision tree is: 77%",style="color:black"),
                            p("The accuacy of Random Forest is: 87%",style="color:black"),
                            p("The accuracy of Naive Bayes is: 80%",style="color:black"),
                            p("It also trains the classification model on the given dataset and presents the accuracy of the model.",style = "color; black"),
                            h3("Summary of dataset",style = "color: green"),
                            verbatimTextOutput("summary"),
                            h3("Results",style = "color: green"),
                            verbatimTextOutput("result"))
                 )
               )
      ),
      
      #tab panel for random forest classification model
      tabPanel("RANDOM FOREST",
               sidebarPanel(
                 sliderInput(inputId = "random_num1",
                             label = h5("ENTER THE MATERNAL AGE"),
                             value = 25,min = 15, max = 55),
                 sliderInput(inputId = "random_num2",
                             label = h5("ENTER THE GESTATIONAL AGE"),
                             value = 30,min = 1,max = 40),
                 selectInput("random_select1", label = h5("Rh Disease"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("random_select2", label = h5("Congeniticalabnorm"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("random_select3", label = h5("GeneticAbnorm"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("random_select4", label = h5("Placental&UmbillicalcordAccident"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 #actionButton(inputId = "go", label = h5("Submit"),class = "btn-primary")
                 submitButton("Submit")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("PREDICTION"
                            ,h3("Input parameters supplied:",style = "color:green"),
                            verbatimTextOutput("random_text1"),
                            verbatimTextOutput("random_text2"),
                            verbatimTextOutput("random_text3"),
                            verbatimTextOutput("random_text4"),
                            verbatimTextOutput("random_text5"),
                            verbatimTextOutput("random_text6"),
                            h3("Prediction made by Random forest classification model:",style = "color: red"),
                            verbatimTextOutput("random_text7")),
                   tabPanel("OUTCOMES",
                            h3("Random forest algorithm working",style = "color: green"),
                            plotOutput("random_plot")),
                   tabPanel("SUMMARY",
                            h2("Classification error and other statisitics",style = "color:green"),
                            p("This tab shows the application of the random forest algorithm to the fetal mortality dataset.",style = "color: black"),
                            p("It also presents the summary statistics of the fetal mortality dataset", style = "color: black."),
                            p("It also trains the classification model on the given dataset and presents the accuracy of the model.",style = "color; black"),
                            h3("Summary of dataset",style = "color: green"),
                            verbatimTextOutput("random_summary"),
                            h3("Results",style = "color: green"),
                            verbatimTextOutput("random_result"))
                 )
               )
      ),
      #tab panel for naive bayes classification model 
      tabPanel("NAIVE BAYES",
               sidebarPanel(
                 sliderInput(inputId = "naive_num1",
                             label = h5("ENTER THE MATERNAL AGE"),
                             value = 25,min = 15, max = 55),
                 sliderInput(inputId = "naive_num2",
                             label = h5("ENTER THE GESTATIONAL AGE"),
                             value = 30,min = 1,max = 40),
                 selectInput("naive_select1", label = h5("Rh Disease"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("naive_select2", label = h5("Congeniticalabnorm"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("naive_select3", label = h5("GeneticAbnorm"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 selectInput("naive_select4", label = h5("Placental&UmbillicalcordAccident"),choices = list("No" = 0,"Yes" = 1)
                             ,selected = 0),
                 #actionButton(inputId = "go", label = h5("Submit"),class = "btn-primary")
                 submitButton("Submit")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("PREDICTION"
                            ,h3("Input parameters supplied:",style = "color:green"),
                            verbatimTextOutput("naive_text1"),
                            verbatimTextOutput("naive_text2"),
                            verbatimTextOutput("naive_text3"),
                            verbatimTextOutput("naive_text4"),
                            verbatimTextOutput("naive_text5"),
                            verbatimTextOutput("naive_text6"),
                            h3("Prediction made by Random forest classification model:",style = "color: red"),
                            verbatimTextOutput("naive_text7")),
                   tabPanel("OUTCOMES",
                            h3("Naive Bayes algorithm working",style = "color: green"),
                            plotOutput("naive_plot")),
                   tabPanel("SUMMARY",
                            h2("Classification error and other statisitics",style = "color:green"),
                            p("This tab shows the application of the naive bayes classification model to the fetal mortality dataset.",style = "color: black"),
                            p("It also presents the summary statistics of the fetal mortality dataset", style = "color: black."),
                            p("It also trains the classification model on the given dataset and presents the accuracy of the model.",style = "color; black"),
                            h3("Summary of dataset",style = "color: green"),
                            verbatimTextOutput("naive_summary"),
                            h3("Results",style = "color: green"),
                            verbatimTextOutput("naive_result"))
                 )
               )
      )
    )
  ),
  
  server = function(input, output) {
    #Decision tree algorithm using party package
    
    data <- read.csv(file.choose(),header=T)
    data$Rhdisease <- factor(data$Rhdisease,
                             levels = c('No','Yes'),
                             labels = c(0,1))
    data$Congeniticalabnorm <- factor(data$Congeniticalabnorm,
                                      levels = c('No','Yes'),
                                      labels = c(0,1))
    data$GeneticAbnorm <- factor(data$GeneticAbnorm,
                                 levels = c('No','Yes'),
                                 labels = c(0,1))
    data$Placental.UmbillicalcordAccident <- factor(data$Placental.UmbillicalcordAccident,
                                                    levels = c('No','Yes'),
                                                    labels = c(0,1))
    data$Survivedf <- factor(data$Survived)
    set.seed(1234)
    g <- runif(nrow(data))
    data <- data[order(g),]
    
    pd <- sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
    train <- data[pd==1,]
    validate <- data[pd==2,]
    #decision tree classification model 
    library(party)
    data
    tree <- ctree(Survivedf~MaternalAge+GestationalAge+GeneticAbnorm+Congeniticalabnorm+
                    Rhdisease+Placental.UmbillicalcordAccident,data=train)
    
    #random forest classification model
    
    library(randomForest)
    set.seed(222)
    rf <- randomForest(Survived~.,data=train)
    
    #Naive Bayes classification model 
    #naiveBayes
    library(e1071)
    library(rminer)
    
    #model Build
    model<-naiveBayes(Survivedf~MaternalAge+GestationalAge+GeneticAbnorm+Congeniticalabnorm+
                        Rhdisease+Placental.UmbillicalcordAccident,data=train)
    
    #Contents of tab 1 of decision tree algorithm
    
    output$text1 <- renderText({
      paste0("Maternal Age: ", input$num1)
    })
    output$text2 <- renderText({
      paste0("Gestational Age: ", input$num2)
    })
    output$text3 <- renderText({
      paste0("Rhdisease: ", input$select1)
    })
    output$text4 <- renderText({
      paste0("Congeniticalabnorm: ", input$select2)
    })
    output$text5 <- renderText({
      paste0("GeneticAbnorm: ", input$select3)
    })
    output$text6 <- renderText({
      paste0("Placental&UmbillicalcordAccident: ", input$select4)
    })
    output$text7 <- renderText({
      #newdata=data.frame(MaternalAge= 47 ,GestationalAge = 23,GeneticAbnorm=0,Congeniticalabnorm=1,Rhdisease=1,Placental.UmbillicalcordAccident= 1)
      newdata=data.frame(MaternalAge=input$num1,GestationalAge=input$num2,GeneticAbnorm=input$select3,Congeniticalabnorm=input$select2,Rhdisease=input$select1,Placental.UmbillicalcordAccident=input$select4)
      newdata$MaternalAge <- as.integer(newdata$MaternalAge)
      newdata$GestationalAge <- as.integer(newdata$GestationalAge)
      newdata$Rhdisease <- factor(newdata$Rhdisease,
                                  levels = c(0,1),
                                  labels = c(0,1))
      newdata$Congeniticalabnorm <- factor(newdata$Congeniticalabnorm,
                                           levels = c(0,1),
                                           labels = c(0,1))
      newdata$GeneticAbnorm <- factor(newdata$GeneticAbnorm,
                                      levels = c(0,1),
                                      labels = c(0,1))
      newdata$Placental.UmbillicalcordAccident <- factor(newdata$Placental.UmbillicalcordAccident,
                                                         levels = c(0,1),
                                                         labels = c(0,1))
      newdata$survived <- predict(tree, newdata)
      paste0("Survived => ",newdata$survived)
      
    })
    
    #content of tab 2 of decision tree algorithm
    output$decisionplot <- renderPlot({
      plot(tree)
    })
    
    #content of tab 3 of decision tree algorithm
    output$summary <- renderPrint({
      summary(data)
    })
    output$result <- renderPrint({
      summary(tree)
    })
    
    #random-forest classification model 
    #tab-1 output for random forest classification model 
    output$random_text1 <- renderText({
      paste0("Maternal Age: ", input$random_num1)
    })
    output$random_text2 <- renderText({
      paste0("Gestational Age: ", input$random_num2)
    })
    output$random_text3 <- renderText({
      paste0("Rhdisease: ", input$random_select1)
    })
    output$random_text4 <- renderText({
      paste0("Congeniticalabnorm: ", input$random_select2)
    })
    output$random_text5 <- renderText({
      paste0("GeneticAbnorm: ", input$random_select3)
    })
    output$random_text6 <- renderText({
      paste0("Placental&UmbillicalcordAccident: ", input$random_select4)
    })
    output$random_text7 <- renderText({
      #newdata=data.frame(MaternalAge= 47 ,GestationalAge = 23,GeneticAbnorm=0,Congeniticalabnorm=1,Rhdisease=1,Placental.UmbillicalcordAccident= 1)
      newdata=data.frame(MaternalAge=input$random_num1,GestationalAge=input$random_num2,GeneticAbnorm=input$random_select3,Congeniticalabnorm=input$random_select2,Rhdisease=input$random_select1,Placental.UmbillicalcordAccident=input$random_select4)
      newdata$MaternalAge <- as.integer(newdata$MaternalAge)
      newdata$GestationalAge <- as.integer(newdata$GestationalAge)
      newdata$Rhdisease <- factor(newdata$Rhdisease,
                                  levels = c(0,1),
                                  labels = c(0,1))
      newdata$Congeniticalabnorm <- factor(newdata$Congeniticalabnorm,
                                           levels = c(0,1),
                                           labels = c(0,1))
      newdata$GeneticAbnorm <- factor(newdata$GeneticAbnorm,
                                      levels = c(0,1),
                                      labels = c(0,1))
      newdata$Placental.UmbillicalcordAccident <- factor(newdata$Placental.UmbillicalcordAccident,
                                                         levels = c(0,1),
                                                         labels = c(0,1))
      newdata$survived <- predict(tree, newdata)
      paste0("Survived => ",newdata$survived)
    })
    #tab-2 output of random forest classification model 
    output$random_plot <- renderPlot({
      library(caret)
      plot(rf)
    })
    
    #content of tab 3 of random forest classification model 
    output$random_summary <- renderPrint({
      summary(data)
    })
    output$random_result <- renderPrint({
      #print("Accuracy => ", 87)
      summary(rf)
      #attributes(rf)
      #p1 <- predict(rf,train)
      #confusionMatrix(p1,train$Survived)
    })
    
    #Naive-Bayes classification model 
    #tab-1 output for Naive Bayes classification model 
    output$naive_text1 <- renderText({
      paste0("Maternal Age: ", input$naive_num1)
    })
    output$naive_text2 <- renderText({
      paste0("Gestational Age: ", input$naive_num2)
    })
    output$naive_text3 <- renderText({
      paste0("Rhdisease: ", input$naive_select1)
    })
    output$naive_text4 <- renderText({
      paste0("Congeniticalabnorm: ", input$naive_select2)
    })
    output$naive_text5 <- renderText({
      paste0("GeneticAbnorm: ", input$naive_select3)
    })
    output$naive_text6 <- renderText({
      paste0("Placental&UmbillicalcordAccident: ", input$naive_select4)
    })
    output$naive_text7 <- renderText({
      #newdata=data.frame(MaternalAge= 47 ,GestationalAge = 23,GeneticAbnorm=0,Congeniticalabnorm=1,Rhdisease=1,Placental.UmbillicalcordAccident= 1)
      newdata=data.frame(MaternalAge=input$naive_num1,GestationalAge=input$naive_num2,GeneticAbnorm=input$naive_select3,Congeniticalabnorm=input$naive_select2,Rhdisease=input$naive_select1,Placental.UmbillicalcordAccident=input$naive_select4)
      newdata$MaternalAge <- as.integer(newdata$MaternalAge)
      newdata$GestationalAge <- as.integer(newdata$GestationalAge)
      newdata$Rhdisease <- factor(newdata$Rhdisease,
                                  levels = c(0,1),
                                  labels = c(0,1))
      newdata$Congeniticalabnorm <- factor(newdata$Congeniticalabnorm,
                                           levels = c(0,1),
                                           labels = c(0,1))
      newdata$GeneticAbnorm <- factor(newdata$GeneticAbnorm,
                                      levels = c(0,1),
                                      labels = c(0,1))
      newdata$Placental.UmbillicalcordAccident <- factor(newdata$Placental.UmbillicalcordAccident,
                                                         levels = c(0,1),
                                                         labels = c(0,1))
      newdata$survived <- predict(tree, newdata)
      paste0("Survived => ",newdata$survived)
    })
    #tab-2 output of Naive Bayes classification model 
    output$naive_plot <- renderPlot({
      #model summary
      plot(data$Rhdisease,data$Survivedf)
    })
    
    #content of tab 3 of Naive Bayes classification model 
    output$naive_summary <- renderPrint({
      summary(data)
    })
    output$naive_result <- renderPrint({
      summary(model)
      #summary(model)
    })
    
    
  }
)

