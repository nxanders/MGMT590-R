library(caret)
library(ggplot2)
library(dplyr)
library(shiny)
library(pROC)
library(xgboost)
library(plotly)
library(plyr)
library(e1071)
library(shinyWidgets)
library(rsconnect)
library(shinyjs)


#Set working directory
#setwd("C:/Users/nrxan/Desktop/Purdue Krannert - MSBA/Semesters/Summer 2021/Module 1/MGMT 590 - R for Analytics/Final Project/heart attack")

#load data
data <- read.csv('heart.csv', header = TRUE)
#testMbr <- read.csv('testMbr.csv', header = TRUE)
#write.csv(train2, file = 'train2.csv')

str(data)

data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp) #Chest pain
data$fbs <- as.factor(data$fbs) #fasting? Y/N
data$restecg <- as.factor(data$restecg) #resting ecg
data$exng <- as.factor(data$exng) #Exercise induced angina
data$thall <- as.factor(data$thall) #
data$output <- as.factor(data$output) # output
data$slp <- as.factor(data$slp)
data$caa <- as.factor(data$caa)


str(data)

#one-hot encoding
dummies <- dummyVars( ~ . , data = data)
ex <- data.frame(predict(dummies, newdata = data))
names(ex) <- gsub("\\.","",names(ex))
d <- cbind(data$output,ex)
names(d)[1] <- "y"

#Clean up
rm(dummies, ex)
str(d)

#Remove highly correlated columns
descrCor <- cor(d[,2:ncol(d)])  # find the correlation of all inputs except y (predictor)

highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80) # Identify higly correlated columns
filterDescr <- d[,2:ncol(d)][,-highlyCorDescr] # Create a data frame and exclude correlations
descrCor2 <- cor(filterDescr)

summary(descrCor2[upper.tri(descrCor2)])  # check if the correlation is within +/- 0.80

d <- cbind(d$y, filterDescr)
names(d)[1] <- "y"

#Clean up
rm(descrCor, descrCor2, filterDescr)


#Identify Linear Dependencies
y <- d$y
d <- cbind(rep(1,nrow(d)),d[2:ncol(d)])
names(d)[1] <- "ones"
(comboInfo <- findLinearCombos(d)) # Idnetify linear combinations

d <- d[,-comboInfo$remove] # Remove columns with high collinearity
d <- d[,c(2:ncol(d))]
d <- cbind(y,d)

rm(y,comboInfo)

#Remove columns with little variation
nzv <- nearZeroVar(d,saveMetrics = TRUE)
str(nzv)
(d <- d[, c(TRUE,!nzv$nzv[2:ncol(d)])])

rm(nzv)

#Min-Max NOrmalization
#preProc_d <- preProcess(d[,c(2:ncol(d))], method = c("range"))
#d <- predict(preProc_d,d)

#rm(preProc_d)


####################################################################

####################################################################

#Develop train an test set
set.seed(1234)
intrain <- createDataPartition(y=d$y, p=.80, list=F)

train <- d[intrain,]
test <- d[-intrain,]

rm(intrain)

tcontrol <- trainControl(method = "cv",
                         number = 3,
                         classProbs = T,
                         summaryFunction = twoClassSummary,
                         allowParallel = T
)

str(train)
str(test)

train <- train %>%
  mutate(y = ifelse(y == 1,"Yes","No"))
test <- test %>%
  mutate(y = ifelse(y == 1,"Yes","No"))


#Make output variable a factor
train$y <- as.factor(train$y)
test$y <- as.factor(test$y)


#Model 2
m1 <- train(y ~ age + sex1 + cp0 + cp1 + cp2 + trtbps + chol + exng1 + thall1 + thall3 , 
            data = train,
            method = "glm",
            family = "binomial",
            trControl = tcontrol
            ,metric = 'ROC')
#metric = "RMSE") #Use RMSC for regression problem

defaultSummary(data=data.frame(obs=train$y, pred=predict(m1, newdata=train))
               , model=m1)
# test 
defaultSummary(data=data.frame(obs=test$y, pred=predict(m1, newdata=test))
               , model=m1)

summary(m1)

predict(m1, test, type = 'prob')$Yes

str(data)

#######################################################################################################
#SHINY APP DEVELOPMENT
#######################################################################################################
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB

ui <- fluidPage(setBackgroundColor(color = c("#F7FBFF", "#2171B5"),gradient = "linear",direction = "bottom"),
                headerPanel('Patient Detail - Risk of Heart Attack Calculation'), 
                sidebarLayout(
                  sidebarPanel(
                    br(),
                    selectInput(inputId = 'age', label = 'Select Patient Age', choices = as.numeric(unique(25:80)),selected = '50'),
                    br(),
                    selectInput(inputId = 'sex', label = 'Select Patient Sex', choices = as.character(c('M','F'))),
                    br(),
                    selectInput(inputId = 'cp', label = "Patient Experiencing Chest Pain?", choices = c('No','Slight','Moderate','Severe')),
                    br(),
                    textInput(inputId = 'bps', label = 'Resting Blood Pressure (systolic)', value = as.numeric(round(mean(data$trtbps),0))),
                    br(),
                    textInput(inputId = 'chol', label = 'Total Cholesterol', value = round(mean(data$chol),0)),
                    br(),
                    textInput(inputId = 'fbs', label = 'Fasting Blood Sugar', value = '120'),
                    br(),
                    selectInput(inputId = 'exng', label = 'Patient Experiencing Exercised Induced Angina?', choices = as.character(c('N','Y'))),
                    br(),
                    selectInput(inputId = 'thal', label = "Does the Patient have a Heart Defect?", choices = c('No', 'Fixed Defect','Reversable Defect')),
                    br(),
                    actionButton(inputId = "runPatient", label = "Determine Heart Attack Risk", icon=icon('chart-line')),
                    br(),
                    br(),
                    #actionButton(inputId = "showStats", label = "Show Patient Statistics", icon = icon('eye')),
                    #br(),
                    #h6('Image:'),
                    #tags$img(src='images/hasymptoms.png', height = 150, width = 150),
                    #tags$div(img(src='images/hasymptoms.png', height = 150, width = 150)),
                    #img(src = 'images/hasymptoms.png', height = 150, width = 150)
                    
                    
                  ),
                  #Tabular Creation w/ Graphical Ouputs
                  mainPanel(tabsetPanel(id = 'tabs',
                                        tabPanel(value = 'patientInfo', title ='Patient Info', icon = icon('clinic-medical'), 
                                                 br(),
                                                 br(), 
                                                 htmlOutput('text'), 
                                                 tags$head(tags$style("#text{color: #66100E; font-size: 24px; font-style: italic;}")),
                                        ),
                                        tabPanel(value = 'patientStats', title = 'Patient File', icon = icon('file-medical-alt'), 
                                                 br(),
                                                 plotOutput('cpbar') , 
                                                 plotOutput('exngbar')),
                                        tabPanel(value = 'genStats', title = 'General Statistics', icon = icon('bar-chart-o'), 
                                                 tabsetPanel(id = 'tab2', 
                                                             tabPanel(value = 'pie', title = 'Pie Chart Selection', icon = icon('heartbeat'),
                                                                      br(),
                                                                      selectInput(inputId = 'pie', label = 'Select Categorical Variable', choices = c('Sex',"Chest Pain", "Blood Sugar", 'Exercise Induced Angina')),
                                                                      br(), 
                                                                      plotOutput('piePlot')),
                                                             tabPanel(value = 'gen', title = 'General', icon = icon('table'),
                                                                      #tags$head(tags$style("#ageGenTxt{color: black; font-size: 18px; font-style: bold;}")), 
                                                                      #tags$head(tags$style("#ageBoxGenTxt{color: black; font-size: 18px; font-style: bold;}")), 
                                                                      #textOutput('ageGenTxt'), 
                                                                      br(), 
                                                                      plotOutput('ageGen'), 
                                                                      br(),
                                                                      #textOutput('ageBoxGenTxt'), 
                                                                      plotOutput('ageBoxGen'), 
                                                                      br(),
                                                                      plotOutput('cholGen'), 
                                                                      br(),
                                                                      plotOutput('otherBoxGen'), 
                                                                      br(),
                                                                      plotOutput('ageBpsGen'),
                                                                      br())
                                                             
                                                 )),
                                        tabPanel(value = 'references', title = 'References', icon = icon('keyboard'),
                                                 tags$head(tags$style("#dataSource{color: black; font-size: 18px; font-style: bold;}")), 
                                                 tags$head(tags$style("#References{color: black; font-size: 18px; font-style: bold;}")), 
                                                 br(),
                                                 textOutput('dataSource'),
                                                 br(),
                                                 textOutput('References'), 
                                        ) 
                                        
                                        
                  ),
                  #tableOutput('table'),
                  #tableOutput('plot1')
                  )
                  
                )
)

server <- function(input, output) {
  
  
  plotData = reactiveVal()
  selectedData <- reactive({
    data[,]
  })
  
  #Go to Patient Info Tab when Run Patient info is Clicked
  #observeEvent(input$runPatient, {
  #    updateTabsetPanel(session, 'tabs', selected = 'patientInfo')
  #  })
  
  ########################################################################################################  
  #Model Prediction - Patient Info Tab
  determineHARisk <- eventReactive(input$runPatient, {
    df <- data.frame(age=as.numeric(input$age), sex1= ifelse(input$sex=="M",1,0), cp0=ifelse(input$cp == "No",1,0) , cp1 = ifelse(input$cp == "Slight",1,0),
                     cp2 = ifelse(input$cp == "Moderate", 1,0) , trtbps = as.numeric(input$bps) , fbs0 = ifelse(input$fbs > 120, 1,0) , chol = as.numeric(input$chol),
                     exng1 = ifelse(input$exng == 'Y',1,0), thall1 = ifelse(input$thal == 'No',1,0) , thall3 = ifelse(input$thal=='Reversable Defect',1,0))
    waist <- ifelse(input$sex == 'M', 37,35)
    alcohol <-ifelse(input$sex == 'M', "two", "one")
    if(round(predict(m1, df, type = 'prob')$Yes,3) < 0.250){
      mhl <- "low"
    }
    else if (round(predict(m1, df, type = 'prob')$Yes,3) < 0.667){
      mhl <- "moderate"
    }
    else {
      mhl <- "high"
    }
    mbrProb <- paste0("The liklihood of a heart attack is ", mhl,": ", round(predict(m1, df, type = 'prob')$Yes,3)*100,"%")
    
    mbrStment <- if(!mhl=="low"){
      HTML(mbrProb, '<br><br><br><br>', 'Here are few ways to reduce the risk of a Heart Attack: <br><br>', 
           '1.) There is risk reduction attributed to not smoking. <br><br>',
           '2.) Diet that is rich in fruits, vegetables, legumes, nuts, reduced-fat dairy products, and fish. <br><br>',
           '3.) Maintain a waistline of ' , waist, ' inches or less. <br><br>',
           '4.) Drink fewer than ', alcohol,' alcohol beverages per day. <br><br>',
           '5.) Moderate daily and/or weekly exercise. <br><br>',
           '6.) Maintain a habit of all of the above.* <br><br>')
    }
    else {
      HTML(mbrProb)
    }
    
  })
  
  output$text <- renderText({
    determineHARisk()
  })
  
  
  
  
  ################################################################################################
  # Patient Statistics Tab
  
  #Chest Pain
  output$angTxt <- renderText({
    if(!input$cp == 'No'){
      paste0("Impact of Chest Pain on ",ifelse(input$sex == "M", "males","females")," on Heart Attacks")
    }
  })
  output$cpbar = renderPlot({
    if(!input$cp == 'No' & input$sex == "M"){
      cpbar <- ggplot(data[data$sex=="1",], aes(cp)) + 
        geom_bar(aes(fill=output),position = "fill") +
        xlab("Chest Pain Severity") +
        ylab("Frequency") + 
        labs(fill = '') +
        ggtitle("Impact of Chest Pain on Heart Attacks") +
        scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
        theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic")) +
        scale_x_discrete(labels = c('No','Slight','Moderate','Severe'))
      
      plot(cpbar)
    }
    if(!input$cp == 'No' & input$sex == "F"){
      cpbar <- ggplot(data[data$sex=="0",], aes(cp)) + 
        geom_bar(aes(fill=output),position = "fill") +
        xlab("Chest Pain Severity") +
        ylab("Frequency") + 
        labs(fill = '') +
        ggtitle("Impact of Chest Pain on Heart Attacks") +
        scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
        theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic")) +
        scale_x_discrete(labels = c('No','Slight','Moderate','Severe'))
      
      plot(cpbar)
    }
  })
  
  #Angina
  
  #output$angTxt <- renderText({
  #  if(!input$exng == 'N'){
  #    paste0("Impact of Exercised Induced Angina on ",ifelse(input$sex == "M", "males","females")," on Heart Attacks")
  #  }
  #})
  
  
  output$exngbar = renderPlot({
    if(!input$exng == 'N' & input$sex == "M"){
      exngbar <- ggplot(data[data$sex=="1",], aes(exng)) + 
        geom_bar(aes(fill=output),position = "fill") +
        xlab("Exercise Induced Angina") +
        ylab("Frequency") + 
        labs(fill = '') +
        ggtitle("Impact of Exercise Induced Angina on Heart Attacks") +
        scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
        theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic")) +
        scale_x_discrete(labels = c('No','Yes'))
      
      plot(exngbar)
    }
    if(!input$exng == 'N' & input$sex == "F"){
      exngbar <- ggplot(data[data$sex=="0",], aes(exng)) + 
        geom_bar(aes(fill=output),position = "fill") +
        xlab("Exercise Induced Angina") +
        ylab("Frequency") + 
        labs(fill = '') +
        ggtitle("Impact of Exercise Induced Angina on Heart Attacks") +
        scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
        theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic")) +
        scale_x_discrete(labels = c('No','Yes'))
      
      plot(exngbar)
    }
  })
  
  ################################################################################################
  #PIE CHARTS
  
  
  output$piePlot = renderPlot({
    
    if(input$pie == "Sex"){
      pieChart <- ggplot(data, aes(x = '', y = sex, fill = sex)) +
        geom_bar(stat = 'identity', width = 1) + 
        coord_polar("y", start = 0) +
        labs(fill = '') +
        theme_void() + 
        #geom_text(aes(x=1.6, label = scales::percent(data1$pct, accuracy = 0.1)), position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c('Female','Male')) +
        #scale_fill_brewer(palette = "Blues") +
        ggtitle("Distribution of Sex in Dataset") + 
        theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic")) 
      
      plot(pieChart)
    }
    
    if(input$pie == "Chest Pain"){
      pieChart <- ggplot(data, aes(x = '', y = cp, fill = cp)) +
        geom_bar(stat = 'identity', width = 1) + 
        coord_polar("y", start = 0) +
        labs(fill = '') +
        theme_void() + 
        scale_fill_manual(values = c("#CEB888", "#66100E","#373A35", "#C28E0E"), labels = c('No','Slight','Moderate','Severe')) +
        #scale_fill_brewer(palette = "Blues") +
        ggtitle("Distribution of Chest Pain in Dataset") + 
        theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic")) 
      
      plot(pieChart)
    }
    
    if(input$pie == "Blood Sugar"){
      pieChart <- ggplot(data, aes(x = '', y = fbs, fill = fbs)) +
        geom_bar(stat = 'identity', width = 1) + 
        coord_polar("y", start = 0) +
        labs(fill = '') +
        theme_void() + 
        scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c('< 120','120 +')) +
        #scale_fill_brewer(palette = "Blues") +
        ggtitle("Distribution of Fasting Blood Sugar in Dataset") + 
        theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic")) 
      
      plot(pieChart)
    }
    
    if(input$pie == "Exercise Induced Angina"){
      pieChart <- ggplot(data, aes(x = '', y = exng, fill = exng)) +
        geom_bar(stat = 'identity', width = 1) + 
        coord_polar("y", start = 0) +
        labs(fill = '') +
        theme_void() + 
        scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c('No','Yes')) +
        #scale_fill_brewer(palette = "Blues") +
        ggtitle("Distribution of Exercise Induce Angina in Dataset") + 
        theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic")) 
      
      plot(pieChart)
    }
    
  }) 
  
  
  ################################################################################################
  #General Statistics Tab
  
  #output$ageGenTxt <- renderText({
  #  paste0("Distribution of Population Age (Normal & Heart Attack)")
  #})
  
  
  #Age Distribution Graph - General Statistics Tab
  output$ageGen = renderPlot({
    age_density <- ggplot(data=data, aes(x=age, group = output)) + geom_density(aes(fill=output), alpha = 0.75) + 
      xlab("Age") +
      ylab("Density") + 
      ggtitle("Distribution of Age") +
      labs(fill = "") +
      scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
      theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic"))
    
    plot(age_density)
    
  })
  
  ##Age BoxPlot Dist
  #output$ageBoxGenTxt <- renderText({
  #  paste0("Distribution of Population Age (BoxPlot)")
  #})
  
  #Age Boxplot
  output$ageBoxGen = renderPlot({
    ageBoxGen <- ggplot(data=data, aes(x=output, y=age)) + 
      geom_boxplot(aes(fill=output)) +      
      xlab("Normal / Heart Attack") +
      ylab("Age") + 
      ggtitle("Distribution of Age (Box Plot)") +
      scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
      theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic"),axis.ticks.x = element_blank(), axis.text.x = element_blank())
    
    plot(ageBoxGen)
  })
  
  #Cholesterol Distribution - General Stats Tab
  output$cholGen = renderPlot({
    chol_density <- ggplot(data=data, aes(x=chol, group = output)) + geom_density(aes(fill=output), alpha = 0.75) + 
      xlab("Cholesterol") +
      ylab("Density") + 
      ggtitle("Distribution of Cholesteral") +
      labs(fill = "") +
      scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
      theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic"))
    
    plot(chol_density)
  })
  
  #cholesterol Boxplot by Sex
  output$otherBoxGen = renderPlot({
    data2 <- data
    data2$sex <- revalue(x=data2$sex, c('0' = 'Female','1'='Male'))
    sex_names <- list("Female" = '0',"Male" = '1')
    
    otherBoxGen <- ggplot(data=data2, aes(x=output, y=chol)) + geom_boxplot(aes(fill=output)) +      
      xlab("Normal / Heart Attack") +
      ylab("Cholesterol") + 
      ggtitle("BoxPlot of Cholesterol by Sex ") +
      labs(fill = "") + 
      scale_fill_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
      theme(plot.title = element_text(color = "black", size = 18, face = "bold.italic"),axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
      facet_wrap(~ sex, ncol = 2, labeller = as_labeller(sex_names)) 
    
    plot(otherBoxGen)
  })
  
  #Age/Blood Pressure Scatter
  output$ageBpsGen = renderPlot({
    data2 <- data
    data2$sex <- revalue(x=data2$sex, c('0' = 'Female','1'='Male'))
    sex_names <- list("Female" = '0',"Male" = '1')
    ageBpsGen <- ggplot(data=data2, aes(x=trtbps, y=age, group = output)) + 
      geom_point(aes(color = output)) +      
      xlab("Blood Pressure") +
      ylab("Age") + 
      labs(color = "") +
      ggtitle("ScatterPlot of Age/Blood Pressure by Sex") +
      scale_color_manual(values = c("#CEB888", "#66100E"), labels = c("Normal", "Heart Attack")) +
      theme(plot.title = element_text(color = 'black', size = 18, face = "bold.italic")) +
      facet_wrap( ~ sex, ncol = 2, labeller = as_labeller(sex_names)) +
      geom_smooth(aes(fill = output), method = "lm", size = 0.5, alpha = 0.2) +
      scale_fill_manual(values = c('#CEB888','#66100E'), labels = c("Normal", "Heart Attack")) +
      labs(fill = "")
    
    
    plot(ageBpsGen)
  })
  
  
  ################################################################################################ 
  #REFERENCES
  output$dataSource <- renderText({
    paste0("Dataset Location: https://www.kaggle.com/rashikrahmanpritom/heart-attack-analysis-prediction-dataset")
  })
  output$References <- renderText({
    paste0("*Reference Location: https://heart.arizona.edu/heart-health/prevention/five-ways-reduce-heart-attack-risk-80-percent")
  })
  
  
  
  ################################################################################################ 
  
}# end of server


shinyApp(ui = ui, server = server)
