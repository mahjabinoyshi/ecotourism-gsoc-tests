getwd()
setwd("C:\\Users\\oyshi\\ecotourism-gsoc-tests\\DataCamp Certification on Shiny\\Building Web Applications with Shiny in R")
getwd()
#####################################Parts of a Shiny app###########################################
#load shiny
library(shiny)
#create the UI with a html function
ui<-fluidPage("Hello world!")
#Define a custom function to create the server
server<-function(input,
                 output,
                 session){
  
}
#Ask a question
ui<-textInput("Name","Enter Your Name")
#Ask a question with an input
#Ask a question with an input
ui<-fluidPage(textInput("name","Enter Your Name:"),)
server<-function(input,output){output$q<-renderText({paste("Do you prefer dogs or cats?,",input$name,"?")})
}

#Run the app
shinyApp(ui=ui,server=server)
#####################################Baby Name Explorer#######################################
library(shiny)
ui<-fluidPage(
  titlepanel("Baby Name Explore"),
  textInput("Name","Enter Name","David"),
  plotOutput('trend')
)
server<-function(input,output,session){
  output$trend<-renderPlot({
    ggplot()
  })
  
}
shinyApp(ui=ui,server=server)
