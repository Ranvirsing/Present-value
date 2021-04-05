library(shiny)
library(tidyverse)
ui<- fluidPage(
  titlePanel("Present Value Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("FA","Final Amount",min=0,max=1000000,value=100000,step=1000),
      selectInput("i","Convertible Nominal Interest",choices=c("Monthly",
                  "Half-Yearly","Quarterly")),
      conditionalPanel(condition="input.i=='Monthly'",
                        sliderInput("rate1","Interest (in %)",
                                    min=0,max=20,value=7,step=0.1)),
      conditionalPanel(condition="input.i=='Half-Yearly'",
                       sliderInput("rate2","Interest (in %)",
                                   min=0,max=20,value=7,step=0.1)),
      conditionalPanel(condition="input.i=='Quarterly'",
                       sliderInput("rate3","Interest (in %)",
                                   min=0,max=20,value=7,step=0.1)),
      sliderInput("Y","Time Period ( in Years )",
                  min=0,max=25,value=5,step=1),
      ),
    
    mainPanel(textOutput("int"),
              textOutput("PV")
              )
              )
              )


r1=function(x){
  r1=(1+(x/100)/12)^12 -1
  return(r1)
}

r2=function(x){
  r2=(1+(x/100)/2)^2 -1
  return(r2)
}

r3=function(x){
  r3=(1+(x/100)/2)^2 -1
  return(r3)
}

server<- function(input, output){
  output$int=renderText({
    if (input$i == "Monthly"){
               r1 = (1+(input$rate1/100)/12)^12 -1
               paste0("Effective Rate of Interest is : ",round(r1*100,2)," %")
    }else if (input$i == "Half-Yearly"){
               r2=(1+(input$rate2/100)/2)^2 - 1
               paste0("Effective Rate of Interest is : ",round(r2*100,2)," %")
    }else {
               r3 = (1+(input$rate3/100)/4)^4 - 1
               paste0("Effective Rate of Interest is : ",round(r3*100,2)," %")
    }
    }
    )
  output$PV = renderText({ 
    r1=r1(x=input$rate1)
    r2=r2(x=input$rate2)
    r3=r3(x=input$rate3)
    if (input$i == "Monthly"){
      pv1=input$FA*((1/(1+r1))^input$Y)
      paste0("The Present value is : "," Rs. ",round(pv1,2))
    }else if (input$i == "Half-Yearly"){
      pv2=input$FA*((1/(1+r2))^input$Y)
      paste0("The Present value is : "," Rs. ",round(pv2,2))
    }else {
      pv3=input$FA*((1/(1+r3))^input$Y)
      paste0("The Present value is : "," Rs. ",round(pv3,2))
    }
    }
    )
}

  
shinyApp(ui = ui, server = server)
  