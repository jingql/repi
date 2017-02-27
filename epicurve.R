
# Description: "Use to descripitive statistics on infectious disease for primary public health workers"
# Author: Jing Qinlong
# Version: 2017.02.25

library(shiny)

ui <- fluidPage(
  tags$p("Notes: If you want to develop, please visit", tags$a("Jingql GitHub",href="https://github.com/jingql/rshinyepi/")),
  
  tags$h1("Descriptive statistics on Infectious Disease"),
  br(),
  "Input the original datatable for the following analysis,'onsettime' is the key
  variable to plot epicurve",
  fluidRow(column(3,fileInput(inputId="file",label="Choose the csv datatable:"))),
  
  tags$h2("Time distributon description"),
  tags$h3("Plot the epicurve"),
  sliderInput(inputId="timeinter",min=1,max=60,value=7,label="Please Input the time interval:"),
  fluidRow(column(12,plotOutput("epicurve"))),
  
  tags$h3("Statistics for histogram variable:"),
  verbatimTextOutput("stats"),
  
  tags$h3("Preview the epicurve table:"),
  tableOutput("epicurvetable"),
  
  tags$h2("Preview the original datatable"),
  #tableOutput("filetable")
)


server <- function(input,output){
  data <- reactive({
    infile <- input$file
    if(is.null(infile)) {return(NULL)} 
    my_data <- read.csv(infile$datapath,fileEncoding="gbk")
    my_data$onsettime <- as.Date(my_data$onsettime,"%m/%d/%Y")
    my_data$week <- format(my_data$onsettime,"%V")
    my_data$month <- format(my_data$onsettime,"%m")
    my_data$year <- format(my_data$onsettime,"%Y")
    my_data
    })
  
  # plot the histogram--epicurve
  output$epicurve <- renderPlot({histg <- hist(data()$onsettime,breaks=input$timeinter,freq=T,col="red",ann=F,xaxt="n");
                                datetick <- seq(min(data()$onsettime),max(data()$onsettime),paste(input$timeinter,"day"));
                                axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0);
                                title(xlab="Onsettime",ylab="Frequency",main="Epicurve")						
					                      })
  
  # Display the histogram variable summary
  output$stats <- renderPrint({summary(as.Date(data()$onsettime))})
  
  # Display the eipcurve table:
  output$epicurvetable <- renderTable({histg <- hist(data()$onsettime,breaks=input$timeinter,plot=F);
                                      data.frame(breaks=as.character(as.Date(histg$breaks[-length(histg$breaks)],origin="1970-01-01"))
                                                  ,midbreaks=as.character(as.Date(histg$mids,origin="1970-01-01"))
                                                  ,counts=histg$counts,density=histg$density)})
  
  # Display the head rows
  #output$filetable <- renderTable({head(data())}) 
}

# run App
shinyApp(ui=ui,server=server)
