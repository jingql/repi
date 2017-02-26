
# Description: "Use to descripitive statistics on infectious disease for primary public health workers"
# Author: Jing Qinlong
# Version: 2017.02.25


library(shiny)

ui <- fluidPage(
  tags$h1("Descriptive statistics on Infectious Disease"),
  
  "Input the original datatable for the following analysis,'onsettime' is the key
  variable to plot epicurve",
  fileInput(inputId="file",label="Choose the csv datatable:"),
  
  tags$h2("Time distributon description"),
  sliderInput(inputId="timeinter",min=1,max=60,value=7,
              label="Please Input the time interval:"),
  
  tags$h2("Plot the epicurve"),
  plotOutput("epicurve"),
  tags$h3("Statistics for histogram variable:"),
  verbatimTextOutput("stats"),
  
  
  tags$h2("Preview the original datatable"),
  tableOutput("filetable")
)


server <- function(input,output){
  data <- reactive({
    infile <- input$file
    if(is.null(infile)) {return(NULL)} 
    my_data <- read.csv(infile$datapath)
    my_data$onsettime <- as.Date(my_data$onsettime,"%m/%d/%Y")
    my_data
    })
  
  # plot the histogram--epicurve
  output$epicurve <- renderPlot({hist(data()$onsettime,breaks=input$timeinter,freq=T,col="red",ann=F,xaxt="n");
                                datetick <- seq(min(data()$onsettime),max(data()$onsettime),paste(input$timeinter,"day"));
                                axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0);
						                    title(xlab="Onsettime",ylab="Frequency",main="Epicurve")						
					                      })
  
  # Display the histogram variable summary
  output$stats <- renderPrint({summary(as.Date(data()$onsettime))})
  
  # Display the head rows
  output$filetable <- renderTable({head(data())}) 
}

shinyApp(ui=ui,server=server)
