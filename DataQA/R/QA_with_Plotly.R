#QA Rolling window

#Load level data
head(level_data)

#################INPUT######################
#date from which to do QA
#Check load_data or folder for last QA \\foe-data-30\a263\gyzrvl\Documents - Y Drive\Analysis\R\TimeSeriesR\QA
check_date<-as.POSIXct("2019-02-26",tz="GMT")
logger<-"LB5U"
###########################################

#minus a week from last QA date to ensure overlap
check_date<-check_date-(7*24*60*60)
check_date
#find last date in level data
end_date<-end(level_data)

#grab that portion of the data
check_data<-window(level_data,start=check_date,end=end_date)

#Convert zoo to dataframe for using the interactive plotting tool
#data_df<-data.frame(datetime=index(check_data),as.data.frame(check_data))
#head(check_data)


##########################################
#https://plot.ly/r/shinyapp-plotly-events/


library(ggplot2)
library(plotly)
library(shiny)
#Use plotly
p<-autoplot(check_data[,1])
(ply<-ggplotly(p,dynamicTicks = TRUE))

output$brush <- renderPrint({
  d <- event_data("plotly_selected")
  if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
})

output<-NA
output



#######
#https://stackoverflow.com/questions/49190820/create-data-set-from-clicks-in-shiny-ggplot
ui <- pageWithSidebar(
  headerPanel("Example"),
  sidebarPanel(
    radioButtons("color", "Pick Color", c("Pink", "Green", "Blue")),
    selectInput("shape", "Select Shape:", c("Circle", "Triangle"))
  ),
  mainPanel(
    fluidRow(column(width = 6,
                    h4("Click plot to add points"),
                    actionButton("rem_point", "Remove Last Point"),
                    plotOutput("plot1", click = "plot_click")),
             column(width = 6,
                    h4("Table of points on plot"),
                    tableOutput("table")))
  )
)

server = function(input, output){
  
  ## 1. set up reactive dataframe ##
  values <- reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric(),
                          color = factor(),
                          shape = factor())
  
  ## 2. Create a plot ##
  output$plot1 = renderPlot({
    autoplot(check_data) 
      # include so that colors don't change as more color/shape chosen
   
  })
  
  ## 3. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- data.frame(x = input$plot_click$x,
                          y = input$plot_click$y,
                          color = factor(input$color, levels = c("Pink", "Green", "Blue")),
                          shape = factor(input$shape, levels = c("Circle", "Triangle")))
    # add row to the data.frame
    values$DT <- rbind(values$DT, add_row)
  })
  
  ## 4. remove row on actionButton click ##
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  ## 5. render a table of the growing dataframe ##
  output$table <- renderTable({
    values$DT
  })
}

shinyApp(ui, server)



##############################
#https://stackoverflow.com/questions/48939382/how-can-i-grab-the-row-of-data-from-a-ggplotly-in-shiny
library(plotly)
library(shiny)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- row.names(mtcars)
   
    p<-autoplot(check_data$LB6U)
    # p <- ggplot(mtcars, aes(x = mpg, y = wt, colour = factor(vs), key = key)) + 
     # geom_point() + facet_wrap(~ cyl)
    ggplotly(p) %>% layout(dragmode = "select")
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    req(d)
    print(mtcars[d$key, ])
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    req(d)
    print(mtcars[d$key, ])
  })
  
}

shinyApp(ui, server)


############
#https://stackoverflow.com/questions/46307742/in-plotly-how-do-i-retain-the-information-about-both-the-lasso-selection-and-th
library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  
 # nms <- row.names(mtcars)
  
  output$plot <- renderPlotly({
    p <- autoplot(check_data$LB6U)
    ggplotly(p) %>% layout(dragmode = "lasso")
  })
  
  # output$click <- renderPrint({
  #   d <- event_data("plotly_click")
  #   if (!is.null(d)) d
  # })
  # 
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (!is.null(d)) d
  })
  
}

shinyApp(ui, server)

