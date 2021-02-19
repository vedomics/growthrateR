#Setup---------------
library("rsconnect")
library("shiny")
library("ggplot2")
library("scales")
library("gridExtra")


Sys.setenv(TZ="America/New_York") #Veda's personal idiosyncrasy
theme_set(theme_bw())

# setwd("/Users/Veda/Dropbox (MIT)/Lieberman Lab/Personal lab notebooks/V/Fun_Stuff/growthrateR/")
source("./functions_for_growthrateR.R")


# df = read.csv("./10_29_19_clean.csv", header = TRUE, stringsAsFactors = FALSE)

# 02. 13.21 built assuming time is a column 



ui <- fluidPage(
  tabsetPanel(
    
    tabPanel("Instructions", fluid = TRUE,
            sidebarPanel(
              img(src = "./lieb.png", height = 100, width = 80),
              br(),
              a("Lieberman Lab byproduct", href = "http://lieberman.science/")
                         ),
             mainPanel( 
               h1("Wlecome."),
               p("Hello! Welcome to my stupid little growth rate estimator. Please read on to make sure that your data is in the correct format for this app."),
               br(),
               strong("When selecting a .csv file to use, please make sure that you"),
               p("1. Are using commas as separators"),
               p("2. Have wells as headers"),
               p("3. Include a column named 'time' (not case-sensitive) that contains the times at which ODs were taken" ),
               br(),
               p("Tecan files with multiple reads per well are NOT yet compatible with this app, but functionality coming soon"),
               br(),
               p(" If you have questions, you know where to find me at:"),
               strong("veda@mit.edu"),
               br()
               
             )
    ),
    
    tabPanel("Upload", fluid = TRUE,
               sidebarPanel(
                 # Input: have user select file 
                 fileInput("file1", 
                           "Choose plate reader OD .csv file",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 # Input: does your file have headers?
                 checkboxInput("header", "Header", TRUE),
                 # What mode of file are we dealing with?
                 checkboxInput("multiple", "Multiple reads per well", FALSE)
                 ),
             
            mainPanel( 
              tableOutput("contents")
              )
    ),
    
    tabPanel("Data and Plots", fluid = TRUE,
             sidebarPanel(
               uiOutput("colcontrols"),
               actionButton("button", "Next Well"),
               br(),
               br(),
               actionButton("addrow", "Add Row"),
               actionButton("deleterow", "Delete Row"),
               br()
             ),

             mainPanel(
               plotOutput("gcplot"),
               uiOutput("timerange"),
               textOutput("growth_rate"),
               tableOutput("table")

             )
    ),

    tabPanel("Download", fluid = TRUE,
             sidebarPanel(
               h3("Download time!"),
               p("Use this space to make sure you aren't missing any data, then go ahead and dowload! Remember to name our file something useful -- maybe in the future I will create an add-on in this app that allows you to do more graphics with the data you upload!"),
               br(),
               downloadButton('download',"Download data")
             ),
             mainPanel(
               tableOutput("table2")
             )
             )
  )
  )



server <- function(input, output, session) {
  
  # Display the user data in second tab
  output$contents <- renderTable({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header, stringsAsFactors = FALSE)
  
    return(df)
  }
  )
  
  #Create a reactive dataframe that other ui functions can now access 
  user_data = reactive({
    df <- read.csv(input$file1$datapath,
                   header = input$header, stringsAsFactors = FALSE)
  })
  
  
  #Dynamic selector for well
  output$colcontrols <- renderUI({
    # remove any column named "time" or any case-variant thereof
    wells = c(colnames(user_data())[!grepl("time", colnames(user_data()), ignore.case = TRUE)])
    selectInput("Well", "Choose well", wells, selected = wells[1])
  })
  
  #Dynamic time range for plotting - populates based on data uploaded
  output$timerange <- renderUI({
    #index of well that contains time data
     timewell = colnames(user_data())[grepl("time", colnames(user_data()), ignore.case = TRUE)]
     indexed_time = which(colnames(user_data())==timewell)
     times = c(as.numeric(user_data()[,indexed_time]))
     sliderInput("linear_time", strong("Times"),
                 min = min(times), max = max(times), step = 2, value = c(min(times),max(times)), width = "100%")
  })
  
  #Plot our growth curves and estimate rate
  
  output$gcplot = renderPlot({
    
    #set up known constants and lookup table
    i <- which(colnames(user_data()) ==input$Well)
    
    timewell = colnames(user_data())[grepl("time", colnames(user_data()), ignore.case = TRUE)]
    indexed_time = which(colnames(user_data())==timewell)
    
    # Add natural log data to our selected well 
    growth_curve = user_data()[,c(i, indexed_time)]
    
    colnames(growth_curve)[2] = "times" #re-write over whatever they called their time column
    
    growth_curve$log_e = log(growth_curve[,1])
    
    # For a detailed view of our expected fit
    
    mini_curve = subset(growth_curve, times < input$linear_time[2] & times > input$linear_time[1])
    
    #Build a linear regression model based on selected data
    linear_reg = lm(log_e ~ times, data = mini_curve)
    
    #Predict data from this regression model
    
    mini_curve$predicted_fit = predict(linear_reg, mini_curve)
    rsq = round(summary(linear_reg)$r.squared, digits=3) #grab Rsq
    gre = round(as.numeric(coef(linear_reg)[2]), digits = 3) #grab growth rate estimate
    
    growth_curve$predicted_fit = predict(linear_reg, growth_curve) #expand predicted fit to entire curve
    
    large_plt = ggplot(growth_curve, aes(x=as.numeric(times), y = log_e))+
      geom_line(alpha = .5, color = "black") +
      # geom_line(aes(x=times, y = predicted_fit), col = "red")+
      labs(x = "time", y = "ln(OD)") +
      annotate("rect", xmin = input$linear_time[1], xmax = input$linear_time[2], ymin = -Inf, ymax = Inf, alpha = 0.2)+
      # scale_x_continuous(breaks = seq(0, 1200, 50))+
      scale_y_continuous(lim = c(min(growth_curve$log_e), max(growth_curve$log_e)))
    
    scaled_plt = ggplot(mini_curve, aes(x = times))+
      geom_point(alpha = .5, color = "black", aes(x=times, y = log_e)) +
      geom_line(aes(x=times, y = predicted_fit), col = "red")+
      labs(x = "time", y = "ln(OD)")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      annotate("text", label = paste("Rsq:", rsq),  x = mean(mini_curve$times), y = min(mini_curve$predicted_fit))
    
    grid.arrange(large_plt, scaled_plt, ncol=2, widths=2:1, nrow=1)
    
  })
  
  #Print out growth rate estimate for selected time
  
  output$growth_rate <- renderText({
    rate = get_growth_rate_estimate(input$Well, input$linear_time[1], input$linear_time[2], user_data())
    # paste(rate)
    paste("The estimated growth rate for that interval is ", rate[2])
  })
  
  values = reactiveValues()
  values$DT = data.frame("time1" = NA, "time2"= NA, "Well" = NA,"R_est"=NA, "Rsq" = NA, "Final_OD" = NA)
 
  newEntry <- observeEvent(input$addrow, {
    
    i <- which(colnames(user_data()) ==input$Well)
    well = colnames(user_data())[i]
    rate_est = get_growth_rate_estimate(input$Well, input$linear_time[1], input$linear_time[2], user_data())
    
    newline = c(input$linear_time[1], input$linear_time[2], input$Well, rate_est[2], rate_est[1], rate_est[3])
    values$DT <- rbind(values$DT, newline)
    values$DT = na.omit(values$DT)
  })
  
  newEntry <- observeEvent(input$deleterow, {
    deleteLine <- values$DT[-nrow(values$DT), ]
    values$DT <- deleteLine
  })
  
  output$table <- output$table2 <- renderTable({values$DT})
  
  #Advance to next well
  observeEvent(input$button, {
    
    nextwell = which(colnames(user_data())==input$Well) + 1
    nextwell_name = colnames(user_data())[nextwell]
    updateSelectInput(session,"Well", selected = nextwell_name)})
  
  output$download <- downloadHandler(
    filename = function(){"Growth_Rates.csv"},
    content = function(fname){
      write.csv(values$DT, fname)}
  )
    
}

shinyApp(ui = ui, server = server)
