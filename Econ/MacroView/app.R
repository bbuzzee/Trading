#==================================================#
#================= Initialize =====================#
#==================================================#

library(shiny)
library(fredr)
library(tidyverse)
library(xts)
library(quantmod)

getFedData <- function(tag = "DFF"){
  
  #=== pull data ===#
  api.key <- "56f632cfd4168b34b239b80b7ec07f77"
  fredr_set_key(api.key)
  
  # REQUIRES global fred object to be loaded
  dt <- fredr(tag)
  dt <- dt %>% as_tibble()
  
  print("removing NAs")
  # remove NA's
  dt$value <- as.numeric(dt$value)
  dt <- dt[!is.na(dt$value),]
  
  #=== clean dataframe up ===#
  print("cleaning data")
  # remove unneeded columns
  dt <-  dt %>% dplyr::select(date, value) %>% dplyr::mutate(date = as.Date(date))
  
  # round all dates to nearest month
  dt$date <- dt$date %>% lubridate::round_date(unit = "month")
  
  # for each month only retain median value
  dt <- dt %>% dplyr::group_by(date) %>% dplyr::summarize(value = median(value))
  
  dates <- as.Date(dt$date, format = "%Y-%m-%d")
  values <- dt[,2]
  
  dt <- xts(x = values, order.by = dates)
  
  return(dt)
  
}


#==================================================#
#================= USER INTERFACE =================#
#==================================================#


# Define UI for application that draws a histogram
ui <- navbarPage("MacroView",

                 
                #========= TAB ONE ===========#
                
                tabPanel("Economic Indicators",
        
                      # one row of stacked plots
                      fluidRow( 
                              column(6,
                                    plotOutput("cuePlot"),
                                    plotOutput("rrsPlot")
                                    ),
                              column(6,
                                    plotOutput("gapPlot"),
                                    plotOutput("indproPlot")
                                    )
                              ),
                      
                     # one row for input slider
                     fluidRow(
                             column(12,
                                    align = "center",
                                    sliderInput("mo",
                                                "Lookback Period (Years)",
                                                min = 1,
                                                max = 30,
                                            value = 2)
                                    )
                             )
                 ), # end tab one
                
                
                #========= TAB TWO ===========#
                tabPanel("Rates and Debt",
                    
                    # one row of stacked plots     
                    fluidRow( 
                            column(6,
                                   
                                   plotOutput("debtgdp"),
                                   plotOutput("t10y3mPlot")
                                   )
                            ),
                    
                    
                    # one row for input slider
                    fluidRow(
                            column(12,
                                   align = "center",
                                   sliderInput("mo2",
                                               "Lookback Period (Years)",
                                               min = 1,
                                               max = 30,
                                               value = 2)
                                   )
                            )
                    ) # end tab two
                ) # end user interface





#==================================================#
#================= SERVER =========================#
#==================================================#


server <- function(input, output) {
   
  #======= Retrieve Data =======#
  
  CUE <- getFedData("UNRATE")
  RRS <- getFedData("RRSFS")
  T10Y3M <- getFedData("T10Y3M")
  INDPRO <- getFedData("INDPRO")
  RGDP <- getFedData("GDPC1")
  PGDP <- getFedData("GDPPOT")
  GDP <- getFedData("GDP")
  CDEBT <- getFedData("NCBDBIQ027S")/1000
  
  
  
  #======= DATA PREP =======#
  
  # unemployment MA
  ma10 <- SMA(CUE$value, n = 10)
  
  # indpro yoy calc and zero line
  INDPRO <- (INDPRO - lag(INDPRO, 12)) / lag(INDPRO, 12)
  INDPRO$zero <- 0
  
  # real retail sales yoy calc and zero line
  RRS <- (RRS - lag(RRS, 12)) / lag(RRS, 12)
  RRS$zero <- 0
  
  # CALC GDP OUTPUT GAP RATIO
  GAP <- 100*(RGDP - PGDP)/RGDP
  GAP$zero <- 0
  
  DEBTGDP <- CDEBT/GDP
  
  #======= LEADING INDICATOR PLOTS =======#
  
   # Unemployment Plot #
   output$cuePlot <- renderPlot({
  
     plot(tail(CUE, 12*as.numeric(input$mo)), main = "Unemployment Rate")
     lines(tail(ma10, 12*as.numeric(input$mo)), col = "red", lwd = 2)
     addLegend(legend.loc = "topright", legend.names = c("Values", "SMA10"), lty = c(1,1), col =  c("black","red"))
     
   })
   
   # Industrial Production Index #
   output$indproPlot <- renderPlot({
     
     plot(tail(INDPRO*100, 12*as.numeric(input$mo)), main = "Industrial Prod. Index, % Change YoY")
     lines(tail(INDPRO$zero, 12*as.numeric(input$mo)), col = "red", lwd = 3)
     
   })
   
   # Retail Sales Plot #
   output$rrsPlot <- renderPlot({
  
     plot(tail(RRS*100, 12*as.numeric(input$mo)), main = "Real Retail Sales, % Change YoY")
     lines(tail(RRS$zero, 12*as.numeric(input$mo)), col = "red", lwd = 3)
     
   })
   
   # output gap
   output$gapPlot <- renderPlot({
     
     # gdp is quarterly measurement
     plot(tail(GAP, 4*as.numeric(input$mo)), main = "Output Gap (GDP)")
     lines(tail(GAP$zero, 4*as.numeric(input$mo)), col = "red", lwd = 3)
     
   })
   

   
   

   
   
   
   #======= FINANCIAL MARKETS PLOTS =======#
   
   # Yield Curve #

   output$debtgdp <- renderPlot({
     
     plot(tail(DEBTGDP, 4*as.numeric(input$mo2)), main = "Corporate Debt to GDP")

     
   })
   
   output$t10y3mPlot <- renderPlot({
     
     T10Y3M$zero <- 0
     plot(tail(T10Y3M*10, 12*as.numeric(input$mo2)), main = "10 Year - 3 Month Treasury")
     lines(tail(T10Y3M$zero, 22*12*as.numeric(input$mo2)), col = "red", lwd = 3)
     
   })

   

   
}

# Run the application 
shinyApp(ui = ui, server = server)

