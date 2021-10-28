#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Akvo colour scheme:
## Akvo Orange red	    HEX #EA5547 rgb(234,85,71)
## Akvo Blue      	    HEX #404898 rgb(64,72,152)
## Akvo Turquoise green HEX #03AD8C rgb(3,173,140)
## Akvo Pink        	HEX #E04D95 rgb(224,77,149)

## Akvo fonts
# Roboto Condensed
# Assistant

library(shiny)
library(shinydashboard)

library(openxlsx)
library(here)
library(extrafont)
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)

# Householde data
household_data <- read.xlsx(here("Data", "SL_household.xlsx"))
names(household_data)[c(1, 43, 44, 45, 90, 91, 92)] <- c("ID", "photo_storage_lat",
                                                         "photo_storage_lon",
                                                         "photo_storage_acc",
                                                         "photo_hand_wash_lat",
                                                         "photo_hand_wash_lon",
                                                         "photo_hand_wash_acc")

# Caddisfly data
caddisfly <- read.xlsx(here("Data","SL_household_monitoring.xlsx"))
caddisfly <- caddisfly[,c("Identifier",
                          "ecoli_result_incubation|E.coli.test.result.AFTER.incubation.|Health.Risk.Category.(Based.on.MPN.and.Confidence.Interval)",
                          "--CADDISFLY--|MPN(MPN/100ml)",
                          "--CADDISFLY--|Upper.95%.Confidence.Interval",
                          "--CADDISFLY--|E.coli.test.result.AFTER.incubation.--Image")]
names(caddisfly) <- c("ID", "result", "MPN", "CI", "image")

# Combine data sources
SL_data <- left_join(household_data, caddisfly, by="ID")
SL_data <- SL_data %>% separate(result, c("risk", "outcome"), sep=" / ")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        #     sliderInput("bins",
        #                 "Number of bins:",
        #                 min = 1,
        #                 max = 10000,
        #                 value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Main water source and risk
    water_risk <- cast(SL_data, main_source_water ~ risk)
    water_risk_long <- melt(water_risk, id="main_source_water")
    water_risk_long$value <- as.numeric(as.character(water_risk_long$value))
    water_risk_long$main_source_water <- gsub("lake, pond, stream, canal, irrigation channels)", "etc.)", water_risk_long$main_source_water)
    
    water_risk_long <- within(
        water_risk_long,
        risk <- factor(risk, levels = c("Very High Risk", "High Risk", "Intermediate Risk", "Low Risk")
        )
    )
    
    # Plots
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        g <- ggplot(water_risk_long, aes(x=main_source_water, y=value, fill=risk))
        g +
            geom_bar(stat="identity", position='fill') +
            theme(axis.text.x = element_text(family='Roboto')) +
            coord_flip() +
            scale_fill_manual("legend",
                              values = c("Very High Risk"="#00e3b7", "High Risk"="#03AD8C", "Intermediate Risk"="#5c68db", "Low Risk"="#404898"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
