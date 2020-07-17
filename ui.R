#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
channelList <- setNames(levels(messages$channel_name), levels(messages$channel_name))
userList <- setNames(people$ID, people$Name)

startTime <- strptime(head(sort(messages$TimeSent), n = 1), format = "%Y-%m-%d")
endTime <-  strptime(tail(sort(messages$TimeSent), n = 1), format = "%Y-%m-%d")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    sidebarLayout(
    sidebarPanel(
        selectInput("channelName",
                    "Select Channels:",
                    channelList, multiple = TRUE),
        selectInput("userName", "Select User(s):",
                    userList, multiple = TRUE),
        dateRangeInput("dateRange", "Date range:",
                    start  = startTime, end    = endTime,
                    min    = startTime, max    = endTime,
                    format = "mm/dd/yy",
                    separator = " - "),
        sliderInput("smoother", "Graph smoothing:",
                    min = 0.01, max = 0.25, value = 0.04, step = 0.01)
        ),
        mainPanel(
            tabsetPanel(
                # Application title
                tabPanel("Top Channels", plotOutput("distPlot")),
                tabPanel("Channel Data", plotOutput("channelPlot")),
                tabPanel("Word Cloud", plotOutput("wordcloudPlot"))
            )
        )
    )
))
