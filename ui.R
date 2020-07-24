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
        sliderInput("period", "Number of weeks to plot by:",
                    min = 1, max = 4, value = 1),
        sliderInput("smoother", "Graph smoothing:",
                    min = 0, max = 1, value = 0, step = 0.01),
        radioButtons("fct_grouping", "Group by:",
                     c("User" = "author",
                       "Channel" = "channel")),
        radioButtons("stacking", "Stack plots:",
                     c("Stacked" = "stacked",
                       "Non-stacked" = "line")),
        radioButtons("smoothing", "Smooth line plot:",
                     c("Smooth" = "smooth",
                       "Raw" = "raw"))
        ),
        mainPanel(
            tabsetPanel(
                # Application title
                tabPanel("Top Channels", plotOutput("linePlot")),
                tabPanel("Word Cloud", plotOutput("wordcloudPlot"))#,
                #tabPanel("Testing", plotOutput("testPlot"))
            )
        )
    )
))
