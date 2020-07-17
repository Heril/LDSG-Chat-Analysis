#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)
library(tidyverse)
`%nin%` = Negate(`%in%`)
nonChannel <- c("minecraft-server-chat")
defChannel <- c("general", "married", "youth", "ysa", "pc-gaming", "playstation", "xbox", "mobile")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$distPlot <- renderPlot({
        nChannel <- length(input$channelName)
        channelList <- ifelse(nChannel == 0, defChannel, input$channelName)
        lumpLength <- ifelse(nChannel == 0 || nChannel > 7, 7, nChannel)
        if(length(input$userName) != 0) {
            lmessages <- messages %>%
                filter(Author %in% input$userName)
        } else {
            lmessages <- messages
        }
        if (nChannel != 0) {
            lmessages <- lmessages %>%
                filter(channel_name %in% input$channelName)
        } else {
            lmessages <- lmessages %>%
                filter(channel_name %nin% nonChannel)
        }
        lmessages <- lmessages %>%
            filter(TimeSent >= input$dateRange[1] & TimeSent <= input$dateRange[2])
        lmessages$channel_name <- lmessages$channel_name %>%
            fct_lump_n(lumpLength) %>%
            fct_infreq() %>%
            fct_rev()
        lmessages$channel_name <- droplevels(lmessages$channel_name)
        weekdata <- lmessages %>%
            mutate(WeekSent = format(TimeSent, "%Y-%V")) %>%
            group_by(WeekSent, channel_name) %>%
            tally %>%
            ungroup() %>%
            complete(WeekSent, channel_name, 
                     fill = list(n = 0))
        weekdata$WeekSent <- as.factor(weekdata$WeekSent)
        print(
            ggplot(weekdata, aes(x = WeekSent, y = n)) +
                geom_area(aes(group = channel_name, fill = channel_name), color = "black", size = 0.1) +
                scale_fill_manual(values=cbbPalette)
        )
    })
    
    output$userText <- renderText(input$userName)
    output$channelPlot <- renderPlot({
        nChannel <- length(input$channelName)
        channelList <- ifelse(nChannel == 0, defChannel, input$channelName)
        lumpLength <- ifelse(nChannel == 0 || nChannel > 7, 7, nChannel)
        if(length(input$userName) != 0) {
            lmessages <- messages %>%
                filter(Author %in% input$userName)
        } else {
            lmessages <- messages
        }
        if (nChannel != 0) {
            lmessages <- lmessages %>%
                filter(channel_name %in% input$channelName)
        } else {
            lmessages <- lmessages %>%
                filter(channel_name %nin% nonChannel)
        }
        lmessages <- lmessages %>%
            filter(TimeSent >= input$dateRange[1] & TimeSent <= input$dateRange[2])
        lmessages$channel_name <- droplevels(lmessages$channel_name)
        channeldata <- lmessages %>%
            filter(channel_name %in% channelList) %>%
            mutate(WeekSent = format(TimeSent, "%Y-%V")) %>%
            group_by(WeekSent, channel_name) %>%
            tally %>%
            ungroup() %>%
            complete(WeekSent, channel_name, 
                     fill = list(n = 0))
        channeldata$WeekSent <- as.factor(channeldata$WeekSent)
        print(
            ggplot(channeldata, aes(x = WeekSent, y = n,  group = channel_name, color = channel_name)) +
            geom_line()
        )
    })
    
    output$wordcloudPlot <- renderPlot({
        if (length(input$userName) == 0){
            umessages <- messages
        } else {
            umessages <- messages %>%
            filter(Author %in% input$userName)
        }
        if(length(input$channelName != 0)) {
            mcloud <- umessages %>%
            filter(channel %in% input$channelName) %>%
            filter(TimeSent >= input$dateRange[1] & TimeSent <= input$dateRange[2])
        } else {
            mcloud <- umessages %>%
                filter(channel %nin% nonChannel) %>%
                filter(TimeSent >= input$dateRange[1] & TimeSent <= input$dateRange[2])
        }
        
        print(wordcloud(mcloud$Content, max.words = 50))
    })

})