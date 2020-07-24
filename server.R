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
theme_set(theme_classic())

`%nin%` = Negate(`%in%`)
nonChannel <- c("minecraft-server-chat")
botList <- people %>% filter(IsBot) %>% select(ID)
defChannel <- c("general", "married", "youth", "ysa", "pc-gaming", "playstation", "xbox", "mobile")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$linePlot <- renderPlot({
        iperiod <- input$period
        interval <- paste(iperiod, "week")
        lumpLength <- 7
        startDate <- as.POSIXct(input$dateRange[1])
        endDate <- as.POSIXct(input$dateRange[2])
        ispan <- floor((endDate - startDate)[[1]]/(7*iperiod))
        sparam <- input$smoother*64/ispan + 8/ispan
        channelList <- input$channelName
        userList <- input$userName
        
        author_messages <- reactive({
            messages %>%
                {if(!is.null(userList))
                    filter(., Author %in% userList)
                    else
                        filter(., Author %nin% botList$ID)}
        })
        
        channel_messages <- reactive({
            author_messages() %>%
                {if(!is.null(channelList))
                    filter(., channel_name %in% channelList)
                    else 
                        filter(., channel_name %nin% nonChannel)}
        })
        
        time_messages <- reactive({
            channel_messages() %>%
                filter(between(TimeSent, startDate, endDate))
        })
        
        summarise_author <- reactive({
            time_messages() %>%
                mutate(Author = fct_infreq(fct_lump(Author, lumpLength)),
                       TimeSent = floor_date(TimeSent, interval)) %>%
                droplevels() %>%
                group_by(TimeSent, Author) %>%
                summarise(n = n()) %>%
                complete(TimeSent, Author,
                         fill = list(n = 0)) %>%
                left_join(people, by = c("Author" = "ID"))
        })
        
        summarise_channel <- reactive({
            time_messages() %>%
                mutate(channel_name = fct_infreq(fct_lump(channel_name, lumpLength)),
                       TimeSent = floor_date(TimeSent, interval)) %>%
                droplevels() %>%
                group_by(TimeSent, channel_name) %>%
                summarise(n = n()) %>%
                complete(TimeSent, channel_name, fill = list(n = 0))
        })
        
        stacked_smooth_author_plot <- reactive({
            summarise_author() %>%
                ggplot(aes(x = TimeSent, y = n, group = Name, fill = Name)) +
                geom_smooth(position = position_stack(reverse = TRUE),
                            color = "black",
                            size = 0.1,
                            alpha = 1,
                            method = "loess",
                            formula = y ~ x,
                            span = sparam
                ) +
                scale_fill_manual(values=cbbPalette) +
                labs(fill = "Users", x = "Year", y = "Number of Messages")
        })
        
        line_smooth_author_plot <- reactive({
            summarise_author() %>%
                ggplot(aes(x = TimeSent, y = n, group = Name, color = Name)) +
                geom_smooth(alpha = 1,
                            method = "loess",
                            formula = y ~ x,
                            span = sparam,
                            se = FALSE
                ) +
                scale_color_manual(values=cbbPalette) +
                labs(color = "Users", x = "Year", y = "Number of Messages")
        })
        
        stacked_smooth_channel_plot <- reactive({
            summarise_channel() %>%
                ggplot(aes(x = TimeSent, y = n, group = channel_name, fill = channel_name)) +
                geom_smooth(position = position_stack(reverse = TRUE),
                            color = "black",
                            size = 0.1,
                            alpha = 1,
                            method = "loess",
                            formula = y ~ x,
                            span = sparam
                ) +
                scale_fill_manual(values=cbbPalette) +
                labs(fill = "Channel", x = "Year", y = "Number of Messages")
        })
        
        line_smooth_channel_plot <- reactive({
            summarise_channel() %>%
                ggplot(aes(x = TimeSent, y = n, group = channel_name, color = channel_name)) +
                geom_smooth(alpha = 1,
                            method = "loess",
                            formula = y ~ x,
                            span = sparam,
                            se = FALSE
                ) +
                scale_color_manual(values=cbbPalette) +
                labs(color = "Channel", x = "Year", y = "Number of Messages")
        })
        
        stacked_raw_author_plot <- reactive({
            summarise_author() %>%
                ggplot(aes(x = TimeSent, y = n, group = Name, fill = Name)) +
                geom_area(position = position_stack(reverse = TRUE),
                            color = "black",
                            size = 0.1,
                ) +
                scale_fill_manual(values=cbbPalette) +
                labs(fill = "Users", x = "Year", y = "Number of Messages")
        })
        
        line_raw_author_plot <- reactive({
            summarise_author() %>%
                ggplot(aes(x = TimeSent, y = n, group = Name, color = Name)) +
                geom_line() +
                scale_color_manual(values=cbbPalette) +
                labs(color = "Users", x = "Year", y = "Number of Messages")
        })
        
        stacked_raw_channel_plot <- reactive({
            summarise_channel() %>%
                ggplot(aes(x = TimeSent, y = n, group = channel_name, fill = channel_name)) +
                geom_area(position = position_stack(reverse = TRUE),
                            color = "black",
                            size = 0.1,
                ) +
                scale_fill_manual(values=cbbPalette) +
                labs(fill = "Channel", x = "Year", y = "Number of Messages")
        })
        
        line_raw_channel_plot <- reactive({
            summarise_channel() %>%
                ggplot(aes(x = TimeSent, y = n, group = channel_name, color = channel_name)) +
                geom_line() +
                scale_color_manual(values=cbbPalette) +
                labs(color = "Channel", x = "Year", y = "Number of Messages")
        })
        
        pstring <- paste(input$stacking,
              input$smoothing,
              input$fct_grouping,
              "plot",
              sep = "_")
        p <- eval(sym(pstring))() + labs(title = "Messages by time")
        print(p)
    })
    
    output$userText <- renderText(
        print(class(input$channelName))
        #format(seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), by = "4 week"), "%Y-%V")
    )
    
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