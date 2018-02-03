## Diss_dashboard.R ##

library(shiny)
library(shinydashboard)
library(leaflet)
library(zipcode)
library(tidyverse)
library(ggpubr)
library(tidytext)
library(plotly)
library(DT)
library(forcats)
library(lubridate)
require(tm)
library(topicmodels)

source("./data/theme_matt.R", local = TRUE)

diss_part2 <- readRDS("./data/diss_part2.rds")

combined_sum_l <- readRDS("./data/combined_sum_l.rds")

diss_data_app <- readRDS("./data/diss_data_app.rds")

tweet_data_ger_app <- readRDS("./data/tweet_data_ger_app.rds")

ui <- dashboardPage(
        dashboardHeader(title = "Matthias Raess - Dissertation Dashboard", titleWidth = 550),
        dashboardSidebar(sidebarMenu(
                menuItem("Introduction", tabName = "intro", icon = icon("book", lib = "glyphicon")),
                menuItem("Interactive Map", tabName = "map", icon = icon("globe", lib = "glyphicon")),
                menuItem("Rosling Chart", tabName = "rosling", icon = icon("stats", lib = "glyphicon"), badgeLabel = "wonky", badgeColor = "orange"),
                menuItem("Participant Explorer", tabName = "part_explorer", icon = icon("user", lib = "glyphicon")),
                menuItem("Tweet Explorer", tabName = "tweet_explorer", icon = icon("sort-by-attributes-alt", lib = "glyphicon")),
                menuItem("Topic Models", tabName = "topic", icon = icon("text-color", lib = "glyphicon"), badgeLabel = "updated", badgeColor = "green"),
                menuItem("Sentiments", tabName = "sent_compare", icon = icon("user", lib = "glyphicon"))
        )),
        
        dashboardBody(tags$head(tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        )),
        tabItems(
                tabItem(tabName = "intro",
                        fluidRow(
                                box(title = tags$b("Introduction"), 
                                width = 12, 
                                collapsible = FALSE, 
                                status = "primary",
                                tags$div(
                                        tags$p("This dashboard is a data product that resulted from my dissertation research with the title 'Interactions", 
                                               tags$sup(3), ": Language, demographics, and personality; an in-depth analysis of German tweets.'"),
                                        tags$p("In the spirit of open science, it is not only meant to complement the actual dissertation by making the collected data
                                         multidimensional, but also to make them accessible to a broader audience."),
                                        tags$p("Overall,", tags$i("N"), "= 62 German participants were surveyd to glean demographic information. 
                                               Then, their tweets", tags$i("N") ,"= 19,772 were collected through Twitter's open API (the limit is ~3,200 tweets per user).")),

                                tags$h5(tags$b("Contact:")),
                                "For any questions, email Matthias Raess at", tags$b("mraess@bsu.edu", style = "color:orange"),
                                br(),
                                br(),
                                "Connect with me via ", tags$b(tags$a("LinkedIn", href = "http://www.matthiasraess.com", target = "_blank")),
                                ", or follow me on ", tags$b(tags$a("Twitter", href = "https://twitter.com/primesty22")), ", where I tweet and retweet about stats, data science and R.",
                                br(), br(),
                                tags$b("Note:"),"For some reason, the Rosling Chart does not work on all browsers/computers.",
                                solidHeader = TRUE)
                        )),
                
                # First tab content
                # Boxes need to be put in a row (or column)
                tabItem(tabName = "map",
                        fluidRow(
                                mainPanel(box(leafletOutput("map", height = 600, width = 720), 
                                    footer = tags$b("For further details, click on the Twitter icons."), height = 600, width = 600, collapsible = FALSE, title = "Interactive Map"))
                                
                        )
                ),
                # Second tab item
                tabItem(tabName = "rosling",
                        fluidPage(
                                titlePanel("Rosling Chart Number of Tweets/Age over Time"),
                                
                                mainPanel(
                                        box(htmlOutput("rosling", height = 600, width = 600), 
                                            footer = "Please wait for the chart to load! Then, click 'play'!", width = 600)
                                )
                        )
                ),
                # Third tab item
                tabItem(tabName = "sent_compare",
                        fluidPage(
                                mainPanel(
                                        box(title = tags$b("Boxplot comparing sentiment scores with BAWL-R scores"),
                                            img(src = 'emo_plot.png', height = 401, width = 650),
                                            br(),
                                            tags$p("This is a boxplot comparing the sentiment scores (sent) with affect scores (emo) obtained from the revised Berlin Affective Word List (BAWL-R) affect dictionary.
                                            Data are only available for", tags$i("n"), " = 35 participants."),
                                            width = 12,
                                            status = "primary",
                                            collapsible = FALSE,
                                            solidHeader = TRUE)
                                )

                        )),
                
                # Forth tab item
                tabItem(tabName = "part_explorer",
                        fluidPage(
                                titlePanel(title = "Participant Explorer"),
                                sidebarPanel(helpText("Subset participant data to drill down,
                                                   and find out more about them. Please make a selection for", tags$b("every") ,"variable."),
                                        sliderInput(inputId = "age", label = "Age Range:", 
                                                    min = 20, 
                                                    max = 45, 
                                                    step = 1, 
                                                    value = c(20, 45)),
                                        helpText("Please select participant gender."),
                                        radioButtons(inputId = "gender", label = "Select Gender:", choices = list("Both" = "both", "Male" = "male", "Female" = "female"), selected = "both"),
                                        selectizeInput("rel", "Relationship Status:", 
                                                    choices = list("Relationship" =  "In a relationship", 
                                                                   "Single" = "Single", "Married" = "Married", "Divorced" = "Divorced"),
                                                    multiple = TRUE, options = list(placeholder = "Select a realtionship status", searchConjunction = "or", closeAfterSelect = TRUE)),
                                        selectizeInput("edu1", "Education:", choices = levels(diss_data_app$edu), 
                                                       multiple = TRUE, options = list(placeholder = "Select education level", searchConjunction = "or", closeAfterSelect = TRUE)),
                                        selectizeInput("edu2", "Continued Education:", choices = levels(diss_data_app$edu2),
                                                       multiple = TRUE, options = list(placeholder = "Select continued education", searchConjunction = "or", closeAfterSelect = TRUE)),
                                        sliderInput("time_twitter", "Time on Twitter [min/day]", min = 5, max = 45, step = 1, value = c(5, 45))),

                                mainPanel(fluidRow(valueBoxOutput("part_sel"),
                                                   valueBoxOutput("mean_age"),
                                                   valueBoxOutput("tweet_num"),
                                                   valueBoxOutput("tweet_per"),
                                                   valueBoxOutput("hash_dens"),
                                                   valueBoxOutput("emoji_dens")),

                                          fluidRow(
                                                  box(title = "Density Plot: Number of Tweets", 
                                                      width = 6, 
                                                      collapsible = TRUE, 
                                                      plotOutput("plot1"), 
                                                      solidHeader = TRUE),
                                                  box(title = "Boxplot of BigFive Scores", 
                                                      width = 6, 
                                                      collapsible = TRUE, 
                                                      plotOutput("plot2"),
                                                      solidHeader = TRUE)),
                                          
                                          fluidRow(dataTableOutput("value1")))
                                
                                
                        )),
                #Fifth tab item
                        tabItem(tabName = "tweet_explorer",
                                fluidPage(titlePanel("Tweet Explorer"),
                                          sidebarPanel(helpText("Subset tweet data to drill down,
                                                   and find out more about the tweets. Takes about ~15 secs for initial load, and about ~5 secs for every variable manipulation."),
                                                       sliderInput(inputId = "hour", label = "Hour of day:",
                                                                   min = hour("2011-06-04 00:00:00"),
                                                                   max = hour("2011-08-10 23:00:00"),
                                                                   value = c(hour("2011-06-04 00:00:00"),
                                                                             hour("2011-08-10 23:00:00")), step = 1),
                                                       selectizeInput("days", "Days of the week:", 
                                                                      choices = list("Sunday" =  "Sunday", "Monday" = "Monday", "Tuesday" = "Tuesday", 
                                                                                     "Wednesday" = "Wednesday", "Thursday" = "Thursday", "Friday" = "Friday", "Saturday" = "Saturday"),
                                                                      multiple = TRUE, options = list(placeholder = "Select a day of the week", 
                                                                                                      searchConjunction = "or", closeAfterSelect = TRUE)),
                                                       sliderInput(inputId = "age2", label = "Select participant age:",
                                                                   min = 20,
                                                                   max = 45,
                                                                   value = c(20,45),
                                                                   step = 1),
                                                       radioButtons(inputId = "gender_tweet", label = "Select Gender:", 
                                                                    choices = list("Both" = "both", "Male" = "male", "Female" = "female"), 
                                                                    selected = "both"),
                                                       helpText("Select the n-gram:"),
                                                       sliderInput(inputId = "ngram", label = "Select n:",
                                                                   min = 2,
                                                                   max = 6, 
                                                                   value = 2, step = 1)),

                                          mainPanel(fluidRow(
                                                             valueBoxOutput("sent_score"),
                                                             valueBoxOutput("unique"),
                                                             valueBoxOutput("tt_50")),
                                                    fluidRow(box(title = "Density plot of tweet times", 
                                                                 width = 12,
                                                                 plotOutput("plot3"),
                                                                 collapsible = TRUE,
                                                                 solidHeader = TRUE)), 
                                                    fluidRow(box(title = " Ngrams", 
                                                                 dataTableOutput("value2"),
                                                                 width = 12,
                                                                 collapsible = TRUE,
                                                                 solidHeader = TRUE)))

                        )),
                # Sixth tab item - topic
                tabItem(tabName = "topic",
                        fluidPage(sidebarPanel(helpText("This section creates a topic model, using Latent Dirichlet Allocation (LDA). Give it some TIME!! :). 
                                                        It takes up to 30 secs for initial load, and up to 15 secs per variable manipulation."),
                                               sliderInput(inputId = "hour2", label = "Hour of day:",
                                                           min = hour("2011-06-04 00:00:00"),
                                                           max = hour("2011-08-10 23:00:00"),
                                                           value = c(hour("2011-06-04 00:00:00"),
                                                                     hour("2011-08-10 23:00:00")), step = 1),
                                               selectizeInput("days2", "Days of the week:", 
                                                              choices = list("Sunday" =  "Sunday", "Monday" = "Monday", "Tuesday" = "Tuesday", 
                                                                             "Wednesday" = "Wednesday", "Thursday" = "Thursday", "Friday" = "Friday", "Saturday" = "Saturday"),
                                                              multiple = TRUE, options = list(placeholder = "Select a day of the week", 
                                                                                              searchConjunction = "or", closeAfterSelect = TRUE)),
                                               sliderInput(inputId = "age3", label = "Select participant age:",
                                                           min = 20,
                                                           max = 45,
                                                           value = c(20,45),
                                                           step = 1),
                                               radioButtons(inputId = "gender_topic", label = "Select Gender: ", 
                                                            choices = list("Both" = "both", "Male" = "male", "Female" = "female"), 
                                                            selected = "both"),
                                               sliderInput(inputId = "k", label = "Select number of topics:",
                                                           min = 2,
                                                           max = 6, 
                                                           value = 2, step = 1)),
                                  
                                  mainPanel(
                                            box(title = "Topic Model",
                                                plotOutput("plot4"),
                                                width = 600, height = 600,
                                                collapsible = TRUE,
                                                solidHeader = TRUE))))
                        

)))




#  ----- server.R -----



twitterIconBlue <- makeIcon(
        iconUrl = "./data/twitter_blue.png",
        iconWidth = 24,
        iconHeight = 24,
        iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

twitterIconRed <- makeIcon(
        iconUrl = "./data/twitter_red.png",
        iconWidth = 24,
        iconHeight = 24,
        iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

## Adding popup-info to data frame


diss_part2 <- diss_part2 %>% mutate(gender = fct_recode(gender, "Female" = "female",
                                                       "Male" = "male")) %>% 
        mutate(popup_info = paste(sep = "<br/>", paste0("<b>","<i>",gender,"<i>", "      
                                                        </b>"), city, edu, edu2, 
                                  paste0("Number of tweets: " , tweet_num)))

twitterIcons <- iconList(Male = twitterIconBlue, Female = twitterIconRed)

server <- function(input, output) {
        
        
## Leaflet map
        output$map <- renderLeaflet({
                
                leaflet(diss_part2) %>%
                        addTiles() %>% 
                        addMarkers(lng = ~lon, 
                                   lat = ~lat, icon = ~twitterIcons[diss_part2$gender],
                                   popup = ~popup_info, clusterOptions = markerClusterOptions())
        })
## Rosling Chart        
        getPage <- function(){
                return(includeHTML("./data/GenderTweetsAge1.html"))
        }
        
        output$rosling <- renderUI({getPage()})

        
## Participant explorer
        # Get inputs and subset data
        reactiveOutput <- reactive({
                
                slider_values <- c(min(input$age), max(input$age))
                
                slider_values2 <- c(min(input$time_twitter), max(input$time_twitter))
                
                selection <- switch(input$gender,
                                   male = "male",
                                   female = "female",
                                   both = c("male", "female"))
                
                # Subsetting data
                diss_data_app %>% filter(age >= slider_values[1] & age <= slider_values[2]) %>% 
                        filter(gender %in% selection) %>% filter(relationship %in% input$rel) %>% 
                        filter(edu %in% input$edu1) %>% filter(edu2 %in% input$edu2) %>% 
                        filter(time_twitter_min >= slider_values2[1] & time_twitter_min <= slider_values2[2])
        })
        # Participant table
        output$value1 <- renderDataTable({
                reactiveOutput()
        }, options = list(scrollX = TRUE, pageLength = 5, lengthMenu = c(5, 10, 15)))
        
        # Histogram
        output$plot1 <- renderPlot({
                ggplot(reactiveOutput(), aes(tweet_num)) + 
                        geom_histogram(aes(y = ..density..), stat = "density", alpha = .5, fill = "#1dcaff") + 
                        geom_line(aes(y = ..density..), lwd = 1, stat = "density", col = "steelblue") +
                        theme_matt() +
                        xlab("Number of tweets") +
                        ylab("Density")
        })
        # BFI-scores - boxplot
        
        output$plot2 <- renderPlot({
                
                
                if(input$gender == "both"){
                
                reactiveOutput() %>% gather(`e`, `a`, `c`, `n`, `o`, key = "p_type", value = "bfi10_score") %>% 
                ggplot() + 
                        geom_boxplot(aes(p_type, bfi10_score, fill = gender), notch = FALSE, outlier.color = "red",
                                                                outlier.size = 1.5, outlier.shape = 18) + 
                        scale_x_discrete(labels=c("a" = "A", "c" = "C", "e" = "E",
                                                  "n" = "N", "o" = "O")) +
                        theme_matt() + 
                        scale_fill_manual(values = c(female = "tomato", male = "steelblue"), name = "Gender", labels = c("Male", "Female")) +
                        theme(legend.position = "right", legend.text = element_text(size = 8),
                              legend.title = element_text(size = 9), axis.title = element_text(size = 9), 
                              axis.text = element_text(size = 9, color = "black")) +
                        xlab("Big Five domain") +
                        ylab("BFI-10 Big Five score") +
                        stat_summary(mapping = aes(p_type, bfi10_score, fill = gender), 
                                     fun.y = mean, colour="orange", geom="point", shape=16, size=1.5, 
                                     position = position_dodge(width = 0.75)) +
                        scale_y_continuous(limits = c(1, 5), breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
                                           labels = c("1", "", "2", "", "3", "", "4", "", "5")) +
                        labs(caption = expression(paste(italic(bar(x)), " = orange dot"))) +
                        theme(plot.caption = element_text(hjust = 0, size = 10))
                }
                else if(input$gender == "male"){
                        reactiveOutput() %>% gather(`e`, `a`, `c`, `n`, `o`, key = "p_type", value = "bfi10_score") %>% 
                                ggplot() + 
                                geom_boxplot(aes(p_type, bfi10_score), fill = "steelblue", col = "black", notch = FALSE,
                                             outlier.size = 1.5, outlier.shape = 18, outlier.colour = "red") + 
                                scale_x_discrete(labels=c("a" = "A", "c" = "C", "e" = "E",
                                                          "n" = "N", "o" = "O")) +
                                theme_matt() + 
                                theme(legend.position = "right", legend.text = element_text(size = 8),
                                      legend.title = element_text(size = 9), axis.title = element_text(size = 9), 
                                      axis.text = element_text(size = 9, color = "black")) +
                                xlab("Big Five domain") +
                                ylab("BFI-10 Big Five score") +
                                stat_summary(mapping = aes(p_type, bfi10_score), 
                                             fun.y = mean, colour="orange", geom="point", shape=16, size=1.5) +
                                scale_y_continuous(limits = c(1,5), breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
                                                   labels = c("1", "", "2", "", "3", "", "4", "", "5")) +
                                labs(caption = expression(paste(italic(bar(x)), " = orange dot"))) +
                                theme(plot.caption = element_text(hjust = 0, size = 10))      
                }
                
                else if(input$gender == "female"){
                        reactiveOutput() %>% gather(`e`, `a`, `c`, `n`, `o`, key = "p_type", value = "bfi10_score") %>% 
                                ggplot() + 
                                geom_boxplot(aes(p_type, bfi10_score), fill = "tomato", col = "black", notch = FALSE,
                                             outlier.size = 1.5, outlier.shape = 18, outlier.colour = "red") + 
                                scale_x_discrete(labels=c("a" = "A", "c" = "C", "e" = "E",
                                                          "n" = "N", "o" = "O")) +
                                theme_matt() + 
                                theme(legend.position = "right", legend.text = element_text(size = 8),
                                      legend.title = element_text(size = 9), axis.title = element_text(size = 9), 
                                      axis.text = element_text(size = 9, color = "black")) +
                                xlab("Big Five domain") +
                                ylab("BFI-10 Big Five score") +
                                stat_summary(mapping = aes(p_type, bfi10_score), 
                                             fun.y = mean, colour="orange", geom="point", shape=16, size=1.5) +
                                scale_y_continuous(limits = c(1,5), breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
                                                   labels = c("1", "", "2", "", "3", "", "4", "", "5")) +
                                labs(caption = expression(paste(italic(bar(x)), " = orange dot"))) +
                                theme(plot.caption = element_text(hjust = 0, size = 10)) 
                }
                
                                 
                                
                })
                
        output$part_sel <- renderValueBox({
                valueBox(value = nrow(reactiveOutput()), subtitle = "Number of participants selected", icon = icon("user"),
                         color = dplyr::case_when(nrow(reactiveOutput()) <= 9 ~ "red",
                                                  nrow(reactiveOutput()) <= 17 ~ "yellow",
                                                  nrow(reactiveOutput()) >= 18 ~ "green"))
                })
        
        output$mean_age <- renderValueBox({
                valueBox(value = round(mean(reactiveOutput()$age), 2), subtitle = "Participants' age [mean]", icon = icon("user"), col = "orange")
        })

        output$tweet_num <- renderValueBox({
                valueBox(value = formatC(sum(reactiveOutput()$tweet_num), format="d", big.mark=','), subtitle = "Number of tweets produced", icon = icon("paper-plane"), col = "aqua")
        })
        
        output$tweet_per <- renderValueBox({
                valueBox(value = paste0(round((sum(reactiveOutput()$tweet_num)/19772) * 100, 2), " %"), subtitle = "Percentage of tweets", icon = icon("paper-plane"), col = "aqua")
        })
        
        output$hash_dens <- renderValueBox({
                valueBox(value = paste0(round(mean(reactiveOutput()$hash_dens), 2), " %"), subtitle = "Hashtag density [mean]", icon = icon("paperclip"), col = "light-blue")
        })
        
        output$emoji_dens <- renderValueBox({
                valueBox(value = paste0(round(mean(reactiveOutput()$emoji_dens), 2), " %"), subtitle = "Emoji density [mean]", icon = icon("heart"), col = "maroon")
        })
        
## Tweet explorer
        ## Get reactive values
        reactiveOutput2 <- reactive({
                
                slider_values2 <- c(min(input$hour), max(input$hour))
                
                slider_values3 <- c(min(input$age2), max(input$age2))

                selection2 <- switch(input$gender_tweet,
                                    male = "male",
                                    female = "female",
                                    both = c("male", "female"))
                
                # Subsetting data
                tweet_data_ger_app %>% mutate(hour = hour(created), day = wday(created, label = TRUE, abbr = FALSE)) %>% 
                        filter(hour >= slider_values2[1] & hour <= slider_values2[2]) %>% 
                        filter(age >= slider_values3[1] & age <= slider_values3[2]) %>% 
                        filter(gender %in% selection2)
                        
        })
        
        ### Value boxes - tweets
        
        output$sent_score <- renderValueBox({
                
                if(is.null(input$days)){
                        
                        sent <- reactiveOutput2() %>% na.omit() %>% summarise(sent = mean(sent_score))
                        
                        valueBox(value = round(sent, 3), subtitle = "Sentiment score [mean]", icon = icon("heart"), col = "maroon")
                }
                
                else{
                
                        sent <- reactiveOutput2() %>% filter(day %in% input$days) %>% na.omit() %>% summarise(sent = mean(sent_score))
                
                        valueBox(value = round(sent, 3), subtitle = "Sentiment score [mean]", icon = icon("heart"), col = "maroon")
                }
        })
        
        output$unique <- renderValueBox({
                
                if(is.null(input$days)){
                        
                        valueBox(value = round(mean(reactiveOutput2()$uniq_words_per_tweet), 2), subtitle = "Words per tweet [mean]", icon = icon("text-color", lib = "glyphicon"), col = "aqua")
                }
                
                else{
                
                        unique <- reactiveOutput2() %>% filter(day %in% input$days)
                
                        valueBox(value = round(mean(unique$uniq_words_per_tweet), 2), subtitle = "Words per tweet [mean]", icon = icon("text-color", lib = "glyphicon"), col = "aqua")
                }
        })
        
        output$tt_50 <- renderValueBox({
                
                if(is.null(input$days)){
                        
                        unique <- reactiveOutput2() %>% filter(!is.na(tt_50))
                        
                        valueBox(value = round(mean(unique$tt_50), 2), subtitle = "Days to 50 tweets [mean]", icon = icon("calendar", lib = "glyphicon"), col = "blue")
                }
                
                else{
                        
                        unique <- reactiveOutput2() %>% filter(day %in% input$days) %>% filter(!is.na(tt_50))
                        
                        valueBox(value = round(mean(unique$tt_50), 2), subtitle = "Days to 50 tweets [mean]", icon = icon("calendar", lib = "glyphicon"), col = "blue")
                }
        })
        
        ## Tweet density plot
        
        output$plot3 <- renderPlot({
                
                day1 <- input$days
                
                if(is.null(input$days)){
                                
                        reactiveOutput2() %>% 
                
                        ggplot(aes(hour)) + 
                                geom_histogram(aes(y = ..density..), stat = "density", alpha = .2, fill = "#1dcaff") + 
                                geom_line(aes(y = ..density..), lwd = 1, stat = "density", col = "steelblue") +
                                geom_density(aes(x = hour, y = ..density..), col = "steelblue", 
                                     lwd = 1, fill = "#1dcaff", alpha = 0.5)+
                                theme_matt() +
                                xlab("Time") +
                                ylab("Density") + 
                                ggtitle("Overall tweet density") +
                                scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), 
                                          labels = c("12am", "2am", "4am", "6am", "8am", "10am", "12pm", "2pm", "4pm", "6pm", "8pm", "10pm"))
                        }
                
                else if(length(input$days) == 1){
                        
                        reactiveOutput2() %>% filter(day %in% input$days) %>% 
                        
                        ggplot(aes(hour)) + 
                                geom_histogram(aes(y = ..density..), stat = "density", alpha = .2, fill = "#1dcaff") + 
                                geom_line(aes(y = ..density..), lwd = 1, stat = "density", col = "steelblue") +
                                geom_density(aes(x = hour, y = ..density..), col = "steelblue", 
                                             lwd = 1, fill = "#1dcaff", alpha = 0.5)+
                                theme_matt() +
                                xlab("Time") +
                                ylab("Density") + 
                                ggtitle(paste0("Tweet Density for ", day1[1])) +
                                scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), 
                                                   labels = c("12am", "2am", "4am", "6am", "8am", "10am", "12pm", "2pm", "4pm", "6pm", "8pm", "10pm"))
                
                        }
                
                else if(length(input$days) == 2){
                        
                        reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                
                                ggplot(aes(hour)) + 
                                geom_histogram(aes(y = ..density..), stat = "density", alpha = .2, fill = "#1dcaff") + 
                                geom_line(aes(y = ..density..), lwd = 1, stat = "density", col = "steelblue") +
                                geom_density(aes(x = hour, y = ..density..), col = "steelblue", 
                                             lwd = 1, fill = "#1dcaff", alpha = 0.5)+
                                theme_matt() +
                                xlab("Time") +
                                ylab("Density") + 
                                ggtitle(paste0("Tweet Density for ", day1[1], " and ", day1[2])) +
                                scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), 
                                                   labels = c("12am", "2am", "4am", "6am", "8am", "10am", "12pm", "2pm", "4pm", "6pm", "8pm", "10pm"))    
                }
                
                else if(length(input$days) == 3){
                        
                        reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                
                                ggplot(aes(hour)) + 
                                geom_histogram(aes(y = ..density..), stat = "density", alpha = .2, fill = "#1dcaff") + 
                                geom_line(aes(y = ..density..), lwd = 1, stat = "density", col = "steelblue") +
                                geom_density(aes(x = hour, y = ..density..), col = "steelblue", 
                                             lwd = 1, fill = "#1dcaff", alpha = 0.5)+
                                theme_matt() +
                                xlab("Time") +
                                ylab("Density") + 
                                ggtitle(paste0("Tweet Density for ", day1[1], ", ", day1[2], ", and ", day1[3])) +
                                scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), 
                                                   labels = c("12am", "2am", "4am", "6am", "8am", "10am", "12pm", "2pm", "4pm", "6pm", "8pm", "10pm"))    
                }
                
                else if(length(input$days) == 4){
                        
                        reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                
                                ggplot(aes(hour)) + 
                                geom_histogram(aes(y = ..density..), stat = "density", alpha = .2, fill = "#1dcaff") + 
                                geom_line(aes(y = ..density..), lwd = 1, stat = "density", col = "steelblue") +
                                geom_density(aes(x = hour, y = ..density..), col = "steelblue", 
                                             lwd = 1, fill = "#1dcaff", alpha = 0.5)+
                                theme_matt() +
                                xlab("Time") +
                                ylab("Density") + 
                                ggtitle(paste0("Tweet Density for ", day1[1], ", ", day1[2], ", ", day1[3], ", and ", day1[4])) +
                                scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), 
                                                   labels = c("12am", "2am", "4am", "6am", "8am", "10am", "12pm", "2pm", "4pm", "6pm", "8pm", "10pm"))    
                }
                
                else if(length(input$days) == 5){
                        
                        reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                
                                ggplot(aes(hour)) + 
                                geom_histogram(aes(y = ..density..), stat = "density", alpha = .2, fill = "#1dcaff") + 
                                geom_line(aes(y = ..density..), lwd = 1, stat = "density", col = "steelblue") +
                                geom_density(aes(x = hour, y = ..density..), col = "steelblue", 
                                             lwd = 1, fill = "#1dcaff", alpha = 0.5)+
                                theme_matt() +
                                xlab("Time") +
                                ylab("Density") + 
                                ggtitle(paste0("Tweet Density for ", day1[1], ", ", day1[2], ", ", day1[3], ", ", day1[4], ", and ", day1[5])) +
                                scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), 
                                                   labels = c("12am", "2am", "4am", "6am", "8am", "10am", "12pm", "2pm", "4pm", "6pm", "8pm", "10pm"))    
                }
                
                else if(length(input$days) == 6){
                        
                        reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                
                                ggplot(aes(hour)) + 
                                geom_histogram(aes(y = ..density..), stat = "density", alpha = .2, fill = "#1dcaff") + 
                                geom_line(aes(y = ..density..), lwd = 1, stat = "density", col = "steelblue") +
                                geom_density(aes(x = hour, y = ..density..), col = "steelblue", 
                                             lwd = 1, fill = "#1dcaff", alpha = 0.5)+
                                theme_matt() +
                                xlab("Time") +
                                ylab("Density") + 
                                ggtitle(paste0("Tweet Density for ", day1[1], ", ", day1[2], ", ", day1[3], ", ", day1[4], ", ", day1[5], ", and " ,day1[6])) +
                                scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), 
                                                   labels = c("12am", "2am", "4am", "6am", "8am", "10am", "12pm", "2pm", "4pm", "6pm", "8pm", "10pm"))    
                }
        })
        
        ### N-gram table

        output$value2 <- renderDataTable({
                
                ger_stop <- tm::stopwords("german") 
                
                if (is.null(input$days)){
                
                        if(input$ngram == 2){
                        
                        tidy_text <- reactiveOutput2()  %>% 
                                unnest_tokens(bigram, clean_w_punct, token = "ngrams", n = 2) %>% 
                                separate(bigram, c("word1", "word2"), sep = " ") %>% 
                                filter(!word1 %in% ger_stop) %>% 
                                filter(!word2 %in% ger_stop) %>% 
                                unite(col = bigram, c("word1", "word2"), sep = " ")
                        
                        tidy_text %>% count(bigram, sort = TRUE)
                }
                
                else if(input$ngram == 3){
                        
                        tidy_text <- reactiveOutput2()  %>% 
                                unnest_tokens(trigram, clean_w_punct, token = "ngrams", n = 3) %>% 
                                separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
                                filter(!word1 %in% ger_stop) %>% 
                                filter(!word2 %in% ger_stop) %>% 
                                filter(!word3 %in% ger_stop) %>% 
                                unite(col = trigram, c("word1", "word2", "word3"), sep = " ")
                        
                        tidy_text %>% count(trigram, sort = TRUE)
                }
                
                else if(input$ngram == 4){
                        
                        tidy_text <- reactiveOutput2()  %>% 
                                unnest_tokens(quadgram, clean_w_punct, token = "ngrams", n = 4) %>% 
                                separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
                                filter(!word1 %in% ger_stop) %>% 
                                filter(!word2 %in% ger_stop) %>% 
                                filter(!word3 %in% ger_stop) %>% 
                                filter(!word4 %in% ger_stop) %>% 
                                unite(col = quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
                        
                        tidy_text %>% count(quadgram, sort = TRUE)
                }
                
                else if(input$ngram == 5){
                        
                        tidy_text <- reactiveOutput2()  %>% 
                                unnest_tokens(pentagram, clean_w_punct, token = "ngrams", n = 5) %>% 
                                separate(pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>% 
                                filter(!word1 %in% ger_stop) %>% 
                                filter(!word2 %in% ger_stop) %>% 
                                filter(!word3 %in% ger_stop) %>% 
                                filter(!word4 %in% ger_stop) %>%
                                filter(!word5 %in% ger_stop) %>% 
                                unite(col = pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
                        
                        tidy_text %>% count(pentagram, sort = TRUE)
                }
                
                else if(input$ngram == 6){
                        
                        tidy_text <- reactiveOutput2() %>% 
                                unnest_tokens(septagram, clean_w_punct, token = "ngrams", n = 6) %>% 
                                separate(septagram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ") %>% 
                                filter(!word1 %in% ger_stop) %>% 
                                filter(!word2 %in% ger_stop) %>% 
                                filter(!word3 %in% ger_stop) %>% 
                                filter(!word4 %in% ger_stop) %>%
                                filter(!word5 %in% ger_stop) %>%
                                filter(!word6 %in% ger_stop) %>% 
                                unite(col = septagram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
                        
                        tidy_text %>% count(septagram, sort = TRUE)
                }
                }
                
                else {                        
                
                        if(input$ngram == 2){
                        
                        tidy_text <- reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                unnest_tokens(bigram, clean_w_punct, token = "ngrams", n = 2) %>% 
                                separate(bigram, c("word1", "word2"), sep = " ") %>% 
                                filter(!word1 %in% ger_stop) %>% 
                                filter(!word2 %in% ger_stop) %>% 
                                unite(col = bigram, c("word1", "word2"), sep = " ")
                        
                        tidy_text %>% count(bigram, sort = TRUE)
                }
                        
                        else if(input$ngram == 3){
                                
                                tidy_text <- reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                        unnest_tokens(trigram, clean_w_punct, token = "ngrams", n = 3) %>% 
                                        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
                                        filter(!word1 %in% ger_stop) %>% 
                                        filter(!word2 %in% ger_stop) %>% 
                                        filter(!word3 %in% ger_stop) %>% 
                                        unite(col = trigram, c("word1", "word2", "word3"), sep = " ")
                                
                                tidy_text %>% count(trigram, sort = TRUE)
                        }
                        
                        else if(input$ngram == 4){
                                
                                tidy_text <- reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                        unnest_tokens(quadgram, clean_w_punct, token = "ngrams", n = 4) %>% 
                                        separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
                                        filter(!word1 %in% ger_stop) %>% 
                                        filter(!word2 %in% ger_stop) %>% 
                                        filter(!word3 %in% ger_stop) %>% 
                                        filter(!word4 %in% ger_stop) %>% 
                                        unite(col = quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
                                
                                tidy_text %>% count(quadgram, sort = TRUE)
                        }
                        
                        else if(input$ngram == 5){
                                
                                tidy_text <- reactiveOutput2() %>% filter(day %in% input$days) %>% 
                                        unnest_tokens(pentagram, clean_w_punct, token = "ngrams", n = 5) %>% 
                                        separate(pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>% 
                                        filter(!word1 %in% ger_stop) %>% 
                                        filter(!word2 %in% ger_stop) %>% 
                                        filter(!word3 %in% ger_stop) %>% 
                                        filter(!word4 %in% ger_stop) %>%
                                        filter(!word5 %in% ger_stop) %>% 
                                        unite(col = pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
                                
                                tidy_text %>% count(pentagram, sort = TRUE)
                        }
                        
                        else if(input$ngram == 6){
                                
                                tidy_text <- reactiveOutput2()%>% filter(day %in% input$days) %>% 
                                        unnest_tokens(septagram, clean_w_punct, token = "ngrams", n = 6) %>% 
                                        separate(septagram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ") %>% 
                                        filter(!word1 %in% ger_stop) %>% 
                                        filter(!word2 %in% ger_stop) %>% 
                                        filter(!word3 %in% ger_stop) %>% 
                                        filter(!word4 %in% ger_stop) %>%
                                        filter(!word5 %in% ger_stop) %>%
                                        filter(!word6 %in% ger_stop) %>% 
                                        unite(col = septagram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
                                
                                tidy_text %>% count(septagram, sort = TRUE)
                        }}
                
        }, options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(15, 25, 50)))
        
        
        ## Topic model
        
        ## Get reactive values
        reactiveOutput3 <- reactive({
                
                slider_values4 <- c(min(input$hour2), max(input$hour2))
                
                slider_values5 <- c(min(input$age3), max(input$age3))
                
                selection3 <- switch(input$gender_topic,
                                     male = "male",
                                     female = "female",
                                     both = c("male", "female"))
                
                # Subsetting data
                
                if(is.null(input$days2)){
                
                        tweet_data_ger_app %>% mutate(hour = hour(created), day = wday(created, label = TRUE, abbr = FALSE)) %>% 
                                filter(hour >= slider_values4[1] & hour <= slider_values4[2]) %>% 
                                filter(age >= slider_values5[1] & age <= slider_values5[2]) %>% 
                                filter(gender %in% selection3)
                }
                
                else{
                        tweet_data_ger_app %>% mutate(hour = hour(created), day = wday(created, label = TRUE, abbr = FALSE)) %>% 
                                filter(hour >= slider_values3[1] & hour <= slider_values3[2]) %>% 
                                filter(gender %in% selection3) %>% dplyr::filter(day %in% input$days2)
                        
                }
                
        })
        
        output$plot4 <- renderPlot({
                
                # The detail to be captured by the progress bar should be contained within this function and its braces
                withProgress(message = 'Creating topic model', 
                             detail = "This might take a while...", value = 0, expr = {
                

                                
                                # Incremental Progress Bar (add some more info if neccessary)
                                incProgress(1/10, detail = paste0(round((1/10)*100, 2), "%")) 
                        
                                corpus <- Corpus(VectorSource(reactiveOutput3()$clean_w_punct))
                
                                docs <- tm_map(corpus, removeNumbers)
                
                                # Remove common German stopwords
                                docs <- tm_map(docs, removeWords, stopwords("german"))
                
                                # Remove punctuations
                                docs <- tm_map(docs, removePunctuation)
                                
                                # Incremental Progress Bar (add some more info if neccessary)
                                incProgress(2/10, detail = paste0(round((3/10)*100, 2), "%")) 
                                
                                # Build DTM
                
                                doc_term <- DocumentTermMatrix(docs, 
                                               control = list(sparse = TRUE, removeNumbers = TRUE, removePunctuation = TRUE, stopwords = TRUE))
                
                                dtm <- doc_term[unique(doc_term$i),]
                                
                                # Incremental Progress Bar (add some more info if neccessary)
                                incProgress(2/10, detail = paste0(round((5/10)*100, 2), "%")) 
                
                                # Fit topic model
                                doc_lda <- LDA(dtm, k = input$k, method = "Gibbs")
                                
                                # Incremental Progress Bar (add some more info if neccessary)
                                incProgress(4/10, detail = paste0(round((9/10)*100, 2), "%")) 
                
                                tweet_topics <- tidy(doc_lda, matrix = "beta")
                                
                
                                top_terms <- tweet_topics %>%
                                         group_by(topic) %>%
                                        top_n(10, beta) %>%
                                        ungroup() %>%
                                        arrange(topic, -beta) %>% 
                                        mutate(term = reorder(term, beta))
                                
                                # Incremental Progress Bar (add some more info if neccessary)
                                incProgress(1/10, detail = paste0(round((10/10)*100, 2), "%")) 
                                
                        
                })

                ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
                        geom_col(show.legend = FALSE) +
                        facet_wrap(~ topic, scales = "free") +
                        coord_flip() +
                        theme_matt() + 
                        ylab("Beta = per-topic-per-word probability") + 
                        xlab("Terms") +
                        scale_fill_brewer(type = "qual", palette = "Set1") +
                        theme(axis.title = element_text(size = 10))
                
                
        })
        
}

shinyApp(ui, server)
