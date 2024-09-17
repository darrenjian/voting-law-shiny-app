# Upload packages
library(rgdal)
library(sp)
library(leaflet)
library(geojsonio)
library(shinythemes)
library(shinydashboard)
library(shiny)
library(tidyverse)

source("./Rsource/SwitchButton.R")

voting <- read.csv("state_voting_laws.csv", header = TRUE)
states <- geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp"
)

voting.df <- merge(states, voting, by.x = "name", by.y = "state")

pal <- colorNumeric(palette = "Purples", domain=NULL)

# create ui
ui <- shinyUI(fluidPage(theme = "button.css",
                        tags$head(HTML("<title>US State Voting Law App</title>")),
                        titlePanel(HTML("<center>US State Voting Law App")),
                        br(),
                        helpText(p(HTML("Use this Shiny app's 'State Simulation' tab to simulate the impact of voting 
                                   laws on each US state's 2020 presidential election turnout. <br/>
                                   Explore the 'Voting Laws' tab to display state maps grouped by four categories of voting laws: voter ID, 
                                   voter registration, felon voting, and early voting.")),),
                        br(),
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("State Simulation", fluid=TRUE,
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     h4(HTML("<b>Voting Policy Simulation</b>")),
                                                     fluidRow(
                                                         column(width = 12, 
                                                                HTML("Modify voting laws for selected
                                                            state (current laws displayed as default). Click button to display projected changes in turnout."),
                                                                br(),
                                                                br(),
                                                         ),
                                                         column(width = 6,
                                                                h5(HTML("<em>Voting expansions</em>")),
                                                                uiOutput("switch.1"),
                                                                uiOutput("switch.2"),
                                                                uiOutput("switch.3")
                                                         ),  
                                                         column(width = 6,
                                                                h5(HTML("<em>Voting restrictions</em>")),
                                                                uiOutput("switch.4"),
                                                                uiOutput("switch.5"),
                                                                uiOutput("switch.6")
                                                         ),
                                                         br(),
                                                         actionButton("simulate_graph", "Simulate turnout effects"),
                                                         # actionButton("clear_simulation", "Reset"),
                                                         br(),
                                                         br(),
                                                         plotOutput('simPlot'),
                                                     )),
                                                 mainPanel(
                                                     leafletOutput('simMap', height = 800),
                                                     br(),
                                                     h4(HTML("<b>Citations</b>")),
                                                     htmlOutput("citations")
                                                 ),
                                                 
                                             ),
                                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                           draggable = TRUE, top = 240, left = "auto", right = "40", bottom = "auto",
                                                           width = 240, height = "auto", style="text-align: left;",
                                                           
                                                           h4(HTML("<b>Selected State Info:</b>")),
                                                           
                                                           htmlOutput("state_info"),
                                             )
                                             ),    
                                    tabPanel("Voting Laws", fluid = TRUE,
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     # Select voting laws to display
                                                     selectInput("lawInput", "Select voting law category to display:",
                                                                 choices = c("Voter ID",
                                                                             "Voter Registration",
                                                                             "Felon Voting Rights",
                                                                             "Early Voting")),
                                                     actionButton("update_plot", "Update Plot"),
                                                 ),
                                                 mainPanel(leafletOutput('lawMap', height = 800))
                                             )
                                    )
                                    
                        )
                        
)
)


# create the server
server <- function( input, output, session ){
    
    
    
    
    # selected law
    
    description <- reactive({
        switch(input$lawInput,
               "Voter ID"=voting.df$ID_law, 
               "Voter Registration"=voting.df$registration_law,
               "Felon Voting Rights"=voting.df$felon_voting_law,
               "Early Voting"=voting.df$early_voting_law)
    })
    
    fill <- reactive({
        switch(input$lawInput,
               "All"=voting.df$all_fill, 
               "Voter ID"=voting.df$ID_fill, 
               "Voter Registration"=voting.df$reg_fill,
               "Felon Voting Rights"=voting.df$felon_fill,
               "Early Voting"=voting.df$EV_fill)
    })
    
    legend_labels <- reactive({
        switch(input$lawInput,
               "Voter ID"=c("No document required to vote",
                            "ID requested, photo not required",
                            "Photo ID requested",
                            "Strict ID"), 
               "Voter Registration"=c("Registration not needed to vote",
                                      "Same day voter registration",
                                      "Registration prior to election, online registration allowed",
                                      "Registration prior to election, online registration not allowed"),
               "Felon Voting Rights"=c("Unrestricted, may vote from prison",
                                       "Vote restored after prison",
                                       "Vote restored after prison and parole",
                                       "May lose vote permanently"),
               "Early Voting"=c("All-mail voting with early voting options",
                                "No-excuse absentee voting",
                                "In-person early voting",
                                "Early voting not allowed"))    
    })
    
    legend_title <- reactive({
        switch(input$lawInput, 
              "Voter ID"="Current Voter ID Requirements", 
              "Voter Registration"="Current Voter Registration Laws",
              "Felon Voting Rights"="Current Felon Voting Laws",
              "Early Voting"="Current Early Voting Laws")
    })
 
    
    labels <- reactive({
        sprintf(
            
            "<strong>%s</strong><br/>%s",
            voting.df$name, description()
        ) %>% lapply(htmltools::HTML)
    })
    
    
    updated_simMap <- eventReactive(input$map_click, {
        #states <- mutate(states,...)
        leaflet(states) %>%
            setView(-100, 35, 4) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = "#FF8040",
                weight = 2,
                opacity = 1,
                color = "white", #color = selected
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
        
    },
    ignoreNULL = FALSE)
        
    updated_lawMap <- eventReactive(input$update_plot, {
        leaflet(states) %>%
            setView(-100, 35, 4) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = ~pal(fill()),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels(),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
            addLegend("topright",
                      colors = c("#6A51A3","#9E9AC8","#CBC9E2","#F2F0F7"),
                      labels = legend_labels(),
                      title= legend_title(),
                      opacity = 1)
        
    },
    ignoreNULL = FALSE)

        
    output$simMap <- renderLeaflet({
        updated_simMap()
    })
    
    output$lawMap <- renderLeaflet({
        updated_lawMap()
        
    }) #
    
    
    vals <- reactiveValues(state_name = "",
                           state_vac_pop = 0,
                           state_turnout = 0,
                           state_pct_turnout = 0,
                           state_all_mail = 0,
                           state_early_voting = 0,
                           state_same_day_reg = 0,
                           state_strict_ID = 0,
                           state_photo_ID = 0,
                           state_felon_restrict = 0,
                           simulated_turnout = 0)
    
    observeEvent(input$simMap_click,{
        state_spatial <- SpatialPoints(data.frame(long=input$simMap_click$lng,
                                                  lat=input$simMap_click$lat),
                                       proj4string = states@proj4string)
        state_overlay <- over(state_spatial,states)
        vals$state_name <- toString(state_overlay$name)
        
    })
    
    
    output$state_info <- renderUI({
        # filtering for variables by state name
        state_population <- filter(voting, state==vals$state_name)$population
        vals$state_vac_pop <- as.integer(filter(voting, state==vals$state_name)$vac_pop)
        vals$state_turnout <- as.integer(filter(voting, state==vals$state_name)$turnout)
        vals$state_pct_turnout <- as.integer(filter(voting, state==vals$state_name)$pct_turnout)
        vals$state_all_mail <- filter(voting, state==vals$state_name)$all_mail_voting
        vals$state_early_voting <- filter(voting, state==vals$state_name)$early_voting
        vals$state_same_day_reg <- filter(voting, state==vals$state_name)$same_day_reg
        vals$state_strict_ID <- filter(voting, state==vals$state_name)$strict_ID
        vals$state_photo_ID <- filter(voting, state==vals$state_name)$photo_ID
        vals$state_felon_restrict <- filter(voting, state==vals$state_name)$felon_restrict
        if(vals$state_name == "" | vals$state_name == "NA") {
            select_message <- "Please select a state by clicking on it."
            population_message <- ""
            vac_message <- ""
            turnout_message <- ""
        } else {
            select_message <- paste0("You have selected ",vals$state_name,".")
            population_message <- paste0("2020 Population: ",
                                         format(state_population, big.mark=",", scientific=FALSE))
            vac_message <- paste0("2020 Voting Age Citizens: ",
                                         format(vals$state_vac_pop, big.mark=",", scientific=FALSE))
            turnout_message <- paste0("2020 Election Turnout: ",
                                      format(vals$state_turnout, big.mark=",", scientific=FALSE))
        }
        HTML(paste(select_message, population_message, vac_message, turnout_message, sep = '<br/>'))

    })
    
    output$switch.1 <- renderUI({
        if(vals$state_name == "" | vals$state_name == "NA") {
            switchButton(inputId = "Switch.1",
                         label = "All-Mail Voting", 
                         value = TRUE, col = "RG", type = "OO")
        } else {
            switchButton(inputId = "Switch.1",
                         label = "All-Mail Voting", 
                         value = ifelse(vals$state_all_mail==0, FALSE, TRUE),
                         col = "RG", type = "OO")
        }
    })
    
    output$switch.2 <- renderUI({
        if(vals$state_name == "" | vals$state_name == "NA") {
            switchButton(inputId = "Switch.2",
                         label = "Early Voting Allowed", 
                         value = TRUE, col = "RG", type = "OO")
        } else {
            switchButton(inputId = "Switch.2",
                         label = "Early Voting Allowed", 
                         value = ifelse(vals$state_early_voting==0, FALSE, TRUE),
                         col = "RG", type = "OO")
        }
    })
    
    output$switch.3 <- renderUI({
        if(vals$state_name == "" | vals$state_name == "NA") {
            switchButton(inputId = "Switch.3",
                         label = "Same Day Voter Registration", 
                         value = TRUE, col = "RG", type = "OO")
        } else {
            switchButton(inputId = "Switch.3",
                         label = "Same Day Voter Registration", 
                         value = ifelse(vals$state_same_day_reg==0, FALSE, TRUE),
                         col = "RG", type = "OO")
        }
    })
    
    output$switch.4 <- renderUI({
        if(vals$state_name == "" | vals$state_name == "NA") {
            switchButton(inputId = "Switch.4",
                         label = "Strict ID Law", 
                         value = TRUE, col = "RG", type = "OO")
        } else {
            switchButton(inputId = "Switch.4",
                         label = "Strict ID Law", 
                         value = ifelse(vals$state_strict_ID==0, FALSE, TRUE),
                         col = "RG", type = "OO")
        }
    })
    
    output$switch.5 <- renderUI({
        if(vals$state_name == "" | vals$state_name == "NA") {
            switchButton(inputId = "Switch.5",
                         label = "Photo ID Requested", 
                         value = TRUE, col = "RG", type = "OO")
        } else {
            switchButton(inputId = "Switch.5",
                         label = "Photo ID Requested", 
                         value = ifelse(vals$state_photo_ID==0, FALSE, TRUE),
                         col = "RG", type = "OO")
        }
    })
    
    output$switch.6 <- renderUI({
        if(vals$state_name == "" | vals$state_name == "NA") {
            switchButton(inputId = "Switch.6",
                         label = "Felon Voting Restrictions", 
                         value = TRUE, col = "RG", type = "OO")
        } else {
            switchButton(inputId = "Switch.6",
                         label = "Felon Voting Restrictions", 
                         value = ifelse(vals$state_felon_restrict==0, FALSE, TRUE),
                         col = "RG", type = "OO")
        }
    })
    
    
    # clear_policies <- eventReactive(input$clear_simulation, {
    #     vals$simulated_turnout <- vals$state_pct_turnout
    #     switchButton(inputId = "Switch.1",
    #                      label = "All-Mail Voting", 
    #                      value = ifelse(vals$state_all_mail==0, FALSE, TRUE),
    #                      col = "RG", type = "OO")
    #    switchButton(inputId = "Switch.2",
    #                      label = "Early Voting Allowed", 
    #                      value = ifelse(vals$state_early_voting==0, FALSE, TRUE),
    #                      col = "RG", type = "OO")
    #     switchButton(inputId = "Switch.3",
    #                      label = "Same Day Voter Registration", 
    #                      value = ifelse(vals$state_same_day_reg==0, FALSE, TRUE),
    #                      col = "RG", type = "OO")
    #     switchButton(inputId = "Switch.4",
    #                      label = "Strict ID Law", 
    #                      value = ifelse(vals$state_strict_ID==0, FALSE, TRUE),
    #                      col = "RG", type = "OO")
    #     switchButton(inputId = "Switch.5",
    #                      label = "Photo ID Requested", 
    #                      value = ifelse(vals$state_photo_ID==0, FALSE, TRUE),
    #                      col = "RG", type = "OO")
    #     switchButton(inputId = "Switch.6",
    #                      label = "Felon Voting Restrictions", 
    #                      value = ifelse(vals$state_felon_restrict==0, FALSE, TRUE),
    #                      col = "RG", type = "OO")
    # },
    # ignoreNULL = TRUE)

    
    simulate_turnout <- eventReactive(input$simulate_graph, {
        if (vals$state_name != "" & vals$state_name != "NA") {
            vals$simulated_turnout <- vals$state_pct_turnout
            if(input$Switch.1) {
                current_mail <- 1
            } else
                current_mail <- 0
            if(vals$state_all_mail - current_mail == 1) {
                vals$simulated_turnout <- vals$simulated_turnout - 7.637
            } else if(vals$state_all_mail - current_mail == -1) {
                vals$simulated_turnout <- vals$simulated_turnout + 7.637
            }
            
            if(input$Switch.2) {
                current_early_voting <- 1
            } else
                current_early_voting <- 0
            if(vals$state_early_voting - current_early_voting == 1) {
                vals$simulated_turnout <- vals$simulated_turnout + 2.089
            } else if(vals$state_early_voting - current_early_voting == -1) {
                vals$simulated_turnout <- vals$simulated_turnout - 2.089
            }
            
            if(input$Switch.3) {
                current_same_day_reg <- 1
            } else
                current_same_day_reg <- 0
            if(vals$state_same_day_reg - current_same_day_reg == 1) {
                vals$simulated_turnout <- vals$simulated_turnout - 6.07
            } else if(vals$state_same_day_reg - current_same_day_reg == -1) {
                vals$simulated_turnout <- vals$simulated_turnout + 6.07
            }
            
            if(input$Switch.4) {
                current_strict_ID <- 1
            } else
                current_strict_ID <- 0
            if(vals$state_strict_ID - current_strict_ID == 1) {
                vals$simulated_turnout <- vals$simulated_turnout - 0.002
            } else if(vals$state_strict_ID - current_strict_ID == -1) {
                vals$simulated_turnout <- vals$simulated_turnout + 0.002
            }
            
            if(input$Switch.5) {
                current_photo_ID <- 1
            } else
                current_photo_ID <- 0
            if(vals$state_photo_ID - current_photo_ID == 1) {
                vals$simulated_turnout <- vals$simulated_turnout + 1.135
            } else if(vals$state_photo_ID - current_photo_ID == -1) {
                vals$simulated_turnout <- vals$simulated_turnout - 1.135
            }
            
            if(input$Switch.6) {
                current_felon_restrict <- 1
            } else
                current_felon_restrict <- 0
            if(vals$state_felon_restrict - current_felon_restrict == 1) {
                vals$simulated_turnout <- vals$simulated_turnout + 1.403
            } else if(vals$state_felon_restrict - current_felon_restrict == -1) {
                vals$simulated_turnout <- vals$simulated_turnout - 1.403
            }
        }
    },
    ignoreNULL = TRUE)
    
    
    output$simPlot <- renderPlot({
        vals$simulated_turnout <- vals$state_pct_turnout
        simulate_turnout()
        if(vals$state_name == "" | vals$state_name == "NA") {
            bp <- plot.new()
        }
        else {
            turnouts <- c(vals$state_pct_turnout, vals$simulated_turnout)
            bp <- barplot(turnouts,
                          main = paste0("2020 Turnout Model: ", vals$state_name),
                          ylab = "Turnout (% of Voting Age Citizens)",
                          names.arg = c("Actual 2020 Turnout", "Simulated 2020 Turnout"),
                          col = "cadetblue3",
                          space = 1,
                          ylim = c(0, 100),
                          cex.axis=0.6, cex.names=0.75)
            text(bp, 0, round(turnouts, 3),cex=1,pos=3)
        }
    })
    
    output$citations <- renderUI({
        cite1 <- paste("National Conference of State Legislatures. 2021. <i>NCSL Election Resources</i>. Retrieved from https://www.ncsl.org/research/elections-and-campaigns/election-laws-and-procedures-overview.aspx.")
        cite2 <- paste("United States Census Bureau. 2021. <i>2020 Census Apportionment Results</i>. Retrieved from https://www.census.gov/data/tables/2020/dec/2020-apportionment-data.html.")
        cite3 <- paste("United States Census Bureau. 2021. <i>Voting and Registration in the Election of November 2020</i>. Retrieved from https://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-585.html. </br></br>")
        HTML(paste(cite1, cite2, cite3, sep = '<br/><br/>'))
    })
        
} # end of server

## run shinyApp ##
shiny::shinyApp(ui = ui, server = server)


# end of script #

