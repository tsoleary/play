# ------------------------------------------------------------------------------
# Baseball stats shiny app
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries
library(shiny)
library(tidyverse)
library(Lahman)
library(plotly)

# Load data, filter to modern era AL & NL teams, & calc other stats ------------
teams <- Teams %>% 
  filter(yearID >= 1901 & 
           lgID %in% c("AL", "NL")) %>%
  group_by(yearID, teamID) %>%
  mutate(TB = H + X2B + 2 * X3B + 3 * HR,
         BA = H / AB,
         WinPct = W/G,
         RpG = R/G,
         RunDiff = R - RA,
         HRpG = HR/G,
         TBpG = TB/G,
         SOpG = SO/G,
         SFpG = SF/G,
         SOpBB = SO/BB,
         KpIP = SOA/(IPouts/3),
         HRpSO = HR/SO,
         BABIP = (H - HR) / (AB - HR - SO + SF),
         WHIP = 3 * (H + BB)/IPouts)

# Stats that can be plotted with names
stat_names <- c("At Bats" = "AB",
                "Batting average" = "BA",
                "Runs scored" = "R",
                "Hits" = "H",
                "Doubles" = "X2B",
                "Triples" = "X3B",
                "Home runs" = "HR",
                "Walks" = "BB",
                "Strike outs" = "SO",
                "Stolen bases" = "SB",
                "Caught stealing" = "CS",
                "Batters hit by pitch" = "HBP",
                "Sacrifice flies" = "SF",
                "Runs allows" = "RA",
                "Earned runs allowed" = "ER",
                "Earned run average" = "ERA",
                "Complete games" = "CG",
                "Shutouts" = "SHO",
                "Saves" = "SV",
                "Total outs pitched" = "IPouts",
                "Hits allowed" = "HA",
                "Home runs allowed" = "HRA",
                "Batting average against" = "BBA",
                "Strikeouts by pitchers" = "SOA",
                "Errors" = "E",
                "Double plays" = "DP",
                "Fielding percentage" = "FP", 
                "Total home attendance" = "attendance",
                "Total bases" = "TB",
                "Win percentage" = "WinPct",
                "Runs scored per game" = "RpG",
                "Total run differential" = "RunDiff",
                "Home runs per game" = "HRpG",
                "Total bases per game" = "TBpG",
                "Strikeouts per game" = "SOpG",
                "Sacrifice flies per game" = "SFpG",
                "Strikout to walk ratio" = "SOpBB",
                "Ks per inning pitched" = "KpIP",
                "Home runs to strikeout ratio" = "HRpSO",
                "Batting average on balls in play" = "BABIP",
                "Walks and hits per inning pitched" = "WHIP")

# Calculate league average
mlb_avg <- teams %>%
  ungroup(teamID) %>%
  summarise(across(where(is.numeric), ~ mean(.))) %>%
  mutate(name = "MLB Average")

# Calculate playoff team average
playoff_avg <- teams %>%
  filter(DivWin == "Y" |
           LgWin == "Y" |
           WCWin == "Y" |
           WSWin == "Y") %>%
  ungroup(teamID) %>%
  summarise(across(where(is.numeric), ~ mean(.))) %>%
  mutate(name = "Playoff Team Average")

teams <- bind_rows(teams, mlb_avg, playoff_avg)

# Define UI for app that draws boxplots ----------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("Historic baseball data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,
                 
                 # Input: Select teams ----
                 selectInput("team", 
                             "Pick a team",
                             c("MLB Average",
                               "Playoff Team Average",
                               "World Series Champions",
                               teams$name), 
                             multiple = TRUE,
                             selected = c("MLB Average",
                                          "Boston Red Sox", 
                                          "New York Yankees"),
                             selectize = TRUE),
                 
                 # Input: Select variable to plot ----
                 selectInput("vars", 
                             "Pick a variable to plot",
                             sort(stat_names),
                             selected = "HRpG",
                             selectize = TRUE),
                 
                 # Input: Slider for the year range ----
                 sliderInput(inputId = "years",
                             label = "Year range",
                             min = 1901,
                             max = 2020,
                             sep = "",
                             value = c(1901, 2020)),
                 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width = 9,
              
              # Output: Tabset w/ plot and table ----
              tabsetPanel(type = "tabs",
                          tabPanel("Plot", 
                                   h2(htmlOutput(outputId = "plot_title")), 
                                   plotlyOutput(outputId = "histPlot")),
                          tabPanel("Info",
                                   h2(HTML("All data from <a href='http://www.seanlahman.com/baseball-archive/statistics/' target='_blank'> Sean Lahaman </a>"))))
              
    )
  )
)

# Define server logic required to draw plot ------------------------------------
server <- function(input, output) {
  
  # Filter data ----
  data <- reactive({
    
    # Filter years and teams
    y <- teams %>% 
      filter(yearID >= input$years[1] &
               yearID <= input$years[2]) %>%
      filter(name %in% input$team)
    
    validate(
      need(nrow(y) > 0,
           paste0("Pick at least one team."))
    )
    
    y
    
  })

  
  # Create the box plots based on selected inputs ----
  output$histPlot <- renderPlotly({
    
    # Filtered data based on inputs
    teams_filt <- data()
      
      p <- ggplot() +
        geom_line(data = teams_filt,
                  aes_string(x = "yearID", 
                     y = input$vars,
                     color = "name")) +
        scale_color_discrete(name = "Team") + 
        labs(x = "Year",
             y = names(stat_names)[stat_names == input$vars]) +
        theme_classic()
      
      if ("World Series Champions" %in% input$team) {
        p <- p +
          geom_point(data = teams %>%
                       filter(WSWin == "Y"),
                     aes_string(x = "yearID",
                                y = input$vars, 
                                color = "name"))
      }

    # Return as plotly object
    ggplotly(p)
    
  })
  
}

# Create Shiny app -------------------------------------------------------------
shinyApp(ui = ui, server = server)