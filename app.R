library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hoopR)
library(MASS)

# Load and preprocess data
df <- load_mbb_player_box(seasons = 2025)
clean <- df %>%
  reframe(
    game_id, game_date, season, athlete_id, 
    name = athlete_display_name, team = team_location, 
    opp = opponent_team_location, minutes, fgm = field_goals_made, 
    fga = field_goals_attempted, rebs = rebounds, assists, points, 
    fouls, steals, blocks, to = turnovers, or = offensive_rebounds, 
    dr = defensive_rebounds, ftm = free_throws_made, fta = free_throws_attempted, 
    position = athlete_position_abbreviation, team_score, opponent_team_score, 
    starter, ejected
  )

playerdb <- split(clean, clean$name)

# UI
ui <- fluidPage(
  tags$style(
    HTML("
      html, body {
        background-color: black;
        color: white;
        height: 100%;
        margin: 0;
      }
      .well {
        background-color: #333333;
        color: white;
        border: none;
      }
      .form-control {
        background-color: #333333;
        color: white;
        border: 1px solid #555555;
      }
      .btn {
        background-color: #555555;
        color: white;
        border: none;
      }
      .btn:hover {
        background-color: #777777;
      }
      #reboundPlot {
        background-color: black;
        border: 2px solid #555555;
      }
      .container-fluid {
        background-color: black;
      }
      .mainPanel {
        background-color: black;
      }
    ")
  ),
  
  titlePanel("Player Rebound Probability Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("player", "Enter Player Name:", value = ""),
      numericInput("threshold", "Rebound Threshold:", value = "", min = 1),
      numericInput("sb_odds", "Enter Sports Book Odds:", value = ""),
      actionButton("calculate", "Calculate")
    ),
    
    mainPanel(
      textOutput("result"),
      textOutput("americanOdds"),
      textOutput("qkResult"),
      plotOutput("reboundPlot", height = "400px")  # Ensure the plot's height is set
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive values to store results
  result <- reactiveVal(NULL)
  odds <- reactiveVal(NULL)
  reboundPlotData <- reactiveVal(NULL)
  qk_result <- reactiveVal(NULL)
  
  observeEvent(input$calculate, {
    pname <- input$player
    threshold <- input$threshold
    
    # Check if the player exists in the database
    if (!pname %in% names(playerdb)) {
      result("Player not found in database.")
      odds(NULL)
      reboundPlotData(NULL)
      ev_result(NULL)
      return()
    }
    
    # Extract player's data
    player_data <- playerdb[[pname]]
    player_data <- player_data %>% arrange(as.Date(game_date))
    
    # Calculate lambdas
    lambda_orb <- mean(player_data$or, na.rm = TRUE)
    lambda_drb <- mean(player_data$dr, na.rm = TRUE)
    
    # Calculate max_trb
    max_trb <- round(max(player_data$rebs, na.rm = TRUE) * 1.75)
    
    # Probability calculation function
    calc_probs_for_threshold <- function(threshold, max_trb, lambda_orb, lambda_drb) {
      total_prob <- 0
      
      for (trb in threshold:max_trb) {
        trb_prob <- 0
        for (orb in 0:trb) {
          drb <- trb - orb
          p_orb <- dpois(orb, lambda = lambda_orb)
          p_drb <- dpois(drb, lambda = lambda_drb)
          trb_prob <- trb_prob + (p_orb * p_drb)
        }
        total_prob <- total_prob + trb_prob
      }
      
      return(total_prob)
    }
    
    # Calculate probability
    probability <- calc_probs_for_threshold(threshold, max_trb, lambda_orb, lambda_drb)
    
    # Calculate American odds from the calculated probability
    decimal <- 1 / probability
    american_odds <- ifelse(decimal > 2, (decimal - 1) * 100, -100 / (decimal - 1))
    
    # Update results
    result(sprintf("The probability for TRB >= %d is %.4f", threshold, probability))
    odds(sprintf("American Odds: %.2f", american_odds))
    
    # Prepare data for plotting
    plot_data <- player_data %>%
      mutate(
        over_threshold = rebs >= threshold,
        bar_color = ifelse(over_threshold, "green", "red")
      )
    reboundPlotData(plot_data)
    
    # EV calculation
    req(input$sb_odds)  # Ensure EV odds are entered before calculating
    sb_odds <- as.numeric(input$sb_odds)
    B <- ifelse(sb_odds > 0, (sb_odds+100)/100, (abs(sb_odds)+100)/abs(sb_odds)) - 1
    P <- probability
    Q <- 1 - P
    kelly <- (B * P - Q) / B
    quarter_kelly <- kelly * 100 / 4
    qk_result(sprintf("Quarter Kelly Criterion: %.2f", quarter_kelly))
  })
  
  # Render outputs only after button press
  output$result <- renderText({
    req(result())  # Ensure that this is only displayed after calculation
    result()
  })
  
  output$americanOdds <- renderText({
    req(odds())  # Ensure that this is only displayed after calculation
    odds()
  })
  
  output$reboundPlot <- renderPlot({
    req(reboundPlotData())  # Ensure that this is only displayed after calculation
    plot_data <- reboundPlotData()
    ggplot(plot_data, aes(x = as.Date(game_date), y = rebs, fill = bar_color)) +
      geom_bar(stat = "identity", width = 1) +  # Set width to 1 for thick, touching bars
      geom_hline(yintercept = input$threshold, linetype = "dashed", color = "blue") +
      scale_fill_identity() +
      labs(
        title = "Player Rebounds Per Game",
        x = "Game Date",
        y = "Total Rebounds"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10)  # Add margin for better spacing
      )
  })
  
  # Render EV result
  output$qkResult <- renderText({
    req(qk_result())  # Ensure that this is only displayed after calculation
    qk_result()
  })
}

# Run the app
shinyApp(ui = ui, server = server)

