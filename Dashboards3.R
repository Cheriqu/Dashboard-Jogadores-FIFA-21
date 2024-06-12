required_packages <- c("tidyverse", "ggplot2", "gridExtra", "forcats", "maps", "lubridate",
                       "corrplot", "shiny", "shinipsum", "tidyr", "purrr", "fmsb")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE) 
  }
}

df <- read_csv("players.csv")
df_clean <- df %>% janitor::clean_names()
df_clean[df_clean == ""] <- NA
df_clean$int_value <- as.integer(df_clean$int_value)
df_clean <- df_clean[, -c(ncol(df_clean)-1, ncol(df_clean))]
df_clean <- df_clean %>%
  mutate(dt_date_of_birth = as.Date(dt_date_of_birth, format = "%d/%m/%Y"))
df <- df %>%
  mutate(Age = as.integer(interval(start = dt_date_of_birth, end = today()) / years(1)))
df_clean <- df_clean[, -c(which(names(df_clean) %in% c("str_work_rate", "str_body_type")))]
formatar_colunas <- function(names) {
  names <- str_replace_all(names, "^int_", "")
  names <- str_replace_all(names, "^str_", "")
  names <- str_replace_all(names, "^dt_", "")
  names <- str_to_title(names)
  return(names)
}
names(df_clean) <- formatar_colunas(names(df_clean))
write_csv(df_clean, 'players_dataset.csv', col_names = TRUE, na = "NA")
df <- subset(df, select = -dt_date_of_birth)
df <- subset(df, select = -str_trait)
df <- subset(df, select = -int_player_id)



# UI do Shiny com uma nova aba
ui <- fluidPage(
  titlePanel("Dashboard Jogadores FIFA 21"),
  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Posição:", choices = sort(unique(df$str_best_position)), selected = NULL),
      sliderInput("overall_rating", "Avaliação Geral:", min = min(df$int_overall_rating, na.rm = TRUE), max = max(df$int_overall_rating, na.rm = TRUE), value = c(min(df$int_overall_rating, na.rm = TRUE), max(df$int_overall_rating, na.rm = TRUE))),
      sliderInput("potential_rating", "Avaliação Potencial:", min = min(df$int_potential_rating, na.rm = TRUE), max = max(df$int_potential_rating, na.rm = TRUE), value = c(min(df$int_potential_rating, na.rm = TRUE), max(df$int_potential_rating, na.rm = TRUE))),
      sliderInput("best_overall_rating", "Melhor Avaliação Geral:", min = min(df$int_best_overall_rating, na.rm = TRUE), max = max(df$int_best_overall_rating, na.rm = TRUE), value = c(min(df$int_best_overall_rating, na.rm = TRUE), max(df$int_best_overall_rating, na.rm = TRUE))),
      sliderInput("value", "Valor de Mercado (em milhões):", min = min(df$int_value, na.rm = TRUE), max = max(df$int_value, na.rm = TRUE), value = c(min(df$int_value, na.rm = TRUE), max(df$int_value, na.rm = TRUE))),
      sliderInput("wage", "Salário (em milhares):", min = min(df$int_wage, na.rm = TRUE), max = max(df$int_wage, na.rm = TRUE), value = c(min(df$int_wage, na.rm = TRUE), max(df$int_wage, na.rm = TRUE))),
      sliderInput("age", "Idade:", min = min(df$Age, na.rm = TRUE), max = max(df$Age, na.rm = TRUE), value = c(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE))),
      selectInput("preferred_foot", "Pé Preferido:", choices = c("Direito", "Esquerdo"), selected = NULL, multiple = FALSE),
      sliderInput("weak_foot", "Habilidade com Pé Fraco:", min = 1, max = 5, value = c(1, 5), step = 1),
      sliderInput("skill_moves", "Habilidades:", min = 1, max = 5, value = c(1, 5), step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela", tableOutput("filtered_table")),
        tabPanel("Gráficos", 
                 fluidRow(
                   column(6, plotOutput("age_distribution")),
                   column(6, plotOutput("rating_distribution"))
                 ),
                 fluidRow(
                   column(6, plotOutput("nation_distribution")),
                   column(6, plotOutput("rating_by_position"))
                 )
        ),
        tabPanel("Radar", 
                 selectInput("players", "Selecione até 3 jogadores:", choices = unique(df$str_player_name), selected = NULL, multiple = TRUE, selectize = TRUE),
                 plotOutput("radar_plot")
        )
      )
    )
  )
)

# Server do Shiny com renderizações de gráficos adicionais
server <- function(input, output) {
  output$filtered_table <- renderTable({
    filtered_df <- df %>%
      filter(
        str_best_position == input$position,
        int_overall_rating >= input$overall_rating[1] & int_overall_rating <= input$overall_rating[2],
        int_potential_rating >= input$potential_rating[1] & int_potential_rating <= input$potential_rating[2],
        int_best_overall_rating >= input$best_overall_rating[1] & int_best_overall_rating <= input$best_overall_rating[2],
        int_value >= input$value[1] & int_value <= input$value[2],
        int_wage >= input$wage[1] & int_wage <= input$wage[2],
        Age >= input$age[1] & Age <= input$age[2],
        if(is.null(input$preferred_foot)) TRUE else str_preferred_foot == ifelse(input$preferred_foot == "Direito", "Right", "Left"),
        int_weak_foot >= input$weak_foot[1] & int_weak_foot <= input$weak_foot[2],
        int_skill_moves >= input$skill_moves[1] & int_skill_moves <= input$skill_moves[2]
      )
    filtered_df
  })
  
  output$age_distribution <- renderPlot({
    ggplot(df, aes(x = Age)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      labs(title = "Distribuição de Idade dos Jogadores", x = "Idade", y = "Frequência")
  })
  
  output$rating_distribution <- renderPlot({
    ggplot(df, aes(x = int_overall_rating)) +
      geom_histogram(binwidth = 1, fill = "green", color = "black") +
      labs(title = "Distribuição de Avaliação Geral dos Jogadores", x = "Avaliação Geral", y = "Frequência")
  })
  
  output$nation_distribution <- renderPlot({
    top_nations <- df %>%
      count(str_nationality, sort = TRUE) %>%
      top_n(15, n) %>% arrange(desc(n))
    ggplot(top_nations, aes(x = reorder(str_nationality, n, decreasing = TRUE), y = n)) +
      geom_bar(stat = "identity", fill = "purple", color = "black") +
      labs(title = "Distribuição de Jogadores por Nacionalidade (Top 15)", x = "Nacionalidade", y = "Frequência") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$rating_by_position <- renderPlot({
    ggplot(df, aes(x = str_best_position, y = int_overall_rating)) +
      geom_boxplot(fill = "orange", color = "black") +
      labs(title = "Avaliação Geral por Posição", x = "Posição", y = "Avaliação Geral") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$radar_plot <- renderPlot({
    selected_players <- input$players
    if(length(selected_players) > 0 & length(selected_players) <= 3){
      radar_data <- df %>%
        filter(str_player_name %in% selected_players) %>%
        select(int_vision, int_acceleration, int_strength, int_crossing, int_finishing, int_stamina)
      radar_data <- as.data.frame(radar_data)
      colnames(radar_data) <- c("Vision", "Acceleration", "Strength", "Crossing", "Finishing", "Stamina")
      
      max_min <- data.frame(
        #Player = c("Max", "Min"),
        Vision = c(100, 50),
        Acceleration = c(100, 50),
        Strength = c(100, 50),
        Crossing = c(100, 50),
        Finishing = c(100, 50),
        Stamina = c(100, 50)
      )
      
      radar_data <- rbind(max_min, radar_data)
      radarchart(radar_data, axistype = 1,
                 pcol = c("red", "blue", "green"),
                 pfcol = c(rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2), rgb(0.7,0.5,0.1,0.2)),
                 plwd = 2, cglcol = "grey", cglty = 1, axislabcol = "grey", 
                 caxislabels = seq(0,100,20), cglwd = 0.8,
                 vlcex = 0.8)
      legend("topright", legend = selected_players, col = c("red", "blue", "green"), 
             lty = 1, lwd = 2, bty = "n")
    }
  })
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)
