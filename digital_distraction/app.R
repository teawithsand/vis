library(shiny) #podstawa aplikacji
library(bslib) #layout dashboardu
library(ggplot2) #wykresy
library(dplyr) #przetwarzanie danych
library(ggbeeswarm) #wykres punktowy
library(ggridges) #warstwowy wykres rozkladu
library(DT) #interaktywna tabela danych
library(scales) #formatowanie osi procentowej

data <- read.csv( #wczytanie danych
  "digital_distraction_source_dataset.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

ui <- page_sidebar( #caly ui
  title = "Digital Distraction Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#3B82F6"
  ),
  sidebar = sidebar( #lewy panel filtrow
    width = 240,
    selectInput(
      "time_of_day",
      "Pora dnia",
      choices = c("Wszystkie", sort(unique(data$time_of_day))),
      selected = "Wszystkie"
    ),
    selectInput(
      "primary_task",
      "Główne zadanie",
      choices = c("Wszystkie", sort(unique(data$primary_task))),
      selected = "Wszystkie"
    ),
    selectInput(
      "distraction_source",
      "Źródło rozproszenia",
      choices = c("Wszystkie", sort(unique(data$distraction_source))),
      selected = "Wszystkie"
    ),
    selectInput(
      "uses_focus_tool",
      "Używa narzędzia do skupienia",
      choices = c("Wszystkie", sort(unique(data$uses_focus_tool))),
      selected = "Wszystkie"
    )
  ),
  layout_columns( #statyczne kafelki z ogolnymi danymi
    col_widths = c(4, 4, 4),
    value_box(
      title = "Śr. rozproszeń / godzinę",
      value = textOutput("avg_distraction"),
      theme_color = "success"
    ),
    value_box(
      title = "Śr. czas utraty koncentracji",
      value = textOutput("avg_focus_loss"),
      theme_color = "warning"
    ),
    value_box(
      title = "Odsetek używających narzędzi do skupienia",
      value = textOutput("focus_tool_rate"),
      theme_color = "danger"
    )
  ),
  navset_card_tab( #zakladki dashboardu
    nav_panel(
      "Źródła rozproszeń",
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Liczba wystąpień źródeł rozproszeń"),
          plotOutput("plot_source_counts", height = "460px")
        ),
        card(
          full_screen = TRUE,
          card_header("Średnia liczba rozproszeń na godzinę"),
          plotOutput("plot_avg_distraction", height = "460px")
        )
      )
    ),
    nav_panel(
      "Zadania i źródła",
      card(
        full_screen = TRUE,
        card_header("Średnia liczba rozproszeń wg zadania i źródła"),
        plotOutput("plot_heatmap", height = "580px")
      )
    ),
    nav_panel(
      "Pora dnia",
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Średnia częstotliwość rozproszeń wg pory dnia"),
          plotOutput("plot_time_source", height = "500px")
        ),
        card(
          full_screen = TRUE,
          card_header("Rozkład czasu utraty koncentracji wg pory dnia"),
          plotOutput("plot_ridges", height = "500px")
        )
      )
    ),
    nav_panel(
      "Miejsce",
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Średnia liczba rozproszeń wg miejsca"),
          plotOutput("plot_location_distraction", height = "500px")
        ),
        card(
          full_screen = TRUE,
          card_header("Średni czas utraty koncentracji wg miejsca"),
          plotOutput("plot_location_focus_loss", height = "500px")
        )
      )
    ),
    nav_panel(
      "Koncentracja",
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Czas utraty koncentracji wg głównego zadania"),
          plotOutput("plot_focus_loss", height = "500px")
        ),
        card(
          full_screen = TRUE,
          card_header("Liczba rozproszeń a deklarowany poziom skupienia"),
          plotOutput("plot_focus_vs_distraction", height = "500px")
        )
      )
    ),
    nav_panel(
      "Narzędzia do skupienia",
      card(
        full_screen = TRUE,
        card_header("Używanie narzędzi do skupienia wg zadania"),
        plotOutput("plot_focus_tool", height = "580px")
      )
    ),
    nav_panel(
      "Dane",
      card(
        full_screen = TRUE,
        card_header("Przefiltrowane dane"),
        DTOutput("data_table")
      )
    )
  )
)

server <- function(input, output, session) { #server czyli logika w shiny

  #data->filtry usera->filtr!="Wszystkie"->ogranicz data->przefiltrowana tabela
  filtered_data <- reactive({
    df <- data

    if (input$time_of_day != "Wszystkie") {
      df <- df %>% filter(time_of_day == input$time_of_day)
    }

    if (input$primary_task != "Wszystkie") {
      df <- df %>% filter(primary_task == input$primary_task)
    }

    if (input$distraction_source != "Wszystkie") {
      df <- df %>% filter(distraction_source == input$distraction_source)
    }

    if (input$uses_focus_tool != "Wszystkie") {
      df <- df %>% filter(uses_focus_tool == input$uses_focus_tool)
    }

    df
  })

  #srednia liczba rozproszen
  output$avg_distraction <- renderText({
    round(mean(filtered_data()$distraction_frequency_per_hour, na.rm = TRUE), 2)
  })

  #sredni czas utraty koncentracji
  output$avg_focus_loss <- renderText({
    paste0(round(mean(filtered_data()$focus_loss_duration_minutes, na.rm = TRUE), 2), " min")
  })

  #odsetek uzywajacych narzedzia do skupienia
  output$focus_tool_rate <- renderText({
    df <- filtered_data()
    paste0(
      round(mean(df$uses_focus_tool %in% c("Yes", "yes", "TRUE", "True", "Tak"), na.rm = TRUE) * 100, 1),
      "%"
    )
  })

  #dla kazdego wykresu bierzemy filtered_data()
  #->czasem agregacja group_by() i summarise()-> wykres ggplot2
  output$plot_source_counts <- renderPlot({
    df_sum <- filtered_data() %>%
      count(distraction_source, name = "n")

    ggplot(df_sum, aes(
      x = n,
      y = reorder(distraction_source, n),
      fill = distraction_source
    )) +
      geom_col(width = 0.75, show.legend = FALSE) +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = NULL,
        x = "Liczba obserwacji",
        y = "Źródło rozproszenia"
      ) +
      theme_minimal(base_size = 13)
  })

  output$plot_avg_distraction <- renderPlot({
    df <- filtered_data()

    avg_df <- df %>%
      group_by(distraction_source) %>%
      summarise(
        mean_freq = mean(distraction_frequency_per_hour, na.rm = TRUE),
        .groups = "drop"
      )

    ggplot(avg_df, aes(
      x = mean_freq,
      y = reorder(distraction_source, mean_freq),
      fill = distraction_source
    )) +
      geom_col(show.legend = FALSE, width = 0.7) +
      scale_fill_brewer(palette = "Pastel1") +
      labs(
        title = NULL,
        x = "Średnia liczba rozproszeń / godzinę",
        y = "Źródło rozproszenia"
      ) +
      theme_minimal(base_size = 13)
  })

  output$plot_heatmap <- renderPlot({
    df <- filtered_data()

    avg_freq <- df %>%
      group_by(primary_task, distraction_source) %>%
      summarise(
        mean_freq = mean(distraction_frequency_per_hour, na.rm = TRUE),
        .groups = "drop"
      )

    ggplot(avg_freq, aes(
      x = distraction_source,
      y = primary_task,
      fill = mean_freq
    )) +
      geom_tile(color = "white", linewidth = 0.6) +
      scale_fill_viridis_c(option = "C") +
      labs(
        title = NULL,
        x = "Źródło rozproszenia",
        y = "Główne zadanie",
        fill = "Śr. / h"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })

  output$plot_time_source <- renderPlot({
    df <- filtered_data()

    freq_time_source <- df %>%
      group_by(time_of_day, distraction_source) %>%
      summarise(
        mean_freq = mean(distraction_frequency_per_hour, na.rm = TRUE),
        .groups = "drop"
      )

    ggplot(freq_time_source, aes(
      x = mean_freq,
      y = reorder(distraction_source, mean_freq),
      color = time_of_day
    )) +
      geom_segment(aes(
        x = 0,
        xend = mean_freq,
        yend = reorder(distraction_source, mean_freq)
      ), linewidth = 0.8) +
      geom_point(size = 3) +
      facet_wrap(~time_of_day, scales = "free_y") +
      scale_color_brewer(palette = "Dark2") +
      labs(
        title = NULL,
        x = "Średnia liczba rozproszeń / godzinę",
        y = "Źródło rozproszenia",
        color = "Pora dnia"
      ) +
      theme_minimal(base_size = 13)
  })

  output$plot_focus_loss <- renderPlot({
    df <- filtered_data()

    ggplot(df, aes(
      x = primary_task,
      y = focus_loss_duration_minutes,
      color = primary_task
    )) +
      geom_quasirandom(alpha = 0.7, width = 0.25, show.legend = FALSE) +
      coord_flip() +
      scale_color_brewer(palette = "Set1") +
      labs(
        title = NULL,
        x = "Główne zadanie",
        y = "Czas utraty koncentracji [min]"
      ) +
      theme_minimal(base_size = 13)
  })

  #wykresy w zakladce miejsce
  output$plot_location_distraction <- renderPlot({
    df <- filtered_data()

    avg_by_location <- df %>%
      group_by(location) %>%
      summarise(
        mean_freq = mean(distraction_frequency_per_hour, na.rm = TRUE),
        .groups = "drop"
      )

    ggplot(avg_by_location, aes(
      x = mean_freq,
      y = reorder(location, mean_freq),
      fill = location
    )) +
      geom_col(show.legend = FALSE, width = 0.7) +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = NULL,
        x = "Średnia liczba rozproszeń / godzinę",
        y = "Miejsce"
      ) +
      theme_minimal(base_size = 13)
  })

  output$plot_location_focus_loss <- renderPlot({
    df <- filtered_data()

    avg_focus_location <- df %>%
      group_by(location) %>%
      summarise(
        mean_focus = mean(focus_loss_duration_minutes, na.rm = TRUE),
        .groups = "drop"
      )

    ggplot(avg_focus_location, aes(
      x = mean_focus,
      y = reorder(location, mean_focus),
      fill = location
    )) +
      geom_col(show.legend = FALSE, width = 0.7) +
      scale_fill_brewer(palette = "Pastel2") +
      labs(
        title = NULL,
        x = "Średni czas utraty koncentracji [min]",
        y = "Miejsce"
      ) +
      theme_minimal(base_size = 13)
  })

  output$plot_focus_tool <- renderPlot({
    df <- filtered_data()

    focus_tool_by_task <- df %>%
      group_by(primary_task, uses_focus_tool) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(primary_task) %>%
      mutate(prop = n / sum(n))

    ggplot(focus_tool_by_task, aes(
      x = primary_task,
      y = prop,
      fill = uses_focus_tool
    )) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = NULL,
        x = "Główne zadanie",
        y = "Udział odpowiedzi",
        fill = "Używa narzędzia do skupienia"
      ) +
      theme_minimal(base_size = 13)
  })

  output$plot_ridges <- renderPlot({
    df <- filtered_data()

    ggplot(df, aes(
      x = focus_loss_duration_minutes,
      y = time_of_day,
      fill = time_of_day
    )) +
      geom_density_ridges(alpha = 0.7, color = "white") +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = NULL,
        x = "Czas utraty koncentracji [min]",
        y = "Pora dnia"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  #boxplot zamiast wygladzonej linii jest czytelniejszy dla skali 1-10
  output$plot_focus_vs_distraction <- renderPlot({
    df <- filtered_data()

    ggplot(df, aes(
      x = factor(self_reported_focus_level),
      y = distraction_frequency_per_hour
    )) +
      geom_boxplot(
        fill = "lightblue",
        alpha = 0.7,
        outlier.shape = NA
      ) +
      geom_jitter(
        width = 0.15,
        alpha = 0.35,
        color = "steelblue"
      ) +
      labs(
        title = NULL,
        x = "Deklarowany poziom skupienia",
        y = "Liczba rozproszeń na godzinę"
      ) +
      theme_minimal(base_size = 13)
  })

  #interaktywna tabela danych
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 8, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

#polaczenie ui, logiki(serwera) i renderowanie aplikacji
shinyApp(ui = ui, server = server)