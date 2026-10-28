library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

data <- read.csv("ai_job_market_insights.csv", stringsAsFactors = FALSE)

data <- data %>%
    mutate(
        Salary_USD = as.numeric(Salary_USD),
        AI_Adoption_Level = factor(AI_Adoption_Level, levels = c("Low", "Medium", "High")),
        Automation_Risk = factor(Automation_Risk, levels = c("Low", "Medium", "High")),
        Job_Growth_Projection = factor(Job_Growth_Projection, levels = c("Decline", "Stable", "Growth"))
    )

ui <- page_sidebar(
    title = "Analiza Rynku Pracy AI",
    theme = bs_theme(
        version = 5,
        bootswatch = "cosmo",
        primary = "#2C3E50",
        success = "#18BC9C"
    ),

    # Panel boczny z filtrami
    sidebar = sidebar(
        width = 300,
        title = htmltools::tags$h4("Filtry Danych", class = "text-primary"),
        selectInput("industry_filter", "Branża (Industry):",
            choices = c("Wszystkie" = "All", sort(unique(data$Industry)))
        ),
        selectInput("location_filter", "Lokalizacja (Location):",
            choices = c("Wszystkie" = "All", sort(unique(data$Location)))
        ),
        selectInput("remote_filter", "Praca zdalna (Remote Friendly):",
            choices = c("Wszystkie" = "All", sort(unique(data$Remote_Friendly)))
        ),
        htmltools::tags$hr(),
        htmltools::tags$p(class = "text-muted small", "System Analizy Rynku Pracy • 2026")
    ),

    # Zakładki z wykresami i danymi
    navset_card_pill(

        # ZAKŁADKA 1: Wynagrodzenia i Zatrudnienie
        nav_panel(
            title = "1. Wynagrodzenia i Zatrudnienie",

            # Ogólne informacje na samej górze (Value Boxes)
            layout_columns(
                fill = FALSE,
                value_box(
                    title = "Liczba stanowisk (Filtr)",
                    value = textOutput("kpi_count"),
                    showcase = icon("users"),
                    theme = "primary"
                ),
                value_box(
                    title = "Średnie wynagrodzenie",
                    value = textOutput("kpi_avg_salary"),
                    showcase = icon("dollar-sign"),
                    theme = "success"
                ),
                value_box(
                    title = "Dominujący trend wzrostu",
                    value = textOutput("kpi_top_growth"),
                    showcase = icon("chart-line"),
                    theme = "info"
                )
            ),

            # Wykresy w zakładce 1
            layout_columns(
                card(
                    card_header("Wykres 1: Rozkład Wynagrodzeń w zależności od Poziomu Adopcji AI"),
                    card_body(
                        p(class = "text-muted small", "Wykres pudełkowy (Boxplot) pokazujący kwartyle, medianę oraz rozrzut zarobków (USD) dla każdego poziomu wdrożenia technologii AI."),
                        plotlyOutput("plot_salary_box")
                    )
                ),
                card(
                    card_header("Wykres 2: Porównanie Prognozy Wzrostu Stanowisk w Branżach"),
                    card_body(
                        p(class = "text-muted small", "Pogrupowany wykres kolumnowy ułatwiający bezpośrednie porównanie wolumenu prognoz wewnątrz sektorów."),
                        plotlyOutput("plot_growth_industry")
                    )
                )
            )
        ),

        # ZAKŁADKA 2: Ryzyko Automatyzacji i Kompetencje
        nav_panel(
            title = "2. Ryzyko Automatyzacji & Umiejętności",
            layout_columns(
                card(
                    card_header("Wykres 3: Ryzyko Automatyzacji a Wymagane Umiejętności"),
                    card_body(
                        p(class = "text-muted small", "Wykres konturowy / ciepła (Heatmap) pokazujący liczbę stanowisk łączących konkretne kluczowe umiejętności z poziomem ryzyka automatyzacji."),
                        plotlyOutput("plot_skills_risk")
                    )
                ),
                card(
                    card_header("Wykres 4: Średnie Wynagrodzenie dla Top Stanowisk"),
                    card_body(
                        p(class = "text-muted small", "Interaktywny wykres słupkowy przedstawiający ranking przeciętnego wynagrodzenia na najpopularniejszych stanowiskach."),
                        plotlyOutput("plot_job_salary")
                    )
                )
            )
        ),

        # ZAKŁADKA 3: Charakterystyka i Wielkość Firm
        nav_panel(
            title = "3. Wielkość Firm & Praca Zdalna",
            layout_columns(
                card(
                    card_header("Wykres 5: Struktura Wielkości Firm w relacji do Adopcji AI"),
                    card_body(
                        p(class = "text-muted small", "Wykres grupowany pokazujący, czy wielkość przedsiębiorstwa (Małe, Średnie, Duże) determinuje stopień zaawansowania wdrożenia AI."),
                        plotlyOutput("plot_company_adoption")
                    )
                ),
                card(
                    card_header("Wykres 6: Praca Zdalna vs Ryzyko Automatyzacji"),
                    card_body(
                        p(class = "text-muted small", "Wykres kołowy (Donut chart) obrazujący procentowy podział stanowisk przyjaznych pracy zdalnej w zestawieniu z poziomami ryzyka."),
                        plotlyOutput("plot_remote_pie")
                    )
                )
            )
        ),

        # NOWA ZAKŁADKA 4: Surowe dane
        nav_panel(
            title = "4. Surowe dane",
            card(
                card_header("Przeglądarka zbioru danych"),
                card_body(
                    p(class = "text-muted small", "Poniższa tabela zawiera pełne dane odpowiadające wybranym kryteriom filtrowania na panelu bocznym."),
                    DTOutput("table_raw_data")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    # Reaktywne filtrowanie zbioru danych na podstawie inputów użytkownika
    filtered_data <- reactive({
        df <- data

        if (input$industry_filter != "All") {
            df <- df %>% filter(Industry == input$industry_filter)
        }
        if (input$location_filter != "All") {
            df <- df %>% filter(Location == input$location_filter)
        }
        if (input$remote_filter != "All") {
            df <- df %>% filter(Remote_Friendly == input$remote_filter)
        }

        return(df)
    })

    # --- LOGIKA WSKAŹNIKÓW (KPI GÓRA) ---
    output$kpi_count <- renderText({
        nrow(filtered_data())
    })

    output$kpi_avg_salary <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("$0")
        }
        paste0("$", format(round(mean(df$Salary_USD, na.rm = TRUE)), big.mark = ","))
    })

    output$kpi_top_growth <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("Brak danych")
        }
        counts <- table(df$Job_Growth_Projection)
        if (length(counts) == 0) {
            return("Brak danych")
        }
        names(counts)[which.max(counts)]
    })

    # --- WYKRES 1: Boxplot Wynagrodzeń vs Adopcja AI ---
    output$plot_salary_box <- renderPlotly({
        df <- filtered_data() %>% filter(!is.na(AI_Adoption_Level))
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        plot_ly(df,
            x = ~AI_Adoption_Level, y = ~Salary_USD, type = "box",
            color = ~AI_Adoption_Level, colors = "Set2",
            boxpoints = "outliers"
        ) %>%
            layout(
                xaxis = list(title = "Poziom Adopcji AI"),
                yaxis = list(title = "Wynagrodzenie (USD)"),
                showlegend = FALSE,
                plot_bgcolor = "#F8FAFC"
            )
    })

    # --- WYKRES 2: ZMIENIONY NA GRUPOWANY (Grouped Bar Chart) ---
    output$plot_growth_industry <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Industry, Job_Growth_Projection) %>%
            summarise(Count = n(), .groups = "drop")
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        plot_ly(df,
            x = ~Industry, y = ~Count, type = "bar",
            color = ~Job_Growth_Projection, colors = c("#E74C3C", "#F39C12", "#2ECC71")
        ) %>%
            layout(
                barmode = "group", # Zmiana ze 'stack' na 'group' dla czystszego odbioru
                xaxis = list(title = "Branża", tickangle = -45),
                yaxis = list(title = "Liczba Stanowisk"),
                legend = list(title = list(text = "Prognoza wzrostu")),
                plot_bgcolor = "#F8FAFC"
            )
    })

    # --- WYKRES 3: Heatmap Kompetencje vs Ryzyko ---
    output$plot_skills_risk <- renderPlotly({
        df_matrix <- filtered_data() %>%
            group_by(Required_Skills, Automation_Risk) %>%
            summarise(Count = n(), .groups = "drop") %>%
            pivot_wider(names_from = Automation_Risk, values_from = Count, values_fill = 0)
        if (nrow(df_matrix) == 0) {
            return(plotly_empty())
        }

        skills <- df_matrix$Required_Skills
        risk_levels <- setdiff(names(df_matrix), "Required_Skills")
        mat <- as.matrix(df_matrix[, risk_levels])

        plot_ly(x = risk_levels, y = skills, z = mat, type = "heatmap", colors = "Purples") %>%
            layout(
                xaxis = list(title = "Ryzyko Automatyzacji"),
                yaxis = list(title = "Wymagana Umiejętność"),
                plot_bgcolor = "#F8FAFC"
            )
    })

    # --- WYKRES 4: Słupki Poziome ---
    output$plot_job_salary <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Job_Title) %>%
            summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(Avg_Salary))
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        plot_ly(df,
            x = ~Avg_Salary, y = ~ reorder(Job_Title, Avg_Salary), type = "bar",
            orientation = "h", marker = list(color = "#3498DB")
        ) %>%
            layout(
                xaxis = list(title = "Średnie Wynagrodzenie (USD)"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC"
            )
    })

    # --- WYKRES 5 ---
    output$plot_company_adoption <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Company_Size, AI_Adoption_Level) %>%
            summarise(Count = n(), .groups = "drop")
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        plot_ly(df,
            x = ~Company_Size, y = ~Count, type = "bar",
            color = ~AI_Adoption_Level, colors = "Set1"
        ) %>%
            layout(
                barmode = "group",
                xaxis = list(title = "Wielkość Firmy (Company Size)"),
                yaxis = list(title = "Liczba Stanowisk"),
                plot_bgcolor = "#F8FAFC"
            )
    })

    # --- WYKRES 6: Donut Chart ---
    output$plot_remote_pie <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Remote_Friendly) %>%
            summarise(Count = n(), .groups = "drop")
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        plot_ly(df,
            labels = ~Remote_Friendly, values = ~Count, type = "pie", hole = 0.5,
            marker = list(colors = c("#E67E22", "#2C3E50"))
        ) %>%
            layout(
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                legend = list(title = list(text = "Praca zdalna?"))
            )
    })

    # --- TABELA: SUROWE DANE (DT) ---
    output$table_raw_data <- renderDT({
        datatable(
            filtered_data(),
            options = list(
                pageLength = 10,
                searchHighlight = TRUE,
                scrollX = TRUE
            ),
            rownames = FALSE,
            class = "cell-border stripe hover"
        )
    })
}

shinyApp(ui = ui, server = server)
