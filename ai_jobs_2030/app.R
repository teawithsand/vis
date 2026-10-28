library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(highcharter)
library(DT)
library(readr)
library(forcats)
library(viridis)

jobs_data <- readr::read_csv(
    "AI_Impact_on_Jobs_2030.csv",
    show_col_types = FALSE
) %>%
    mutate(
        Job_Title = factor(Job_Title),
        Industry = factor(Industry),
        Country = factor(Country),
        Education_Level = factor(Education_Level, levels = c("High School", "Bachelor", "Master", "PhD")),
        Remote_Work_Possibility = factor(Remote_Work_Possibility),
        Automation_Level = factor(Automation_Level, levels = c("Low", "Medium", "High")),
        Company_Size = factor(Company_Size, levels = c("Startup", "Mid-sized", "Enterprise")),
        AI_Tool_Usage = factor(AI_Tool_Usage, levels = c("Low", "Moderate", "High")),
        Upskilling_Needed = factor(Upskilling_Needed),
        Hiring_Trend_2026 = factor(Hiring_Trend_2026, levels = c("Declining", "Stable", "Growing"))
    )

base_theme <- theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),
        plot.subtitle = element_text(size = 11, color = "grey40", margin = margin(b = 10)),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 11),
        plot.margin = margin(15, 15, 15, 15)
    )

ui <- page_navbar(
    title = "Wpływ AI na Rynek Pracy do 2030 Roku",
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#2c3e50",
        secondary = "#18bc9c"
    ),
    sidebar = sidebar(
        title = "Filtrowanie Danych",
        selectInput(
            "select_industry", "Branża:",
            choices = c("Wszystkie", levels(jobs_data$Industry)),
            selected = "Wszystkie"
        ),
        selectInput(
            "select_country", "Kraj:",
            choices = c("Wszystkie", levels(jobs_data$Country)),
            selected = "Wszystkie"
        ),
        checkboxGroupInput(
            "check_remote", "Możliwość pracy zdalnej:",
            choices = levels(jobs_data$Remote_Work_Possibility),
            selected = levels(jobs_data$Remote_Work_Possibility)
        ),
        sliderInput(
            "slider_experience", "Lata doświadczenia:",
            min = min(jobs_data$Years_Experience),
            max = max(jobs_data$Years_Experience),
            value = c(min(jobs_data$Years_Experience), max(jobs_data$Years_Experience)),
            step = 1
        )
    ),
    header = tags$div(
        style = "margin-bottom: 20px;",
        layout_columns(
            value_box(
                title = "Średnie Ryzyko Zastąpienia przez AI",
                value = textOutput("kpi_avg_risk"),
                showcase = icon("triangle-exclamation"),
                theme = "danger"
            ),
            value_box(
                title = "Średni Wynik Przyszłego Popytu",
                value = textOutput("kpi_avg_demand"),
                showcase = icon("arrow-trend-up"),
                theme = "success"
            ),
            value_box(
                title = "Średnie Wynagrodzenie (USD)",
                value = textOutput("kpi_avg_salary"),
                showcase = icon("money-bill-wave"),
                theme = "primary"
            )
        )
    ),
    nav_panel(
        title = "Ryzyko i Automatyzacja",
        icon = icon("robot"),
        layout_columns(
            col_widths = c(6, 6),
            card(
                card_header("Wykres 1: Ryzyko Zastąpienia przez AI vs Przyszły Popyt wg Stanowisk"),
                plotlyOutput("plot_risk_demand")
            ),
            card(
                card_header("Wykres 2: Poziom Automatyzacji a Konieczność Przekwalifikowania (Upskilling)"),
                plotOutput("plot_auto_upskilling")
            )
        )
    ),
    nav_panel(
        title = "Branże i Edukacja",
        icon = icon("graduation-cap"),
        layout_columns(
            col_widths = c(6, 6),
            card(
                card_header("Wykres 3: Rozkład Ryzyka Zastąpienia przez AI w Branżach"),
                plotlyOutput("plot_risk_distribution")
            ),
            card(
                card_header("Wykres 4: Trendy Zatrudnienia 2026 w relacji do Wykształcenia"),
                plotlyOutput("plot_education_trends")
            )
        )
    ),
    nav_panel(
        title = "Płace i Doświadczenie",
        icon = icon("money-bill-trend-up"),
        layout_columns(
            col_widths = c(6, 6),
            card(
                card_header("Wykres 5: Top 10 Najlepiej Płatnych Stanowisk (Średnia USD)"),
                highchartOutput("plot_top_salaries")
            ),
            card(
                card_header("Wykres 6: Zależność między Wynagrodzeniem a Latami Doświadczenia"),
                plotlyOutput("plot_salary_experience")
            )
        )
    ),
    nav_panel(
        title = "Narzędzia i Rynek",
        icon = icon("chart-line"),
        layout_columns(
            col_widths = c(6, 6),
            card(
                card_header("Wykres 7: Wykorzystanie Narzędzi AI a Efektywność Pracownika"),
                plotlyOutput("plot_tools_performance")
            ),
            card(
                card_header("Wykres 8: Wzrost Miejsc Pracy do 2030 r. vs Tygodniowy Czas Pracy"),
                plotlyOutput("plot_growth_hours")
            )
        )
    ),
    nav_panel(
        title = "Satysfakcja i Środowisko",
        icon = icon("briefcase"),
        layout_columns(
            col_widths = c(6, 6),
            card(
                card_header("Wykres 9: Poziom Satysfakcji z Pracy w zależności od Wielkości Firmy"),
                plotlyOutput("plot_satisfaction_size")
            ),
            card(
                card_header("Wykres 10: Udział Pracy Zdalnej w Średnim Ryzyku AI"),
                highchartOutput("plot_remote_share")
            )
        )
    ),
    nav_panel(
        title = "Surowe Dane",
        icon = icon("table"),
        card(
            card_header("Interaktywna Baza Danych (Przefiltrowana)"),
            DTOutput("raw_data_table")
        )
    )
)

server <- function(input, output, session) {
    filtered_data <- reactive({
        data <- jobs_data

        if (input$select_industry != "Wszystkie") {
            data <- data %>% filter(Industry == input$select_industry)
        }

        if (input$select_country != "Wszystkie") {
            data <- data %>% filter(Country == input$select_country)
        }

        data <- data %>%
            filter(Remote_Work_Possibility %in% input$check_remote) %>%
            filter(Years_Experience >= input$slider_experience[1] & Years_Experience <= input$slider_experience[2])

        return(data)
    })

    output$kpi_avg_risk <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("Brak danych")
        }
        paste0(round(mean(df$AI_Replacement_Risk, na.rm = TRUE) * 100, 1), "%")
    })

    output$kpi_avg_demand <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("Brak danych")
        }
        round(mean(df$Future_Demand_Score, na.rm = TRUE), 2)
    })

    output$kpi_avg_salary <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("Brak danych")
        }
        format(round(mean(df$Average_Salary_USD, na.rm = TRUE), 0), big.mark = " ")
    })

    output$plot_risk_demand <- renderPlotly({
        df_agg <- filtered_data() %>%
            group_by(Job_Title) %>%
            summarise(
                Avg_Risk = mean(AI_Replacement_Risk, na.rm = TRUE),
                Avg_Demand = mean(Future_Demand_Score, na.rm = TRUE),
                Count = n(),
                .groups = "drop"
            )

        if (nrow(df_agg) == 0) {
            return(plotly_empty())
        }

        p <- ggplot(df_agg, aes(x = Avg_Risk, y = Avg_Demand, text = paste("Stanowisko:", Job_Title, "<br>Liczba pracowników:", Count))) +
            geom_point(aes(size = Count, color = Avg_Demand), alpha = 0.8) +
            scale_color_viridis_c(option = "plasma") +
            expand_limits(
                x = c(min(df_agg$Avg_Risk) * 0.9, max(df_agg$Avg_Risk) * 1.1),
                y = c(min(df_agg$Avg_Demand) * 0.9, max(df_agg$Avg_Demand) * 1.1)
            ) +
            labs(x = "Średnie Ryzyko Zastąpienia przez AI", y = "Średni Wynik Przyszłego Popytu") +
            base_theme +
            theme(legend.position = "none")

        # Konwersja do Plotly wraz z dodaniem dodatkowego marginesu z lewej strony (l = 70) i od góry (t = 30)
        ggplotly(p, tooltip = "text") %>%
            layout(margin = list(l = 70, r = 20, t = 30, b = 50))
    })

    output$plot_auto_upskilling <- renderPlot({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(NULL)
        }

        ggplot(df, aes(x = Automation_Level, fill = Upskilling_Needed)) +
            geom_bar(position = "fill", alpha = 0.85) +
            scale_fill_brewer(palette = "Set2") +
            scale_y_continuous(labels = scales::percent) +
            labs(x = "Poziom Automatyzacji Stanowiska", y = "Odsetek", fill = "Potrzebne Przekwalifikowanie") +
            base_theme
    })

    output$plot_risk_distribution <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        p <- ggplot(df, aes(x = fct_reorder(Industry, AI_Replacement_Risk, .fun = median), y = AI_Replacement_Risk, fill = Industry)) +
            geom_violin(alpha = 0.4, trim = FALSE, color = NA) +
            geom_boxplot(width = 0.15, fill = "white", color = "grey30", outlier.alpha = 0.5) +
            coord_flip() +
            labs(x = "Branża", y = "Wskaźnik Ryzyka Zastąpienia przez AI") +
            base_theme +
            theme(legend.position = "none")

        ggplotly(p) %>%
            layout(margin = list(l = 100)) # Dodatkowy margines na długie nazwy branż z lewej strony
    })

    output$plot_education_trends <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        p <- ggplot(df, aes(x = Education_Level, fill = Hiring_Trend_2026)) +
            geom_bar(position = "dodge", alpha = 0.85) +
            scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
            labs(x = "Poziom Wykształcenia", y = "Liczba Pracowników", fill = "Trend Zatrudnienia (2026)") +
            base_theme

        ggplotly(p)
    })

    output$plot_top_salaries <- renderHighchart({
        df_sal <- filtered_data() %>%
            group_by(Job_Title) %>%
            summarise(Avg_Sal = round(mean(Average_Salary_USD, na.rm = TRUE), 0)) %>%
            arrange(desc(Avg_Sal)) %>%
            head(10)

        if (nrow(df_sal) == 0) {
            return(NULL)
        }

        highchart() %>%
            hc_chart(type = "column") %>%
            hc_xAxis(categories = df_sal$Job_Title) %>%
            hc_add_series(name = "Średnie Zarobki (USD)", data = df_sal$Avg_Sal, color = "#18bc9c") %>%
            hc_yAxis(title = list(text = "Wynagrodzenie roczne USD")) %>%
            hc_tooltip(pointFormat = "Zarobki: <b>{point.y} USD</b>")
    })

    output$plot_salary_experience <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        p <- ggplot(df, aes(x = Years_Experience, y = Average_Salary_USD)) +
            geom_point(alpha = 0.15, color = "#2c3e50") +
            geom_smooth(method = "lm", color = "#e74c3c", se = FALSE) +
            labs(x = "Lata doświadczenia", y = "Wynagrodzenie (USD)") +
            base_theme

        ggplotly(p) %>%
            layout(margin = list(l = 80)) # Margines na długie kwoty wynagrodzenia na osi Y
    })

    output$plot_tools_performance <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        p <- ggplot(df, aes(x = Performance_Score, fill = AI_Tool_Usage)) +
            geom_density(alpha = 0.5) +
            scale_fill_brewer(palette = "Accent") +
            labs(x = "Ocena Efektywności Pracownika (Performance Score)", y = "Gęstość", fill = "Poziom użycia AI") +
            base_theme

        ggplotly(p)
    })

    output$plot_growth_hours <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        p <- ggplot(df, aes(x = Work_Hours_Per_Week, y = Job_Growth_2030, color = Automation_Level)) +
            geom_jitter(alpha = 0.4, width = 0.5) +
            scale_color_manual(values = c("Low" = "#2ecc71", "Medium" = "#f1c40f", "High" = "#e74c3c")) +
            labs(x = "Laparoskopowa liczba godzin pracy", y = "Wzrost Miejsc Pracy do 2030 (%)", color = "Automatyzacja") +
            base_theme

        ggplotly(p)
    })

    output$plot_satisfaction_size <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        p <- ggplot(df, aes(x = Company_Size, y = Job_Satisfaction, fill = Company_Size)) +
            geom_boxplot(alpha = 0.7, outlier.colour = "red") +
            scale_fill_brewer(palette = "Pastel1") +
            labs(x = "Wielkość Przedsiębiorstwa", y = "Satysfakcja z Pracy (0-5)") +
            base_theme +
            theme(legend.position = "none")

        ggplotly(p)
    })

    output$plot_remote_share <- renderHighchart({
        df_rem <- filtered_data() %>%
            group_by(Remote_Work_Possibility) %>%
            summarise(Count = n(), .groups = "drop")

        if (nrow(df_rem) == 0) {
            return(NULL)
        }

        highchart() %>%
            hc_chart(type = "pie") %>%
            hc_add_series(
                name = "Liczba pracowników",
                data = list_parse2(df_rem),
                innerSize = "50%"
            ) %>%
            hc_tooltip(pointFormat = "Udział: <b>{point.percentage:.1f}%</b> ({point.y})")
    })

    output$raw_data_table <- renderDT({
        datatable(
            filtered_data(),
            rownames = FALSE,
            options = list(
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100),
                scrollX = TRUE,
                language = list(
                    url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Polish.json"
                )
            )
        )
    })
}

shinyApp(ui = ui, server = server)
