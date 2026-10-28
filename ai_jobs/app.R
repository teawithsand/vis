library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(packcircles)

# =========================================================
#  Dashboard: Analiza rynku pracy AI
#  Plik danych powinien znajdować się w tym samym folderze
#  co app.R: ai_job_market_insights.csv
# =========================================================

data <- read.csv("ai_job_market_insights.csv", stringsAsFactors = FALSE)

# --- Porządkowanie i polskie etykiety ---
data <- data %>%
    mutate(
        Salary_USD = as.numeric(Salary_USD),
        AI_Adoption_Level = factor(AI_Adoption_Level, levels = c("Low", "Medium", "High")),
        Automation_Risk = factor(Automation_Risk, levels = c("Low", "Medium", "High")),
        Job_Growth_Projection = factor(Job_Growth_Projection, levels = c("Decline", "Stable", "Growth")),
        Company_Size = factor(Company_Size, levels = c("Small", "Medium", "Large")),
        Remote_Friendly = factor(Remote_Friendly, levels = c("No", "Yes")),
        AI_Adoption_PL = factor(
            dplyr::recode(as.character(AI_Adoption_Level), "Low" = "Niska", "Medium" = "Średnia", "High" = "Wysoka"),
            levels = c("Niska", "Średnia", "Wysoka")
        ),
        Automation_Risk_PL = factor(
            dplyr::recode(as.character(Automation_Risk), "Low" = "Niskie", "Medium" = "Średnie", "High" = "Wysokie"),
            levels = c("Niskie", "Średnie", "Wysokie")
        ),
        Job_Growth_PL = factor(
            dplyr::recode(as.character(Job_Growth_Projection), "Decline" = "Spadek", "Stable" = "Stabilnie", "Growth" = "Wzrost"),
            levels = c("Spadek", "Stabilnie", "Wzrost")
        ),
        Company_Size_PL = factor(
            dplyr::recode(as.character(Company_Size), "Small" = "Mała", "Medium" = "Średnia", "Large" = "Duża"),
            levels = c("Mała", "Średnia", "Duża")
        ),
        Remote_Friendly_PL = factor(
            dplyr::recode(as.character(Remote_Friendly), "No" = "Nie", "Yes" = "Tak"),
            levels = c("Nie", "Tak")
        )
    )

salary_min <- floor(min(data$Salary_USD, na.rm = TRUE) / 1000) * 1000
salary_max <- ceiling(max(data$Salary_USD, na.rm = TRUE) / 1000) * 1000

# --- Pomocnicze funkcje ---
dollar_fmt <- function(x) {
    paste0("$", format(round(x, 0), big.mark = ",", scientific = FALSE))
}

pct_fmt <- function(x, digits = 1) {
    paste0(round(x, digits), "%")
}

empty_plot <- function(message = "Brak danych dla wybranych filtrów") {
    plot_ly() %>%
        layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
                text = message,
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 15)
            ),
            plot_bgcolor = "#F8FAFC",
            paper_bgcolor = "#F8FAFC"
        )
}

chart_card <- function(title, description, output_id, height = "430px") {
    card(
        full_screen = TRUE,
        class = "chart-card",
        card_header(title),
        card_body(
            p(class = "text-muted small", description),
            plotlyOutput(output_id, height = height)
        )
    )
}

chart_row <- function(left, right) {
    div(
        class = "row g-3 chart-row",
        div(class = "col-12 col-lg-6", left),
        div(class = "col-12 col-lg-6", right)
    )
}

# =========================================================
# UI
# =========================================================
ui <- page_sidebar(
    title = "Analiza Rynku Pracy AI",
    fillable = FALSE,
    theme = bs_theme(
        version = 5,
        bootswatch = "cosmo",
        primary = "#2C3E50",
        success = "#18BC9C"
    ),
    tags$head(
        tags$style(HTML("
      html, body {
        height: auto !important;
        min-height: 100% !important;
        overflow-y: auto !important;
        overflow-x: hidden !important;
      }

      .bslib-page-fill,
      .bslib-page-main,
      .bslib-sidebar-layout,
      main,
      .main {
        height: auto !important;
        max-height: none !important;
        overflow: visible !important;
      }

      .nav-pills {
        flex-wrap: nowrap;
        overflow-x: auto;
        white-space: nowrap;
        padding-bottom: 0.25rem;
        margin-bottom: 1rem;
      }
      .nav-pills .nav-link {
        white-space: nowrap;
      }
      .bslib-card .card-header {
        font-weight: 600;
      }
      .sidebar .form-label, .sidebar label {
        font-weight: 600;
      }
      .small-helper {
        font-size: 0.85rem;
        color: #6c757d;
      }

      .kpi-row {
        margin-bottom: 1rem;
      }
      .kpi-row .bslib-value-box {
        min-height: 145px !important;
      }
      .kpi-row .value-box-showcase {
        font-size: 2.2rem !important;
      }
      .kpi-row .value-box-title {
        font-size: 0.95rem !important;
      }
      .kpi-row .value-box-value {
        font-size: 1.7rem !important;
        line-height: 1.15 !important;
      }

      .chart-row {
        margin-bottom: 1rem;
      }
      .chart-card {
        min-height: 600px !important;
        height: auto !important;
        overflow: visible !important;
      }
      .chart-card .card-body {
        min-height: 520px !important;
        overflow: visible !important;
      }
      .chart-card .html-widget,
      .chart-card .plotly,
      .chart-card .shiny-plot-output {
        min-height: 430px !important;
      }
    "))
    ),
    sidebar = sidebar(
        width = 330,
        title = tags$h4("Filtry", class = "text-primary"),
        selectInput(
            "industry_filter", "Branża:",
            choices = c("Wszystkie" = "All", sort(unique(as.character(data$Industry))))
        ),
        selectInput(
            "location_filter", "Lokalizacja:",
            choices = c("Wszystkie" = "All", sort(unique(as.character(data$Location))))
        ),
        selectInput(
            "remote_filter", "Praca zdalna:",
            choices = c("Wszystkie" = "All", "Nie" = "No", "Tak" = "Yes")
        ),
        tags$hr(),
        selectInput(
            "company_size_filter", "Wielkość firmy:",
            choices = c("Wszystkie" = "All", "Mała" = "Small", "Średnia" = "Medium", "Duża" = "Large")
        ),
        selectInput(
            "ai_adoption_filter", "Poziom adopcji AI:",
            choices = c("Wszystkie" = "All", "Niska" = "Low", "Średnia" = "Medium", "Wysoka" = "High")
        ),
        selectInput(
            "automation_risk_filter", "Ryzyko automatyzacji:",
            choices = c("Wszystkie" = "All", "Niskie" = "Low", "Średnie" = "Medium", "Wysokie" = "High")
        ),
        selectInput(
            "growth_filter", "Prognoza zatrudnienia:",
            choices = c("Wszystkie" = "All", "Spadek" = "Decline", "Stabilnie" = "Stable", "Wzrost" = "Growth")
        ),
        tags$hr(),
        sliderInput(
            "salary_filter", "Wynagrodzenie (USD):",
            min = salary_min,
            max = salary_max,
            value = c(salary_min, salary_max),
            step = 5000,
            pre = "$",
            sep = ","
        ),
        selectizeInput(
            "job_title_filter", "Stanowiska:",
            choices = sort(unique(as.character(data$Job_Title))),
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Wszystkie")
        ),
        selectizeInput(
            "skills_filter", "Wymagane umiejętności:",
            choices = sort(unique(as.character(data$Required_Skills))),
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Wszystkie")
        ),
        actionButton("reset_filters", "Resetuj filtry", icon = icon("rotate-left"), class = "btn-outline-primary w-100"),
        tags$hr(),
        tags$p(class = "text-muted small", "System Analizy Rynku Pracy • 2026")
    ),
    navset_pill(
        nav_panel(
            title = "Dane",
            card(
                full_screen = TRUE,
                card_header(
                    div(
                        class = "d-flex justify-content-between align-items-center w-100",
                        span("Przeglądarka zbioru danych"),
                        downloadButton("download_data", "Pobierz dane (CSV)", class = "btn-success btn-sm")
                    )
                ),
                card_body(
                    p(class = "text-muted small", "Tabela zawiera dane po zastosowaniu aktualnych filtrów."),
                    DTOutput("table_raw_data")
                )
            )
        ),
            chart_row(
                chart_card(
                    "Struktura branż",
                    "Podział stanowisk według sektorów.",
                    "plot_industry_count"
                ),
                chart_card(
                    "Średnie wynagrodzenie według lokalizacji",
                    "Ranking lokalizacji według przeciętnego wynagrodzenia w USD.",
                    "plot_location_salary_overview"
                )
            ),
        ),
        nav_panel(
            title = "Płace",
            chart_row(
                chart_card(
                    "Wynagrodzenie a adopcja AI",
                    "Wykres skrzypcowy pokazujący medianę, kwartyle i obserwacje odstające dla każdego poziomu adopcji AI.",
                    "plot_salary_ai_box"
                ),
                chart_card(
                    "Wynagrodzenie a ryzyko automatyzacji",
                    "Porównanie rozkładu wynagrodzeń między poziomami ryzyka automatyzacji.",
                    "plot_salary_risk_box"
                )
            ),
            chart_row(
                chart_card(
                    "Top 10 najlepiej płatnych stanowisk",
                    "Średnie wynagrodzenie dla poszczególnych ról.",
                    "plot_job_salary"
                ),
                chart_card(
                    "Top 10 najlepiej płatnych kompetencji",
                    "Wpływ głównych umiejętności na poziom zarobków.",
                    "plot_skills_salary"
                )
            ),
        ),
        nav_panel(
            title = "AI i ryzyko",
            chart_row(
                chart_card(
                    "Branże z najwyższym udziałem wysokiego ryzyka",
                    "Udział stanowisk o wysokim ryzyku automatyzacji w każdej branży.",
                    "plot_industry_high_risk"
                ),
                chart_card(
                    "Branże z najwyższym udziałem wysokiej adopcji AI",
                    "Udział stanowisk z wysokim poziomem adopcji AI w każdej branży.",
                    "plot_industry_high_ai"
                )
            ),
        ),
        nav_panel(
            title = "Starcie Cyfrowych Sił",
            chart_row(
                chart_card(
                    "Adopcja AI vs Ryzyko Automatyzacji w Branżach",
                    "Wykres radarowy porównuje stopień zaawansowania technologicznego (niebieski) z zagrożeniem redukcji etatów (czerwony) dla poszczególnych sektorów gospodarki.",
                    "plot_ai_vs_risk_radar",
                    height = "520px"
                ),
                card(
                    class = "chart-card",
                    card_header("Jak analizować konflikt technologiczny?"),
                    card_body(
                        p("Wykres nakłada na siebie dwie kluczowe cechy rynkowe przeliczone na punkty procentowe:"),
                        tags$ul(
                            tags$li(tags$span(style = "color: #3498DB; font-weight: bold;", "Wysoka Adopcja AI:"), " Jaki procent firm w danej branży wdrożył AI na najwyższym poziomie (High)."),
                            tags$li(tags$span(style = "color: #E74C3C; font-weight: bold;", "Wysokie Ryzyko Automatyzacji:"), " Jaki procent stanowisk w tej branży uważa się za wysoce podatne na automatyzację (High).")
                        ),
                        p(tags$b("Interpretacja kształtu:")),
                        p("• Miejsca, gdzie obie plamy szeroko się pokrywają, to sektory przechodzące agresywną transformację (np. potencjalne cięcia etatów na rzecz algorytmów)."),
                        p("• Sekcje, gdzie dominuje kolor niebieski (a czerwony jest schowany w środku), to bezpieczne rynki, gdzie AI wspiera produktywność ludzi, zamiast ich wypychać."),
                    )
                )
            )
        ),
        nav_panel(
            title = "Kompetencje",
            chart_row(
                chart_card(
                    "Kompetencje vs ryzyko automatyzacji",
                    "Mapa cieplna pokazująca liczbę stanowisk łączących konkretną umiejętność z poziomem ryzyka automatyzacji.",
                    "plot_skills_risk_heatmap",
                    height = "520px"
                ),
                chart_card(
                    "Kompetencje vs adopcja AI",
                    "Wykres 100% skumulowany pokazujący poziomy adopcji AI dla stanowisk wymagających danej umiejętności.",
                    "plot_skills_ai_100",
                    height = "520px"
                )
            ),
            chart_row(
                chart_card(
                    "Kompetencje vs prognoza zatrudnienia",
                    "Wykres 100% skumulowany pokazujący prognozy zatrudnienia dla stanowisk wymagających danej umiejętności.",
                    "plot_skills_growth_100",
                    height = "520px"
                ),
                chart_card(
                    "Popularność umiejętności na rynku pracy",
                    "Najczęściej pojawiające się wymagane kompetencje w ofertach.",
                    "plot_skills_count",
                    height = "520px"
                )
            )
        ),
        nav_panel(
            title = "Dane",
            card(
                full_screen = TRUE,
                card_header("Przeglądarka zbioru danych"),
                card_body(
                    p(class = "text-muted small", "Tabela zawiera dane po zastosowaniu aktualnych filtrów."),
                    DTOutput("table_raw_data")
                )
            )
        )
    )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
    observeEvent(input$reset_filters, {
        updateSelectInput(session, "industry_filter", selected = "All")
        updateSelectInput(session, "location_filter", selected = "All")
        updateSelectInput(session, "remote_filter", selected = "All")
        updateSelectInput(session, "company_size_filter", selected = "All")
        updateSelectInput(session, "ai_adoption_filter", selected = "All")
        updateSelectInput(session, "automation_risk_filter", selected = "All")
        updateSelectInput(session, "growth_filter", selected = "All")
        updateSliderInput(session, "salary_filter", value = c(salary_min, salary_max))
        updateSelectizeInput(session, "job_title_filter", selected = character(0))
        updateSelectizeInput(session, "skills_filter", selected = character(0))
    })

    filtered_data <- reactive({
        df <- data

        if (!is.null(input$industry_filter) && input$industry_filter != "All") {
            df <- df %>% filter(Industry == input$industry_filter)
        }
        if (!is.null(input$location_filter) && input$location_filter != "All") {
            df <- df %>% filter(Location == input$location_filter)
        }
        if (!is.null(input$remote_filter) && input$remote_filter != "All") {
            df <- df %>% filter(Remote_Friendly == input$remote_filter)
        }
        if (!is.null(input$company_size_filter) && input$company_size_filter != "All") {
            df <- df %>% filter(Company_Size == input$company_size_filter)
        }
        if (!is.null(input$ai_adoption_filter) && input$ai_adoption_filter != "All") {
            df <- df %>% filter(AI_Adoption_Level == input$ai_adoption_filter)
        }
        if (!is.null(input$automation_risk_filter) && input$automation_risk_filter != "All") {
            df <- df %>% filter(Automation_Risk == input$automation_risk_filter)
        }
        if (!is.null(input$growth_filter) && input$growth_filter != "All") {
            df <- df %>% filter(Job_Growth_Projection == input$growth_filter)
        }
        if (!is.null(input$salary_filter)) {
            df <- df %>% filter(Salary_USD >= input$salary_filter[1], Salary_USD <= input$salary_filter[2])
        }
        if (!is.null(input$job_title_filter) && length(input$job_title_filter) > 0) {
            df <- df %>% filter(Job_Title %in% input$job_title_filter)
        }
        if (!is.null(input$skills_filter) && length(input$skills_filter) > 0) {
            df <- df %>% filter(Required_Skills %in% input$skills_filter)
        }

        df
    })


    # =======================================================
    # KPI
    # =======================================================
    output$kpi_count <- renderText({
        nrow(filtered_data())
    })

    output$kpi_avg_salary <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("Brak danych")
        }
        dollar_fmt(mean(df$Salary_USD, na.rm = TRUE))
    })

    output$kpi_median_salary <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("Brak danych")
        }
        dollar_fmt(median(df$Salary_USD, na.rm = TRUE))
    })

    output$kpi_high_risk <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("Brak danych")
        }
        pct_fmt(mean(df$Automation_Risk == "High", na.rm = TRUE) * 100)
    })

    # =======================================================
    # PRZEGLĄD
    # =======================================================

    output$plot_industry_count <- renderPlotly({
        df <- filtered_data() %>%
            count(Industry, name = "Count") %>%
            arrange(desc(Count))

        if (nrow(df) == 0) {
            return(empty_plot())
        }

        # 10-color soft spectrum (competency-style)
        colors <- c(
            "#6C5CE7", # purple
            "#A29BFE", # light purple
            "#74B9FF", # blue
            "#81ECEC", # cyan
            "#55EFC4", # teal
            "#FDCB6E", # soft yellow
            "#FAB1A0", # peach
            "#FFB142", # orange
            "#E17055", # warm orange-red
            "#D63031" # soft red
        )

        plot_ly(
            data = df,
            type = "funnelarea",
            labels = ~Industry,
            values = ~Count,
            textinfo = "label+value+percent initial",
            marker = list(
                colors = scales::alpha(colors, 0.60),
                line = list(
                    color = "rgba(255,255,255,0.85)",
                    width = 1
                )
            ),
            hoverlabel = list(
                bgcolor = "rgba(255,255,255,0.96)",
                bordercolor = "rgba(116, 185, 255, 0.20)",
                font = list(
                    family = "Inter, Arial, sans-serif",
                    size = 13,
                    color = "#2D3436"
                )
            ),
            hovertemplate = paste(
                "<b>%{label}</b><br>",
                "Stanowiska: %{value}<br>",
                "Udział: %{percentInitial:.1%}",
                "<extra></extra>"
            )
        ) %>%
            layout(
                paper_bgcolor = "#F8FAFC",
                plot_bgcolor = "#F8FAFC",
                font = list(
                    family = "Inter, Arial, sans-serif",
                    color = "#2D3436"
                ),
                margin = list(t = 10, l = 10, r = 10, b = 10)
            )
    })

    output$plot_location_salary_overview <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Location) %>%
            summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
            arrange(Avg_Salary)
        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(df) %>%
            add_segments(
                x = 0, xend = ~Avg_Salary,
                y = ~ factor(Location, levels = Location), yend = ~ factor(Location, levels = Location),
                line = list(color = "#BDC3C7", width = 2),
                showlegend = FALSE
            ) %>%
            add_markers(
                x = ~Avg_Salary,
                y = ~ factor(Location, levels = Location),
                marker = list(color = "#18BC9C", size = 10),
                hovertext = ~ paste0("Lokalizacja: ", Location, "<br>Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
                hoverinfo = "text",
                showlegend = FALSE
            ) %>%
            layout(
                xaxis = list(title = "Średnie wynagrodzenie (USD)"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    # =======================================================
    # PŁACE
    # =======================================================
    output$plot_salary_ai_box <- renderPlotly({
        df <- filtered_data() %>%
            filter(!is.na(AI_Adoption_PL), !is.na(Salary_USD))

        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(
            df,
            x = ~AI_Adoption_PL,
            y = ~Salary_USD,
            type = "violin",
            color = ~AI_Adoption_PL,
            colors = "Set2",
            box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            hovertemplate = "Wynagrodzenie: $%{y:,.0f}<extra></extra>"
        ) %>%
            layout(
                xaxis = list(title = "Poziom adopcji AI"),
                yaxis = list(title = "Wynagrodzenie (USD)"),
                showlegend = FALSE
            )
    })

    output$plot_salary_risk_box <- renderPlotly({
        df <- filtered_data() %>% filter(!is.na(Automation_Risk_PL), !is.na(Salary_USD))
        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(
            df,
            x = ~Automation_Risk_PL,
            y = ~Salary_USD,
            type = "box",
            color = ~Automation_Risk_PL,
            colors = c("#2ECC71", "#F39C12", "#E74C3C"),
            boxpoints = "outliers",
            hovertemplate = "Wynagrodzenie: $%{y:,.0f}<extra></extra>"
            
        ) %>%
            layout(
                xaxis = list(title = "Ryzyko automatyzacji"),
                yaxis = list(title = "Wynagrodzenie (USD)"),
                showlegend = FALSE,
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    output$plot_job_salary <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Job_Title) %>%
            summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
            arrange(desc(Avg_Salary)) %>%
            slice_head(n = 10) %>%
            arrange(Avg_Salary)
        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(df) %>%
            add_segments(
                x = 0, xend = ~Avg_Salary,
                y = ~ factor(Job_Title, levels = Job_Title), yend = ~ factor(Job_Title, levels = Job_Title),
                line = list(color = "#BDC3C7", width = 2),
                showlegend = FALSE
            ) %>%
            add_markers(
                x = ~Avg_Salary,
                y = ~ factor(Job_Title, levels = Job_Title),
                marker = list(color = "#3498DB", size = 10),
                hovertext = ~ paste0("Stanowisko: ", Job_Title, "<br>Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
                hoverinfo = "text",
                showlegend = FALSE
            ) %>%
            layout(
                xaxis = list(title = "Średnie wynagrodzenie (USD)"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    output$plot_skills_salary <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Required_Skills) %>%
            summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
            arrange(desc(Avg_Salary)) %>%
            slice_head(n = 10) %>%
            arrange(Avg_Salary)
        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(df) %>%
            add_segments(
                x = 0, xend = ~Avg_Salary,
                y = ~ factor(Required_Skills, levels = Required_Skills), yend = ~ factor(Required_Skills, levels = Required_Skills),
                line = list(color = "#BDC3C7", width = 2),
                showlegend = FALSE
            ) %>%
            add_markers(
                x = ~Avg_Salary,
                y = ~ factor(Required_Skills, levels = Required_Skills),
                marker = list(color = "#9B59B6", size = 10),
                hovertext = ~ paste0("Umiejętność: ", Required_Skills, "<br>Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
                hoverinfo = "text",
                showlegend = FALSE
            ) %>%
            layout(
                xaxis = list(title = "Średnie wynagrodzenie (USD)"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    output$plot_industry_salary <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Industry) %>%
            summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
            arrange(Avg_Salary)
        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(
            df,
            x = ~Avg_Salary,
            y = ~ factor(Industry, levels = Industry),
            type = "bar",
            orientation = "h",
            marker = list(color = "#18BC9C"),
            hovertext = ~ paste0("Branża: ", Industry, "<br>Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
            hoverinfo = "text"
        ) %>%
            layout(
                xaxis = list(title = "Średnie wynagrodzenie (USD)"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    output$plot_ai_vs_risk_radar <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(empty_plot())
        }

        library(dplyr)

        # Agregacja danych: obliczamy % poziomu "High" dla AI oraz dla Automatyzacji
        radar_data <- df %>%
            group_by(Industry) %>%
            summarise(
                Total = n(),
                # Zapobiegamy dzieleniu przez zero i liczymy odsetek wartości "High"
                High_AI = if_not_empty(sum(AI_Adoption_Level == "High", na.rm = TRUE) / Total * 100, 0),
                High_Risk = if_not_empty(sum(Automation_Risk == "High", na.rm = TRUE) / Total * 100, 0)
            ) %>%
            ungroup()

        # Bezpiecznik na wypadek problemów z danymi
        if (nrow(radar_data) == 0) {
            return(empty_plot())
        }

        # Aby zamknąć pętlę w wykresie radarowym Plotly, musimy powtórzyć pierwszy wiersz na końcu bazy
        radar_data_closed <- rbind(radar_data, radar_data[1, ])

        # Budowanie wielowarstwowego wykresu radarowego
        plot_ly() %>%
            # Warstwa 1: Poziom wdrożenia AI (Kolor Niebieski)
            add_trace(
                data = radar_data_closed,
                type = "scatterpolar",
                r = ~High_AI,
                theta = ~Industry,
                fill = "toself",
                fillcolor = "rgba(52, 152, 219, 0.25)",
                line = list(color = "#3498DB", width = 2.5),
                marker = list(color = "#3498DB", size = 6),
                name = "Wysoka Adopcja AI (%)",
                hovertemplate = "Branża: %{theta}<br>Wysoka adopcja AI: %{r:.1f}%<extra></extra>"
            ) %>%
            # Warstwa 2: Ryzyko Automatyzacji (Kolor Czerwony)
            add_trace(
                data = radar_data_closed,
                type = "scatterpolar",
                r = ~High_Risk,
                theta = ~Industry,
                fill = "toself",
                fillcolor = "rgba(231, 76, 60, 0.25)",
                line = list(color = "#E74C3C", width = 2.5),
                marker = list(color = "#E74C3C", size = 6),
                name = "Wysokie Ryzyko Automatyzacji (%)",
                hovertemplate = "Branża: %{theta}<br>Ryzyko automatyzacji: %{r:.1f}%<extra></extra>"
            ) %>%
            # Konfiguracja osi i tła wykresu
            layout(
                polar = list(
                    radialaxis = list(
                        visible = TRUE,
                        ticksuffix = "%",
                        range = c(0, 100),
                        gridcolor = "#E2E8F0",
                        linecolor = "#E2E8F0"
                    ),
                    angularaxis = list(
                        gridcolor = "#E2E8F0",
                        linecolor = "#E2E8F0"
                    )
                ),
                showlegend = TRUE,
                legend = list(
                    orientation = "h",
                    xanchor = "center",
                    x = 0.5,
                    y = -0.15
                ),
                margin = list(t = 40, b = 60, l = 40, r = 40),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    # Pomocnicza funkcja inline zabezpieczająca przed wartościami NaN
    if_not_empty <- function(val, default) {
        if (length(val) == 0 || is.nan(val) || is.na(val)) {
            return(default)
        }
        return(val)
    }
    # =======================================================
    # AI I RYZYKO
    # =======================================================
    output$plot_industry_high_risk <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Industry) %>%
            summarise(
                High_Risk_Share = mean(Automation_Risk == "High", na.rm = TRUE) * 100,
                Count = n(),
                .groups = "drop"
            ) %>%
            arrange(High_Risk_Share)

        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(
            df,
            x = ~High_Risk_Share,
            y = ~ reorder(Industry, High_Risk_Share),
            type = "bar",
            orientation = "h",
            marker = list(color = "#A29BFE"),
            hovertemplate =
                "<b>%{y}</b><br>High risk: %{x:.1f}%<extra></extra>"
        ) %>%
            layout(
                xaxis = list(title = "Udział wysokiego ryzyka (%)", ticksuffix = "%"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })
    output$plot_industry_high_ai <- renderPlotly({
        df <- filtered_data() %>%
            group_by(Industry) %>%
            summarise(
                High_AI_Share = mean(AI_Adoption_Level == "High", na.rm = TRUE) * 100,
                Count = n(),
                .groups = "drop"
            ) %>%
            arrange(High_AI_Share)

        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(
            df,
            x = ~High_AI_Share,
            y = ~ reorder(Industry, High_AI_Share),
            type = "bar",
            orientation = "h",
            marker = list(color = "#74B9FF"),
            hovertemplate =
                "<b>%{y}</b><br>High AI: %{x:.1f}%<extra></extra>"
        ) %>%
            layout(
                xaxis = list(title = "Udział wysokiej adopcji AI (%)", ticksuffix = "%"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    # =======================================================
    # KOMPETENCJE
    # =======================================================
    output$plot_skills_risk_heatmap <- renderPlotly({
        base_df <- filtered_data()
        if (nrow(base_df) == 0) {
            return(empty_plot())
        }

        df_heat <- base_df %>%
            count(Required_Skills, Automation_Risk_PL, name = "Count") %>%
            complete(Required_Skills, Automation_Risk_PL, fill = list(Count = 0))

        mat_df <- df_heat %>%
            pivot_wider(names_from = Automation_Risk_PL, values_from = Count, values_fill = 0) %>%
            arrange(Required_Skills)

        risk_levels <- c("Niskie", "Średnie", "Wysokie")
        mat <- as.matrix(mat_df[, risk_levels, drop = FALSE])

        plot_ly(
            x = risk_levels,
            y = as.character(mat_df$Required_Skills),
            z = mat,
            type = "heatmap",
            colors = "Blues",
            hovertemplate = "Umiejętność: %{y}<br>Ryzyko: %{x}<br>Liczba: %{z}<extra></extra>"
        ) %>%
            layout(
                xaxis = list(title = "Ryzyko automatyzacji"),
                yaxis = list(title = "Wymagana umiejętność"),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    output$plot_skills_ai_100 <- renderPlotly({
        df <- filtered_data() %>%
            count(Required_Skills, AI_Adoption_PL, name = "Count") %>%
            group_by(Required_Skills) %>%
            mutate(Share = Count / sum(Count) * 100) %>%
            ungroup()
        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(
            df,
            x = ~Share,
            y = ~Required_Skills,
            type = "bar",
            orientation = "h",
            color = ~AI_Adoption_PL,
            colors = "Purples",
            text = ~ pct_fmt(Share),
            customdata = ~Count,
            hovertemplate = "Udział: %{x:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
        ) %>%
            layout(
                barmode = "stack",
                xaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
                yaxis = list(title = ""),
                legend = list(title = list(text = "Adopcja AI")),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    output$plot_skills_growth_100 <- renderPlotly({
        df <- filtered_data() %>%
            count(Required_Skills, Job_Growth_PL, name = "Count") %>%
            group_by(Required_Skills) %>%
            mutate(Share = Count / sum(Count) * 100) %>%
            ungroup()
        if (nrow(df) == 0) {
            return(empty_plot())
        }

        plot_ly(
            df,
            x = ~Share,
            y = ~Required_Skills,
            type = "bar",
            orientation = "h",
            color = ~Job_Growth_PL,
            colors = c("#ef887c", "#ecb55d", "#ebe97c"),
            text = ~ pct_fmt(Share),
            customdata = ~Count,
            hovertemplate = "Udział: %{x:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
        ) %>%
            layout(
                barmode = "stack",
                xaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
                yaxis = list(title = ""),
                legend = list(title = list(text = "Prognoza")),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    output$plot_skills_count <- renderPlotly({
        df <- filtered_data() %>%
            count(Required_Skills, name = "Count") %>%
            arrange(desc(Count)) %>%
            slice_head(n = 12)

        if (nrow(df) == 0) {
            return(empty_plot())
        }

        # WAŻNE: zamiana na character + ręczne sortowanie
        df <- df %>%
            arrange(Count)

        plot_ly(
            df,
            x = ~Count,
            y = ~Required_Skills,
            type = "scatter",
            mode = "markers",
            marker = list(
                size = ~Count,
                color = ~Count,
                colorscale = "Purples",
                showscale = TRUE
            ),
            hovertemplate =
                "<b>%{y}</b><br>Liczba stanowisk: %{x}<extra></extra>"
        ) %>%
            layout(
                xaxis = list(title = "Liczba stanowisk"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC",
                paper_bgcolor = "#F8FAFC"
            )
    })

    # =======================================================
    # DANE (TABELA)
    # =======================================================
    output$table_raw_data <- renderDT({
        df <- filtered_data()
        datatable(
            df,
            options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
            rownames = FALSE
        )
    })


    # =======================================================
    # OBSŁUGA POBIERANIA DANYCH
    # =======================================================
    output$download_data <- downloadHandler(
        filename = function() {
            paste("ai_job_market_filtered_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            # Pobieramy aktualnie przefiltrowane dane
            export_df <- filtered_data()
            
            # Opcjonalnie: usuwamy kolumny pomocnicze z sufiksem _PL przed eksportem,
            # aby zachować czysty plik wynikowy. Jeśli chcesz je zostawić, usuń poniższy select.
            export_df <- export_df %>% 
                select(-ends_with("_PL"))
            
            write.csv(export_df, file, row.names = FALSE, na = "")
        }
    )
}

shinyApp(ui = ui, server = server)
