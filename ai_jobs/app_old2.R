library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

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

share_by_group <- function(df, group_var, fill_var) {
  df %>%
    count({{ group_var }}, {{ fill_var }}, name = "Count") %>%
    group_by({{ group_var }}) %>%
    mutate(Share = Count / sum(Count) * 100) %>%
    ungroup()
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
      title = "Przegląd",
      div(
        class = "kpi-row",
        layout_columns(
          fill = FALSE,
          value_box(
            title = "Liczba stanowisk",
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
            title = "Mediana wynagrodzenia",
            value = textOutput("kpi_median_salary"),
            showcase = icon("chart-simple"),
            theme = "info"
          ),
          value_box(
            title = "Stanowiska z wysokim ryzykiem",
            value = textOutput("kpi_high_risk"),
            showcase = icon("triangle-exclamation"),
            theme = "danger"
          )
        )
      ),

      chart_row(
        chart_card(
          "Struktura branż",
          "Liczba stanowisk w poszczególnych branżach dla aktualnie wybranych filtrów.",
          "plot_industry_count"
        ),
        chart_card(
          "Prognoza zatrudnienia",
          "Ogólny rozkład prognoz: spadek, stabilnie lub wzrost.",
          "plot_growth_overall"
        )
      ),
      chart_row(
        chart_card(
          "Średnie wynagrodzenie według lokalizacji",
          "Ranking lokalizacji według przeciętnego wynagrodzenia w USD.",
          "plot_location_salary_overview"
        ),
        chart_card(
          "Poziom adopcji AI",
          "Ogólny rozkład poziomu adopcji AI w przefiltrowanym zbiorze.",
          "plot_ai_adoption_overall"
        )
      ),
      chart_row(
        chart_card(
          "Wielkość firm w danych",
          "Liczba stanowisk według wielkości firmy.",
          "plot_company_count"
        ),
        chart_card(
          "Praca zdalna w danych",
          "Ogólny podział stanowisk na przyjazne i nieprzyjazne pracy zdalnej.",
          "plot_remote_overall"
        )
      )
    ),

    nav_panel(
      title = "Płace",
      chart_row(
        chart_card(
          "Wynagrodzenie a adopcja AI",
          "Boxplot pokazujący medianę, kwartyle i obserwacje odstające dla każdego poziomu adopcji AI.",
          "plot_salary_ai_box"
        ),
        chart_card(
          "Wynagrodzenie a ryzyko automatyzacji",
          "Porównanie rozkładu wynagrodzeń między poziomami ryzyka automatyzacji.",
          "plot_salary_risk_box"
        )
      )
    ),

    nav_panel(
      title = "AI i ryzyko",
      chart_row(
        chart_card(
          "Mapa cieplna: adopcja AI i ryzyko automatyzacji",
          "Liczba stanowisk w każdej kombinacji poziomu adopcji AI i ryzyka automatyzacji.",
          "plot_ai_risk_heatmap"
        ),
        chart_card(
          "Adopcja AI a wysokie ryzyko automatyzacji w branżach",
          "Każda kropka to branża. Oś X pokazuje udział wysokiej adopcji AI, oś Y udział wysokiego ryzyka automatyzacji. Linia trendu pomaga ocenić kierunek zależności.",
          "plot_industry_ai_risk_trend"
        )
      ),
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
      )
    ),

    nav_panel(
      title = "Branże",
      chart_row(
        chart_card(
          "Prognoza zatrudnienia w branżach",
          "Wykres 100% skumulowany pokazujący strukturę prognoz zatrudnienia w każdej branży.",
          "plot_growth_industry_100"
        ),
        chart_card(
          "Adopcja AI w branżach",
          "Wykres 100% skumulowany pokazujący strukturę poziomów adopcji AI w każdej branży.",
          "plot_ai_industry_100"
        )
      ),
      chart_row(
        chart_card(
          "Ryzyko automatyzacji w branżach",
          "Wykres 100% skumulowany pokazujący strukturę ryzyka automatyzacji w każdej branży.",
          "plot_risk_industry_100"
        ),
        chart_card(
          "Liczba stanowisk według branży i prognozy",
          "Pogrupowany wykres słupkowy pokazujący liczebności prognoz w branżach.",
          "plot_growth_industry_grouped"
        )
      )
    ),

    nav_panel(
      title = "Firmy i remote",
      chart_row(
        chart_card(
          "Wielkość firmy vs adopcja AI",
          "Wykres 100% skumulowany pokazujący, czy struktura adopcji AI różni się między małymi, średnimi i dużymi firmami.",
          "plot_company_ai_100"
        ),
        chart_card(
          "Wielkość firmy vs ryzyko automatyzacji",
          "Wykres 100% skumulowany pokazujący strukturę ryzyka automatyzacji według wielkości firmy.",
          "plot_company_risk_100"
        )
      ),
      chart_row(
        chart_card(
          "Praca zdalna vs ryzyko automatyzacji",
          "Wykres 100% skumulowany pokazujący strukturę ryzyka dla stanowisk przyjaznych i nieprzyjaznych pracy zdalnej.",
          "plot_remote_risk_100"
        ),
        chart_card(
          "Praca zdalna vs adopcja AI",
          "Wykres 100% skumulowany pokazujący strukturę adopcji AI dla stanowisk zdalnych i niezdalnych.",
          "plot_remote_ai_100"
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
          "Ranking umiejętności według liczby stanowisk",
          "Najczęściej pojawiające się wymagane umiejętności w aktualnie przefiltrowanych danych.",
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
    if (nrow(df) == 0) return("Brak danych")
    dollar_fmt(mean(df$Salary_USD, na.rm = TRUE))
  })

  output$kpi_median_salary <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("Brak danych")
    dollar_fmt(median(df$Salary_USD, na.rm = TRUE))
  })

  output$kpi_top_growth <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("Brak danych")

    counts <- df %>%
      count(Job_Growth_PL, name = "n") %>%
      mutate(Share = n / sum(n) * 100) %>%
      arrange(desc(n), Job_Growth_PL)

    paste0(
      as.character(counts$Job_Growth_PL), ": ", pct_fmt(counts$Share),
      collapse = " | "
    )
  })

  output$kpi_high_risk <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("Brak danych")
    pct_fmt(mean(df$Automation_Risk == "High", na.rm = TRUE) * 100)
  })

  # =======================================================
  # PRZEGLĄD
  # =======================================================
  output$plot_industry_count <- renderPlotly({
    df <- filtered_data() %>% count(Industry, name = "Count") %>% arrange(Count)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Count,
      y = ~factor(Industry, levels = Industry),
      type = "bar",
      orientation = "h",
      marker = list(color = "#3498DB"),
      hovertemplate = "Liczba stanowisk: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Liczba stanowisk"),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_growth_overall <- renderPlotly({
    df <- filtered_data() %>% count(Job_Growth_PL, name = "Count")
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      labels = ~Job_Growth_PL,
      values = ~Count,
      type = "pie",
      hole = 0.45,
      textinfo = "label+percent",
      hovertemplate = "Liczba stanowisk: %{value}<br>Udział: %{percent}<extra></extra>",
      marker = list(colors = c("#E74C3C", "#F39C12", "#2ECC71"))
    ) %>%
      layout(
        showlegend = FALSE,
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_location_salary_overview <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Location) %>%
      summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
      arrange(Avg_Salary)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Avg_Salary,
      y = ~factor(Location, levels = Location),
      type = "bar",
      orientation = "h",
      marker = list(color = "#18BC9C"),
      hovertext = ~paste0("Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Średnie wynagrodzenie (USD)"),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_ai_adoption_overall <- renderPlotly({
    df <- filtered_data() %>% count(AI_Adoption_PL, name = "Count")
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~AI_Adoption_PL,
      y = ~Count,
      type = "bar",
      color = ~AI_Adoption_PL,
      colors = "Set2",
      hovertemplate = "Liczba stanowisk: %{y}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Poziom adopcji AI"),
        yaxis = list(title = "Liczba stanowisk"),
        showlegend = FALSE,
        plot_bgcolor = "#F8FAFC"
      )
  })

  # =======================================================
  # PŁACE
  # =======================================================
  output$plot_salary_ai_box <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(AI_Adoption_PL), !is.na(Salary_USD))
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~AI_Adoption_PL,
      y = ~Salary_USD,
      type = "box",
      color = ~AI_Adoption_PL,
      colors = "Set2",
      boxpoints = "outliers",
      hovertemplate = "Wynagrodzenie: $%{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Poziom adopcji AI"),
        yaxis = list(title = "Wynagrodzenie (USD)"),
        showlegend = FALSE,
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_salary_risk_box <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(Automation_Risk_PL), !is.na(Salary_USD))
    if (nrow(df) == 0) return(empty_plot())

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
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_salary_remote_box <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(Remote_Friendly_PL), !is.na(Salary_USD))
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Remote_Friendly_PL,
      y = ~Salary_USD,
      type = "box",
      color = ~Remote_Friendly_PL,
      colors = "Set2",
      boxpoints = "outliers",
      hovertemplate = "Wynagrodzenie: $%{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Praca zdalna"),
        yaxis = list(title = "Wynagrodzenie (USD)"),
        showlegend = FALSE,
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_salary_company_box <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(Company_Size_PL), !is.na(Salary_USD))
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Company_Size_PL,
      y = ~Salary_USD,
      type = "box",
      color = ~Company_Size_PL,
      colors = "Set2",
      boxpoints = "outliers",
      hovertemplate = "Wynagrodzenie: $%{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Wielkość firmy"),
        yaxis = list(title = "Wynagrodzenie (USD)"),
        showlegend = FALSE,
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_job_salary <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Job_Title) %>%
      summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
      arrange(desc(Avg_Salary)) %>%
      slice_head(n = 10) %>%
      arrange(Avg_Salary)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Avg_Salary,
      y = ~factor(Job_Title, levels = Job_Title),
      type = "bar",
      orientation = "h",
      marker = list(color = "#3498DB"),
      hovertext = ~paste0("Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Średnie wynagrodzenie (USD)"),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_skills_salary <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Required_Skills) %>%
      summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
      arrange(desc(Avg_Salary)) %>%
      slice_head(n = 10) %>%
      arrange(Avg_Salary)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Avg_Salary,
      y = ~factor(Required_Skills, levels = Required_Skills),
      type = "bar",
      orientation = "h",
      marker = list(color = "#9B59B6"),
      hovertext = ~paste0("Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Średnie wynagrodzenie (USD)"),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_industry_salary <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Industry) %>%
      summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
      arrange(Avg_Salary)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Avg_Salary,
      y = ~factor(Industry, levels = Industry),
      type = "bar",
      orientation = "h",
      marker = list(color = "#18BC9C"),
      hovertext = ~paste0("Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Średnie wynagrodzenie (USD)"),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_location_salary <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Location) %>%
      summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
      arrange(Avg_Salary)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Avg_Salary,
      y = ~factor(Location, levels = Location),
      type = "bar",
      orientation = "h",
      marker = list(color = "#F39C12"),
      hovertext = ~paste0("Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Średnie wynagrodzenie (USD)"),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  # =======================================================
  # AI I RYZYKO
  # =======================================================
  output$plot_ai_risk_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(AI_Adoption_PL, Automation_Risk_PL, name = "Count") %>%
      group_by(AI_Adoption_PL) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~AI_Adoption_PL,
      y = ~Share,
      type = "bar",
      color = ~Automation_Risk_PL,
      colors = c("#2ECC71", "#F39C12", "#E74C3C"),
      text = ~pct_fmt(Share),
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Poziom adopcji AI"),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Ryzyko")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_ai_risk_heatmap <- renderPlotly({
    base_df <- filtered_data()
    if (nrow(base_df) == 0) return(empty_plot())

    df_heat <- base_df %>%
      count(AI_Adoption_PL, Automation_Risk_PL, name = "Count") %>%
      complete(AI_Adoption_PL, Automation_Risk_PL, fill = list(Count = 0))

    mat_df <- df_heat %>%
      pivot_wider(names_from = Automation_Risk_PL, values_from = Count, values_fill = 0) %>%
      arrange(AI_Adoption_PL)

    risk_levels <- c("Niskie", "Średnie", "Wysokie")
    mat <- as.matrix(mat_df[, risk_levels, drop = FALSE])

    plot_ly(
      x = risk_levels,
      y = as.character(mat_df$AI_Adoption_PL),
      z = mat,
      type = "heatmap",
      colors = "Purples",
      hovertemplate = "Liczba stanowisk: %{z}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Ryzyko automatyzacji"),
        yaxis = list(title = "Poziom adopcji AI"),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_growth_risk_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Automation_Risk_PL, Job_Growth_PL, name = "Count") %>%
      group_by(Automation_Risk_PL) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Automation_Risk_PL,
      y = ~Share,
      type = "bar",
      color = ~Job_Growth_PL,
      colors = c("#E74C3C", "#F39C12", "#2ECC71"),
      text = ~pct_fmt(Share),
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Ryzyko automatyzacji"),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Prognoza")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_industry_high_risk <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Industry) %>%
      summarise(High_Risk_Share = mean(Automation_Risk == "High", na.rm = TRUE) * 100, Count = n(), .groups = "drop") %>%
      arrange(High_Risk_Share)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~High_Risk_Share,
      y = ~factor(Industry, levels = Industry),
      type = "bar",
      orientation = "h",
      marker = list(color = "#E74C3C"),
      hovertext = ~paste0("Wysokie ryzyko: ", pct_fmt(High_Risk_Share), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Udział wysokiego ryzyka (%)", ticksuffix = "%", range = c(0, 100)),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_industry_ai_risk_trend <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Industry) %>%
      summarise(
        High_AI_Share = mean(AI_Adoption_Level == "High", na.rm = TRUE) * 100,
        High_Risk_Share = mean(Automation_Risk == "High", na.rm = TRUE) * 100,
        Avg_Salary = mean(Salary_USD, na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      )
    if (nrow(df) == 0) return(empty_plot())

    p <- plot_ly(
      df,
      x = ~High_AI_Share,
      y = ~High_Risk_Share,
      type = "scatter",
      mode = "markers+text",
      text = ~Industry,
      textposition = "top center",
      size = ~Count,
      sizes = c(10, 45),
      marker = list(color = "#3498DB", opacity = 0.75, line = list(width = 1, color = "#2C3E50")),
      customdata = ~paste0("Branża: ", Industry, "<br>Wysoka adopcja AI: ", pct_fmt(High_AI_Share), "<br>Wysokie ryzyko: ", pct_fmt(High_Risk_Share), "<br>Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
      hovertemplate = "%{customdata}<extra></extra>"
    )

    if (nrow(df) >= 2 && length(unique(df$High_AI_Share)) > 1) {
      fit <- lm(High_Risk_Share ~ High_AI_Share, data = df)
      line_df <- data.frame(High_AI_Share = seq(min(df$High_AI_Share), max(df$High_AI_Share), length.out = 100))
      line_df$High_Risk_Share <- predict(fit, newdata = line_df)

      p <- p %>% add_trace(
        data = line_df,
        x = ~High_AI_Share,
        y = ~High_Risk_Share,
        type = "scatter",
        mode = "lines",
        name = "Linia trendu",
        line = list(color = "#E74C3C", width = 3),
        inherit = FALSE,
        hovertemplate = "Linia trendu<extra></extra>"
      )
    }

    p %>%
      layout(
        xaxis = list(title = "Udział wysokiej adopcji AI (%)", ticksuffix = "%", range = c(0, 50)),
        yaxis = list(title = "Udział wysokiego ryzyka automatyzacji (%)", ticksuffix = "%"),
        legend = list(orientation = "h"),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_industry_high_ai <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Industry) %>%
      summarise(High_AI_Share = mean(AI_Adoption_Level == "High", na.rm = TRUE) * 100, Count = n(), .groups = "drop") %>%
      arrange(High_AI_Share)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~High_AI_Share,
      y = ~factor(Industry, levels = Industry),
      type = "bar",
      orientation = "h",
      marker = list(color = "#9B59B6"),
      hovertext = ~paste0("Wysoka adopcja AI: ", pct_fmt(High_AI_Share), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Udział wysokiej adopcji AI (%)", ticksuffix = "%", range = c(0, 100)),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  # =======================================================
  # BRANŻE
  # =======================================================
  output$plot_growth_industry_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Industry, Job_Growth_PL, name = "Count") %>%
      group_by(Industry) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Industry,
      y = ~Share,
      type = "bar",
      color = ~Job_Growth_PL,
      colors = c("#E74C3C", "#F39C12", "#2ECC71"),
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Branża", tickangle = -35),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Prognoza")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_ai_industry_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Industry, AI_Adoption_PL, name = "Count") %>%
      group_by(Industry) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Industry,
      y = ~Share,
      type = "bar",
      color = ~AI_Adoption_PL,
      colors = "Set2",
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Branża", tickangle = -35),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Adopcja AI")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_risk_industry_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Industry, Automation_Risk_PL, name = "Count") %>%
      group_by(Industry) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Industry,
      y = ~Share,
      type = "bar",
      color = ~Automation_Risk_PL,
      colors = c("#2ECC71", "#F39C12", "#E74C3C"),
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Branża", tickangle = -35),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Ryzyko")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_remote_industry_share <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Industry) %>%
      summarise(Remote_Share = mean(Remote_Friendly == "Yes", na.rm = TRUE) * 100, Count = n(), .groups = "drop") %>%
      arrange(Remote_Share)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Remote_Share,
      y = ~factor(Industry, levels = Industry),
      type = "bar",
      orientation = "h",
      marker = list(color = "#18BC9C"),
      hovertext = ~paste0("Praca zdalna: ", pct_fmt(Remote_Share), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Udział pracy zdalnej (%)", ticksuffix = "%", range = c(0, 100)),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_growth_industry_grouped <- renderPlotly({
    df <- filtered_data() %>% count(Industry, Job_Growth_PL, name = "Count")
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Industry,
      y = ~Count,
      type = "bar",
      color = ~Job_Growth_PL,
      colors = c("#E74C3C", "#F39C12", "#2ECC71"),
      hovertemplate = "Liczba stanowisk: %{y}<extra></extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Branża", tickangle = -35),
        yaxis = list(title = "Liczba stanowisk"),
        legend = list(title = list(text = "Prognoza")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_industry_salary_n <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Industry) %>%
      summarise(Avg_Salary = mean(Salary_USD, na.rm = TRUE), Count = n(), .groups = "drop") %>%
      arrange(Avg_Salary)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Avg_Salary,
      y = ~factor(Industry, levels = Industry),
      type = "bar",
      orientation = "h",
      marker = list(color = "#3498DB"),
      hovertext = ~paste0("Średnie wynagrodzenie: ", dollar_fmt(Avg_Salary), "<br>Liczba stanowisk: ", Count),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Średnie wynagrodzenie (USD)"),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  # =======================================================
  # FIRMY I REMOTE
  # =======================================================
  output$plot_company_ai_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Company_Size_PL, AI_Adoption_PL, name = "Count") %>%
      group_by(Company_Size_PL) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Company_Size_PL,
      y = ~Share,
      type = "bar",
      color = ~AI_Adoption_PL,
      colors = "Set2",
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Wielkość firmy"),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Adopcja AI")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_company_risk_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Company_Size_PL, Automation_Risk_PL, name = "Count") %>%
      group_by(Company_Size_PL) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Company_Size_PL,
      y = ~Share,
      type = "bar",
      color = ~Automation_Risk_PL,
      colors = c("#2ECC71", "#F39C12", "#E74C3C"),
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Wielkość firmy"),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Ryzyko")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_remote_risk_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Remote_Friendly_PL, Automation_Risk_PL, name = "Count") %>%
      group_by(Remote_Friendly_PL) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Remote_Friendly_PL,
      y = ~Share,
      type = "bar",
      color = ~Automation_Risk_PL,
      colors = c("#2ECC71", "#F39C12", "#E74C3C"),
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Praca zdalna"),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Ryzyko")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_remote_ai_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Remote_Friendly_PL, AI_Adoption_PL, name = "Count") %>%
      group_by(Remote_Friendly_PL) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Remote_Friendly_PL,
      y = ~Share,
      type = "bar",
      color = ~AI_Adoption_PL,
      colors = "Set2",
      customdata = ~Count,
      hovertemplate = "Udział: %{y:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Praca zdalna"),
        yaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        legend = list(title = list(text = "Adopcja AI")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_company_count <- renderPlotly({
    df <- filtered_data() %>% count(Company_Size_PL, name = "Count")
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Company_Size_PL,
      y = ~Count,
      type = "bar",
      color = ~Company_Size_PL,
      colors = "Set2",
      hovertemplate = "Liczba stanowisk: %{y}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Wielkość firmy"),
        yaxis = list(title = "Liczba stanowisk"),
        showlegend = FALSE,
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_remote_overall <- renderPlotly({
    df <- filtered_data() %>% count(Remote_Friendly_PL, name = "Count")
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      labels = ~Remote_Friendly_PL,
      values = ~Count,
      type = "pie",
      hole = 0.45,
      textinfo = "label+percent",
      hovertemplate = "Liczba stanowisk: %{value}<br>Udział: %{percent}<extra></extra>",
      marker = list(colors = c("#E67E22", "#2C3E50"))
    ) %>%
      layout(
        showlegend = FALSE,
        plot_bgcolor = "#F8FAFC"
      )
  })

  # =======================================================
  # KOMPETENCJE
  # =======================================================
  output$plot_skills_risk_heatmap <- renderPlotly({
    base_df <- filtered_data()
    if (nrow(base_df) == 0) return(empty_plot())

    df_heat <- base_df %>%
      count(Required_Skills, Automation_Risk_PL, name = "Count") %>%
      complete(Required_Skills, Automation_Risk_PL, fill = list(Count = 0))

    order_skills <- df_heat %>%
      group_by(Required_Skills) %>%
      summarise(High_Risk = sum(Count[Automation_Risk_PL == "Wysokie"]), Total = sum(Count), .groups = "drop") %>%
      mutate(High_Risk_Share = ifelse(Total > 0, High_Risk / Total, 0)) %>%
      arrange(High_Risk_Share, Total) %>%
      pull(Required_Skills)

    mat_df <- df_heat %>%
      mutate(Required_Skills = factor(Required_Skills, levels = order_skills)) %>%
      arrange(Required_Skills) %>%
      pivot_wider(names_from = Automation_Risk_PL, values_from = Count, values_fill = 0)

    risk_levels <- c("Niskie", "Średnie", "Wysokie")
    mat <- as.matrix(mat_df[, risk_levels, drop = FALSE])

    plot_ly(
      x = risk_levels,
      y = as.character(mat_df$Required_Skills),
      z = mat,
      type = "heatmap",
      colors = "Purples",
      hovertemplate = "Liczba stanowisk: %{z}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Ryzyko automatyzacji"),
        yaxis = list(title = "Wymagana umiejętność"),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_skills_ai_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Required_Skills, AI_Adoption_PL, name = "Count") %>%
      group_by(Required_Skills) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Share,
      y = ~Required_Skills,
      type = "bar",
      orientation = "h",
      color = ~AI_Adoption_PL,
      colors = "Set2",
      customdata = ~Count,
      hovertemplate = "Udział: %{x:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        yaxis = list(title = ""),
        legend = list(title = list(text = "Adopcja AI")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_skills_growth_100 <- renderPlotly({
    df <- filtered_data() %>%
      count(Required_Skills, Job_Growth_PL, name = "Count") %>%
      group_by(Required_Skills) %>%
      mutate(Share = Count / sum(Count) * 100) %>%
      ungroup()
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Share,
      y = ~Required_Skills,
      type = "bar",
      orientation = "h",
      color = ~Job_Growth_PL,
      colors = c("#E74C3C", "#F39C12", "#2ECC71"),
      customdata = ~Count,
      hovertemplate = "Udział: %{x:.1f}%<br>Liczba stanowisk: %{customdata}<extra></extra>"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Udział stanowisk (%)", ticksuffix = "%", range = c(0, 100)),
        yaxis = list(title = ""),
        legend = list(title = list(text = "Prognoza")),
        plot_bgcolor = "#F8FAFC"
      )
  })

  output$plot_skills_count <- renderPlotly({
    df <- filtered_data() %>% count(Required_Skills, name = "Count") %>% arrange(Count)
    if (nrow(df) == 0) return(empty_plot())

    plot_ly(
      df,
      x = ~Count,
      y = ~factor(Required_Skills, levels = Required_Skills),
      type = "bar",
      orientation = "h",
      marker = list(color = "#3498DB"),
      hovertemplate = "Liczba stanowisk: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Liczba stanowisk"),
        yaxis = list(title = ""),
        plot_bgcolor = "#F8FAFC"
      )
  })

  # =======================================================
  # TABELA
  # =======================================================
  output$table_raw_data <- renderDT({
    df <- filtered_data() %>%
      select(
        Job_Title,
        Industry,
        Company_Size,
        Location,
        AI_Adoption_Level,
        Automation_Risk,
        Required_Skills,
        Salary_USD,
        Remote_Friendly,
        Job_Growth_Projection
      )

    datatable(
      df,
      options = list(
        pageLength = 12,
        searchHighlight = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "cell-border stripe hover"
    ) %>%
      formatCurrency("Salary_USD", currency = "$", interval = 3, mark = ",", digits = 0)
  })
}

shinyApp(ui = ui, server = server)