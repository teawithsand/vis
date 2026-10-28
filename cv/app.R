library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

data <- read.csv("ats_resume_dataset_elite_v3.csv", stringsAsFactors = FALSE)

data <- data %>%
    mutate(
        experience_years = as.numeric(experience_years),
        final_score = as.numeric(final_score),
        skill_match_score = as.numeric(skill_match_score),
        shortlisted = factor(shortlisted, levels = c(0, 1), labels = c("Odrzucony", "Zaakceptowany")),
        education_level = factor(education_level, levels = c("Bachelors", "Masters", "PhD"))
    )

ui <- page_sidebar(
    title = "Jak firmy przegądają CV? Analiza danych z ATS",
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#1F4E79",
        success = "#27AE60"
    ),
    sidebar = sidebar(
        width = 300,
        title = htmltools::tags$h4("Filtry Rekrutacyjne", class = "text-primary"),
        selectInput("role_filter", "Stanowisko docelowe (Job Role):",
            choices = c("Wszystkie" = "All", sort(unique(data$job_role)))
        ),
        selectInput("edu_filter", "Poziom wykształcenia kandydata:",
            choices = c("Wszystkie" = "All", "Bachelors", "Masters", "PhD")
        ),
        htmltools::tags$hr(),
        htmltools::tags$p(class = "text-muted small", "ATS Executive Intelligence • 2026")
    ),
    navset_card_pill(
        nav_panel(
            title = "1. Efektywność Selekcji",
            layout_columns(
                value_box(
                    title = "Liczba Aplikacji",
                    value = textOutput("vbox_total_apps"),
                    showcase = bsicons::bs_icon("people-fill"),
                    theme = "primary"
                ),
                value_box(
                    title = "Średni Wynik Końcowy",
                    value = textOutput("vbox_avg_score"),
                    showcase = bsicons::bs_icon("graph-up"),
                    theme = "info"
                ),
                value_box(
                    title = "Współczynnik Akceptacji",
                    value = textOutput("vbox_accept_rate"),
                    showcase = bsicons::bs_icon("check-circle-fill"),
                    theme = "success"
                ),
                col_widths = c(4, 4, 4)
            ),
            layout_columns(
                card(
                    card_header("Wykres 1: Rozkład Wyniku Końcowego a Decyzja ATS"),
                    card_body(
                        p(class = "text-muted small", "Wykres pudełkowy (Boxplot) pokazujący kwartyle i medianę punktów (Final Score) dla grup zaakceptowanych i odrzuconych."),
                        plotlyOutput("plot_final_score_box")
                    )
                ),
                card(
                    card_header("Wykres 2: Macierz Pozycjonowania Stanowisk (Bubble Chart)"),
                    card_body(
                        p(class = "text-muted small", "Informacja: Pozycja pokazuje relację średniego dopasowania skilli (X) do wyniku końcowego (Y). Wielkość bąbelka to wolumen aplikacji na dane stanowisko."),
                        plotlyOutput("plot_match_bubble")
                    )
                )
            )
        ),
        nav_panel(
            title = "2. Doświadczenie i Edukacja",
            layout_columns(
                card(
                    card_header("Wykres 3: Doświadczenie Kandydata vs Wymagania Stanowiska"),
                    card_body(
                        p(class = "text-muted small", "Analiza porównawcza słupkowa: średnie lata doświadczenia posiadane przez kandydata w zestawieniu z wymaganiami w ofercie."),
                        plotlyOutput("plot_exp_compare")
                    )
                ),
                card(
                    card_header("Wykres 4: Profil Edukacyjny vs Skuteczność ATS (Radar Chart)"),
                    card_body(
                        p(class = "text-muted small", "Informacja: Wykres radarowy pokazujący jak poziom wykształcenia (Bachelors, Masters, PhD) koreluje ze średnim wynikiem końcowym kandydata."),
                        plotlyOutput("plot_edu_radar")
                    )
                )
            )
        ),
        nav_panel(
            title = "3. Statystyki Aplikacji",
            layout_columns(
                card(
                    card_header("Wykres 5: Skuteczność Selekcji na Stanowiskach"),
                    card_body(
                        p(class = "text-muted small", "Horyzontalne zestawienie wolumenu aplikacji na poszczególne stanowiska z podziałem na status akceptacji."),
                        plotlyOutput("plot_role_count")
                    )
                ),
                card(
                    card_header("Wykres 6: Top 10 Najczęstszych Umiejętności u Kandydatów"),
                    card_body(
                        p(class = "text-muted small", "Ranking najpopularniejszych słów kluczowych i kompetencji wyciągniętych dynamicznie z nadesłanych CV."),
                        plotlyOutput("plot_top_skills")
                    )
                )
            )
        ),
        nav_panel(
            title = "4. Analiza Korelacji i Gęstości",
            layout_columns(
                card(
                    card_header("Wykres 7: Zależność Wyniku od Lat Doświadczenia (Trend)"),
                    card_body(
                        p(class = "text-muted small", "Wykres rozrzutu z linią dopasowania. Pozwala sprawdzić, czy kandydaci z większym stażem pracy otrzymują wyższe noty w algorytmie ATS."),
                        plotlyOutput("plot_exp_vs_score")
                    )
                ),
                card(
                    card_header("Wykres 8: Gęstość Dopasowania Umiejętności (Skill Match Density)"),
                    card_body(
                        p(class = "text-muted small", "Wykres liniowy gęstości prawdopodobieństwa (KDE) pokazujący, w którym przedziale punktowym (0-1) najczęściej plasują się aplikanci."),
                        plotlyOutput("plot_skill_density")
                    )
                )
            )
        ),
        nav_panel(
            title = "5. Surowe Dane",
            card(
                card_header("Baza Aplikacji ATS (Zbiór przefiltrowany)"),
                card_body(
                    p(class = "text-muted small", "Poniższa tabela wyświetla dane spełniające kryteria wybranych filtrów bocznych. Możesz wyszukiwać frazy oraz sortować kolumny."),
                    DTOutput("raw_data_table")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    filtered_data <- reactive({
        df <- data

        if (input$role_filter != "All") {
            df <- df %>% filter(job_role == input$role_filter)
        }
        if (input$edu_filter != "All") {
            df <- df %>% filter(education_level == input$edu_filter)
        }

        return(df)
    })

    output$vbox_total_apps <- renderText({
        nrow(filtered_data())
    })

    output$vbox_avg_score <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("0.00")
        }
        round(mean(df$final_score, na.rm = TRUE), 2)
    })

    output$vbox_accept_rate <- renderText({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return("0.0%")
        }
        rate <- mean(df$shortlisted == "Zaakceptowany", na.rm = TRUE) * 100
        paste0(round(rate, 1), "%")
    })

    output$plot_final_score_box <- renderPlotly({
        df <- filtered_data() %>% filter(!is.na(shortlisted))
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        plot_ly(df,
            x = ~shortlisted, y = ~final_score, type = "box",
            color = ~shortlisted, colors = c("#E74C3C", "#2ECC71")
        ) %>%
            layout(
                xaxis = list(title = "Status Decyzji ATS"),
                yaxis = list(title = "Wynik Końcowy (Final Score)"),
                showlegend = FALSE,
                plot_bgcolor = "#F8FAFC"
            )
    })

    output$plot_match_bubble <- renderPlotly({
        df_bubble <- filtered_data() %>%
            group_by(job_role) %>%
            summarise(
                Avg_Skill = mean(skill_match_score, na.rm = TRUE),
                Avg_Final = mean(final_score, na.rm = TRUE),
                Applications = n(),
                .groups = "drop"
            )
        if (nrow(df_bubble) == 0) {
            return(plotly_empty())
        }

        plot_ly(df_bubble,
            x = ~Avg_Skill, y = ~Avg_Final, type = "scatter", mode = "markers",
            size = ~Applications, sizes = c(15, 60), color = ~job_role, colors = "Dark2",
            text = ~ paste("Stanowisko:", job_role, "<br>Liczba aplikacji:", Applications),
            marker = list(opacity = 0.8, line = list(width = 1, color = "#FFFFFF"))
        ) %>%
            layout(
                xaxis = list(title = "Śr. Dopasowanie Umiejętności"),
                yaxis = list(title = "Średni Wynik Końcowy"),
                plot_bgcolor = "#F8FAFC"
            )
    })

    output$plot_exp_compare <- renderPlotly({
        df <- filtered_data() %>%
            group_by(job_role) %>%
            summarise(
                Avg_Candidate = mean(experience_years, na.rm = TRUE),
                Avg_Required = mean(as.numeric(job_experience_required), na.rm = TRUE),
                .groups = "drop"
            )
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        plot_ly(df, x = ~job_role) %>%
            add_bars(y = ~Avg_Candidate, name = "Kandydat", marker = list(color = "#34495E")) %>%
            add_bars(y = ~Avg_Required, name = "Wymagane", marker = list(color = "#3498DB")) %>%
            layout(
                barmode = "group",
                xaxis = list(title = "Rola", tickangle = -30),
                yaxis = list(title = "Średnia liczba lat"),
                plot_bgcolor = "#F8FAFC"
            )
    })

    output$plot_edu_radar <- renderPlotly({
        df_radar <- filtered_data() %>%
            filter(!is.na(education_level)) %>%
            group_by(education_level) %>%
            summarise(Avg_Score = mean(final_score, na.rm = TRUE), .groups = "drop")

        if (nrow(df_radar) == 0) {
            return(plotly_empty())
        }
        df_radar <- rbind(df_radar, df_radar[1, ])

        plot_ly(
            type = "scatterpolar",
            r = df_radar$Avg_Score,
            theta = df_radar$education_level,
            fill = "toself",
            fillcolor = "rgba(52, 152, 219, 0.4)",
            line = list(color = "#2980B9", width = 2)
        ) %>%
            layout(
                polar = list(
                    radialaxis = list(visible = TRUE, range = c(0, max(df_radar$Avg_Score) * 1.1))
                ),
                showlegend = FALSE
            )
    })

    output$plot_role_count <- renderPlotly({
        df <- filtered_data() %>%
            group_by(job_role, shortlisted) %>%
            summarise(Count = n(), .groups = "drop")
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        plot_ly(df,
            x = ~Count, y = ~ reorder(job_role, Count), type = "bar",
            color = ~shortlisted, colors = c("#95A5A6", "#27AE60")
        ) %>%
            layout(
                barmode = "stack",
                xaxis = list(title = "Łączna liczba przesłanych CV"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC"
            )
    })

    output$plot_top_skills <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        all_skills <- unlist(strsplit(df$resume_skills, ",\\s*"))
        all_skills <- all_skills[all_skills != "" & !is.na(all_skills)]

        if (length(all_skills) == 0) {
            return(plotly_empty())
        }

        skills_df <- as.data.frame(table(all_skills), stringsAsFactors = FALSE) %>%
            rename(Skill = all_skills, Freq = Freq) %>%
            arrange(desc(Freq)) %>%
            head(10)

        plot_ly(skills_df,
            x = ~Freq, y = ~ reorder(Skill, Freq), type = "bar",
            marker = list(color = "#1ABC9C")
        ) %>%
            layout(
                xaxis = list(title = "Częstotliwość wystąpienia"),
                yaxis = list(title = ""),
                plot_bgcolor = "#F8FAFC"
            )
    })

    output$plot_exp_vs_score <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0) {
            return(plotly_empty())
        }

        p <- plot_ly(df,
            x = ~experience_years, y = ~final_score, type = "scatter", mode = "markers",
            marker = list(color = "#8E44AD", opacity = 0.6, size = 7),
            text = ~ paste("Rola:", job_role)
        )

        if (nrow(df) > 1) {
            fit <- lm(final_score ~ experience_years, data = df)
            p <- p %>% add_lines(
                x = ~experience_years, y = predict(fit),
                line = list(color = "#2C3E50", width = 2), name = "Linia Trendu"
            )
        }

        p %>% layout(
            xaxis = list(title = "Lata Doświadczenia"),
            yaxis = list(title = "Wynik Końcowy ATS"),
            showlegend = FALSE,
            plot_bgcolor = "#F8FAFC"
        )
    })

    output$plot_skill_density <- renderPlotly({
        df <- filtered_data()
        if (nrow(df) == 0 || all(is.na(df$skill_match_score))) {
            return(plotly_empty())
        }

        dens <- density(df$skill_match_score, na.rm = TRUE)

        plot_ly(
            x = dens$x, y = dens$y, type = "scatter", mode = "lines",
            fill = "tozeroy", fillcolor = "rgba(230, 126, 34, 0.3)",
            line = list(color = "#D35400", width = 2)
        ) %>%
            layout(
                xaxis = list(title = "Wskaźnik Dopasowania Umiejętności (0 - 1)"),
                yaxis = list(title = "Gęstość Prawdopodobieństwa"),
                plot_bgcolor = "#F8FAFC"
            )
    })

    output$raw_data_table <- renderDT({
        df_table <- filtered_data() %>%
            select(
                job_role, education_level, experience_years, job_experience_required,
                skill_match_score, final_score, shortlisted
            )

        datatable(
            df_table,
            colnames = c(
                "Rola", "Wykształcenie", "Doświadczenie (lata)", "Wymagane lata",
                "Dopasowanie Skilli", "Wynik Końcowy", "Decyzja ATS"
            ),
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
