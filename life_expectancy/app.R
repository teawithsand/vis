library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(highcharter)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(janitor)
library(viridis)
library(DT)
library(readr)
library(forcats)

life_exp <- readr::read_csv(
    "life_expectancy_by_country_pl_columns.csv",
    show_col_types = FALSE
) %>%
    clean_names() %>%
    mutate(
        continent = factor(continent),
        country = as.factor(country),
        continent_pl = factor(continent_pl),
        country_pl = as.factor(country_pl),
        gender_gap = if_else(
            !is.na(life_expectancy_female) & !is.na(life_expectancy_male),
            life_expectancy_female - life_expectancy_male,
            NA_real_
        ),
        rank = if_else(
            !is.na(life_expectancy_overall),
            min_rank(desc(life_expectancy_overall)),
            NA_integer_
        )
    )

base_theme <- theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(size = 14, face = "bold", margin = margin(b = 8)),
        plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
        plot.margin = margin(15, 15, 15, 15),
        axis.title = element_text(margin = margin(t = 8, r = 8)),
        axis.text = element_text(color = "grey20"),
        panel.background = element_rect(fill = "#F8FBFF", color = NA),
        plot.background = element_rect(fill = "#F8FBFF", color = NA)
    )

continent_levels <- levels(life_exp$continent_pl)
set2_palette <- if (length(continent_levels) <= 8) {
    RColorBrewer::brewer.pal(max(3, length(continent_levels)), "Set2")[seq_along(continent_levels)]
} else {
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(continent_levels))
}
continent_colors <- setNames(set2_palette, continent_levels)

# Podsumowanie po kontynentach (do wykresów słupkowych / highcharter)
life_exp_summary_cont <- life_exp %>%
    group_by(continent_pl) %>%
    summarise(
        mean_life = mean(life_expectancy_overall, na.rm = TRUE),
        se_life = sd(life_expectancy_overall, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
    )

life_exp_continent_gender <- life_exp %>%
    group_by(continent_pl) %>%
    summarise(
        male = mean(life_expectancy_male, na.rm = TRUE),
        female = mean(life_expectancy_female, na.rm = TRUE),
        .groups = "drop"
    )

# Dane przestrzenne
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

life_exp_join <- life_exp %>%
    mutate(
        country_join = case_when(
            country == "United States" ~ "United States of America",
            country == "Russia" ~ "Russian Federation",
            country == "Congo (DRC)" ~ "Democratic Republic of the Congo",
            country == "Congo (Republic)" ~ "Republic of the Congo",
            country == "Ivory Coast" ~ "Cote d'Ivoire",
            country == "Bahamas" ~ "The Bahamas",
            TRUE ~ as.character(country)
        )
    )

world_sf_join <- world_sf %>%
    mutate(country_join = name) %>%
    left_join(life_exp_join, by = "country_join") %>%
    mutate(
        continent_join = dplyr::coalesce(
            as.character(continent.y),
            as.character(continent.x)
        )
    )

world_for_plotly <- world_sf_join %>%
    st_drop_geometry() %>%
    filter(
        !is.na(life_expectancy_overall),
        !is.na(iso_a3),
        iso_a3 != ""
    ) %>%
    distinct(iso_a3, .keep_all = TRUE)

ui <- page_sidebar(
    title = "Dashboard długości życia",
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#d9b4c1",
        secondary = "#cfa2af",
        bg = "#F8FBFF",
        fg = "#8f556e"
    ),
    sidebar = sidebar(
        width = 260,
        bg = "#fffdff8b",
        fg = "#98483a",
        border = FALSE,
        selectInput(
            "continent",
            "Kontynent",
            choices = c("Wszystkie", sort(unique(as.character(life_exp$continent_pl)))),
            selected = "Wszystkie"
        ),
        sliderInput(
            "life_range",
            "Zakres długości życia (ogółem)",
            min = floor(min(life_exp$life_expectancy_overall, na.rm = TRUE)),
            max = ceiling(max(life_exp$life_expectancy_overall, na.rm = TRUE)),
            value = c(
                floor(min(life_exp$life_expectancy_overall, na.rm = TRUE)),
                ceiling(max(life_exp$life_expectancy_overall, na.rm = TRUE))
            ),
            step = 1
        )
    ),
    tags$head(
        tags$style(HTML(paste0(
            "#life_range .irs-bar,\n",
            "#life_range .irs-bar-edge {\n",
            "  background-color: #8D6E63;\n",
            "  border-color: #8D6E63;\n",
            "}\n\n",
            "#life_range .irs-line {\n",
            "  background-color: #f5f0eb;\n",
            "  border-color: #f5f0eb;\n",
            "}\n\n",
            "#life_range .irs-handle>i:first-child {\n",
            "  background-color: #8D6E63;\n",
            "  border-color: #5D4037;\n",
            "}\n\n",
            "#life_range .irs-single {\n",
            "  background-color: #8D6E63;\n",
            "}\n",
            ".nav-tabs .nav-link,\n",
            ".nav-tabs .nav-link:hover,\n",
            ".nav-tabs .nav-link:focus {\n",
            "  color: #714e55;\n",
            "}\n\n",
            ".nav-tabs .nav-link.active,\n",
            ".nav-tabs .nav-item.show .nav-link {\n",
            "  color: #603a50;\n",
            "  background-color: #edd3e1;\n",
            "  border-color: #d9b4c1 #d9b4c1 #edd3e1;\n",
            "}\n"
        )))
    ),
    layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
            title = "Średnia długość życia (ogółem)",
            value = textOutput("avg_life"),
            theme_color = "#9AD3BC"
        ),
        value_box(
            title = "Średnia luka płciowa",
            value = textOutput("avg_gap"),
            theme_color = "#F6C1D1"
        ),
        value_box(
            title = "Liczba krajów",
            value = textOutput("n_countries"),
            theme_color = "#FFD9A5"
        )
    ),
    navset_card_tab(
        nav_panel(
            "Rozkład",
            layout_columns(
                col_widths = c(7, 5),
                card(
                    full_screen = TRUE,
                    height = 420,
                    card_header("Histogram długości życia (ogółem)"),
                    card_body_fill(
                        plotlyOutput("hist_overall", height = "100%", width = "100%")
                    )
                ),
                card(
                    full_screen = TRUE,
                    card_header("Średnia długość życia wg kontynentów"),
                    plotOutput("density_overall", height = "420px")
                )
            )
        ),
        nav_panel(
            "Płeć",
            layout_columns(
                col_widths = c(7, 5),
                card(
                    full_screen = TRUE,
                    card_header("Porównanie długości życia: mężczyźni vs kobiety"),
                    plotlyOutput("box_gender", height = "430px")
                ),
                card(
                    full_screen = TRUE,
                    card_header("Średnia długość życia wg kontynentów – mężczyźni vs kobiety"),
                    highchartOutput("hc_continent_gender", height = "430px")
                )
            )
        ),
        nav_panel(
            "Rankingi",
            layout_columns(
                col_widths = c(7, 5),
                card(
                    full_screen = TRUE,
                    card_header("20 krajów z największą luką płciową"),
                    plotlyOutput("top20_plot", height = "520px")
                ),
                card(
                    full_screen = TRUE,
                    card_header("20 krajów z najmniejszą luką płciową"),
                    plotlyOutput("bottom20_plot", height = "520px")
                )
            )
        ),
        nav_panel(
            "Mapy",
            layout_columns(
                col_widths = c(7, 5),
                card(
                    full_screen = TRUE,
                    card_header("Mapa Europy – luka płciowa"),
                    plotOutput("map_static", height = "540px")
                ),
                card(
                    full_screen = TRUE,
                    card_header("Interaktywna mapa – długość życia"),
                    plotlyOutput("map_plotly", height = "480px")
                )
            )
        ),
        nav_panel(
            "Relacje",
            layout_columns(
                col_widths = c(7, 5),
                card(
                    full_screen = TRUE,
                    card_header("Zależność: długość życia a luka płciowa"),
                    plotlyOutput("scatter_gap_vs_life", height = "460px")
                ),
                card(
                    full_screen = TRUE,
                    card_header("Zróżnicowanie długości życia w kontynentach"),
                    plotOutput("violin_continent", height = "460px")
                )
            )
        ),
        nav_panel(
            "Dane",
            card(
                full_screen = TRUE,
                card_header("Przefiltrowane dane life_expectancy"),
                DTOutput("table_life")
            )
        )
    )
)

server <- function(input, output, session) {
    filtered_data <- reactive({
        df <- life_exp

        if (input$continent != "Wszystkie") {
            df <- df %>% filter(as.character(continent_pl) == input$continent)
        }

        df <- df %>%
            filter(
                !is.na(life_expectancy_overall),
                life_expectancy_overall >= input$life_range[1],
                life_expectancy_overall <= input$life_range[2]
            )

        df
    })

    output$avg_life <- renderText({
        round(mean(filtered_data()$life_expectancy_overall, na.rm = TRUE), 1)
    })

    output$avg_gap <- renderText({
        paste0(round(mean(filtered_data()$gender_gap, na.rm = TRUE), 2), " roku")
    })

    output$n_countries <- renderText({
        dplyr::n_distinct(filtered_data()$country)
    })

    histogram_bins <- reactive({
        df <- filtered_data() %>%
            filter(!is.na(life_expectancy_overall))

        req(nrow(df) > 0)

        binwidth <- 2
        xmin_all <- floor(min(df$life_expectancy_overall, na.rm = TRUE))
        xmax_all <- ceiling(max(df$life_expectancy_overall, na.rm = TRUE))

        breaks <- seq(xmin_all, xmax_all + binwidth, by = binwidth)

        h <- hist(
            df$life_expectancy_overall,
            breaks = breaks,
            plot = FALSE,
            right = FALSE
        )

        data.frame(
            xmin = h$breaks[-length(h$breaks)],
            xmax = h$breaks[-1],
            n = h$counts,
            stringsAsFactors = FALSE
        ) %>%
            mutate(
                xmid = (xmin + xmax) / 2
            )
    })

    # Histogram średniej długości życia w krajach
    output$hist_overall <- renderPlotly({
        bins <- histogram_bins()

        plot_ly(
            data = bins,
            x = ~xmid,
            y = ~n,
            type = "bar",
            customdata = ~ paste0(xmin, "–", xmax),
            hovertemplate = paste0(
                "Przedział: %{customdata}<br>",
                "Liczba krajów: %{y}<extra></extra>"
            ),
            hoverlabel = list(
                bgcolor = "rgba(255, 255, 255, 0.49)",
                bordercolor = "#ffd725",
                font = list(color = "#111111", size = 12)
            ),
            marker = list(
                color = "#ffd725",
                line = list(color = "white", width = 1)
            )
        ) %>%
            layout(
                autosize = TRUE,
                xaxis = list(
                    title = "Długość życia (lata)",
                    tickmode = "linear",
                    dtick = 4
                ),
                yaxis = list(title = "Liczba krajów"),
                bargap = 0.05,
                margin = list(l = 50, r = 20, t = 10, b = 50),
                paper_bgcolor = "#F8FBFF",
                plot_bgcolor = "#F8FBFF"
            ) %>%
            config(
                responsive = TRUE,
                displayModeBar = FALSE
            )
    })

    # Średnia długość życia (ogółem) wg kontynentów
    output$density_overall <- renderPlot({
        df <- filtered_data() %>%
            group_by(continent_pl) %>%
            summarise(
                mean_life = mean(life_expectancy_overall, na.rm = TRUE),
                .groups = "drop"
            )

        ggplot(df, aes(x = continent_pl, y = mean_life, fill = continent_pl)) +
            geom_col(alpha = 0.9, width = 0.7) +
            scale_fill_brewer(palette = "Set2") +
            labs(
                x = "Kontynent",
                y = "Średnia długość życia (lata)",
                fill = "Kontynent"
            ) +
            base_theme
    })

    # Boxplot długości życia kobiet i mężczyzn
    output$box_gender <- renderPlotly({
        life_exp_long_gender <- filtered_data() %>%
            select(country, continent, life_expectancy_male, life_expectancy_female) %>%
            pivot_longer(
                cols = starts_with("life_expectancy_"),
                names_to = "sex",
                values_to = "life_expectancy"
            ) %>%
            mutate(
                sex = recode(
                    sex,
                    "life_expectancy_male" = "Mężczyźni",
                    "life_expectancy_female" = "Kobiety"
                )
            ) %>%
            filter(!is.na(life_expectancy))

        stats_gender <- life_exp_long_gender %>%
            group_by(sex) %>%
            summarise(
                n = n(),
                min_val = min(life_expectancy, na.rm = TRUE),
                q1 = quantile(life_expectancy, 0.25, na.rm = TRUE),
                median_val = median(life_expectancy, na.rm = TRUE),
                q3 = quantile(life_expectancy, 0.75, na.rm = TRUE),
                max_val = max(life_expectancy, na.rm = TRUE),
                mean_val = mean(life_expectancy, na.rm = TRUE),
                .groups = "drop"
            )

        male_stats <- stats_gender %>% filter(sex == "Mężczyźni")
        female_stats <- stats_gender %>% filter(sex == "Kobiety")

        male_hover <- paste0(
            "Płeć: Mężczyźni",
            "<br>Liczba krajów: ", male_stats$n,
            "<br>Średnia: ", round(male_stats$mean_val, 2),
            "<br>Minimum: ", round(male_stats$min_val, 2),
            "<br>Q1: ", round(male_stats$q1, 2),
            "<br>Mediana: ", round(male_stats$median_val, 2),
            "<br>Q3: ", round(male_stats$q3, 2),
            "<br>Maksimum: ", round(male_stats$max_val, 2),
            "<extra></extra>"
        )

        female_hover <- paste0(
            "Płeć: Kobiety",
            "<br>Liczba krajów: ", female_stats$n,
            "<br>Średnia: ", round(female_stats$mean_val, 2),
            "<br>Minimum: ", round(female_stats$min_val, 2),
            "<br>Q1: ", round(female_stats$q1, 2),
            "<br>Mediana: ", round(female_stats$median_val, 2),
            "<br>Q3: ", round(female_stats$q3, 2),
            "<br>Maksimum: ", round(female_stats$max_val, 2),
            "<extra></extra>"
        )

        plot_ly() %>%
            add_trace(
                data = life_exp_long_gender %>% filter(sex == "Mężczyźni"),
                x = ~sex,
                y = ~life_expectancy,
                type = "box",
                name = "Mężczyźni",
                boxpoints = FALSE,
                width = 0.56,
                fillcolor = "rgba(16, 93, 200, 0.75)",
                line = list(color = "#105dc8", width = 1.5),
                hovertemplate = male_hover,
                hoverlabel = list(
                    bgcolor = "rgba(255, 255, 255, 0.49)",
                    bordercolor = "#105dc8",
                    font = list(color = "#111111", size = 12)
                ),
                showlegend = TRUE
            ) %>%
            add_trace(
                data = life_exp_long_gender %>% filter(sex == "Kobiety"),
                x = ~sex,
                y = ~life_expectancy,
                type = "box",
                name = "Kobiety",
                boxpoints = FALSE,
                width = 0.56,
                fillcolor = "rgba(226, 97, 217, 0.75)",
                line = list(color = "#e261d9", width = 1.5),
                hovertemplate = female_hover,
                hoverlabel = list(
                    bgcolor = "rgba(255, 255, 255, 0.49)",
                    bordercolor = "#e261d9",
                    font = list(color = "#111111", size = 12)
                ),
                showlegend = TRUE
            ) %>%
            layout(
                boxmode = "group",
                boxgap = 0.02,
                boxgroupgap = 0,
                xaxis = list(
                    title = "Płeć",
                    categoryorder = "array",
                    categoryarray = c("Mężczyźni", "Kobiety"),
                    showline = FALSE,
                    zeroline = FALSE
                ),
                yaxis = list(
                    title = "Długość życia (lata)",
                    showline = FALSE,
                    zeroline = FALSE
                ),
                legend = list(
                    title = list(text = "Płeć")
                ),
                paper_bgcolor = "#F8FBFF",
                plot_bgcolor = "#F8FBFF",
                margin = list(l = 60, r = 20, t = 20, b = 50)
            ) %>%
            config(
                responsive = TRUE,
                displayModeBar = FALSE
            )
    })

    # Średnie długości życia kobiet i mężczyzn wg kontynentów
    output$hc_continent_gender <- renderHighchart({
        df <- filtered_data() %>%
            group_by(continent_pl) %>%
            summarise(
                male = mean(life_expectancy_male, na.rm = TRUE),
                female = mean(life_expectancy_female, na.rm = TRUE),
                .groups = "drop"
            )

        highchart() %>%
            hc_chart(
                type = "column",
                backgroundColor = "#F8FBFF",
                spacingTop = 20,
                spacingBottom = 20,
                spacingLeft = 10,
                spacingRight = 10
            ) %>%
            hc_xAxis(categories = df$continent_pl) %>%
            hc_yAxis(title = list(text = "Długość życia (lata)")) %>%
            hc_add_series(
                name = "Mężczyźni",
                data = df$male,
                color = "#105dc8"
            ) %>%
            hc_add_series(
                name = "Kobiety",
                data = df$female,
                color = "#e261d9"
            ) %>%
            hc_tooltip(shared = TRUE, valueDecimals = 1) %>%
            hc_plotOptions(
                column = list(
                    grouping = TRUE,
                    dataLabels = list(enabled = FALSE),
                    borderWidth = 0,
                    pointPadding = 0.1
                )
            ) %>%
            hc_exporting(enabled = TRUE)
    })

    # TOP 20 krajów z największą luką płciową
    output$top20_plot <- renderPlotly({
        df_top <- filtered_data() %>%
            filter(!is.na(gender_gap)) %>%
            mutate(
                gender_gap = as.numeric(gender_gap),
                country_label = forcats::fct_reorder(country_pl, gender_gap),
                continent_label = as.character(continent_pl)
            ) %>%
            arrange(desc(gender_gap)) %>%
            slice_head(n = 20)

        plot_ly(
            data = df_top,
            x = ~gender_gap,
            y = ~country_label,
            type = "bar",
            orientation = "h",
            color = ~continent_label,
            colors = continent_colors,
            hovertemplate = ~ paste0(
                "<b>Kraj:</b> ", country_pl,
                "<br><b>Kontynent:</b> ", continent_pl,
                "<br><b>Średnia długość życia:</b> ",
                round(life_expectancy_overall, 2),
                "<br><b>Luka płciowa:</b> ",
                round(gender_gap, 2),
                " lat",
                "<extra></extra>"
            ),
            marker = list(
                line = list(
                    color = "rgba(255,255,255,0.9)",
                    width = 1
                )
            ),

            # TO DAJE LEPSZY HOVER
            selected = list(
                marker = list(
                    opacity = 1,
                    line = list(
                        color = "#111111",
                        width = 2
                    )
                )
            ),
            unselected = list(
                marker = list(
                    opacity = 0.45
                )
            )
        ) %>%
            layout(
                hovermode = "closest",
                xaxis = list(
                    title = "Luka płciowa (lata)",
                    automargin = TRUE
                ),
                yaxis = list(
                    title = list(
                        text = "Kraj",
                        standoff = 20
                    ),
                    automargin = TRUE,
                    tickmode = "linear",
                    dtick = 1
                ),
                legend = list(
                    title = list(text = "Kontynent")
                ),
                hoverlabel = list(
                    bgcolor = "white",
                    bordercolor = "#333333",
                    font = list(
                        color = "#111111",
                        size = 12
                    )
                ),
                margin = list(
                    l = 210,
                    r = 40,
                    t = 40,
                    b = 60
                ),
                bargap = 0.35,
                paper_bgcolor = "#F8FBFF",
                plot_bgcolor = "#F8FBFF"
            ) %>%
            config(
                responsive = TRUE,
                displayModeBar = FALSE
            )
    })

    # TOP 20 krajów z najmniejszą luką płciową
    output$bottom20_plot <- renderPlotly({
        df_bottom <- filtered_data() %>%
            filter(!is.na(gender_gap)) %>%
            mutate(
                gender_gap = as.numeric(gender_gap),
                country_label = forcats::fct_reorder(country_pl, gender_gap),
                continent_label = as.character(continent_pl)
            ) %>%
            arrange(gender_gap) %>%
            slice_head(n = 20)

        plot_ly(
            data = df_bottom,
            x = ~gender_gap,
            y = ~country_label,
            type = "bar",
            orientation = "h",
            color = ~continent_label,
            colors = continent_colors,
            hovertemplate = ~ paste0(
                "<b>Kraj:</b> ", country_pl,
                "<br><b>Kontynent:</b> ", continent_pl,
                "<br><b>Średnia długość życia:</b> ",
                round(life_expectancy_overall, 2),
                "<br><b>Luka płciowa:</b> ",
                round(gender_gap, 2),
                " lat",
                "<extra></extra>"
            )
        ) %>%
            layout(
                xaxis = list(title = "Luka płciowa (lata)", automargin = TRUE),
                yaxis = list(
                    title = list(text = "Kraj", standoff = 20),
                    automargin = TRUE,
                    tickmode = "linear",
                    dtick = 1
                ),
                legend = list(title = list(text = "Kontynent")),
                hoverlabel = list(
                    bgcolor = "white",
                    bordercolor = "#333333",
                    font = list(color = "#111111", size = 12)
                ),
                margin = list(l = 210, r = 40, t = 40, b = 60),
                bargap = 0.35,
                paper_bgcolor = "#F8FBFF",
                plot_bgcolor = "#F8FBFF"
            )
    })
    # Statyczna mapa Europy – luka płciowa w długości życia
    output$map_static <- renderPlot(
        {
            world_filtered <- world_sf_join

            # Pokazujemy tylko Europę, niezależnie od wybranego filtra kontynentu
            world_filtered <- world_filtered %>%
                filter(as.character(continent_join) == "Europe")

            ggplot(world_filtered) +
                geom_sf(
                    aes(fill = gender_gap),
                    color = "white",
                    linewidth = 0.15
                ) +
                scale_fill_viridis_c(
                    option = "plasma",
                    na.value = "grey92",
                    name = "Luka płciowa\n(lata)"
                ) +
                coord_sf(
                    xlim = c(-15, 35),
                    ylim = c(34, 72),
                    expand = FALSE
                ) +
                labs(
                    x = NULL,
                    y = NULL
                ) +
                theme_minimal(base_size = 13) +
                theme(
                    legend.position = "right",
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank(),
                    panel.background = element_rect(fill = "#F8FBFF", color = NA),
                    plot.background = element_rect(fill = "#F8FBFF", color = NA),
                    plot.margin = margin(5, 5, 5, 5)
                )
        },
        bg = "#F8FBFF"
    )

    # Interaktywna mapa świata – średnia długość życia w krajach
    output$map_plotly <- renderPlotly({
        df <- world_for_plotly

        if (input$continent != "Wszystkie") {
            df <- df %>% filter(as.character(continent_pl) == input$continent)
        }

        plot_ly(
            data = df,
            type = "choropleth",
            locations = ~iso_a3,
            locationmode = "ISO-3",
            z = ~life_expectancy_overall,
            text = ~ paste(
                "Kraj:", country_pl,
                "<br>Kontynent:", continent_pl,
                "<br>Długość życia:", round(life_expectancy_overall, 2)
            ),
            colorscale = "Viridis",
            marker = list(
                line = list(color = "white", width = 0.3)
            ),
            colorbar = list(
                title = list(text = "Długość życia (lata)"),
                ticksuffix = " lat",
                thickness = 15
            )
        ) %>%
            layout(
                geo = list(
                    bgcolor = "#F8FBFF",
                    showframe = FALSE,
                    showcoastlines = TRUE,
                    projection = list(type = "natural earth")
                ),
                margin = list(l = 0, r = 0, t = 40, b = 0),
                paper_bgcolor = "#F8FBFF",
                plot_bgcolor = "#F8FBFF"
            )
    })

    # Zależność między długością życia ogółem a luką płciową
    output$scatter_gap_vs_life <- renderPlotly({
        df <- filtered_data() %>%
            filter(
                !is.na(life_expectancy_overall),
                !is.na(gender_gap),
                !is.na(continent_pl),
                !is.na(country_pl)
            ) %>%
            mutate(continent_label = as.character(continent_pl))

        model <- lm(gender_gap ~ life_expectancy_overall, data = df)

        line_df <- data.frame(
            life_expectancy_overall = seq(
                min(df$life_expectancy_overall, na.rm = TRUE),
                max(df$life_expectancy_overall, na.rm = TRUE),
                length.out = 100
            )
        )

        line_df$gender_gap <- predict(model, newdata = line_df)

        plot_ly(
            data = df,
            x = ~life_expectancy_overall,
            y = ~gender_gap,
            type = "scatter",
            mode = "markers",
            color = ~continent_label,
            colors = c(
                "Afryka" = "#E9C46A",
                "Ameryki" = "#F4A261",
                "Azja" = "#A8DADC",
                "Europa" = "#EF709D",
                "Oceania" = "#7180AC"
            ),
            text = ~ paste(
                "Kraj:", country_pl,
                "<br>Kontynent:", continent_pl,
                "<br>Długość życia (ogółem):", round(life_expectancy_overall, 2),
                "<br>Luka płciowa:", round(gender_gap, 2)
            ),
            hovertemplate = "%{text}<extra></extra>",
            marker = list(size = 9, opacity = 0.8)
        ) %>%
            add_lines(
                data = line_df,
                x = ~life_expectancy_overall,
                y = ~gender_gap,
                inherit = FALSE,
                line = list(color = "#9AD3BC", width = 2),
                hoverinfo = "skip",
                showlegend = FALSE
            ) %>%
            layout(
                xaxis = list(title = "Długość życia (ogółem)"),
                yaxis = list(title = "Luka płciowa"),
                legend = list(title = list(text = "Kontynent")),
                paper_bgcolor = "#F8FBFF",
                plot_bgcolor = "#F8FBFF",
                margin = list(l = 60, r = 20, t = 20, b = 60)
            ) %>%
            config(
                responsive = TRUE,
                displayModeBar = FALSE
            )
    })

    # Rozkład długości życia w kontynentach
    output$violin_continent <- renderPlot({
        df <- filtered_data() %>%
            filter(
                !is.na(life_expectancy_overall),
                !is.na(continent_pl)
            )

        ggplot(df, aes(
            x = continent_pl,
            y = life_expectancy_overall,
            fill = continent_pl
        )) +
            geom_violin(alpha = 0.75, trim = FALSE, color = NA) +
            geom_boxplot(
                width = 0.14,
                fill = "white",
                color = "grey25",
                outlier.alpha = 0.25
            ) +
            scale_fill_brewer(palette = "Pastel2") +
            labs(
                x = "Kontynent",
                y = "Długość życia (lata)",
                fill = "Kontynent"
            ) +
            base_theme +
            theme(legend.position = "none")
    })

    # Tabela z przefiltrowanymi danymi
    output$table_life <- renderDT({
        datatable(
            filtered_data() %>%
                transmute(
                    Kraj = country_pl,
                    Kontynent = continent_pl,
                    `Długość życia (ogółem)` = life_expectancy_overall,
                    `Długość życia kobiet` = life_expectancy_female,
                    `Długość życia mężczyzn` = life_expectancy_male,
                    `Luka płciowa` = gender_gap,
                    Ranking = rank
                ),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })
}

shinyApp(ui = ui, server = server)
