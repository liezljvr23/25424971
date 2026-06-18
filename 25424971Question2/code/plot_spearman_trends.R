plot_spearman_trends <- function(spearman_results) {

    result_labels <- spearman_results %>%
        mutate(
            lag_label    = factor(lag),
            gender_label = if_else(gender == "F", "Girls", "Boys")
        )

    g <- result_labels %>%
        ggplot(aes(x = year, y = correlation, colour = lag_label)) +
        geom_line() +
        facet_wrap(~gender_label) +
        scale_color_brewer(palette = "Paired") +
        labs(
            title    = "Time Series Representation of the Top 25 Rank-Correlation",
            x        = "Base Year",
            y        = "Spearman Rank Correlation",
            colour   = "Lag",
            caption  = "Source: US Baby Names 1910–2014"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
}

