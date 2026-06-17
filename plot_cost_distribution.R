plot_cost_distribution <- function(data, title = "Distribution of Price") {

    choc_aroma_coffee <- data %>%
        filter(
            matched_words == "chocolat, aroma"
        )

    g <- choc_aroma_coffee  %>%
        ggplot(aes(x = cost_per_100g)) +
        geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
        theme_minimal() +
        labs(
            title = title,
            x = "Cost per 100g",
            y = "Count"
        )

    g
}