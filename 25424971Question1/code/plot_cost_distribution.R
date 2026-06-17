plot_cost_distribution <- function(data, cutoff = NULL, title = "Distribution of Price") {

    choc_aroma_names <- coffee_indicators %>%
        filter(matched_words == "chocolat, aroma") %>%
        distinct(name)

    choc_aroma_coffee <- coffee_filtered %>%
        filter(
            name %in% choc_aroma_names$name,
            cost_per_100g < 130
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

    if (!is.null(cutoff)) {
        g <- g + geom_vline(xintercept = cutoff, linetype = "dashed", colour = "red")
    }

    g
}