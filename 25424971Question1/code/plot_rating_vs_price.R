plot_rating_vs_price <- function(data, group_var,
                                 title = "Average Rating and Price") {
    grouped_data <- data %>%
        summarise(
            mean_price = mean(cost_per_100g),
            mean_rating = mean(rating),
            .by = {{ group_var }}
        )

    g <- grouped_data %>%
        ggplot(aes(x = mean_price, y = mean_rating, colour = {{ group_var }})) +
        geom_point() +
        theme_minimal() +
        scale_color_ucscgb() +
        labs(
            title = title,
            x = "Cost per 100 grams",
            y = "Rating",
            colour = deparse(substitute(group_var))
        )
    g
}
