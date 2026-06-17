plot_rating_vs_price_by_country <- function(data, title = "Average Rating and Price by Location of Roaster")
    {
    country_data <- data %>%
        summarise(
            mean_price = mean(cost_per_100g),
            mean_rating = mean(rating),
            .by = loc_country
        )

    g <- country_data %>%
        ggplot(aes(x = mean_price, y = mean_rating, colour = loc_country)) +
        geom_point()+
        theme_minimal()+
        scale_color_ucscgb() +
        labs(
            title = title,
            x = "Cost per 100 grams",
            y = "Rating",
            colour = "Country"
        )
    g

}

