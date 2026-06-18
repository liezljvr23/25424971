plot_choc_aroma_scatter <- function(data) {

    choc_aroma_names <- coffee_indicators %>%
        filter(matched_words == "chocolat, aroma") %>%
        distinct(name)

    choc_aroma_coffee <- coffee_filtered %>%
        filter(name %in% choc_aroma_names$name) %>%
        filter(cost_per_100g < 8) %>%
        group_by(roaster) %>%
        filter(n_distinct(roast) > 1) %>%
        ungroup()

   choc_aroma_coffee %>%
        ggplot(aes(x = cost_per_100g, y = rating, colour = roaster)) +
        geom_point(size = 3) +
        theme_minimal() +
        scale_color_brewer(palette = "Paired")+
        labs(
            title = "Price vs Rating for Chocolate and Aroma Coffees",
            x = "Cost per 100g",
            y = "Rating",
            colour = "Roaster"
        )

}


