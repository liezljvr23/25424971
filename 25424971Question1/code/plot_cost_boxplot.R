plot_cost_boxplot <- function(data, title = "Cost per 100g by Country and Roast Level") {


    choc_aroma_names <- coffee_indicators %>%
        filter(matched_words == "chocolat, aroma") %>%
        distinct(name)

    choc_aroma_coffee <- coffee_filtered %>%
        filter(
            name %in% choc_aroma_names$name,
            cost_per_100g < 132)


g <- choc_aroma_coffee %>%
    ggplot(
        aes(x= loc_country, y = cost_per_100g, fill = roast )
    )+
    geom_boxplot(outlier.colour = "red") +
    theme_minimal() +
    labs(
        title = title,
        x = "Country",
        y = "Cost per 100g",
        fill = "Roast"
    )

g

}