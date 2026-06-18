plot_choc_aroma_tile <- function(data){

    choc_aroma_names <- coffee_indicators %>%
        filter(matched_words == "chocolat, aroma") %>%
        distinct(name)

    choc_aroma_coffee <- coffee_filtered %>%
        filter(
            name %in% choc_aroma_names$name)

    choc_aroma_coffee <- choc_aroma_coffee %>%
        filter(
            cost_per_100g < 8
        ) %>%
        group_by(roaster) %>%
        filter(n_distinct(roast) > 1) %>%
        ungroup()



    midpoint <- choc_aroma_coffee %>%
        summarise(
            mid = (max(rating) + min(rating))/2) %>%
        pull(mid) # trial and error: have to pull to get actual number value

   g <- choc_aroma_coffee %>%
        summarise(
            mean_rating = mean(rating),
            .by = c(roaster, roast)
        ) %>%
        ggplot(aes(x = roast, y = roaster, fill = mean_rating)) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient2(
            low = "red",
            mid = "lightyellow",
            high = "darkgreen",
            midpoint = midpoint
        ) +
       theme_minimal() +
       theme(
           axis.text.y = element_text(size = 6)

       ) +
        labs(
            title = "Average Rating by Roaster and Roast Level",
            x = "Roast Level",
            y = "Roaster",
            fill = "Average Rating"
        )
   g
}



