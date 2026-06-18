plot_country_genre_heatmap <- function(data, top_n_countries = 15) {

    top_countries <- data %>%
        count(primary_country, sort = TRUE) %>%
        slice_head(n = top_n_countries) %>%
        pull(primary_country)

    data %>%
        filter(
            primary_country %in% top_countries,
            !is.na(genres_clean),
            !is.na(primary_country)
            ) %>%
        summarise(
            avg_score = mean(imdb_score, na.rm = TRUE),
            .by = c(primary_country, genres_clean)
                    )%>%
  ggplot(aes(x = genres_clean, y = primary_country, fill = avg_score)) +
        geom_tile(colour = "white", linewidth = 0.5) +
        scale_fill_viridis_c(option = "turbo") +
        theme_classic() +
        scale_fill_gradient(low = "red", high = "green", na.value = "grey90")+
        theme(axis.text.x = element_text(angle = 45)) +
        labs(
            title = "Average IMDb Score by Country and Genre",
            x     = "Genre",
            y     = "Country",
            fill  = "Avg IMDb Score"
        )
}




#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +