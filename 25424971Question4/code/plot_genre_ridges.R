plot_genre_ridges <- function(data, score_var, title, x_axis) {

data %>%
    mutate(
        genres_clean = fct_reorder(genres_clean, {{ score_var }}, median)
    ) %>%
    ggplot(aes( x = {{ score_var }}, y = genres_clean, fill = genres_clean)) +
    geom_density_ridges(show.legend = FALSE) +
    theme_classic() +
    scale_fill_viridis_d(option = "turbo")+
    labs(
        title = title,
        x = x_axis,
        y = "Genre"
    )

}