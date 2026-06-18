clean_movies <- function(data, max_year = 2022) {

    data %>%
        filter(type == "MOVIE",
               release_year <= max_year) %>%
        mutate(
            primary_country = str_extract(production_countries, "'([^']+)'", group = 1),
            genres_clean    = str_replace_all(genres, "\\[|\\]|'", "") %>% str_trim()
        ) %>%
        select(!c(genres, production_countries)) %>%
        mutate(primary_genre = str_extract(genres_clean, "^[^,]+") %>% str_trim())
}


