make_movies_long <- function(data) {

data %>%
    separate_rows(genres_clean, sep = ",") %>%
    mutate(
        genres_clean = str_trim(genres_clean)
        )  %>%
        filter(
            genres_clean != "") %>%
        select(!primary_genre)

}