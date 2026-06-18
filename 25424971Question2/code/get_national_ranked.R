get_national_ranked <- function(baby_names) {
    baby_names %>%
        summarise(
            total = sum(count),
            .by = c(name, year, gender)
        ) %>%
        mutate(
            rank = rank(-total),
            .by = c(year, gender)
        )
}