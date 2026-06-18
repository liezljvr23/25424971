get_top_25_names <- function(national_ranked, n = 25) {
    national_ranked %>%
        filter(rank <= n)
}