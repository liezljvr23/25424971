get_spearman_results <- function(top_25_names, national_ranked) {
    expand_grid(
        year   = 1910:2011,
        lag    = 1:3,
        gender = c("F", "M")
    ) %>%
        mutate(
            correlation = pmap_dbl(
            list(year, lag, gender),
            compute_spearman_lag,
            top_25_names = top_25_names,
            national_ranked = national_ranked
        ))
}
