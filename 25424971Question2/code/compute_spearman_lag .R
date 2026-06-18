compute_spearman_lag <- function(base_year, lag, gender_filter, top_25_names, national_ranked) {

    base <- top_25_names %>%
        filter(
            year == base_year,
            gender == gender_filter) %>%
        select(name,
               rank_base = rank)

    future <- national_ranked %>%
        filter(
            year == base_year + lag,
            gender == gender_filter) %>%
        select(name,
               rank_future = rank)
 # put base and future together
   compare <- base %>%
        left_join(future, by = "name") %>%
        summarise(
            correlation = cor(rank_base, rank_future, method = "spearman", use = "complete.obs")) %>%
        pull(correlation)
}

