get_surges <- function(national_ranked, threshold = 5000) {
    national_ranked %>%
        arrange(name, gender, year) %>%
        mutate(
            prev_total = lag(total),
            yoy_change = ((total - prev_total) / prev_total) * 100,
            .by = c(name, gender)
        ) %>%
        filter(
            !is.na(yoy_change),
            yoy_change >= threshold)
}

#surges <- get_surges(national_ranked)
#%>%
    #arrange(desc(yoy_change))