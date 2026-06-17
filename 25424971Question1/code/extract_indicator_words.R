extract_indicator_words <- function(data, indicator_words = c("sweet", "chocolat", "aroma")) {
    # make longer
    data %>%
        pivot_longer(
            cols = desc_1:desc_3,
            names_to = "review_number",
            values_to = "review"
        ) %>%
        mutate(
            matched_words = map_chr(review, ~ {
                indicator_words[str_detect(.x, regex(indicator_words, ignore_case = TRUE))] %>%
                    paste(collapse = ", ")
            })
        ) %>%
        filter(matched_words != "")
}

