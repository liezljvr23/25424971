table_dti_hardcap <- function(data) {

    table_data <- data %>%
        mutate(dti_bin = cut(dti,
                             breaks = seq(0, 50, by = 5),
                             labels = c("0-5", "5-10", "10-15", "15-20",
                                        "20-25", "25-30", "30-35", "35-40",
                                        "40-45", "45-50"))) %>%
        filter(!is.na(dti_bin)) %>%
        summarise(
            default_rate = mean(concluded_loan),
            n            = n(),
            .by          = dti_bin
        ) %>%
        arrange(dti_bin) %>%
        mutate(default_rate = paste0(round(default_rate * 100, 1), "%")) %>%
        rename(
            `DTI Band`     = dti_bin,
            `Default Rate` = default_rate,
            `No. of Loans` = n
        ) %>%
        as.data.frame()

    table <- xtable(table_data,
                    caption = "Default Rate by DTI Band \\label{tab:dti}")

    print.xtable(table,
                 floating          = TRUE,
                 table.placement   = 'H',
                 comment           = FALSE,
                 caption.placement = 'bottom',
                 include.rownames  = FALSE)
}