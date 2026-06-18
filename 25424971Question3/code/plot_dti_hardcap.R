plot_dti_hardcap <- function(data,
                             tolerance_lines = c(0.20, 0.25, 0.30)) {

    summary_data <- data %>%
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
        arrange(dti_bin)

    # Tolerance label data for annotations
    tolerance_data <- tibble(
        rate  = tolerance_lines,
        label = paste0(tolerance_lines * 100, "% tolerance")
    )

    summary_data %>%
        ggplot(aes(x = dti_bin, y = default_rate, group = 1)) +
        geom_line(linewidth = 1, colour = "steelblue") +
        geom_point(aes(size = n), colour = "steelblue", alpha = 0.7) +
        geom_hline(data = tolerance_data,
                   aes(yintercept = rate, linetype = label, colour = label),
                   linewidth = 0.8) +
        scale_y_continuous(labels = scales::percent_format(),
                           limits = c(0, 0.45)) +
        scale_size_continuous(name = "No. of Loans",
                              labels = scales::comma_format()) +
        scale_linetype_manual(name = "Default Tolerance",
                              values = c("dashed", "dotdash", "dotted")) +
        scale_colour_manual(name = "Default Tolerance",
                            values = c("darkgreen", "orange", "red")) +
        labs(
            title    = "Default Rate by DTI Band with Tolerance Thresholds",
            subtitle = "Horizontal lines indicate proposed DTI hard-cap levels",
            x        = "Debt-to-Income Ratio Band",
            y        = "Default Rate",
            caption  = "Source: Lending Club loan data. Resolved loans only."
        ) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}