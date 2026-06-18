plot_dti_by_outcome <- function(data) {


    data %>% mutate(
        loan_outcome = factor(loan_outcome, levels = c("paid", "current", "partial default",
                                                     "default", "loss"))
        ) %>%
        filter(
            !is.na(loan_outcome)
            ) %>%
        summarise(
            mean_dti = mean(dti, na.rm = TRUE),
            .by = loan_outcome
            ) %>%
        arrange(loan_outcome) %>%

        ggplot(aes(x = loan_outcome, y = mean_dti, fill = loan_outcome)) +
            geom_col()+
            geom_text(aes(label = round(mean_dti, 1))) +
            scale_fill_viridis_d(option = "plasma") +
            labs(
                title   = "Average DTI by Loan Outcome",
                x       = "Loan Outcome",
                y       = "Mean Debt-to-Income Ratio"
        ) +
            theme_classic() +
            theme(legend.position = "none")
}