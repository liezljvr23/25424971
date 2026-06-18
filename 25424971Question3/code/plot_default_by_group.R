plot_default_by_group <- function(data, group_var, title    = NULL,
                                  x_label  = NULL
                                  )
    {

    summary_data <- data %>%
        filter(!is.na({{ group_var }})
               ) %>%
        summarise(
            default_rate = mean(concluded_loan),
            .by          = {{ group_var }}
        )%>%

        ggplot(aes(x = {{ group_var }}, y = default_rate, fill = {{ group_var }})) +
        geom_col() +
        geom_text(aes(label = paste0(round(default_rate * 100, 1), "%"))
                  ) +
        scale_y_continuous(labels = scales::percent_format()
                           ) +
        labs(
            title   = title,
            x       = x_label,
            y       = "Default Rate"
        ) +
        theme_classic() +
        scale_fill_viridis_d(option = "plasma") +
        theme(legend.position = "none",
              axis.text.x     = element_text(angle = 45, hjust = 1))

summary_data
}