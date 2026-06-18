ST_plot_default_by_state <- function(data) {


    data %>%
        filter(
            term == 36
        ) %>%
        summarise(
            default_rate = mean(concluded_loan),
            .by = addr_state
        ) %>%
        arrange(addr_state) %>%
        ggplot(aes(x = addr_state, y = default_rate, fill = addr_state)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent_format()) +

        labs(
            title    = "Default Rate by State",
            x        = "State",
            y        = "Default Rate"
        ) +
        theme_classic() +
        scale_fill_viridis_d(option = "plasma")+
        theme(legend.position = "none",
              axis.text.x     = element_text(angle = 45, hjust = 1))
}