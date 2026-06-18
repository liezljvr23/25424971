plot_score_scatter <- function(data, x_var, y_var, size_var, label_var, title, x_axis, y_axis) {

    data %>%
        filter(!is.na({{ x_var }}), !is.na({{ y_var }}), !is.na({{ label_var }})) %>%
        group_by({{ label_var }}) %>%
        summarise(
            x    = mean({{ x_var }}, na.rm = TRUE),
            y    = mean({{ y_var }}, na.rm = TRUE),
            size = mean({{ size_var }}, na.rm = TRUE),
            n    = n(),

        ) %>%
        ggplot(aes(x = x, y = y, colour = {{ label_var }}, size = size)) +
        geom_point() +
        theme_classic() +
        guides(
            size   = "none"
        )+
        theme(legend.position = "bottom",
             ) +
        scale_colour_viridis_d(option = "turbo") +
        labs(
            title  = title,
            x      = x_axis,
            y      = y_axis,
            colour = "Group",
            size   = "Size"
        )
}
