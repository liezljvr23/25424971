plot_name_trend <- function(national_ranked, choose_name)
    {
        name_data <- national_ranked %>%
        filter(name == choose_name)

        g <- name_data %>%
            ggplot(aes(x = year, y = total)) +
            geom_line(colour = "purple", linewidth = 1) +
            geom_vline(xintercept = 1982, linetype = "dashed", colour = "darkgreen") +
            annotate("text", x = 1983, y = 3200,
                     label = "Family Ties premiers (1982)")+
            theme_minimal() +
            scale_colour_brewer(palette = "Paired") +
            labs(
                title = paste("Popularity of", choose_name, "Over Time"),
                x     = "Year",
                y     = "Total"
         )
        g
    }

