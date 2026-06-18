plot_name_jude<- function(national_ranked, choose_name) {

    national_ranked %>%
        filter(name == choose_name) %>%
        ggplot(aes(x = year, y = total, colour = gender)) +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = 1968, linetype = "dashed", colour = "darkgreen") +
        annotate("text", x = 1969, y = 200,
                 label = "Hey Jude (1968)",
                 colour = "grey40") +
        geom_vline(xintercept = 1999, linetype = "dashed", colour = "darkblue") +
        annotate("text", x = 2000, y = 1500,
                 label = "Jude Law breakout (1999)",
                 colour = "grey40") +
        scale_colour_brewer(palette = "Paired") +
        labs(
            title    = "Popularity of Jude Over Time",
            subtitle = "Influenced by The Beatles (1968) and Jude Law (1999)",
            x        = "Year",
            y        = "Total",
            colour   = "Gender"
        ) +
        theme_minimal()
}

