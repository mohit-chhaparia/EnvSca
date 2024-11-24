#' @import ggplot2
plot_individual_envelope <- function(envelope_group, scaling_group){

  hw <- theme_grey() + theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = - 0.5),

    strip.background = element_rect(fill = rgb(0.9, 0.95, 1),
                                    colour = gray(0.5),
                                    size = 0.2),

    panel.border = element_rect(fill = FALSE,
                                colour = gray(0.7)),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing.x = unit(0.10, "cm"),
    panel.spacing.y = unit(0.05, "cm"),

    axis.ticks = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(margin = margin(0, 3, 0, 3)),
    axis.text.x = element_text(margin = margin(-1, 0, 3, 0))
  )



}


