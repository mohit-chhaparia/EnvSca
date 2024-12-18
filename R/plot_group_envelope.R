#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom abind abind
#' @importFrom grDevices rgb gray
#' @importFrom graphics par
#' @importFrom utils globalVariables
#' @noRd
utils::globalVariables(c("Frequency", "value", "variable"))
plot_group_envelope <- function(envelope_group){

  hw <- theme_minimal() + theme(
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
    axis.text.x = element_text(margin = margin(-1, 0, 3, 0)),

    plot.margin = margin(0, 0, 0, 0, "cm")
  )

  nf <- length(envelope_group[[1]])
  enveldat <- abind((1:nf) / (2 * nf), abind(envelope_group, along = 2))
  colnames(enveldat) <- c('Frequency', paste("Class: ", as.character(1:(dim(enveldat)[2] - 1)), sep = ""))
  enveldatlong <- melt(as.data.frame(enveldat), id.vars = 'Frequency')

  par(mfrow=c(1,1))
  p.env <- ggplot(data = enveldatlong, aes(x = Frequency, y = value)) +
    geom_line(size = 1, alpha = 1, aes(group = variable, color = variable)) +
    hw +
    labs(x = "Frequency", y = expression(hat(lambda)), title = 'Group-level Envelope') +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    xlim(c(0, 0.5))
  print(p.env)
}


