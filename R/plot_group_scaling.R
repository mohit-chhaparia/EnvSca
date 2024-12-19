#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom abind abind
#' @importFrom grDevices rgb gray
#' @importFrom graphics par
#' @importFrom utils globalVariables
#' @noRd
utils::globalVariables(c("Frequency", "value", "variable"))
plot_group_scaling <- function(scaling_group){

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

  nf <- nrow(scaling_group[[1]])
  for(i in 1:length(scaling_group)){

    scadat <- abind((1:nf) / (2 * nf), scaling_group[[i]])
    colnames(scadat) <- c('Frequency', paste("Category: ", as.character(1:(dim(scadat)[2] - 1)), sep = ""))
    scadatlong <- melt(as.data.frame(scadat), id.vars = 'Frequency')
    lim <- round(max(abs(min(scadatlong$value)), abs(max(scadatlong$value))), 1) + 0.1

    par(mfrow=c(1,1))
    p.sca <- ggplot(scadatlong, aes(x = Frequency, y = variable, fill = value)) +
      geom_tile() +
      hw +
      scale_fill_distiller(palette = "Spectral", limits = c(-lim, lim), name = "") +
      labs(x = "Frequency", y = expression(hat(gamma)), title = paste('Group-level Scaling for Class:', i)) +
      xlim(c(0, 0.5))
    print(p.sca)
  }
}





