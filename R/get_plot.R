#' @import ggplot2
get_plot <- function(envelope_ind, envelope_group){

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


  nf <- length(envelope_ind[[1]])
  enveldat <- cbind((1:nf) / (2 * nf), do.call(cbind, envelope_ind))
  colnames(enveldat) <- c('Frequency', as.character(1:(dim(enveldat)[2] - 1)))
  enveldatlong <- melt(as.data.frame(enveldat), id.vars = 'Frequency')

  par(mfrow=c(1,1))
  p.env <- ggplot(data = enveldatlong, aes(x = Frequency, y = value)) +
    geom_line(size = 1, alpha = 0.3, aes(group = variable, color='red')) + hw +
    labs(x = "Frequency", y = expression(hat(lambda))) +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    xlim(c(0, 0.5))

  enveldat <- cbind((1:nf) / (2*nf), envelope_group)
  colnames(enveldat) <- c('Frequency', 'Group');
  p.env <- p.env + geom_line(data = enveldat, aes(x = Frequency, y = Group, color='red'), size = 1.5, alpha = 1)
  print(p.env)

}

