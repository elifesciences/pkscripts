# Definition of theme that is used to set appearance of all graphs

# Define standard colours

e.dark.grey <- c("#1D2129")
e.mid.grey <- c("#585257")
e.light.grey <- c("#ABABAB")
e.red <- c("#EC334D")
e.green <- c("#0BAC45")

# Set default theme

dashboard.theme <- theme_classic() +
  theme(plot.background = element_rect(fill = e.dark.grey),
        panel.background = element_rect(fill = e.dark.grey),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(family = 'Helvetica', colour = e.light.grey),
        axis.ticks = element_line(colour = e.light.grey),
        plot.title = element_text(family = 'Helvetica', colour = e.light.grey, size = rel(1.2)),
        legend.background = element_rect(fill = e.dark.grey),
        legend.text = element_text(family = 'Helvetica', colour = e.light.grey),
        legend.title = element_blank(),
        legend.position = 'bottom')

# Set dimensions of graphic outputs by defining width and aspect ratio

g.width <- 6.67
aspect.ratio <- 1.3788
g.height <- g.width / aspect.ratio

# Create function to calculate y axis limits

axis.adj.factor <- 0.3

# y.axis.limits <- function(a, b = axis.adj.factor) {
#               y.min <- min(a) + b * (max(a) - min(a))
#               y.max <- max(a) - b * (max(a) - min(a))
#               return(c(y.min, y.max))
# }

