# Load Net Promoter Score from Google Sheet and graph

# Load required packages

library(ggplot2)
library(googlesheets)
library(scales)      # Used to format y axis labels as percent
library(reshape2)
library(xts)

# Set default sytles for graphs

source('Theme6.R')

# Set file save location

source('Location6.R')

nps <- gs_title("Performance dashboard 2014-2015")

nps.data <- gs_read(nps, ws = "Net promoter score", range = "b2:i6")
nps.data.t <- t(nps.data)
nps.data.t <- as.data.frame(nps.data.t)
names(nps.data.t) <- c('Accepted', 'Rejected.Initial', 'Rejected.Full', 'All')
nps.data.t$date <- row.names(nps.data.t)
nps.data.t$date <- as.Date(as.yearmon(nps.data.t$date, '%B %Y'))

nps.data.t <- subset(nps.data.t, select = c(-Rejected.Full, -Rejected.Initial))

# monthly.rates <- subset(monthly.rates, select = c(-ef, -rji, - af, -rjf, -rvf))

# Convert wide to long format

long.nps <- melt(nps.data.t, id.vars = 'date', variable.name = 'stage', 
                           value.name = 'score')

# Plot data

ggplot(long.nps, aes(x = date, y = score, colour = stage)) +
  geom_line(size = 2) + 
  scale_color_manual(labels = c('Accepted authors', 'All'), values = c(e.red, e.light.grey)) +
#  scale_y_continuous(limits = c(-50, 100)) +
  ggtitle('Net Promoter Score') +
  dashboard.theme

# Save plot as SVG

ggsave(paste(output.location, 'nps.svg', sep = ''), width = g.width, height = g.height, units = 'in')

