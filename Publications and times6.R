# This script does the following:
#
# -  calculates and plots the median number of days each month from initial submission
#    to version 1 publication
# -  calculates and plots the number of version 1 publications per month
# -  calculates and plots the daily cumulative number of publications for current and previous months
# -  calculates individual numerical values for number of publications so far this month,
#    projected number for the full month, and number of publications last month. Combines these
#    with text into one graphic
#
# Graphs are saved as SVG files. Text and number combinations are saved as PNG.

# Load required packages

library(xts)
library(ggplot2)
library(svglite)
library(Hmisc)
library(grid)
library(scales)

# Set default styles for graphs

source('Theme6.R')

# Set file save location

source('Location6.R')

# Reads in initial qc date and version 1 publication date for all records with a version 1 
# publication date

publishing.times <- read.csv('../Source data/Initial submission and publication dates.csv', header = TRUE, sep = ',')

# Convert the initial qc and publication date fields to date formats

publishing.times$date_initial_qc <- as.Date(substr(publishing.times$date_initial_qc,1,10), format = '%Y-%m-%d')
publishing.times$datetime_published <- as.Date(substr(publishing.times$datetime_published,1,10), format = '%Y-%m-%d')
publishing.times$date_full_qc <- as.Date(substr(publishing.times$date_full_qc,1,10), format = '%Y-%m-%d')

# Create a receipt date filed which is the date_initial_qc unless that is blank in which case it is date_full_qc

publishing.times$receipt.date <- pmin(publishing.times$date_initial_qc, publishing.times$date_full_qc, na.rm = TRUE)

# Calculate the publishing times as the number of days from receipt.date to version 1 publication for each record

publishing.times$pub.time <- (publishing.times$datetime_published - publishing.times$receipt.date)

# Create a xts object containing just the publication dates and publishing times

pub.times <- xts(publishing.times$pub.time, publishing.times$datetime_published)

# Remove rows with NA values for publishing time, which will occur when there is no recorded inital qc date

pub.times <- pub.times[!is.na(pub.times)]

# Calculate the median publishing time for each calendar month's data

monthly.median.pub.times <- apply.monthly(pub.times, median)

# Convert to dataframe

monthly.median.pub.times <- data.frame(date = index(monthly.median.pub.times), monthly.median.pub.times, row.names = NULL)

names(monthly.median.pub.times) <- c('date', 'median')

# Trim data to last 12 months for graphing 

monthly.median.pub.times <- monthly.median.pub.times[(nrow(monthly.median.pub.times)-11):
                                                     (nrow(monthly.median.pub.times)), ]

# Calculate y axis limits

ylim <- c((min(monthly.median.pub.times$median) - axis.adj.factor * 
          (max(monthly.median.pub.times$median) - min(monthly.median.pub.times$median))),
          (max(monthly.median.pub.times$median) + axis.adj.factor * 
          (max(monthly.median.pub.times$median) - min(monthly.median.pub.times$median))))

# Plot monthly medians for last twelve months

ggplot(monthly.median.pub.times, aes(x = date, y = median)) + 
  scale_y_continuous(limits = ylim) +
  geom_line(size = 2, colour = e.red) +
  ggtitle('Submission to Publication - Last 12 Months') + 
  ylab('Days') +
  dashboard.theme +
  theme(axis.title.y = element_text(family = 'Helvetica', colour = e.light.grey, angle = 90))

# Save plot as SVG

ggsave(paste(output.location, 'medians.svg', sep = ''), width = g.width, height = g.height, units = 'in')

# -------------------------------------------------------

# Calculate monthly total number of articles published

pub.number <- apply.monthly(pub.times, nrow)

# Convert to dataframe

pub.number <- data.frame(date = index(pub.number), number = pub.number, row.names = NULL)

# Trim data to last 12 months for graphing

t.pub.number <- pub.number[(nrow(pub.number)-12):(nrow(pub.number)-1), ]

# Calculate y axis limits

ylim <- c((min(t.pub.number$number) - axis.adj.factor * (max(t.pub.number$number) - 
           min(t.pub.number$number))),
          (max(t.pub.number$number) + axis.adj.factor * (max(t.pub.number$number) - 
           min(t.pub.number$number))))

# Plot monthly total number of articles published in the last year

ggplot(t.pub.number, aes(x = date, y = number)) + 
       scale_y_continuous(limits = ylim) +
       geom_line(size = 2, colour = e.red) +
       ggtitle('Last 12 Months') + 
       dashboard.theme

# Save plot as SVG file

ggsave(paste(output.location, 'pub-number.svg', sep = ''), width = g.width, height = g.height, units = 'in')

# ----------------------------------------------------

# CREATE AND PLOT DAILY CUMULATIVE NUMBER OF PUBLISHED ARTICLES

# Create a subset of the data containing only the current and previous month

recent.pubs <- last(pub.times, '2 months')

# Count the publications by day

daily.publications <-  apply.daily(recent.pubs,nrow)

# Convert to dataframe, creating separate column for dates

daily.pubs <- data.frame(date = index(daily.publications), daily.publications, row.names = NULL)

# Add a column showing just the month to enable distinguishing between current and previous months' data

daily.pubs$month <- months(daily.pubs$date)

# Rename columns to be more meaningful

names(daily.pubs) <- c('date', 'number', 'month')

# Calculate cumulative numbers within each month, and set text flags for 'This month'
# and 'Last month' to be used in legend on graph

for (i in 1:nrow(daily.pubs)) {
  if (i == 1) {daily.pubs$cum[i] <- daily.pubs$number[i]
  daily.pubs$month.flag[i] <- c('Last month')
  } else {
    if (!daily.pubs$month[i] == daily.pubs$month[i-1]) {
      daily.pubs$cum[i] <- daily.pubs$number[i]
      daily.pubs$month.flag[i] <- c('This month')
    } else {daily.pubs$cum[i] <- daily.pubs$cum[i-1] + daily.pubs$number[i]
    daily.pubs$month.flag[i] <- daily.pubs$month.flag[i-1]
    }
  }
}

# Add a column showing on the day of the month, to be used for x axis on graph

daily.pubs$day <- as.numeric(format(daily.pubs$date, '%d'))

# Plot daily cumulative totals for current and previous month using ggplot2

ggplot(daily.pubs,  aes(x = day, y = cum, colour = month.flag)) + 
  geom_line(size = 2) + scale_y_continuous(labels = comma) +
  scale_color_manual(values = c(e.light.grey, e.red)) +
  ggtitle('This Month and Last Month') +
  dashboard.theme

# Save plot as SVG

ggsave(paste(output.location, 'daily-publications.svg', sep = ''), width = g.width, height = g.height, units = 'in')

# -----------------------------------------------------------------------

# INDIVIDUAL VARIABLES COMBINED WITH TEXT

# Create variables with this month's and last month's submission totals, for separate display
# Create a PNG that combines these variables with text

pubs.this.month <- pub.number$number[nrow(pub.number)]

pubs.last.month <- pub.number$number[nrow(pub.number)-1]

# Calculate the most recent day of the month for which data exists, and the total number
# of days in the current month, and then calculate a projected total for the full month

date.last.data <- as.numeric(substr(pub.number$date[nrow(pub.number)], 9, 10))

days.in.current.month <- monthDays(pub.number$date[nrow(pub.number)])

pubs.projected.this.month <- as.integer((days.in.current.month * pubs.this.month) / date.last.data)

# Create a PNG combining these values with text

png(filename = paste(output.location, 'publications-text.png', sep = ''),
    width = g.width, height = g.height, units = "in", res = 600, pointsize = 30,
    bg = "white", type = c("quartz"))

grid.newpage()

grid.rect(gp = gpar(fill = e.dark.grey))

grid.text(pubs.this.month, x = unit(0.04, 'npc'), y = unit(0.948, 'npc'), just = c('left', 'top'), 
          gp = gpar(col = e.red, cex = 5.2), draw = TRUE)
grid.text('This', x = unit(0.96, 'npc'), y = unit(0.94, 'npc'), just = c('right', 'top'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('month', x = unit(0.96, 'npc'), y = unit(0.75, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('Projected', x = unit(0.04, 'npc'), y = unit(0.32, 'npc'), just = c('left', 'bottom'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('Last month', x = unit(0.96, 'npc'), y = unit(0.32, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text(pubs.projected.this.month, x = unit(0.04, 'npc'), y = unit(0.06, 'npc'), just = c('left', 'bottom'), 
          gp = gpar(col = 'darkgoldenrod1', cex = 3.2), draw = TRUE)
grid.text(pubs.last.month, x = unit(0.96, 'npc'), y = unit(0.06, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 3.2), draw = TRUE)

dev.off()



# ----------------------------------------------------------------

# CREATE SINGLE VARIABLES  FOR PUBLICATION TIMES AND COMBINE WITH TEXT

# Create variables with this month's and last month's median publication times
# Create a PNG that combines these variables with text

median.this.month <- monthly.median.pub.times$median[nrow(monthly.median.pub.times)]

median.last.month <- monthly.median.pub.times$median[nrow(monthly.median.pub.times)-1]

# Create a PNG combining these values with text

png(filename = paste(output.location, 'median-text.png', sep = ''),
    width = g.width, height = g.height, units = "in", res = 600, pointsize = 30,
    bg = "white", type = c("quartz"))

grid.newpage()

grid.rect(gp = gpar(fill = e.dark.grey))

grid.text(median.this.month, x = unit(0.04, 'npc'), y = unit(0.948, 'npc'), just = c('left', 'top'), 
          gp = gpar(col = e.red, cex = 5.2), draw = TRUE)
grid.text('This', x = unit(0.96, 'npc'), y = unit(0.94, 'npc'), just = c('right', 'top'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('month', x = unit(0.96, 'npc'), y = unit(0.75, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('days', x = unit(0.04, 'npc'), y = unit(0.45, 'npc'), just = c('left', 'bottom'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('Last month', x = unit(0.96, 'npc'), y = unit(0.32, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text(median.last.month, x = unit(0.96, 'npc'), y = unit(0.06, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 3.2), draw = TRUE)

grid.text('submission', x = unit(0.04, 'npc'), y = unit(0.32, 'npc'), just = c('left', 'bottom'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('to', x = unit(0.04, 'npc'), y = unit(0.19, 'npc'), just = c('left', 'bottom'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('publication', x = unit(0.04, 'npc'), y = unit(0.06, 'npc'), just = c('left', 'bottom'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)


dev.off()

