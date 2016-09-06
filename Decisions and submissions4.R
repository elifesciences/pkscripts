# This script does the following:
#
# -  calculates and plots initial, full and combined acceptance rates per month
# -  calculates and plots the number of intial submissions per month
# -  calculates and plots the daily cumulative number of submissions for current and previous months
# -  calculates individual numerical values for number of submissions so far this month,
#    projected number for the full month, and number of submissions last month. Combines these
#    with text into one graphic
#
# Graphs are saved as SVG files. Text and number combinations are saved as PNG.

# Load requried packages

library(ggplot2)
library(reshape2)
library(scales)      # Used to format y axis labels as percent
library(xts)
library(Hmisc)
library(grid)
library(svglite)

# Set default styles for graphs

source('Theme3.R')

# Set file save location

source('Location.R')

# Read data file

decisions <- read.csv("../Source data/Decisions and submissions.csv", header = TRUE)

# Add columns with numerical values of 0 or 1 to represent all decisions of interest

decisions$ef <- ifelse(decisions$initial_decision == 'EF', 1,0)
decisions$rji <- ifelse(decisions$initial_decision == 'RJI', 1,0)
decisions$af <- ifelse(decisions$decision == 'AF', 1,0)
decisions$rjf <- ifelse(decisions$decision == 'RJF', 1,0)
decisions$rvf <- ifelse(decisions$decision == 'RVF', 1,0)

# Create subset of just initial decisions
# Remove rows with blank dates
# Convert decision dates to Date format

i.decisions <- subset(decisions, select = c(-date_full_decision, -decision, -ejp_type, -type))
i.decisions <- i.decisions[!i.decisions$date_initial_decision == "", ]
i.decisions$date_initial_decision <- as.Date(i.decisions$date_initial_decision)

# Do the same for full decisions

f.decisions <- subset(decisions, select = c(-date_initial_decision, -initial_decision, -ejp_type, -type))
f.decisions <- f.decisions[!f.decisions$date_full_decision == "", ]
f.decisions$date_full_decision <- as.Date(f.decisions$date_full_decision)

# Create xts objects from both initial and full decisions, bringing into each only the decision
# columns relevant to that type of decision

x.i.decisions <- xts(i.decisions[, c('ef', 'rji')], as.Date(i.decisions$date_initial_decision))
x.f.decisions <- xts(f.decisions[, c('af', 'rjf', 'rvf')], as.Date(f.decisions$date_full_decision))

# Merge these two files so that the monthly grouping can be done on both at the same time

x.decisions <- merge.xts(x.i.decisions, x.f.decisions, fill = 0, join = 'outer')

# Sum the number of decisions of each type in each month

monthly.decisions <- apply.monthly(x.decisions, function(x) apply(x, 2, sum))

# Calcualte monthly acceptance rates for initial and full decisions, and a combined rate
# by multiplying the initial and full rates for the month

monthly.decisions$initial <- (monthly.decisions$ef/(monthly.decisions$ef + monthly.decisions$rji))

monthly.decisions$full <- (monthly.decisions$af + monthly.decisions$rvf)/(monthly.decisions$af +
                           monthly.decisions$rvf + monthly.decisions$rjf)

monthly.decisions$combined <- monthly.decisions$initial * monthly.decisions$full

# Remove the row containing NA which was caused by there not being any full decisions in 
# the first month

# *****  This could cause a problem when the data includes just a few days from the start of a new
# *****  month and there are either no initial or no full decisions yet. This step will remove the
# *****  data for the other type of decision.

monthly.decisions <- na.omit(monthly.decisions)

# Convert from a xts object to a dataframe, as a prelude to using ggplot2

monthly.rates <- data.frame(date = index(monthly.decisions), monthly.decisions, row.names = NULL)

# Remove unwanted columns to simplify the conversion from wide to long format required by ggplot2

monthly.rates <- subset(monthly.rates, select = c(-ef, -rji, - af, -rjf, -rvf))

# Convert wide to long format

long.monthly.rates <- melt(monthly.rates, id.vars = 'date', variable.name = 'stage', value.name = 'rate')

# Plot monthly acceptance rates using ggplot2

ggplot(subset(long.monthly.rates, date >= (max(date)-365)), aes(x = date, y = rate, colour = stage)) + 
  geom_line(size = 2) + scale_y_continuous(labels = percent) +
  scale_color_manual(labels = c('Initial', 'Full', 'Combined'), values = c(e.green, 'deepskyblue', e.red)) +
  ggtitle('Acceptance Rates - Last 12 Months') +
  dashboard.theme

# Save plot as SVG

ggsave(paste(output.location, 'acceptance-rates.svg', sep = ''), width = g.width, 
       height = g.height, units = 'in')

# ------------------------------------------------------------------------

# INITIAL SUBMISSION NUMBERS

# Create file of initial submission dates, dropping all decision data

initial.submissions <- subset(decisions, select = c(manuscript_id, date_initial_qc))

# Remove rows with blank dates

initial.submissions <- initial.submissions[!initial.submissions$date_initial_qc == "",]

# Create an xts object, also reformatting the dates as dates

x.initial.submissions <- xts(initial.submissions[, 'manuscript_id'], 
                             as.Date(initial.submissions$date_initial_qc))

# Calculate the number of submissions each month

monthly.submissions <- as.data.frame(apply.monthly(x.initial.submissions, nrow))

# Convert the dates held in row names to a separately addressable column,
# and convert to a dataframe as required by ggplot2

monthly.submissions$date <- row.names(monthly.submissions)
monthly.submissions$date <- as.Date(monthly.submissions$date)

# Trim data to last 12 months for graphing

t.monthly.submissions <- monthly.submissions[(nrow(monthly.submissions)-12):(nrow(monthly.submissions)-1), ]

# Set y axis limits to give space around maximum and minimum

ylim <- c((min(t.monthly.submissions$V1) - axis.adj.factor * (max(t.monthly.submissions$V1) - 
           min(t.monthly.submissions$V1))),
          (max(t.monthly.submissions$V1) + axis.adj.factor * (max(t.monthly.submissions$V1) - 
           min(t.monthly.submissions$V1))))

# Plot last twelve month's monthly submissions

ggplot(t.monthly.submissions, aes(x = date, y = V1)) +
       geom_line(size = 2, colour = e.red) +
       scale_y_continuous(limits = ylim) +
       ggtitle('Last 12 Months') + 
       dashboard.theme

# Save as SVG file

ggsave(paste(output.location, 'monthly-submissions.svg', sep = ''), width = g.width, 
       height = g.height, units = 'in')

# dev.off()

# ----------------------------------------------------

# CREATE SINGLE VARIABLES FOR NUMBER OF SUBMISSIONS AND COMBINE WITH TEXT

# Create variables with this month's and last month's submission totals, for separate display
# Create a PNG that combines these variables with text

subs.this.month <- monthly.submissions$V1[nrow(monthly.submissions)]

subs.last.month <- monthly.submissions$V1[nrow(monthly.submissions)-1]

# Calculate the most recent day of the month for which data exists, and the total number
# of days in the current month, and then calculate a projected total for the full month

date.last.data <- as.numeric(substr(monthly.submissions$date[nrow(monthly.submissions)], 9, 10))

days.in.current.month <- monthDays(monthly.submissions$date[nrow(monthly.submissions)])

subs.projected.this.month <- as.integer((days.in.current.month * subs.this.month) / date.last.data)

# Create a PNG combining these values with text

png(filename = paste(output.location, 'submission-text.png', sep = ''),
    width = g.width, height = g.height, units = "in", res = 600, pointsize = 30,
    bg = "white", type = c("quartz"))

grid.newpage()

grid.rect(gp = gpar(fill = e.dark.grey))

grid.text(subs.this.month, x = unit(0.04, 'npc'), y = unit(0.948, 'npc'), just = c('left', 'top'), 
          gp = gpar(col = e.red, cex = 5.2), draw = TRUE)
grid.text('This', x = unit(0.96, 'npc'), y = unit(0.94, 'npc'), just = c('right', 'top'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('month', x = unit(0.96, 'npc'), y = unit(0.75, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('Projected', x = unit(0.04, 'npc'), y = unit(0.32, 'npc'), just = c('left', 'bottom'), 
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text('Last month', x = unit(0.96, 'npc'), y = unit(0.32, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 1.2), draw = TRUE)
grid.text(subs.projected.this.month, x = unit(0.04, 'npc'), y = unit(0.06, 'npc'), just = c('left', 'bottom'), 
          gp = gpar(col = 'darkgoldenrod1', cex = 3.2), draw = TRUE)
grid.text(subs.last.month, x = unit(0.96, 'npc'), y = unit(0.06, 'npc'), just = c('right', 'bottom'),
          gp = gpar(col = e.light.grey, cex = 3.2), draw = TRUE)

dev.off()



# ----------------------------------------------------

# CREATE AND PLOT DAILY CUMULATIVE NUMBER OF SUBMISSIONS

# Create a subset of the data containing only the current and previous month

recent.submissions <- last(x.initial.submissions, '2 months')

# Count the submissions by day

daily.submissions <-  apply.daily(recent.submissions,nrow)

# Convert to dataframe, creating separate column for dates

daily.subs <- data.frame(date = index(daily.submissions), daily.submissions, row.names = NULL)

# Add a column showing just the month to enable distinguishing between current and previous months' data

daily.subs$month <- months(daily.subs$date)

# Rename columns to be more meaningful

names(daily.subs) <- c('date', 'number', 'month')

# Calculate cumulative numbers within each month, and set text flags for 'This month'
# and 'Last month' to be used in legend on graph

for (i in 1:nrow(daily.subs)) {
  if (i == 1) {daily.subs$cum[i] <- daily.subs$number[i]
  daily.subs$month.flag[i] <- c('Last month')
  } else {
    if (!daily.subs$month[i] == daily.subs$month[i-1]) {
      daily.subs$cum[i] <- daily.subs$number[i]
      daily.subs$month.flag[i] <- c('This month')
    } else {daily.subs$cum[i] <- daily.subs$cum[i-1] + daily.subs$number[i]
    daily.subs$month.flag[i] <- daily.subs$month.flag[i-1]
    }
  }
  
}

# Add a column showing on the day of the month, to be used for x axis on graph

daily.subs$day <- as.numeric(format(daily.subs$date, '%d'))

# Plot daily cumulative totals for current and previous month using ggplot2

ggplot(daily.subs,  aes(x = day, y = cum, colour = month.flag)) + 
  geom_line(size = 2) + scale_y_continuous(labels = comma) +
  scale_color_manual(values = c(e.light.grey, e.red)) +
  ggtitle('This Month and Last Month') +
  dashboard.theme

# Save plot as SVG

ggsave(paste(output.location, 'daily-submissions.svg', sep = ''), 
       width = g.width, height = g.height, units = 'in')

