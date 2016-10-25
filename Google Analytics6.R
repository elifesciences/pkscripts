# Access Google Sheet for Google Analytics data

# Load packages required

library(ggplot2)
library(googlesheets)
library(Hmisc)
library(scales)

# Set default sytles for graphs

source('Theme6.R')

# Set file save location

source('Location6.R')

# Read data from four pages in spreadsheet
# Beware this sometimes times out

web.traffic <- gs_title("Website traffic - automated reports")
web.users.last7 <- gs_read(web.traffic, ws = "Users last seven days", range = "A15:B22")
web.users.previous7 <- gs_read(web.traffic, ws = "Users previous seven days", range = "A15:B22")
web.users.monthly.thisyear <- gs_read(web.traffic, ws = "Monthly users - this year", range = "A15:B27")
web.users.monthly.lastyear <- gs_read(web.traffic, ws = "Monthly users - last year", range = "A15:B27")

# Add text flags to distinguish the most recent week, and the previous week.
# Then combine the two weeks' data in one dataframe in long format as required by ggplot2

web.users.last7$week <- 'This week'
web.users.previous7$week <- 'Last week'
web.users.weekly <- rbind(web.users.last7, web.users.previous7)

# Rename the columns for clarity, and to avoid the problems caused the the colons in the automatic names

names(web.users.weekly) <- c('date', 'users', 'week')

# Create two character day-of-the-week labels to aid plotting the data as two grouped sets rather than one
# Set the order of the day-of-the-week labels so that they print in the correct order

web.users.weekly$day <- as.factor(strtrim(weekdays(web.users.weekly$date), 2))
day.order <- as.vector(web.users.weekly$day[1:7])
web.users.weekly$day <- factor(web.users.weekly$day, levels = day.order)

# Plot a grouped column graph

ggplot(web.users.weekly, aes(x = day, y = users, fill = week)) +
       geom_bar(stat = 'identity', position = position_dodge()) +
       scale_y_continuous(labels = comma) +
       scale_fill_manual(values = c(e.light.grey, e.red)) + 
       ggtitle('Website Users - Daily') +
       dashboard.theme

ggsave(paste(output.location, 'weekly-analytics.svg', sep = ''), width = g.width, height = g.height, units = 'in')

# -------------------------------------------------------

# MONTHLY DATA

# NEEDS WORK

# Add text flags to distinguish current and prior years.
# Then combine the two year' data in one dataframe in long format as required by ggplot2

web.users.monthly.thisyear$year <- 'This year'

# decisions$ef <- ifelse(decisions$initial_decision == 'EF', 1,0)

web.users.monthly.lastyear$year <- 'Last year'
web.users.monthly <- rbind(web.users.monthly.thisyear, web.users.monthly.lastyear)

# Rename the columns for clarity, and to avoid the problems caused by the the colons in the automatic names

names(web.users.monthly) <- c('month', 'users', 'year')

# Create a framework of all month number and year flag combinations
# Merge this with data to create NA values for months later in the current year
# to stop the graph columns for prior year data in these months from increasing in width

month.frame <-rbind(data.frame(month = 1:12, year = 'This year'), 
                  data.frame(month = 1:12, year = 'Last year'))

w.users.monthly <- merge(web.users.monthly, month.frame, all = TRUE)

# Add month names (rather than just month numbers) to be used on x axis
# These need to be of type factor, so we can set their correct order

w.users.monthly$month.name <- as.factor(month.abb[w.users.monthly$month])

# Sort data so that first 12 rows will have month names in correct order

w.users.monthly <- w.users.monthly[with(w.users.monthly, order(year, month)), ]

# Create a vector from the first 12 month names, to use to set the order of the
# factor value month.name

month.order <- as.vector(w.users.monthly$month.name[1:12])
w.users.monthly$month.name <- factor(w.users.monthly$month.name, levels = month.order)

# Plot a grouped column graph

ggplot(w.users.monthly, aes(x = month.name, y = users, fill = year)) +
 geom_bar(stat = 'identity', position = position_dodge()) +
 scale_y_continuous(labels = comma) +
 scale_fill_manual(values = c(e.light.grey, e.red)) + 
 ggtitle('Website Users - Monthly') +
 dashboard.theme

ggsave(paste(output.location, 'monthly-analytics.svg', sep = ''), width = g.width, height = g.height, units = 'in')
