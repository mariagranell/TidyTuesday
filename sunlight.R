# Title     : TODO
# Objective : TODO
# Created by: mariagranell
# Created on: 11/12/2020

library(dplyr)
library(ggplot2)
library(ggridges) # to make ridgeline plots
library(streamgraph) # to make streamgraphs
library(htmlwidgets) # to save the streamgraphs
library(gganimate)
library(tidyr) # to change between long and wide formats
# library(hrbrthemes)
library("rjson") # library to read JSON
library("jsonlite") # get a list of key/value vectors, convert that to a two column data.frame with stack, rearrange the columns and change the column names (if needed).
# https://stackoverflow.com/questions/52152785/convert-json-list-to-data-frame

# TUTORIAL GGPLOT
# https://cedricscherer.netlify.app/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/
setwd("/Users/mariagranell/Desktop/Rcourses/TidyverseTuesday")

# read the data for 5 years
dat <- fromJSON("/Users/mariagranell/Desktop/Rcourses/TidyverseTuesday/fiveyearsglobal.json")
dat <- as.data.frame(dat)
head(dat)
str(dat)

# cleaning data
dat[] <- lapply(dat, gsub, pattern = "T00:00:00Z", replacement = "")

# create collums of year, month day.
dat$split <- strsplit(dat$date_time,"-") # you create a list call split in thedat frame
dat[c("year", "month", "day")] <- t(data.frame(dat$split)) # to store the list in diff collums
dat <- subset(dat, select = -c(split)) # remove the list

all_months <- dat
# change the name of the month
dat<-within(dat,{
  month<-as.factor(month)
  day<-as.numeric(day)
  year<-as.factor(year)
  value<-as.numeric(value)
  date_time<-as.Date(date_time)
})

all_months<-within(all_months,{
  month<-as.numeric(month)
  day<-as.numeric(day)
  year<-as.factor(year)
  value<-as.numeric(value)
  date_time<-as.Date(date_time)
})


dat <- transform(dat, month = month.abb[month])
head(dat)

dat$value <- ifelse(dat$value < 0, 0, dat$value)
all_months$value <- ifelse(all_months$value < 0, 0, all_months$value)

#for plotting
d <- subset(dat, month == "Dec")
d <- subset(d, day < 11)
d$year <- as.character(d$year)

just2020 <- subset(dat, year == 2020)
just2020 <- subset(just2020, day < 10)
NovDec <- subset(all_months, month > 10)
NovDec <- mutate(NovDec, date = paste0(month,day, sep=""))
?mutate()
#---------------------------------------------------
# STREAM PLOT

streamgraph(d, key="month", value="value", date="date_time", offset="zero", interpolate="cardinal") %>%
  sg_legend(show=TRUE, label="months: ")

d%>%
  group_by(year, day) %>%
  streamgraph( "year", "value",  "day", scale="continuous") %>%
  sg_fill_brewer("PuOr")
?streamgraph()

all_months%>%
  group_by(year, month) %>%
  summarise(mean_value = mean(value))%>%
  streamgraph( "year", "mean_value",  "month", scale="continuous") %>%
  sg_fill_brewer("PuOr")

just2020%>%
  group_by(day, month) %>%
  streamgraph( "month", "value",  "day", scale="continuous") %>%
  sg_fill_brewer("PuOr")

decglobstream
subset(just2020, just2020$month == "Feb")

# save the widget
saveWidget(decglobstream, file=paste0( getwd(), "/decglobstream.html"))

#---------------------------------------------------
# DENSITY PLOT

# drawing plot
d %>%
  ggplot( aes(x=day, y=value, group=year, color=year, fill=year)) +
  #geom_line() +
  #geom_point() +
  geom_area()+
  #scale_x_date(breaks=brks, labels=lbls) +
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Global radiation in 2020") +
  #theme_ipsum()
  xlab("months") +
  transition_reveal(day)

anim_save("Global_2020.gif")

#---------------------------------------------------
# SPAGUETTI PLOT

# Plot
a<-d %>%
  ggplot( aes(x=day, y=value, group=year, color=year, fill=year)) +
  geom_smooth(se=F) +
  #scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("A spaghetti chart of global radiation in december")+
  theme_minimal()
a
?geom_area()

#---------------------------------------------------
# RIDGELINE PLOT

d %>%
  ggplot( aes(x=value, y=year, fill=year)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none")

#---------------------------------------------------
# LINE PLOT
# to put the data in wide format:

d_wide <- subset(d, select = -c(date_time))
d_wide <- spread(d_wide, year, value)
names(d_wide) <- c("month", "day","y2015","y2016","y2017","y2018","y2019","y2020")
head(d_wide)

library(plotly)
a<-ggplot(d_wide, aes(x = day)) +
  stat_smooth(aes(y = y2015), geom = 'area', method = 'loess', span = 1/3, fill = "#b35806", color = "#b35806", alpha = 0.5) +
  stat_smooth(aes(y = y2016), geom = 'area', method = 'loess', span = 1/3, fill = "#f1a340", color = "#f1a340", alpha = 0.5) +
  stat_smooth(aes(y = y2017), geom = 'area', method = 'loess', span = 1/3, fill = "#fee0b6", color = "#fee0b6", alpha = 0.5) +
  stat_smooth(aes(y = y2018), geom = 'area', method = 'loess', span = 1/3, fill = "#d8daeb", color = "#d8daeb", alpha = 0.5) +
  stat_smooth(aes(y = y2019), geom = 'area', method = 'loess', span = 1/3, fill = "#998ec3", color = "#998ec3", alpha = 0.5) +
  stat_smooth(aes(y = y2020), geom = 'area', method = 'loess', span = 1/3, fill = "#542788", color = "#542788", alpha = 0.5) +
  scale_x_continuous(name = "day", breaks = 1:10) +
  scale_y_continuous(name = "Global radiation") +
  theme_minimal()
a
library("Cairo")
p<-ggplotly(a, tooltip="groups")
p

key <- highlight_key(d_wide, ~year)
p <- ggplot(key, aes(x = day)) +
  stat_smooth(aes(y = y2015), geom = 'area', method = 'loess', span = 1/3, fill = "#b35806", color = "#b35806", alpha = 0.5) +
  stat_smooth(aes(y = y2016), geom = 'area', method = 'loess', span = 1/3, fill = "#f1a340", color = "#f1a340", alpha = 0.5) +
  stat_smooth(aes(y = y2017), geom = 'area', method = 'loess', span = 1/3, fill = "#fee0b6", color = "#fee0b6", alpha = 0.5) +
  stat_smooth(aes(y = y2018), geom = 'area', method = 'loess', span = 1/3, fill = "#d8daeb", color = "#d8daeb", alpha = 0.5) +
  stat_smooth(aes(y = y2019), geom = 'area', method = 'loess', span = 1/3, fill = "#998ec3", color = "#998ec3", alpha = 0.5) +
  stat_smooth(aes(y = y2020), geom = 'area', method = 'loess', span = 1/3, fill = "#542788", color = "#542788", alpha = 0.5) +
  scale_x_continuous(name = "day", breaks = 1:10) +
  scale_y_continuous(name = "Global radiation") +
  theme_minimal()
ggplotly( p, tooltip = "groups" )

# declare `city` as the SQL 'query by' column
tx <- highlight_key(d, ~year)

# initiate a plotly object
base <- plot_ly(tx, colors=  "YlOrRd") %>%
  group_by(year)

?plot_ly()
# create a time series of median house price
time_series <- base %>%
  group_by(year) %>%
  add_lines(x = ~day, y = ~value)

?add_lines()

highlight(
  time_series,
  on = "plotly_hover",
  selectize = FALSE,
  dynamic = FALSE,
  color = "red",
  persistent = FALSE
)