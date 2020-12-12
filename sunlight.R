# Title     : TODO
# Objective : TODO
# Created by: mariagranell
# Created on: 11/12/2020

library(dplyr)
library(ggplot2)
library(streamgraph)
library(gganimate)
library(hrbrthemes)
library("rjson") # library to read JSON
library("jsonlite") # get a list of key/value vectors, convert that to a two column data.frame with stack, rearrange the columns and change the column names (if needed).
# https://stackoverflow.com/questions/52152785/convert-json-list-to-data-frame

setwd("/")

# read the data
dat <- fromJSON("/Users/mariagranell/Desktop/Rcourses/TidyverseTuesday/thisyearglobal.json")
dat <- as.data.frame(dat)
head(dat)
str(dat)

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

# change the name of the month
dat<-within(dat,{
  month<-as.factor(month)
  day<-as.integer(day)
  year<-as.factor(year)
  value<-as.numeric(value)
  date_time<-as.Date(date_time)
})

dat <- transform(dat, month = month.abb[month])
head(dat)

#for plotting
d <- subset(dat, month == "Dec")
head(d)

streamgraph(d, key="month", value="value", date="date_time", offset="zero", interpolate="cardinal") %>%
  sg_legend(show=TRUE, label="months: ")
streamgraph(d, key="month", value="value", date="date_time", offset="zero", interpolate="cardinal") %>%
  sg_legend(show=TRUE, label="months: ")
pp
?streamgraph()

# drawing plot
d %>%
  ggplot( aes(x=date_time, y=value, group=month, color=month, fill=month)) +
  #geom_line() +
  #geom_point() +
  geom_area()+
  #scale_x_date(breaks=brks, labels=lbls) +
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Global radiation in 2020") +
  #theme_ipsum()
  xlab("months") +
  transition_reveal(date_time)

anim_save("Global_2020.gif")