# Title     : TODO
# Objective : TODO
# Created by: mariagranell
# Created on: 16/12/2020
library(dplyr)
library(ggthemes)
library(ggplot2)
library(tidyr) # to change between long and wide formats
library("rjson") # library to read JSON
library("jsonlite") # get a list of key/value vectors, convert that to a two column data.frame with stack, rearrange the columns and change the column names (if needed).
library(highcharter)
library(htmlwidgets) # to save

setwd("/Users/mariagranell/Desktop/Rcourses/TidyverseTuesday")

# --------------------------------------------------
# DATA GLOBAL RADIATION

d_glov5 <- fromJSON("/Users/mariagranell/Desktop/Rcourses/TidyverseTuesday/fiveyearsglobal.json")
d_glov5 <- as.data.frame(d_glov5)
head(d_glov5)
str(d_glov5)

# cleaning data
d_glov5[] <- lapply(d_glov5, gsub, pattern = "T00:00:00Z", replacement = "")
d_glov5$value <- ifelse(d_glov5$value < 0, 0, d_glov5$value)

# create collums of year, month and day.
d_glov5$split <- strsplit(d_glov5$date_time, "-") # you create a list call split in thedat frame
d_glov5[c("year", "month", "day")] <- t(data.frame(d_glov5$split)) # to store the list in diff collums
d_glov5 <- subset(d_glov5, select = -c(split)) # remove the list

# Determine the data type
d_glov5<-within(d_glov5,{
  month<-as.numeric(month)
  day<-as.numeric(day)
  year<-as.numeric(year)
  value<-as.numeric(value)
  date_time<-as.Date(date_time)
})

# --------------------------------------------------
# data just december
d_glov5dec <- subset(d_glov5, day < 11)
d_glov5dec <- subset(d_glov5dec, month == 12)
d_glov5dec$year <- as.factor(d_glov5dec$year)

# data just 2020
d_glov2020 <- subset(d_glov5, day < 11)
d_glov2020 <- subset(d_glov2020, year == 2020)
d_glov2020$month <- as.factor(d_glov2020$month)


# --------------------------------------------------
# ggplot
p <-ggplot(d_glov5dec, aes(x= day, y = value, fill = year, colour = year)) +
  labs(x = "Day", y = "Global radiation",
       title = "Global radiation for December",
       caption = "Data: SMHI") +
  geom_point()+
  scale_x_continuous(breaks = 1:10) +
  #scale_color_brewer(palette = "BrBG")
  #scale_color_tableau()
  #theme(plot.title.position = "plot",plot.caption.position = "plot")

#p + geom_ribbon(aes(ymin = 0, ymax = value))
stat_smooth(geom = "area", method = 'loess', span = 0.3,
                alpha = 1/2, se = FALSE)

cols <- brewer.pal(6, "RdYlBu")

# --------------------------------------------------
# Interactive area plot
hchart(d_glov5dec, "area", hcaes(x= day, y = value, group = year)) %>%
  hc_colors(cols) %>%
  hc_xAxis(title = list(text="Day")) %>%
  hc_yAxis(title = list(text="Global radiation")) %>%
  hc_title(
    text = "Why am I more despressed this year?") %>%
  hc_subtitle( text = "During the frist 10 days comparing 6 years") %>%
  hc_caption( text = "Data: SMHI", align = "right")
  #hc_add_theme(thm)

decglobarea
saveWidget(decglobarea, file=paste0( getwd(), "/decglobarea.html"))
 #thm <- hc_theme(
 #  chart = list(
 #    backgroundColor = "#dbd9d8"
 #  )#,
   #title = list(
   #  style = list(
   #    color = "#333333",
   #    fontFamily = "Erica One"
   #  )
   #),
   #subtitle = list(
   #  style = list(
   #    color = "#666666",
   #    fontFamily = "Shadows Into Light"
   #  )
   #),
   #legend = list(
   #  itemStyle = list(
   #    fontFamily = "Tangerine",
   #    color = "black"
   #  ),
   #  itemHoverStyle = list(
   #    color = "gray" )))

# --------------------------------------------------
# Ridge Plot
# library(ggridges)
ggplot(d_glov5dec, aes(x = value, y = factor(year))) +
  geom_density_ridges_gradient(scale = 2, gradient_lwd = .9)

# --------------------------------------------------
# DATA ON UV

d_uv <- fromJSON("/Users/mariagranell/Desktop/Rcourses/TidyverseTuesday/uv2017.json")
d_uv <- as.data.frame(d_uv)
head(d_uv)
str(d_uv)

# cleaning data
d_uv[] <- lapply(d_uv, gsub, pattern = "T00:00:00Z", replacement = "")
d_uv$value <- ifelse(d_uv$value < 0, 0, d_uv$value)

# create collums of year, month and day.
d_uv$split <- strsplit(d_uv$date_time, "-") # you create a list call split in thedat frame
d_uv[c("year", "month", "day")] <- t(data.frame(d_uv$split)) # to store the list in diff collums
d_uv <- subset(d_uv, select = -c(split)) # remove the list

# Determine the data type
d_uv<-within(d_uv,{
  month<-as.numeric(month)
  day<-as.numeric(day)
  year<-as.numeric(year)
  value<-as.numeric(value)
  date_time<-as.Date(date_time)
})

# --------------------------------------------------
# data just december
d_uvdec <- subset(d_uv, day < 17)
d_uvdec <- subset(d_uvdec, month == 12)

d_uvdec$year <- as.factor(d_uvdec$year)
head(d_uvdec)

cols <- brewer.pal(4, "RdBu")
# Interactive area plot
uv <-hchart(d_uvdec, "area", hcaes(x= day, y = value, group = year)) %>%
  hc_colors(cols) %>%
  hc_xAxis(title = list(text="Days in December")) %>%
  hc_yAxis(title = list(text="UV irradiance (mW/m²) in Stockholm")) %>%
  hc_title(
    text = "Why am I more despressed this year?") %>%
  hc_subtitle( text = "The absortion of UV radiation produce vitamin D3") %>%
  hc_caption( text = "Data: SMHI", align = "right") %>%
  #hc_add_theme(thm)%>%
hc_add_annotation(
  labels = list(
    list(
      point = list(
        xAxis =0 ,
        yAxis =0 ,
        x =d_uvdec$day[62],
        y = d_uvdec$value[62],
        xAxis=0,
        yAxis=0
      ),
      text = "Everyone taking 'sunny' <br/> pictures for instagram"
    )
  )
)
saveWidget(uv, file=paste0( getwd(), "/uv.html"))
