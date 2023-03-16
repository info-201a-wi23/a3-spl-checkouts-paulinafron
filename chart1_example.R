#Set the working directory
setwd("~/Desktop/INFO201/a3-spl-checkouts-paulinafron")

#Load packages
library("dplyr")
library("tidyverse")
library("ggplot2")

#Load the data
library_checkouts <- read_csv("Checkouts_by_Title.csv")

values_chart_1 <- library_checkouts %>% 
  group_by(UsageClass, CheckoutYear) %>%
  summarise(total = sum(Checkouts))

chart1 <- ggplot(data = values_chart_1) +
  geom_line(aes(x = CheckoutYear, y = total, color = UsageClass)) +
  labs(x = "Total checkouts", y = "Year", title = "Total checkouts per year", colour = "Item type") +
  theme(legend.position = "bottom")
