#Set the working directory
setwd("~/Desktop/INFO201/a3-spl-checkouts-paulinafron")

#Load packages
library("dplyr")
library("tidyverse")
library("ggplot2")

#Load the data
library_checkouts <- read_csv("Checkouts_by_Title.csv")

# Filter out the data and add Month variable
values_chart_3 <- library_checkouts %>% 
  filter(CheckoutYear == 2020) %>% 
  group_by(UsageClass, CheckoutMonth) %>% 
  summarise(total_checkouts = sum(Checkouts)) %>% 
  mutate(Month = case_when(
    CheckoutMonth == 1 ~ "Jan",
    CheckoutMonth == 2 ~ "Feb",
    CheckoutMonth == 3 ~ "Mar",
    CheckoutMonth == 4 ~ "Apr",
    CheckoutMonth == 5 ~ "May",
    CheckoutMonth == 6 ~ "Jun",
    CheckoutMonth == 7 ~ "Jul",
    CheckoutMonth == 8 ~ "Aug",
    CheckoutMonth == 9 ~ "Sep",
    CheckoutMonth == 10 ~ "Oct",
    CheckoutMonth == 11 ~ "Nov",
    CheckoutMonth == 12 ~ "Dec")) %>% 
  mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 
              "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE))

# Plot the data
chart3 <- ggplot(values_chart_3, aes(x = Month, y = total_checkouts, fill = UsageClass)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Checkouts by Month and Usage Class in 2020",
       x = "Month", y = "Total Checkouts", fill = "Item Type")

