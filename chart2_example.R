#Set the working directory
setwd("~/Desktop/INFO201/a3-spl-checkouts-paulinafron")

#Load packages
library("dplyr")
library("tidyverse")
library("ggplot2")

#Load the data
library_checkouts <- read_csv("Checkouts_by_Title.csv")

values_chart_2 <- library_checkouts %>% filter((Title == "Educated: A Memoir") | 
                                         (Title == "Where the Crawdads Sing") | 
                                         (Title == "Becoming") | 
  (Title == "White Fragility: Why It's So Hard for White People to Talk About Racism") |
  (Title == "Me and White Supremacy: Combat Racism, Change the World, and Become a Good Ancestor") |
  (Title == "How to Be an Antiracist")) %>% 
  group_by(Title, CheckoutYear) %>%
  summarise(total = sum(Checkouts))

chart2 <- ggplot(data = values_chart_2) +
  geom_line(aes(x = CheckoutYear, y = total, color = Title)) +
  labs(x = "Total checkouts", y = "Year", title = "Total checkouts per year - popular titles", colour = "Title") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 6, byrow = TRUE))
