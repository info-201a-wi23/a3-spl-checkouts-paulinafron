#Set the working directory
setwd("~/Desktop/INFO201/a3-spl-checkouts-paulinafron")

#Load packages
library("dplyr")
library("tidyverse")
library("ggplot2")

#Load the data
library_checkouts <- read_csv("Checkouts_by_Title.csv")

# Summary Information
# What is the average number of checkouts for each item (digital vs physical)?
average_checkouts <- library_checkouts %>% 
                        group_by(UsageClass) %>% 
                        summarise(avg_checkouts = mean(Checkouts))

#What is the average number of checkouts for each item (digital vs physical) before, 
  # during and after lockdown?
checkouts_before_lockdown <- library_checkouts %>% 
  group_by(UsageClass) %>% 
  filter(((CheckoutMonth == 12) & (CheckoutYear == 2019)) | 
           ((CheckoutMonth < 3) & (CheckoutYear == 2020))) %>% 
  summarise(avg_checkouts = mean(Checkouts))

checkouts_during_lockdown <- library_checkouts %>% 
  group_by(UsageClass) %>% 
  filter((2 < CheckoutMonth) & (CheckoutMonth < 6) & (CheckoutYear == 2020)) %>% 
  summarise(avg_checkouts = mean(Checkouts))

checkouts_after_lockdown <- library_checkouts %>% 
  group_by(UsageClass) %>% 
  filter((5 < CheckoutMonth) & (CheckoutMonth < 9) & (CheckoutYear == 2020)) %>% 
  summarise(avg_checkouts = mean(Checkouts))      

#What is the year and month of that year with the most checkouts for ebooks?
most_checkouts_date <- library_checkouts %>% 
  filter(MaterialType == 'EBOOK') %>% 
  group_by(CheckoutYear, CheckoutMonth) %>% 
  summarise(total_checkouts = sum(Checkouts)) %>% 
  arrange(desc(total_checkouts)) %>% 
  ungroup() %>%
  slice_head() 

#What is the most popular title to be checked out? Was it a book or an ebook?
most_popular_title <- library_checkouts %>% 
  group_by(Title, MaterialType) %>% 
  summarise(total_checkouts = sum(Checkouts)) %>% 
  arrange(desc(total_checkouts)) %>% 
  ungroup() %>%
  slice_head(n = 3)

# What was the most popular title before, during, after lockdown?
popular_before_lockdown <- library_checkouts %>% 
  group_by(Title, MaterialType) %>% 
  filter(((CheckoutMonth == 12) & (CheckoutYear == 2019)) | 
           ((CheckoutMonth < 3) & (CheckoutYear == 2020))) %>% 
  summarise(total_checkouts = sum(Checkouts)) %>% 
  arrange(desc(total_checkouts)) %>% 
  ungroup() %>%
  slice_head(n = 3)
  
popular_during_lockdown <- library_checkouts %>% 
  group_by(Title, MaterialType) %>% 
  filter((2 < CheckoutMonth) & (CheckoutMonth < 6) & (CheckoutYear == 2020)) %>% 
  summarise(total_checkouts = sum(Checkouts)) %>% 
  arrange(desc(total_checkouts)) %>% 
  ungroup() %>%
  slice_head(n = 3)

popular_after_lockdown <- library_checkouts %>% 
  group_by(Title, MaterialType) %>% 
  filter((5 < CheckoutMonth) & (CheckoutMonth < 9) & (CheckoutYear == 2020)) %>% 
  summarise(total_checkouts = sum(Checkouts)) %>% 
  arrange(desc(total_checkouts)) %>% 
  ungroup() %>%
  slice_head(n = 3)

popular_after_lockdown$Title

