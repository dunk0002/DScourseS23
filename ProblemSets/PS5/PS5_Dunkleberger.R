#!/usr/bin/env Rscript

# web scraping practice
library(rvest)
goalie_stats <- read_html("https://www.icydata.hockey/goalie_stats/36")
goalie_stats_table <- goalie_stats %>% 
  html_element("#goalie-stats") %>% 
  html_table() 
head(goalie_stats_table)

# API using hockeyR
install.packages("devtools")
install.packages("sportyR")
devtools::install_github("danmorse314/hockeyR")
pbp <- load_pbp('2018-2019')
library(hockeyR)
library(sportyR)
pbp <- load_pbp('2018-2019')
head(pbp)
