table_processor <- function(c, filename, course_code) {
  library(readr)
  library(tidyverse)
  library(cowplot)
  library(ggrepel)
  library(svglite)
  library(reshape2)
  library(hash)
  library(gridExtra)
  source("Util.R")
  primary <- read.csv(
    filename,
    colClasses = c(
      "character",
      "character",
      "character",
      "integer",
      "integer",
      "integer"
    )
  )
  
  clo <- c
  
  df <- subset(primary, clonum == clo & course == course_code)
  sections <- df$section
  
  df2 <-
    data.frame(
      section = character(),
      score = factor(levels = c("1", "2", "3")),
      desc = character(),
      stringsAsFactors = TRUE
    )
  
  for (i in sections) {
    if (subset(df, section == i)$below > 0) {
      for (j in 1:subset(df, section == i)$below) {
        df2 <-
          rbind(df2,
                data.frame(
                  section = i,
                  score = "1",
                  desc = "Below Expectations"
                ))
      }
    }
    if (subset(df, section == i)$meets > 0) {
      for (j in 1:subset(df, section == i)$meets) {
        df2 <-
          rbind(df2,
                data.frame(
                  section = i,
                  score = "2",
                  desc = "Meets Expectations"
                ))
      }
    }
    if (subset(df, section == i)$exceeds > 0) {
      for (j in 1:subset(df, section == i)$exceeds) {
        df2 <-
          rbind(df2,
                data.frame(
                  section = i,
                  score = "3",
                  desc = "Exceeds Expectations"
                ))
      }
    }
  }
  
  #print(table(df2$desc, df2$section))
  
  #p0 <- addmargins(prop.table(table()))
  
  ndata<-select(df, section, below, meets, exceeds)
  totals <- c("Total", sum(df$below), sum(df$meets), sum(df$exceeds))

  ndata <- rbind(ndata, totals)

  ndata <- transform(ndata, below = as.numeric(below))
  ndata <- transform(ndata, meets = as.numeric(meets))
  ndata <- transform(ndata, exceeds = as.numeric(exceeds))

  ndata <- ndata %>% mutate(total=rowSums(select_if(., is.numeric)))

  ndata<-rename(ndata, 
                "Course Section"=section, 
                "Below Expectations"=below, 
                "Meets Expectations"=meets, 
                "Exceeds Expectations"=exceeds,
                "Total"=total)

  tt3 <- ttheme_default()
  tbl <- tableGrob(ndata, rows=NULL, theme=tt3)
  figure <- plot_grid(tbl, nrow = 1)
  
  
}