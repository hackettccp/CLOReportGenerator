course_processor <- function(filename, course_code) {
  library(readr)
  library(tidyverse)
  library(cowplot)
  library(ggrepel)
  library(svglite)
  library(reshape2)
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
  
  df <- subset(primary, course == course_code)
  clonums <- unique(df$clonum)
  
  
  df2 <- data.frame(
    clonum = clonums,
    below = vector(mode = "numeric", length(clonums)),
    meets = vector(mode = "numeric", length(clonums)),
    exceeds = vector(mode = "numeric", length(clonums)),
    stringsAsFactors = FALSE
  )
  
  for (i in clonums) {
    for (j in subset(df, clonum == i)$below) {
      df2[i, "below"] <- df2[i, "below"] + j
    }
    for (j in subset(df, clonum == i)$meets) {
      df2[i, "meets"] <- df2[i, "meets"] + j
    }
    for (j in subset(df, clonum == i)$exceeds) {
      df2[i, "exceeds"] <- df2[i, "exceeds"] + j
    }
  }
  
  xscale <-
    max(apply(
      X = select(df2, "below", "meets", "exceeds"),
      MARGIN = 1,
      FUN = max
    )) + 10
  df_wide <- df2
  df2 <- melt(df2, id.vars = "clonum")
  
  cc <- c("firebrick3", "goldenrod1", "chartreuse4")
  p1 <- ggplot(data = df2, aes(x = clonum)) +
    geom_bar(aes(fill = variable, y = value),
             position = "fill",
             stat = "identity") +
    labs(title = "Course Learning Outcome Distributions", x = "Course Learning Outcomes", y =
           "") +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 10)
    ) +
    geom_hline(aes(yintercept = .75), linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(
      values = cc,
      name = "",
      labels = c(
        "Below Expectations",
        "Meets Expectations",
        "Exceeds Expectations"
      )
    ) +
    theme(legend.position = "right",
          legend.text = element_text(size = 10))
  
  
  
  p2 <- ggplot(data = df_wide) +
    geom_density(aes(x = below, fill = "below"),
                 alpha = 0.75,
                 color = NA) +
    geom_density(aes(x = meets, fill = "meets"),
                 alpha = 0.75,
                 color = NA) +
    geom_density(aes(x = exceeds, fill = "xceeds"),
                 alpha = 0.75,
                 color = NA) +
    scale_x_continuous(breaks = seq(0, xscale, 10),
                       limits = c(0, xscale)) +
    labs(x = "Discrete Assessments", y = "", title = "Outcome Density") +
    scale_fill_manual(
      values = cc,
      name = "",
      labels = c(
        "Below Expectations",
        "Meets Expectations",
        "Exceeds Expectations"
      )
    ) +
    theme(legend.position = "none",
          legend.text = element_text(size = 10))
  
  figure <- plot_grid(p1, p2, nrow = 2)
  return(figure)
}
