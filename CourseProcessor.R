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
      ), drop = FALSE
    ) +
    theme(legend.position = "right",
          legend.text = element_text(size = 10))
  
  
  sumofmeans<-sum(c(mean(df_wide$below), mean(df_wide$meets), mean(df_wide$exceeds)))
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
    # geom_vline(
    #   aes(xintercept = round(mean(below), 0)),
    #   linetype = "solid",
    #   size = 1,
    #   color = "firebrick3"
    # ) +
    # geom_vline(
    #   aes(xintercept = round(mean(meets), 0)),
    #   linetype = "solid",
    #   size = 1,
    #   color = "goldenrod1"
    # ) +
    # geom_vline(
    #   aes(xintercept = round(mean(exceeds), 0)),
    #   linetype = "solid",
    #   size = 1,
    #   color = "chartreuse4"
    # ) +
    geom_label_repel(
      data = data.frame(mean = c(mean(df_wide$below), mean(df_wide$meets), mean(df_wide$exceeds))),
      xlim = c(-1, Inf),
      ylim = c(0, Inf),
      mapping = aes(
        x = mean,
        y = 0,
        label = paste0(round(mean, 0), "\n", 
                       round(mean/sumofmeans*100, 0), "%")
      ),
      fill = cc,
      size = 5,
      box.padding = unit(1, "lines")
    ) +
    # geom_label_repel(
    #   data = data.frame(mean = round(mean(df_wide$below), 0)),
    #   xlim = c(-1, Inf),
    #   ylim = c(0, Inf),
    #   mapping = aes(
    #     x = mean,
    #     y = 0.02,
    #     label = paste0(round(mean, 0))
    #   ),
    #   fill = "firebrick3",
    #   size = 5,
    #   box.padding = unit(1, "lines")
    # ) +
    # geom_label_repel(
    #   data = data.frame(mean = round(mean(df_wide$meets), 0)),
    #   xlim = c(-1, Inf),
    #   ylim = c(0, Inf),
    #   mapping = aes(
    #     x = mean,
    #     y = 0.02,
    #     label = paste0(round(mean, 0))
    #   ),
    #   fill = "goldenrod1",
    #   size = 5,
    #   box.padding = unit(1, "lines")
    # ) +
    # geom_label_repel(
    #   data = data.frame(mean = round(mean(df_wide$exceeds), 0)),
    #   xlim = c(-1, Inf),
    #   ylim = c(0, Inf),
    #   mapping = aes(
    #     x = mean,
    #     y = 0.02,
    #     label = paste0(round(mean, 0))
    #   ),
    #   fill = "chartreuse4",
    #   size = 5,
    #   box.padding = unit(1, "lines")
    # ) +
    scale_x_continuous(breaks = seq(0, xscale, 10),
                       limits = c(0, xscale)) +
    labs(x = "Students", y = "", title = paste0(course_code, " Student Success Densities"),
         subtitle="Expected number of students below, meeting, or exceeding expectations") +
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
