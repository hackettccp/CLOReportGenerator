processor <- function(c, filename, course_code) {
  library(readr)
  library(tidyverse)
  library(cowplot)
  library(ggrepel)
  library(svglite)
  library(reshape2)
  library(hash)
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
  cc <- hash()
  cc[["Below Expectations"]] <- "firebrick3"
  cc[["Meets Expectations"]] <- "goldenrod1"
  cc[["Exceeds Expectations"]] <- "chartreuse4"
  
  #Section Dist
  p1 <- ggplot(data = df2, aes(x = section)) +
    geom_bar(aes(fill = desc), position = "fill") +
    labs(subtitle = "Course Sections", x = "", y = "") +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 10)
    ) +
    geom_hline(aes(yintercept = .75), linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(
      values = values(cc, levels(df2$desc), USE.NAMES = FALSE),
      name = "",
      labels = levels(df2$desc)
    ) +
    theme(legend.position = "right",
          legend.text = element_text(size = 10))
  
  #Course Dist
  p2 <- ggplot(data = df2, aes(x = "", fill = desc)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = values(cc, levels(df2$desc), USE.NAMES =
                                        FALSE),
                      name = "") +
    labs(x = "", y = "", subtitle = "All Sections") +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(aes(yintercept = .75), linetype = "dashed") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 10)
    )
  
  
  #means <-
  #  data.frame(
  #    section = character(),
  #    mn = numeric(),
  #    std = numeric(),
  #    stringsAsFactors = FALSE
  #  )
  #for (i in sections) {
  #  tempdf <- subset(df, section == i)
  #  tempmean <-
  #    (tempdf$below[1] + tempdf$meets[1] * 2 + tempdf$exceeds[1] * 3) / (tempdf$below[1] + tempdf$meets[1] + tempdf$exceeds[1])
  #  tempvar <-
  #    var(as.numeric(as.character(subset(df2, section == i)$score)))
  #  tempsd <- sqrt(tempvar)
  #  means <- rbind(means,
  #                 data.frame(
  #                   section = i,
  #                   mn = tempmean,
  #                   std = tempsd
  #                 ))
  #}
  #p3 <- dens(df, df2, xscale, cc)
  
  
  #if (length(df$below) < 2) {
    figure <- plot_grid(p2, p1, nrow = 1, rel_widths = c(.3, 1))
  #}
  #else {
  #  figure <-
  #    plot_grid(
  #      p1,
  #      plot_grid(p2, p3, ncol = 2, rel_widths = c(2, 4)),
  #      nrow = 2,
  #      rel_heights = c(1, 1.25)
  #    )
  #}
  return(figure)
  #ggsave(figure, file=paste0(course_code, "_CLO", clo, ".png"), width = 18, height = 15, units = "cm")
  
  
}

dens <- function(df, df2, xscale, cc) {
  p <- ggplot(data = df) +
    geom_density(aes(x = below, fill = "below"),
                 alpha = 0.75,
                 color = NA) +
    geom_density(aes(x = meets, fill = "meets"),
                 alpha = 0.75,
                 color = NA) +
    geom_density(aes(x = exceeds, fill = "xceeds"),
                 alpha = 0.75,
                 color = NA) +
    scale_x_continuous(breaks = seq(0, xscale[1], 10),
                       limits = c(0, xscale[1])) +
    labs(x = "Discrete Assessments", y = "", subtitle = "Assessment Density") +
    scale_fill_manual(values = values(cc, levels(df2$desc), USE.NAMES =
                                        FALSE),
                      name = "") +
    theme(legend.position = "none",
          legend.text = element_text(size = 10))
  return(p)
}


zplot <- function(means) {
  z <- (2 - means$mn) / means$std
  p <- ggplot(data = means, aes(x = as.numeric(as.character(mn)))) +
    stat_function(
      fun = sdrange,
      args = list(n = 3, m = 0, s = 1),
      geom = "area",
      fill = "goldenrod3"
    ) +
    stat_function(
      fun = sdrange,
      args = list(n = 2, m = 0, s = 1),
      geom = "area",
      fill = "goldenrod2"
    ) +
    stat_function(
      fun = sdrange,
      args = list(n = 1, m = 0, s = 1),
      geom = "area",
      fill = "goldenrod1"
    ) +
    geom_vline(
      aes(xintercept = 0),
      linetype = "solid",
      size = .75,
      alpha = 0.35
    ) +
    scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3)) +
    labs(
      x = "",
      y = "",
      subtitle = "Z scores",
      caption = "x = Meets Expectations (2)"
    ) +
    geom_vline(
      aes(xintercept = z),
      linetype = "dashed",
      size = .5,
      alpha = 0.35
    ) +
    geom_label_repel(
      data = means,
      xlim = c(-3, 3),
      ylim = c(0, .5),
      mapping = aes(
        x = z,
        y = .2,
        label = paste0(section, "\n", round(z, 2))
      ),
      size = 3,
      box.padding = unit(1.5, "lines")
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
  return(p)
}
