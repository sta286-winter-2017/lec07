library(tidyverse)

plot_cdf <- function(pmf) {
  cdf <- pmf
  cdf$y <- cumsum(cdf$y)

  padding <- 1.618*mean(diff(pmf$x))   
  before <- data_frame(x=min(pmf$x) - padding, y=0)
  after <- data_frame(x=max(pmf$x) + padding, y=max(cdf$y))
  
  cdf <- bind_rows(before, cdf, after)
  cdf$xend <- c(cdf$x[2:nrow(cdf)], NA)
  cdf$yend <- cdf$y

  cdf %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_point(size=2) +
    geom_point(aes(x = xend, y = y), shape = 1, size=3) +
    geom_segment() +
    geom_segment(aes(x=before$x- 0.05, y=before$y, 
                     xend=min(pmf$x), yend=before$y),
                 arrow=arrow(length=unit(0.15, "inches"), 
                             ends="first", type = "closed")) + 
    geom_segment(aes(x=max(pmf$x), y=after$y, 
                     xend=after$x + 0.05, yend=after$y),
                 arrow=arrow(length=unit(0.15, "inches"), 
                             ends="last", type = "closed")) + 
    geom_hline(aes(yintercept = 0), linetype="dotted") + 
    geom_hline(aes(yintercept = 1), linetype="dotted") +
    ylab(expression(P(X <= x))) + 
    scale_x_continuous(breaks=c(min(pmf$x)-1, pmf$x, max(pmf$x)+1)) + 
    theme_classic() + 
    theme(axis.text = element_text(size=14, colour="black"),
          axis.title = element_text(size=14, face="bold"))
}  

