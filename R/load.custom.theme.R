
#ggplot custom
###

theme_custom <- function() {
  theme_bw() +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      axis.text=element_text(size=11, family="Arial"),
      axis.title=element_text(size=12,face="bold", family="Arial"),
      panel.grid=element_line(color = "grey80"),
      legend.position="bottom",
      legend.justification = c("right", "bottom"),
      legend.text.align = 0,
      legend.title=element_text(size=12),
      legend.text=element_text(size=12),
      legend.background = element_rect(colour = 'black', fill = 'white', size = 1, linetype='solid'),
      plot.title = element_text(size=18),
      plot.subtitle = element_text(size=14)
    )
}