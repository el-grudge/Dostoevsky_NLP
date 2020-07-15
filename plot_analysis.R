#define some colors to use throughout
my_colors <- list(
  "Poor Folk"="deepskyblue3", 
  "Notes from the Underground" = "darkred", 
  "Crime and Punishment" = "goldenrod", 
  "The Brothers Karamazov" = "darkgreen"
  )

theme_lyrics <- function() 
{
  theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
}

# start with sentiment plot


# dct transform plot
plot_syuzhet <- list()
for (title in unique(dostoevsky_DCT$title)){
  plot_syuzhet[[title]] <- ggplot(dostoevsky_DCT[dostoevsky_DCT$title==title,]) +
    aes(x=n, y=dct_values) +
    geom_line(color=my_colors[[title]]) +
    geom_hline(yintercept = 0, lty=2) +
    labs(x=NULL, y=NULL, title=title) +
    theme_lyrics()
}

grid.arrange(
  plot_syuzhet$`Poor Folk`, 
  plot_syuzhet$`Notes from the Underground`, 
  plot_syuzhet$`Crime and Punishment`, 
  plot_syuzhet$`The Brothers Karamazov`,
  nrow = 2, ncol = 2, 
  top = textGrob("Story Arc",gp=gpar(fontsize=20)), 
  left = textGrob("Sentiment",gp=gpar(fontsize=20), rot=90))
