theme_matt <- function(){
        theme(panel.border = element_blank(), panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "slategray"),
              panel.grid = element_blank(),
              axis.line.x = element_line(color = "black"), 
              axis.line.y = element_line(color = "black"),
              plot.title = element_text(hjust = 0.5, size = 13),
              legend.key = element_blank(),
              legend.title.align = 0.4,
              text = element_text(family = "sans"), 
              strip.background = element_rect(color = "black", fill = "white"),
              strip.text = element_text(size = 10, face = "bold"),
              strip.text.x = element_text(size = 10),
              strip.text.y = element_text(size = 10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10), 
              axis.title = element_text(size = 10), 
              axis.text = element_text(size = 10, color = "black"))
}