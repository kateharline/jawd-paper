library(RColorBrewer)
library(ggdark)

for (i in 1:length(plts)) {
  plts[[i]] <- plts[[i]] + dark_theme_classic()
  
  ggsave(paste0(dot_dir, file, '.png'), height = 10)
}