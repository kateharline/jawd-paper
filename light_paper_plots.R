library(RColorBrewer)
# general aesthetic guidelines https://ggplot2.tidyverse.org/articles/ggplot2-specs.html#point-1
# viewing palettes https://stackoverflow.com/questions/25726276/visualize-a-list-of-colors-palette-in-r
library(scales)
# for checking colors
library(colorBlindness)
# scale theory https://ggplot2-book.org/scale-colour.html
library(viridis)
# prism themes
library(ggprism)

# themes https://ggplot2-book.org/polishing.html
dir.create(file.path(dot_dir, 'plots'))

small_text_size <- 10
med_text_size <- 12

# custom palettes http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
time_palette <- viridis::viridis(7, direction = -1)
# real hex
time_palette <- substr(time_palette, 1, nchar(time_palette)-2)

lo_colors <- c("#F8FAE8", "#EDF6E8", "#DBEFEA", 
               "#D1E8EC","#CBDBEB", "#C7CAE3",
               "#C3BFD2")

purple_pal <- c('#0E0A48', '#201A78', '#3C33BD', '#463CDA', '#6D63CE', '#9289E9', '#9C95E8', '#ADA7E9', '#B6B0E6', 
                '#B9B6D2', '#DBDAEA')

# view
colorBlindness::displayAllColors(time_palette)

outline_color <- 'black'

dpi <- 300

# palette gradients https://ab604.github.io/docs/colour-palette-tutorial-10-11-2018.html#creating-a-set-of-gradients-from-a-custom-palette
#time_gradients <- scale_color_gradient(low = time_palette[i], high = white)

# shapes to use... wt, jawD
shapes <- c(22, 21)
shapes_solid <- c(15, 16)
# line types to use... wt, jawD
linetypes <- c(1, 3)

save_light_graph <- function(plt_name, plt, dims = c()) {
  plt <- plt +
    theme_classic() +
    theme(panel.grid.major = element_line(linetype = "dotted"),
          aspect.ratio = 1,
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = small_text_size),
          legend.position = 'none',
          # https://stackoverflow.com/questions/29303577/getting-rid-of-facet-grid-labels-on-those-gray-boxes
          # faceting hide box labels
          strip.text.y = element_blank(),
          strip.text.x = element_blank()) +
    # specific legend hide https://stackoverflow.com/questions/35618260/remove-legend-ggplot-2-2
    guides(color="none")
  # 
  if (is_empty(dims)) {
    ggsave(file.path(dot_dir, 'plots', paste0(plt_name, '.png')), device = 'png', height = 3,
           width = 3, units = 'in', dpi = dpi, plot = plt)
  }
  else {
    ggsave(file.path(dot_dir, 'plots', paste0(plt_name, '.png')), device = 'png', height = dims[1],
           width = dims[2], units = 'in', dpi = dpi, plot = plt)
  }
  

  
}


