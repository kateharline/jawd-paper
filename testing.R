ToothGrowth$dose <- as.factor(ToothGrowth$dose)
mtcars$cyl <- as.factor(mtcars$cyl)
head(ToothGrowth)

head(mtcars)


library(ggplot2)
library(RColorBrewer)
library(ggdark)

plts <- list()

plts[[1]] <- ggplot(ToothGrowth, aes(x=dose, y=len, color = len)) +geom_boxplot()
plts[[2]] <- ggplot(mtcars, aes(x=wt, y=mpg, color =qsec)) + geom_point()

for (i in 1:length(plts)) {
  plts[[i]] <- plts[[i]] + 
    #scale_color_fermenter(palette = "Blues", aesthetics = c("fill", "color")) + 
    dark_theme_classic() 
  plts[[i]]
}

mtcars$gear <- as.factor(mtcars$gear)

mtcars$forColor <- paste0(mtcars$gear, "_", mtcars$qsec)

plt <- ggplot(mtcars, aes(x=wt, y=mpg, color =forColor)) + geom_point() +
  scale_color_gradient(low = time_palette[3], high = white) +
  facet_wrap(~ gear) 
  #scale_color_manual(values = c(scale_color_gradient(low = time_palette[3], high = white),
   #                             scale_color_gradient(low = time_palette[4], high = white),
    #                            scale_color_gradient(low = time_palette[5], high = white)))

plt





for(i in 1:length(csv_names)) {
  # why aren't col names the same
  print(length(colnames(csvs[[i]])))
}
