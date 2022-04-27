library(ggplot2)
# melt
library(reshape2)
# filter
library(dplyr)
# palettes and themes
library(RColorBrewer)
# formatting tick marks
library(scales)
# for dynamic mutate?
library(glue)
# for statistics 
library(ggpubr)
# for is_empty
library(purrr)
# for pivot
library(tidyr)


# figure 1
plot_small_scatter <- function(summary, time_palette, shapes, y) {
  # must group for proper dodge
  plt <- summary %>% group_by(cond) %>% 
    ggplot(aes_string(x = 'time', y = y, shape = 'cond', fill = 'time', alpha = 'cond')) + 
    geom_point(position = position_jitterdodge(), size = 5, color = outline_color, stroke = 1) +
    # choose shapes https://blog.albertkuo.me/post/point-shape-options-in-ggplot/
    scale_shape_manual(values = shapes) +
    scale_y_continuous(labels = scientific) +
    # draw_key_point(color = 'black') +
    # make time scales same https://stackoverflow.com/questions/38238960/how-do-you-control-color-using-facet-wrap-and-ggplot
    # custom palette
    scale_color_manual(values = time_palette) +
    scale_fill_manual(values = time_palette) +
    scale_alpha_manual(values = c(1, 0.5))
  
  save_light_graph(y, plt)
}

# iteration help https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
plot_small_scatter_helper <- function(summary, time_palette, shapes, ys) {
  for (i in 1:length(ys)) {
    plot_small_scatter(summary, time_palette, shapes, ys[i])
  }
}

# small boxplots
# https://cran.r-project.org/web/packages/ggprism/vignettes/pvalues.html
small_box <- function(d, measure) {
  d <- d %>% group_by(cond) %>%
    mutate(outlier.high = !!sym(measure) > quantile(!!sym(measure), .75) + 1.5*IQR(!!sym(measure)),
           outlier.low = !!sym(measure) < quantile(!!sym(measure), .25) - 1.5*IQR(!!sym(measure)))
  # hacky hide outliers
  d$for_alpha <- if_else(d['outlier.high'] | d['outlier.low'], .7, 0)
  
  plt <- d %>% group_by(cond) %>%
    ggplot(aes_string(x = 'cond', y = measure, 
                      linetype = 'cond', alpha = 'cond', shape = 'cond')) +
    scale_shape_manual(values = c(shapes[1], shapes[2])) +
    scale_linetype_manual(values = linetypes) +
    scale_alpha_manual(values = c(0.7, 0.3)) +
    geom_violin(fill = outline_color) +
    geom_point(position = position_jitterdodge(), size = 3, alpha = d$for_alpha,
               color = outline_color, fill = outline_color)
  # geom_boxplot(outlier.shape = NA, color = outline_color, fill = outline_color) +
  
  return(plt)
}

small_box_helper <- function(d, measures) {
  for(i in 1:length(measures)){
    plt <- small_box(d, measures[i])
    save_light_graph(paste0('big_leaf_', measures[i]), plt)
  }
}
# figure 2 
# - bubble plots showing division and growth per cell per days 3-6 for all together
# - bubble plots showing division and growth per cell per days 3-6 for each example
plot_bubbles <- function(bubble_df, slicing_var, v_slice, x_axis, y_axis, hi_color, 
                         lo_color, slice_shape) {
  plot_d <- bubble_df[bubble_df[slicing_var] == v_slice,]
  plt <- plot_d %>% ggplot(aes_string(x= x_axis, y = y_axis,
                                      size = 'd_Proliferation',
                                      fill = 'd_Area', color = 'd_Area')) +
    geom_point(shape = slice_shape, alpha = 0.3) +
    # to handle outliers, use oob ... cheat way to display most of the data with nicer colors
    scale_fill_gradient(low = lo_color, high = hi_color, limits = c(0, 400), 
                        oob = oob_squish) +
    scale_color_gradient(low = lo_color, high = hi_color, limits = c(0, 400), 
                         oob = oob_squish) +
    scale_size_area(limits = c(0, 4), oob = oob_squish) +
    xlim(c(0,1)) +
    ylim(c(0,1))
  return(plt)
}

plot_bubbles_helper <- function(slices, slicing_var, identifier, d, x_axis, y_axis, time_palette, 
                                lo_colors, shapes) {
  for (i in 1:length(slices)) {
    # use a different base color for each day
    for (t in 1:length(unique(d[d[slicing_var] == slices[i],]$time))) {
      t_d <- d[d$time == t+2,]
      plt <- plot_bubbles(t_d, slicing_var, slices[i], 
                          x_axis, y_axis, time_palette[t], lo_colors[t], shapes[i])
      p_name <- paste0(slices[i], '_', 'DAS ', t+2, identifier)
      save_light_graph(p_name, plt)
    }
    
  }
}

plot_dens <- function(d, slicing_var, v_slice, x_axis, y_axis, hi_color, 
                      lo_color, slice_shape, linetype) {
  plot_d <- d[d[slicing_var] == v_slice,]
  plt <- plot_d %>% ggplot(aes_string(x= x_axis, y = y_axis, fill = 'd_Area')) +
    geom_density_2d_filled(colour = outline_color, fill = d_Area) +
    # scale_fill_distiller(palette = time_palette) +
    xlim(c(0,1)) +
    ylim(c(0,1))
  return(plt)
}
# figures 3-5
# function to make binary boxplots of measures between different axes in wt v jawd ex: 
# proximal-distal, d - dataframe, d_..._name - string naming for range
# group_cut - double where to cut the binary location (i.e. .5 distance)
# measure - string which cell measure to display for given cut

binnn <- function(d, cut_column_name, group_cut, d_max_name, d_min_name) {
  d <- d %>% mutate(bin = if_else(!!sym(cut_column_name) 
                                  >= group_cut, d_max_name, d_min_name))
  d$bin <- factor(d$bin, levels = c(d_min_name, d_max_name))
  # add factored column for graph grouping
  d$cond_bin <- paste0(d$cond, d$bin)
  d$cond_bin <- factor(d$cond_bin, levels = c(paste0('wt',d_min_name), paste0('wt',d_max_name),
                                              paste0('jawD',d_min_name), paste0('jawD',d_max_name)))
  
  return(d)
}

plot_bin_box <- function(d, measure, time_palette, shapes) {
  # add column for the binary
  # help with dynamics from https://stackoverflow.com/questions/60895225/dplyr-mutate-with-an-ifelse-using-dynamic-variable-names
  # calculate outliers
  d <- d %>% group_by(cond_bin, time) %>% 
    mutate(outlier.high = !!sym(measure) > quantile(!!sym(measure), .75) + 1.5*IQR(!!sym(measure)),
           outlier.low = !!sym(measure) < quantile(!!sym(measure), .25) - 1.5*IQR(!!sym(measure)))
  # hacky hide outliers
  d$for_alpha <- if_else(d['outlier.high'] | d['outlier.low'], .8, 0)
  bins <- levels(d$bin)
  d$for_alpha[d$for_alpha & d$bin == bins[2]] <- .3
  
  plt <- d %>% group_by(cond_bin) %>%
    ggplot(aes_string(x = 'time', y = measure, fill = 'time', 
                      linetype = 'cond', alpha = 'bin', shape = 'cond_bin')) +
    scale_fill_manual(values = time_palette) +
    scale_color_manual(values = time_palette) +
    scale_shape_manual(values = c(shapes[1], shapes[1], shapes[2], shapes[2])) +
    scale_alpha_manual(values = c(0.8, .3)) +
    scale_linetype_manual(values = linetypes) +
    geom_point(position = position_jitterdodge(), size = 1, alpha = d$for_alpha) +
    #data = function(x) dplyr::filter_(x, ~ outlier.low)
    #geom_point(position = position_jitterdodge(), size = 1) +
    geom_boxplot(outlier.shape = NA, color = outline_color)
  return(plt)
  
}

plot_bin_box_helper <- function(d, axis_closes, axis_fars, cuts, axes, measures, 
                                time_palette, shapes, identifier='') {
  # axes
  for (i in 1:length(axes)) {
    # measures
    d <- binnn(d, axes[i], cuts[i], axis_fars[i], axis_closes[i])  # factor in correct order
    
    for(j in 1:length(measures)) {
      plt<- plot_bin_box(d, measures[j], time_palette, shapes)
      save_light_graph(paste0(axis_closes[i], '-', axis_fars[i], '_', measures[j], '_',
                              identifier), plt)
    }
  }
  
}

# scatter/linear plots of measures v location axes
plot_scatter <- function(d, x, y, time_palette, shapes, identifier = '', linetypes){
  plt <- d %>% ggplot(aes_string(x = x, y = y, shape = 'cond', fill = 'time',
                                 linetype = 'cond')) +
    geom_point(alpha = 0.4, color = outline_color) +
    # line of fit https://bookdown.org/dli/rguide/scatterplots-and-best-fit-lines-two-sets.html
    geom_smooth(method = 'lm', color = outline_color, fill = outline_color, alpha = 0.6) +
    facet_grid(rows = vars(cond), cols = vars(time)) +
    scale_fill_manual(values = time_palette) +
    scale_shape_manual(values = shapes) +
    scale_linetype_manual(values = linetypes)
  save_light_graph(identifier, plt, c(3, 8))
}

custom_pal <- function(low_col, high_col, num) {
  return(scales::seq_gradient_pal(low_col, high_col, "Lab")(seq(0,1,length.out=num)))
}

plot_dens <- function(d, x, y, pale, linetype, bin_num, y_lims, identifier = '') {
  # https://ggplot2.tidyverse.org/reference/geom_density_2d.html
  plt2 <- d %>% ggplot(aes_string(x = x, y = y)) +
    geom_density_2d_filled(colour = outline_color, bins = bin_num, linetype = linetype) +
    scale_fill_manual(values = pale) +
    scale_linetype_manual(values = c(linetype)) + 
    ylim(y_lims)
  save_light_graph(paste0('dens_',identifier), plt2)
}

plot_dens_helper <- function(d, conds, axes, measures, lo_colors, time_palette, linetypes, bin_num = 7, ylims = matrix(),
                             identifier='') {
  
  if (all(is.na(ylims))) {
    ylims <- matrix(nrow = length(measures), ncol = 2)
    for(m in 1:length(measures)) {
      ylims[m,] <- c(min(d[measures[m]]), max(d[measures[m]]))
    }
  }
  for(c in 1:length(conds)){
    df <- d[d$cond == conds[c],] 
    for (m in 1:length(measures)){
      for (t in 1:length(unique(df$time))) {
        # make palette
        pal <- custom_pal('white', time_palette[t], bin_num)
        for(a in 1:length(axes)) {
          plot_dens(df[df$time == as.character(t+2),], axes[a], measures[m], pal,
                    linetypes[c], bin_num, ylims[m,],
                    identifier = paste0(identifier, '_', measures[m], '_', axes[a], '_', conds[c], '_', as.character(t+2), '_DAS'))
          
        }
      }
    }
  }
  
}

plot_scatter_helper <- function(d, axes, measures, time_palette, shapes, linetypes, identifier = ''){
  for(i in 1: length(axes)){
    for (j in 1:length(measures)) {
      plot_scatter(d, axes[i], measures[j], time_palette, shapes, paste0(axes[i], '_', measures[j], identifier, 'f_scatter'), linetypes)
    }
  }
}
# histograms for each measure per time
plot_hist <- function(d, x, time_palette, shapes, linetypes, identifier = '', has_value = T) {
  if (has_value) {
    d <- d[d[x] > 0,]
  }
  plt <- d %>% ggplot(aes_string(x = x, shape = 'cond', fill = 'time', linetype = 'cond')) +
    geom_histogram(color = outline_color, alpha = 0.8) +
    facet_grid(rows = vars(cond), cols = vars(time)) +
    scale_fill_manual(values = time_palette) +
    scale_shape_manual(values = shapes) +
    scale_linetype_manual(values = linetypes)
  save_light_graph(paste0(identifier,'_hist_per_time'), plt, c(3,8))
  # total for measure
  plt <- d %>% ggplot(aes_string(x = x, shape = 'cond', linetype = 'cond', alpha = 'cond')) +
    geom_histogram(color = outline_color) +
    scale_shape_manual(values = shapes) +
    scale_linetype_manual(values = linetypes) +
    scale_alpha_manual(values = c(.9, 0.1)) +
    facet_grid(rows = vars(cond))
  save_light_graph(paste0(identifier, '_hist_overall'), plt)
}

plot_hist_helper <- function(d, measures, time_palette, shapes, linetypes, identifier = ''){
  for(i in 1:length(measures)) {
    plot_hist(d, measures[i], time_palette, shapes, linetypes, paste0(measures[i], identifier))
  }
}

# figure 4
plot_small_scatter_binned <- function(summary, time_palette, shapes, y, identifier = '') {
  # must group for proper dodge
  plt <- summary %>% group_by(cond, bin) %>% ggplot(aes_string(x = 'time', y=y, shape = 'cond', fill = 'time', 
                                                               alpha = 'bin')) + 
    geom_point(position = position_jitterdodge(), size = 5, color = outline_color) +
    # choose shapes https://blog.albertkuo.me/post/point-shape-options-in-ggplot/
    scale_shape_manual(values = shapes) +
    scale_x_discrete(name = 'DAS', labels = c(4:9)) +
    scale_y_continuous(labels = scientific) +
    # draw_key_point(color = 'black') +
    # make time scales same https://stackoverflow.com/questions/38238960/how-do-you-control-color-using-facet-wrap-and-ggplot
    # custom palette
    scale_color_manual(values = time_palette) +
    scale_fill_manual(values = time_palette) +
    scale_alpha_manual(values = c(1, 0.5))
  
  save_light_graph(paste0(identifier, '_', y), plt)
}

plot_small_scatter_binned_helper <- function(d, time_palette, shapes, axes, cuts, 
                                             axis_fars, axis_closes) {
  for (i in 1:length(axes)) {
    d_stom <- binnn(d, axes[i], cuts[i], axis_fars[i], axis_closes[i])  # factor in correct order
    # calculate stomatal summary
    stomate_sum <- d_stom %>% group_by(time, bin, cond, sample_id) %>% 
      # density
      # summaries based on conditionals
      # count density cells/cell
      # areal density area stomata/area region
      # https://stackoverflow.com/questions/23528862/summarize-all-group-values-and-a-conditional-subset-in-the-same-call
      summarise(stom_c_dens = length(.data$Label[.data$Stomata_Distance == 0])/length(.data$Label),
                areal_stom_dens = sum(.data$Geometry.Area[.data$Stomata_Distance == 0])/sum(.data$Geometry.Area))
    
    measures <- colnames(stomate_sum)[5:6]
    for (j in 1:length(measures)) {
      plot_small_scatter_binned(stomate_sum, time_palette, shapes, measures[j], paste0(axis_closes[i], '_', axis_fars[i]))
    }
    
  }
}

# clonal analysis
get_clonal_value <- function(d_clone_assigned, d_main, early_time, values) {
  # select label, measure, identifiers from main dataframe (sample_id, time)
  d_select <- d_main[d_main$time == early_time, c(c('Label', 'sample_id'), values)]
  for(i in 1:length(values)) {
    # rename value column to be different from late time point col
    names(d_select)[names(d_select) == values[i]] <- paste0(values[i], '_d', early_time)
  }
  # rename Label column to correspond w later time point
  names(d_select)[names(d_select) == 'Label'] <- paste0('Parent_d_', early_time)
  # merge selection with clonal df
  d_clone_assigned <- merge(d_clone_assigned, d_select)
  
  return(d_clone_assigned)
}

# plot clonal bars
clonal_bars <- function(d, x, y, time_palette, linetypes, identifier='') {
  plt <- d %>% group_by(cond) %>% ggplot(aes_string(x = x, y = y, alpha = 'cond',
                                                    linetype = 'cond')) +
    geom_bar(stat = 'identity', position = 'dodge', fill = time_palette[6], color = outline_color) +
    # make time scales same https://stackoverflow.com/questions/38238960/how-do-you-control-color-using-facet-wrap-and-ggplot
    # custom palette
    scale_linetype_manual(values = linetypes) +
    scale_alpha_manual(values = c(1, 0.5))
  save_light_graph(paste0(identifier, 'clonal'), plt)
  plt
}

# plot scatter early-late spatial
clonal_scatter <- function(d, x, y, time_palette, linetypes, shapes, identifier=''){
  plt <- d %>% group_by(cond) %>% ggplot(aes_string(x = x, y = y, shape = 'cond')) +
    facet_grid(cols = vars(cond)) + 
    scale_shape_manual(values = shapes) +
    geom_point(alpha = 0.4, color = outline_color, fill = time_palette[6]) 
  save_light_graph(paste0(identifier, 'clonal_scatter'), plt)
  plt
}
