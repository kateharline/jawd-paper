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
# for stats 
library(rstatix)
# for ppretty jitter on violins
library(ggbeeswarm)


# figure 1

# make pval dataframe
# add signif
# https://cran.r-project.org/web/packages/ggprism/vignettes/pvalues.html
p_val_df <- function(df, grouper, y, x) {
  p_df <- df %>%
    rstatix::group_by(!! sym(grouper))  %>%
    # finally! pass strings as formulas https://stackoverflow.com/questions/66515390/t-test-with-column-number-instead-of-column-name
    rstatix::t_test(reformulate(x, y)) %>%
    rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
    rstatix::add_significance(p.col = "p.adj") %>% 
    rstatix::add_xy_position(x = grouper, dodge = 0.8) # important for positioning!
 
  return(p_df)
}

plot_small_scatter <- function(summary, time_palette, shapes, x, y, add_p = F, add_lm=F, identifier='', is_log=F) {
  # must group for proper dodge
  plt <- summary %>% group_by(cond) %>% 
    ggplot(aes_string(x = x, y = y))
  if (add_lm) {
    # http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
    plt <- plt + geom_smooth(color = outline_color, method = 'lm', aes(alpha = cond))
    identifier <- 'lm'
  }
  plt <- plt + 
    geom_point(position = position_jitterdodge(), size = 5, color = outline_color, stroke = 1,
               aes_string(shape = 'cond', alpha = 'cond', fill = 'sum_fill')) +
    # choose shapes https://blog.albertkuo.me/post/point-shape-options-in-ggplot/
    scale_shape_manual(values = shapes) +
    scale_y_continuous(labels = scientific) +
    # draw_key_point(color = 'black') +
    # make time scales same https://stackoverflow.com/questions/38238960/how-do-you-control-color-using-facet-wrap-and-ggplot
    # custom palette
    scale_fill_manual(values = time_palette) +
    scale_alpha_manual(values = c(1, 0.5)) 
  if (add_p) {
    # calculate p 
    df_p <- p_val_df(summary, x, y, 'cond')
    plt <- plt +
      add_pvalue(df_p,
                 color = outline_color,
                     xmin = 'xmin',
                     xmax = 'xmax',
                     label = '{p.adj}',
                     tip.length = 0) 
  }
  if (is_log) {
    plt <- plt + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                 labels = trans_format("log10", math_format(10^.x))) +
      annotation_logticks(sides = 'l') 
  }
  
  save_light_graph(paste0(y, identifier), plt)
}

# iteration help https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
plot_small_scatter_helper <- function(summary, time_palette, shapes, ys, add_p = F, is_log=F, identifier='') {
  for (i in 1:length(ys)) {
    plot_small_scatter(summary, time_palette, shapes, 'time', ys[i], add_p, is_log=is_log, identifier = identifier)
  }
}

p_val_df_2 <- function(df, x, y) {
  df_p <- df %>%  
    rstatix::t_test(reformulate(x, y)) %>%
    rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
    rstatix::add_significance(p.col = "p.adj") %>% 
    rstatix::add_xy_position(x = x, dodge = 0.8) # important for positioning!

  return(df_p)
}

# small boxplots
# https://cran.r-project.org/web/packages/ggprism/vignettes/pvalues.html
small_box <- function(d, measure, add_p = F) {
  d <- d %>% group_by(cond) %>%
    mutate(outlier.high = !!sym(measure) > quantile(!!sym(measure), .75) + 1.5*IQR(!!sym(measure)),
           outlier.low = !!sym(measure) < quantile(!!sym(measure), .25) - 1.5*IQR(!!sym(measure))) %>% ungroup()
  # hacky hide outliers
  #d$for_alpha <- if_else(d['outlier.high'] | d['outlier.low'], .7, 0)\
  # decide to show all points 
  d$for_alpha <- 0.7

  plt <- d %>%
    ggplot(aes_string(y = measure, x = 'cond')) +
    scale_shape_manual(values = shapes) +
    scale_linetype_manual(values = linetypes) +
    scale_alpha_manual(values = c(0.7, 0.3)) +
    geom_violin(fill = outline_color, aes_string(linetype = 'cond', alpha = 'cond'), color = outline_color) +
    # geom_point(position = position_jitterdodge(), size = 3, alpha = d$for_alpha,
    #            color = outline_color, fill = outline_color, aes_string(shape = 'cond'))
    # for showing all points https://apreshill.github.io/data-vis-labs-2018/04-distributions.html
    geom_quasirandom(size = 3, alpha = d$for_alpha, color = outline_color, 
                     fill = outline_color, aes_string(shape = 'cond'))
  if (add_p) {
    # calculate p 
    df_p <- p_val_df_2(d, 'cond', measure)
    plt <- plt +
      add_pvalue(df_p,
                 color = outline_color,
                 xmin = 'xmin',
                 xmax = 'xmax',
                 label = '{p.adj.signif}',
                 tip.length = 0.01) 
  }

  return(plt)
}

small_box_helper <- function(d, measures, add_p = F) {
  for(i in 1:length(measures)){
    plt <- small_box(d, measures[i], add_p)
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
    ylim(c(0,1)) + 
    annotate('text', x = 0.75, y = 0.01, label = paste('n =', nrow(plot_d)))
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
# changed to lose facets
plot_scatter <- function(d, x, y, t_color, c_shape, c_linetype, ylims, identifier = ''){
  plt <- d %>% ggplot(aes_string(x = x, y = y)) +
    geom_point(alpha = 0.4, color = outline_color, fill = t_color, shape = c_shape, linetype = c_linetype) + 
    ylim(ylims)
    # line of fit https://bookdown.org/dli/rguide/scatterplots-and-best-fit-lines-two-sets.html
    # geom_smooth(method = 'lm', color = outline_color, fill = outline_color, alpha = 0.6) +
  save_light_graph(identifier, plt)
}

get_y_lims <- function(d, measures) {
  ylims <- matrix(nrow = length(measures), ncol = 2)
  for(m in 1:length(measures)) {
    ylims[m,] <- c(min(d[measures[m]], na.rm = T), max(d[measures[m]], na.rm = T))
  }
  return(ylims)
}

plot_scatter_helper <- function(d, axes, measures, time_palette, shapes, linetypes, identifier = '', ylims = c()){
  times <- unique(d$time)
  conds <- levels(d$cond)
  if (is_empty(ylims)) {
    ylims <- get_y_lims(d, measures)
  }
  
  
  for (c in 1:length(conds)) {
    for(t in 1:length(times)) {
      for(i in 1: length(axes)){
        for (j in 1:length(measures)) {
          # check d exists
          if (!(is_empty(d[(d$time == times[t]) & (d$cond == conds[c]),]))) {
            plot_scatter(d[(d$time == times[t]) & (d$cond == conds[c]),], axes[i], measures[j], time_palette[t], 
                         shapes[c], linetypes[c], ylims[j,],
                         paste0(conds[c], times[t], '_',axes[i], '_', measures[j], identifier, 'f_scatter'))
            
          }
         }
      }
 
    }
  }

}

custom_pal <- function(low_col, high_col, num) {
  return(scales::seq_gradient_pal(low_col, high_col, "Lab")(seq(0,1,length.out=num)))
}

plot_dens <- function(d, x, y, pale, linetype, bin_num, is_log, y_lims, shape, identifier = '', adjust = 1) {
  # https://ggplot2.tidyverse.org/reference/geom_density_2d.html
  plt2 <- d %>% ggplot(aes_string(x = x, y = y)) +
    geom_point(color = outline_color, shape = shape) +
    geom_density_2d_filled(colour = outline_color, bins = bin_num, adjust=adjust, linetype = linetype, alpha = 0.8) +
    scale_fill_manual(values = pale) +
    scale_linetype_manual(values = c(linetype))
  if (!all(is.na(y_lims))) {
    plt2 <- plt2 + ylim(y_lims)
  }
  if (is_log) {
    plt2 <- plt2 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)), limits = y_lims) +
      annotation_logticks() 
  }
    
  save_light_graph(paste0('dens_',identifier), plt2)
}

plot_dens_helper <- function(d, conds, axes, measures, lo_colors, time_palette, linetypes, bin_num = 7, ylims = matrix(),
                             identifier='', is_log=F, is_curv=F, shapes=shapes_solid, adjust = 1) {
  times <- unique(d$time)
  if (all(is.na(ylims)) & !is_curv) {
    ylims <- get_y_lims(d, measures)
  }
  for (m in 1:length(measures)){
    for(c in 1:length(conds)){
      df <- d[d$cond == conds[c],] 
      for (t in 1:length(times)) {
        # make palette
        if (is_curv) {
          ylims <- rbind(unname(quantile(d[d$time == times[t],c(measures[m])], probs = c(.075, .925))))
        }
        pal <- custom_pal('white', time_palette[t], bin_num)
        if (darkmode) {
          pal <- custom_pal('black', time_palette[t], bin_num)
        }
        for(a in 1:length(axes)) {
          plot_dens(df[df$time == times[t],], axes[a], measures[m], pal,
                    linetypes[c], bin_num, is_log, ylims[m,], shapes[c],
                    identifier = paste0(identifier, '_', measures[m], '_', axes[a], '_', conds[c], '_', times[t], '_DAS'), adjust = adjust)
        
        }
      }
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


# plot clonal bars
# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
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

clonal_box <- function(d, x, y, color_t, linetypes, shapes, identifier='', add_p = F){
  # get rid of max value which doesn't allow p
  if (add_p) {
    d <- d[d[x] < 1,]
  }

  # calculate outliers
  d <- d %>% group_by(cond, !! sym(x)) %>% 
    mutate(outlier.high = !!sym(y) > quantile(!!sym(y), .75) + 1.5*IQR(!!sym(y)),
           outlier.low = !!sym(y) < quantile(!!sym(y), .25) - 1.5*IQR(!!sym(y))) %>% ungroup()
  # hacky hide outliers
  d$for_alpha <- if_else(d['outlier.high'] | d['outlier.low'], .8, 0)
  d$for_alpha[d$for_alpha & d$cond == 'jawD'] <- .3
  
  plt <- d %>% ggplot(aes_string(x = x, y = y)) +
    geom_boxplot(outlier.shape = NA, aes_string(linetype = 'cond', alpha = 'cond', fill = x)) +
    scale_linetype_manual(values = linetypes) +
    scale_alpha_manual(values = c(0.8, 0.3)) +
    scale_shape_manual(values = shapes) +
    scale_fill_manual(values = purple_pal) +
    geom_point(position = position_jitterdodge(), size = 1, alpha = d$for_alpha, color = outline_color, fill = color_t,
               aes_string(shape = 'cond')) +
    ylim(c(0,1.1))
  if (add_p) {
    # calculate p 
    df_p <- p_val_df(d, x, y, 'cond')
    plt <- plt +
      add_pvalue(df_p,
                 color = outline_color,
                 xmin = 'xmin',
                 xmax = 'xmax',
                 label = '{p.adj.signif}',
                 tip.length = 0)
  }
  
  save_light_graph(paste0(identifier, 'clonal_box'), plt)
}

# plot scatter early-late spatial
clonal_scatter <- function(d, x, y, time_palette, linetypes, shapes, identifier=''){
  plt <- d %>% group_by(cond) %>% ggplot(aes_string(x = x, y = y, shape = 'cond')) +
    facet_grid(cols = vars(cond)) + 
    scale_shape_manual(values = shapes) +
    geom_point(alpha = 0.4, color = outline_color, fill = time_palette[6]) 
  save_light_graph(paste0(identifier, 'clonal_scatter'), plt, dims = c(4, 8))
  plt
}
