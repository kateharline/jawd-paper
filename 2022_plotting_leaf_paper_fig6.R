# import functions 
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_plot_functions.R')
# import axes
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_params.R')
# import my styling
source('/Users/kateharline/workspace/data_analysis/final_plotting/light_paper_plots.R')

# figure 5 differentiation amongst regions -- cell type
# pavement
measures <- c('Lobeyness.Lobeyness')
plot_dens_helper(d_lam, conds, axes, measures, lo_colors, time_palette, linetypes)

plot_bin_box_helper(d_lam, axis_closes, axis_fars, cuts, axes, measures, time_palette, shapes,
                    'lamina')
plot_scatter_helper(d_lam, axes, measures, time_palette, shapes, linetypes)
plot_hist_helper(d_lam, measures, time_palette, shapes, linetypes)

# stomata
plot_dens_helper(d_stom_time, conds, axes, 'Stomata_Distance', lo_colors, time_palette[2:7], linetypes)
plot_dens_helper(d_stom_time, conds, axes, c('Geometry.Area', 'Geometry.Aspect.Ratio', 'Lobeyness.Circularity'), lo_colors, time_palette[2:7], 
                 linetypes, identifier =  '_stomata_', is_log = F)

plot_bin_box_helper(d_only_stom, axis_closes, axis_fars, cuts, axes, 'Geometry.Area', 
                    time_palette[2:7], shapes, 'Stomatal_area')

plot_bin_box_helper(d_stom_time, axis_closes, axis_fars, cuts, axes, 
                    # distance to next stomata
                    'Stomata_Distance', 
                    time_palette[2:7], shapes,'Stomata_Distance')

plot_scatter_helper(d_only_stom, axes, 'Geometry.Area', time_palette[2:7], shapes, linetypes, '_stomata_')
plot_scatter_helper(d_stom_time, axes, 'Stomata_Distance', time_palette[2:7], shapes, linetypes)

plot_hist_helper(d_only_stom, 'Geometry.Area', time_palette, shapes, linetypes, '_stomata_')
plot_hist_helper(d_stom_time, 'Stomata_Distance', time_palette, shapes, linetypes)

# summary
plot_small_scatter_binned_helper(d_stom_time, time_palette[2:7], shapes, axes, cuts, axis_fars, 
                                 axis_closes)





# figure 5 differentiation amongst regions -- general cell size/shape
measures <- c('Network.Neighbors', 'Geometry.Aspect.Ratio', 'Geometry.Area')

plot_bin_box_helper(d_lam, axis_closes, axis_fars, cuts, axes, measures, time_palette, shapes,
                    'lamina')

plot_scatter_helper(d_lam, axes, measures, time_palette, shapes, linetypes)

plot_hist_helper(d_lam, measures, time_palette, shapes, linetypes)

# getting cells to demo lobeyness and stomata
samps <- d8_to_4_clonal[d8_to_4_clonal$sample_id %in% c('jawD_2-7', 'wt_2-4'),]
near_cells <- samps[samps$binned_p_d_early > .2 & samps$Proximal.Distal_Distance_norm < 0.6,]
far_cells <-  samps[samps$binned_p_d_early > .5 & samps$Proximal.Distal_Distance_norm < 0.89,]

wt_ns <- near_cells[near_cells$cond == 'wt',]
jawD_ns <- near_cells[near_cells$cond == 'jawD',]

wt_fs <- far_cells[far_cells$cond == 'wt',]
jawD_fs <- far_cells[far_cells$cond == 'jawD',]
