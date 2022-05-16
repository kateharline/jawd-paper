# import functions 
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_plot_functions.R')
# import axes
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_params.R')
# import my styling
source('/Users/kateharline/workspace/data_analysis/final_plotting/light_paper_plots.R')

# figure 4 formalizing//defining tissue regions to compare between WT and jaw-D
measures <- c('d_Area', 'd_Proliferation')

# other viz options
plot_bin_box_helper(d_lam, axis_closes, axis_fars, cuts, axes, measures, time_palette, shapes,
                    'lamina')
plot_scatter_helper(d_lam_delt, axes, measures, time_palette, shapes, linetypes)
plot_hist_helper(d_lam_delt, measures, time_palette, shapes, linetypes)

# final viz
plot_dens_helper(d_lam_delt, 'wt', axes, 'd_Area', lo_colors, time_palette, linetypes[1], shapes = shapes_solid[1])
plot_dens_helper(d_lam_delt, 'jawD', axes, 'd_Area', lo_colors, time_palette, linetypes[2], shapes = shapes_solid[2])

# limit waves on edges of int cell divs
plot_dens_helper(d_lam_delt, 'wt', axes, 'd_Proliferation', lo_colors, time_palette, linetypes[2], shapes = shapes_solid[2], adjust = 2)
plot_dens_helper(d_lam_delt, 'jawD', axes, 'd_Proliferation', lo_colors, time_palette, linetypes[2], shapes = shapes_solid[2], adjust = 2)
# dumb skip wt problems

# zoom 
y_lims <- rbind(c(0, 600), c(0, 6))
plot_dens_helper(d_lam_delt, conds, axes, measures, lo_colors, time_palette, linetypes, ylims = y_lims,
                 identifier = 'lim')
# scatter to go with, try to make lims match dens
plot_scatter_helper(d_lam_delt[d_lam_delt$time %in% c('6', '7', '8'),], axes, 'd_Proliferation', time_palette[4:6], shapes, linetypes, ylims = rbind(c(0, 11)))

# dumb skip wt problems
plot_dens_helper(d_lam_delt, 'jawD', axes, measures, lo_colors, time_palette, linetypes[2], ylims = y_lims,
                 identifier = 'lim')
