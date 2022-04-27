# import functions 
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_plot_functions.R')
# import axes
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_params.R')
# import my styling
source('/Users/kateharline/workspace/data_analysis/final_plotting/light_paper_plots.R')


# figure 2 regional dynamics of growth and division

# all
plot_bubbles_helper(c('wt', 'jawD'), 'cond', '', d, x_axis, y_axis, time_palette, 
                    lo_colors, shapes)
# whole per sample
plot_bubbles_helper(c("wt_1-5", "wt_2-4", "0728_wt_2-4"), 'sample_id', 'whole', 
                    d[d$cond == 'wt',], x_axis, y_axis, time_palette, lo_colors, rep(shapes[1], 3))
plot_bubbles_helper(c("jawD_2-7", "jawD_2-4", "jawD_2-2"), 'sample_id', 'whole', 
                    d[d$cond == 'jawD',], x_axis, y_axis, time_palette, lo_colors, rep(shapes[2], 3))

# lamina, all
plot_bubbles_helper(c('wt', 'jawD'), 'cond', 'blade', d_lam, x_axis, y_axis_lam, time_palette, 
                    lo_colors, shapes)
# lamina, per sample
plot_bubbles_helper(c("wt_1-5", "wt_2-4", "0728_wt_2-4"), 'sample_id', 'blade', 
                    d_wt_lam, x_axis, y_axis_lam, time_palette, lo_colors, rep(shapes[1], 3))
plot_bubbles_helper(c("jawD_2-7", "jawD_2-4", "jawD_2-2"), 'sample_id', 'blade', 
                    d_jaw_lam, x_axis, y_axis_lam, time_palette, lo_colors, rep(shapes[2], 3))

# for whatever reason, a few of the normalized petiole distances of 0 change to NA
# (just note)

# petiole, all
plot_bubbles_helper(c('wt', 'jawD'), 'cond', 'petiole', d_petiole, x_axis, y_axis_petiole, time_palette, 
                    lo_colors, shapes)
# petiole, per sample
plot_bubbles_helper(c("wt_1-5", "wt_2-4", "0728_wt_2-4"), 'sample_id', 'petiole', 
                    d_wt_petiole, x_axis, y_axis_petiole, time_palette, lo_colors, rep(shapes[1], 3))
plot_bubbles_helper(c("jawD_2-7", "jawD_2-4", "jawD_2-2"), 'sample_id', 'petiole', 
                    d_jawd_petiole, x_axis, y_axis_petiole, time_palette, lo_colors, rep(shapes[2], 3))

