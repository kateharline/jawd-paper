# import functions 
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_plot_functions.R')
# import axes
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_params.R')
# import my styling
source('/Users/kateharline/workspace/data_analysis/final_plotting/light_paper_plots.R')

#gaussian curvature... relationships with growth anisotropies, differentiation, polarity
# get maximal lamina length ... or width? for tissue curvature

length_Summary <- d %>% group_by(time) %>% summarise(max_len = max(Proximal.Distal_Distance),
                                                                    max_wid = max(Medial.Lateral_Distance))

length_Summary$sample_id <- factor(length_Summary$sample_id, levels = c('wt_1-5', 'wt_2-4', '0728_wt_2-4',
                                                                        'jawD_2-2', 'jawD_2-4', 'jawD_2-7'))
p <- length_Summary %>% ggplot(aes(x = time, y = max_len, fill = sample_id)) +
  geom_bar(stat = 'identity', position = 'dodge')
p

p <- length_Summary %>% ggplot(aes(x = time, y = mean(max_wid), fill = cond)) +
  geom_bar(stat = 'identity', position = 'dodge')
p

#give values to use in mgx script
print(length_Summary$max_len / 2)

# calculate abs_signed_avg
d_lam$abs_signed_avg <- if_else(d_lam$Curvature.evalsx. < 0 | d_lam$Curvature.evalsy. <0,
                            -1 *(( abs(d_lam$Curvature.evalsx.) + abs(d_lam$Curvature.evalsy.))/2),
                            (( abs(d_lam$Curvature.evalsx.) + abs(d_lam$Curvature.evalsy.))/2))
d_lam$Gauss <- d_lam$Curvature.evalsx. * d_lam$Curvature.evalsy.
# check 
gauss_check <- read.csv('/Users/kateharline/workspace/finals/temp_gaus.csv')
check <- merge(gauss_check, d_lam)
# limit to usual range displayed on the mesh

plot_dens_helper(d_lam, conds, axes, 'abs_signed_avg', lo_colors, time_palette, linetypes, is_curv=T)
plot_dens_helper(d_lam, conds, axes, 'Curvature.evalsx.', lo_colors, time_palette, linetypes, is_curv=T)
plot_dens_helper(d_lam, conds, axes, 'Gaussian_heat', lo_colors, time_palette, linetypes, is_curv=T)
