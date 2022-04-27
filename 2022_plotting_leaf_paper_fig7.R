# import functions 
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_plot_functions.R')
# import axes
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_params.R')
# import my styling
source('/Users/kateharline/workspace/data_analysis/final_plotting/light_paper_plots.R')


# max/min .. aniso .. gives weird hard to compare results bc of zeros and negative values
# d_pdg$aniso <- if_else(d_pdg$PDGs.evalsy. > 0,
#                        d_pdg$PDGs.evalsx./d_pdg$PDGs.evalsy., NaN)
# d_pdg$aniso <- d_pdg$PDGs.evalsx./d_pdg$PDGs.evalsy.
# plot_dens_helper(d_pdg[d_pdg$aniso < 5,], conds, axes, 'aniso', lo_colors, time_palette, linetypes)
d_lam_delt$rel_pdg_align <- d_lam_delt$StretchCustomX - d_lam_delt$StretchCustomY
plot_dens_helper(d_lam_delt, conds, 'Proximal.Distal_lamina_Distance_norm', 'rel_pdg_align', lo_colors, time_palette, linetypes, ylims = rbind(c(-2,2)))

# growth orientation
# PDG, other growth/shape anisotropy, maybe stomatal aniso
# dens plot of maximal growth value
d_lam_delt$rel_pdg <- d_lam_delt$PDGs.evalsx. - d_lam_delt$PDGs.evalsy.
plot_dens_helper(d_lam_delt[d_lam_delt$PDGs.evalsx. < 9.5,], conds, axes, 'PDGs.evalsx.', lo_colors, time_palette, linetypes)
plot_dens_helper(d_lam_delt, conds, 'Proximal.Distal_lamina_Distance_norm', 'rel_pdg', lo_colors, time_palette, linetypes, ylims = rbind(c(.01,2)))
plot_dens_helper(d_lam_delt, conds, 'Medial.Lateral_Distance_norm', 'rel_pdg', lo_colors, time_palette, linetypes, ylims = rbind(c(.01,2)))


# dens plot of  P-D 'alignment' = 90-angle aniso
#d_lam$alignment <- ((90 - d_lam$aniso_angle_max) /90)*100

plot_dens_helper(d_lam, conds, axes, 'aniso_angle_max', lo_colors, time_palette, linetypes)
