# import axes
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_params.R')
# import my styling
source('/Users/kateharline/workspace/data_analysis/final_plotting/light_paper_plots.R')
# import functions 
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_plot_functions.R')
# import clonal funcs 
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_leaf_paper_clonal.R')
# import parent search funcs
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_fig5_parent_search.R')

# clonal analysis
# display final time point with relative contribution from beginning time point
# import older lineage tracked
d8_to_3_clonal <- make_clonal_df(file.path(dot_dir, 'families_d8_to_ds_3_4.csv'), '3', d)
d8_to_4_clonal <- make_clonal_df(file.path(dot_dir, 'families_d8_to_ds_3_4.csv'), '4', d)

# hacky version
d8_to_3_clonal_summ <- d8_to_3_clonal %>% group_by(cond, binned_p_d_early) %>% 
  summarise(max_8 = max(.data$binned_p_d_late))

clonal_bars(d8_to_3_clonal_summ, 'binned_p_d_early', 'max_8', time_palette, linetypes, 'p-d')

clonal_box(d8_to_3_clonal, 'binned_p_d_early', 'Proximal.Distal_Distance_norm', time_palette[6], linetypes, shapes, add_p = T)
clonal_box(d8_to_3_clonal, 'binned_m_l_early', 'Medial.Lateral_Distance_norm', time_palette[6], linetypes, shapes, identifier = 'm_l', add_p = T)

clonal_box(d8_to_4_clonal, 'binned_p_d_early', 'Proximal.Distal_Distance_norm', time_palette[6], linetypes, shapes, identifier = 'p-d_4', add_p = T)
clonal_box(d8_to_4_clonal, 'binned_m_l_early', 'Medial.Lateral_Distance_norm', time_palette[6], linetypes, shapes, identifier = 'm_l_4', add_p = T)

# bin clonal location m-l

clonal_scatter(d8_to_3_clonal, 'Proximal.Distal_Distance_norm_d3', 'Proximal.Distal_Distance_norm', 
               time_palette, linetypes, shapes, 'p-d')
clonal_scatter(d8_to_3_clonal, 'Medial.Lateral_Distance_norm_d3', 'Medial.Lateral_Distance_norm', 
               time_palette, linetypes, shapes, 'm-l')

clonal_scatter(d8_to_4_clonal, 'Proximal.Distal_Distance_norm_d4', 'Proximal.Distal_Distance_norm', 
               time_palette, linetypes, shapes, 'p-d_4')
clonal_scatter(d8_to_4_clonal, 'Medial.Lateral_Distance_norm_d4', 'Medial.Lateral_Distance_norm', 
               time_palette, linetypes, shapes, 'm-l_4')

clonal_scatter(d8_to_5_clonal, 'Proximal.Distal_Distance_norm_d5', 'Proximal.Distal_Distance_norm', 
               time_palette, linetypes, shapes, 'p-d_5')
clonal_scatter(d8_to_5_clonal, 'Medial.Lateral_Distance_norm_d5', 'Medial.Lateral_Distance_norm', 
               time_palette, linetypes, shapes, 'm-l_5')

# mgx pictures
export_lineage_for_mgx(d8_to_3_clonal, 'wt_2-4', '3', '8', 'binned_p_d_early')
export_lineage_for_mgx(d8_to_3_clonal, 'jawD_2-7', '3', '8', 'binned_p_d_early')

export_lineage_for_mgx(d8_to_4_clonal, 'wt_2-4', '4', '8', 'binned_p_d_early')
export_lineage_for_mgx(d8_to_4_clonal, 'jawD_2-7', '4', '8', 'binned_p_d_early')

# check all m-l labeling
export_lineage_for_mgx(d8_to_3_clonal, 'wt_2-4', '3', '8', 'binned_m_l_early')
export_lineage_for_mgx(d8_to_3_clonal, 'wt_1-5', '3', '8', 'binned_m_l_early')
export_lineage_for_mgx(d8_to_3_clonal, '0728_wt_2-4', '3', '8', 'binned_m_l_early')
export_lineage_for_mgx(d8_to_3_clonal, 'jawD_2-2', '3', '8', 'binned_m_l_early')
export_lineage_for_mgx(d8_to_3_clonal, 'jawD_2-4', '3', '8', 'binned_m_l_early')
export_lineage_for_mgx(d8_to_3_clonal, 'jawD_2-7', '3', '8', 'binned_m_l_early')

export_lineage_for_mgx(d8_to_4_clonal, 'wt_2-4', '4', '8', 'binned_m_l_early')
export_lineage_for_mgx(d8_to_4_clonal, 'jawD_2-7', '4', '8', 'binned_m_l_early')

# new 5e whole lineage
print_lineage(d, 162, 'wt_2-4')
print_lineage(d[d$time > '3',], 1552, 'wt_2-4')
print_lineage(d, 950, 'jawD_2-7')
print_lineage(d[d$time > '3',], 2163, 'jawD_2-7')

# for raw clonal snaps
export_lineage_for_mgx(d8_to_4_clonal, 'wt_2-4', '4', '8', 'Parent_d_4', binned =F)
export_lineage_for_mgx(d8_to_4_clonal, 'jawD_2-7', '4', '8', 'Parent_d_4', binned =F)

export_lineage_for_mgx(d8_to_4_clonal, '0728_wt_2-4', '4', '8', 'Parent_d_4', binned =F)
export_lineage_for_mgx(d8_to_4_clonal, 'jawD_2-2', '4', '8', 'Parent_d_4', binned =F)
export_lineage_for_mgx(d8_to_4_clonal, 'wt_1-5', '4', '8', 'Parent_d_4', binned =F)
export_lineage_for_mgx(d8_to_4_clonal, 'jawD_2-4', '4', '8', 'Parent_d_4', binned =F)

# for raw clonal vid
days <- c('5','6','7','8')
samps <- c('jawD_2-7', 'wt_2-4')
for (s in 1:length(samps)) {
  for (ds in 1:length(days)) {
    d_df <-  make_clonal_df(file.path(dot_dir, paste0('families_d',days[ds],'_to_ds_4.csv')), '4', d)
    export_lineage_for_mgx(d_df, samps[s], '4', days[ds], paste0('Parent_d_4'), binned=F)
  }
}

