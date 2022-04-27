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

# main func
make_clonal_df <- function(csv_path, day, d) {
  # load file

  tracked_df <- read.csv(csv_path)
  # cleaning
  tracked_df$X <- NULL
  # merge early clone with late
  tracked_big <- merge(d, tracked_df)
  
  # ------ #
  tracked_w_clonal_vals <- get_clonal_value(tracked_big, d, paste0(day), c('Proximal.Distal_Distance_norm', 'Medial.Lateral_Distance_norm'))
  
  # bin clonal locations p-d
  tracked_w_clonal_vals$binned_p_d_early <- plyr::round_any(tracked_w_clonal_vals[[paste0('Proximal.Distal_Distance_norm_d',day)]], accuracy=.1, f=floor)
  tracked_w_clonal_vals$binned_p_d_late <- plyr::round_any(tracked_w_clonal_vals$Proximal.Distal_Distance_norm, accuracy=.1, f=floor)
  tracked_w_clonal_vals$binned_m_l_early <- plyr::round_any(tracked_w_clonal_vals[[paste0('Medial.Lateral_Distance_norm_d',day)]], accuracy=.1, f=floor)
  
  # a little more robust version w boxplots
  tracked_w_clonal_vals$binned_p_d_early <- as.character(tracked_w_clonal_vals$binned_p_d_early)
  tracked_w_clonal_vals$binned_m_l_early <- as.character(tracked_w_clonal_vals$binned_m_l_early)
  
  return(tracked_w_clonal_vals)
}

export_lineage_for_mgx <- function(tracked_w_clonal_df, sample, early_das, late_das, bin_parm, binned=T) {
  # d8 with d3 bin labels, jawD example
  late_mgx <- tracked_w_clonal_df[tracked_w_clonal_df$sample_id == sample,c('Label', bin_parm)]
  early_mgx <- tracked_w_clonal_df[tracked_w_clonal_df$sample_id == sample,c(paste0('Parent_d_',early_das), bin_parm)]
  # hacky change d3 bin column to 'Parent'
  names(late_mgx)[names(late_mgx) == bin_parm] <- 'Parent'
  names(early_mgx)[names(early_mgx) == bin_parm] <- 'Parent'
  names(early_mgx)[names(early_mgx) == paste0('Parent_d_', early_das)] <- 'Label'

  # maybe make mgx happier with bin labels
  if (binned) {
    late_mgx$Parent <- (as.numeric(late_mgx$Parent) + .1)*10
    early_mgx$Parent <- (as.numeric(early_mgx$Parent) + .1) *10
  }
  
  # export
  write.csv(late_mgx, file.path(dot_dir, paste0(sample, '_d', late_das, 'to_d', early_das,bin_parm,'.csv')), row.names = F)
  write.csv(early_mgx, file.path(dot_dir, paste0(sample, '_d', early_das, 'to_d', early_das,bin_parm,'.csv')), row.names = F)
  
}