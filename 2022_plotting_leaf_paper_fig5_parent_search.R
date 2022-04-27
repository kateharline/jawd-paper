# export big df for long term parent tracking

# but first, example
d_e_small <- d[d$cond == 'jawD' & d$time %in% c(3, 4, 5),]
d_e_small <- rbind(d_e_small, d[d$cond == 'wt' & d$time %in% c(3, 4, 5),])
# select important cols
d_e_small <- d_e_small[c('cond', 'time', 'sample_id', 'Parent', 'Label')]
# export
write.csv(d_e_small, file.path(dot_dir, 'd_ex_parent_track.csv'))

# export full for reverse lineage tracking
d_full_lineage <- d[c('time', 'sample_id', 'Parent', 'Label')]
write.csv(d_full_lineage, file.path(dot_dir, 'd_full_for_parent_track.csv'))



#get early clone spatial, other info


# finding whole lineage
sample_id <- 'wt_2-4'
time <- '4'
wt_line_starts <- c(162, 1552)
jawD_line_starts <- c(950, 2163)

print_lineage <- function(d, parents, sample_id){
  times <- unique(d[d$sample_id == sample_id,]$time)
  
  for(t in 2:length(times)){
    ps <- c()
    print(paste0(times[t], 'DAS'))
    for (p in 1:length(parents)) {
      ps <- c(ps, d[d$Parent == parents[p] & d$sample_id == sample_id & d$time == times[t],]$Label)
    }
    print(ps)
    parents <- ps
  }
}

parents <- 162

