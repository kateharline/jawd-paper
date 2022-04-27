# load big dataset
dot_dir <- "/Users/kateharline/workspace/finals"
d <- read.csv(file.path(dot_dir, 'big_d.csv'))


# load big leaf dataset 
big_leaf_d <- read.csv('/Users/kateharline/workspace/image_analysis/big_jawD_wt/wt_jawd_big_data.csv')
# cleaning
d[[1]] <- NULL
d$cond <- factor(d$cond, levels = c('wt', 'jawD'))
d$time <- as.character(d$time)

# axis selection grid
# all cells per condition
x_axis <- "Medial.Lateral_Distance_norm"
y_axis <- "Proximal.Distal_Distance_norm"
# just lamina
y_axis_lam <- "Proximal.Distal_lamina_Distance_norm"
# just petiole
y_axis_petiole <- "Proximal.Distal_petiole_Distance_norm"

# axis selection binaries
axis_closes <- c('Proximal', 'Medial')
axis_fars <- c('Distal', 'Lateral')
cuts <- c(0.5, 0.5)
axes <- c('Proximal.Distal_lamina_Distance_norm', 'Medial.Lateral_Distance_norm')
conds <- levels(d$cond)

# dataframes
# lamina
d_lam <- d[d$Proximal.Distal_lamina_Distance > 0,]
d_lam_delt <- d_lam[(d_lam$cond == 'wt' & d_lam$time < '9') | (d_lam$cond == 'jawD' & d_lam$time < '8'),]
d_lam_d_delt <- d_lam_delt[(d_lam_delt$d_Proliferation > 0 | d_lam_delt$d_Area > 0),]
# each sample for given cond, lamina
d_wt_lam <- d_lam[d_lam$cond == 'wt',]
d_jaw_lam <- d_lam[d_lam$cond == 'jawD',]
# growind and or dividing
d_delt <- d[(d$d_Proliferation > 0 | d$d_Area > 0),]
# petioles
d_petiole <- d[d$Proximal.Distal_lamina_Distance == 0,]
d_wt_petiole <- d_petiole[d_petiole$cond == 'wt',]
d_jawd_petiole <- d_petiole[d_petiole$cond == 'jawD',]
# only featuring stomata 
# cut days where no stomata
d_stom_time <- d_lam[d_lam$time > '3',]
d_only_stom <- d_stom_time[d_stom_time$Stomata_Distance == 0,]


# for supravat 
s_d <- d_lam[c('time', 'sample_id', 'cond', 'Label', 'd_Area', 'd_Proliferation', 'Proximal.Distal_lamina_Distance',
               'Medial.Lateral_Distance')]
s_d$time <- as.numeric(as.character(s_d$time)) - 2
s_d$d_Proliferation <- s_d$d_Proliferation + 1
s_d$d_Area <- (s_d$d_Area / 100) + 1
write.csv(s_d, file.path(dot_dir, 'data_for_sd.csv'))
