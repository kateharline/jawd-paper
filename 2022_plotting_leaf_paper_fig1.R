# import functions 
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_plot_functions.R')
# import axes
source('/Users/kateharline/workspace/data_analysis/final_plotting/2022_plotting_leaf_paper_params.R')
# import my styling
source('/Users/kateharline/workspace/data_analysis/final_plotting/light_paper_plots.R')


# figure 1 overview of leaf growth 
summary <- d %>% group_by(cond, time, sample_id) %>% summarise(leaf_area = sum(.data$`Geometry.Area`),
                                                            cell_num = length(.data$`Label`),
                                                            max_l = max(.data$`Proximal.Distal_Distance`),
                                                            max_w = max(.data$`Medial.Lateral_Distance`),
                                                            cell_dens = cell_num/leaf_area) %>% ungroup()
# statistics
mean_summary <- summary %>% group_by(cond, time) %>% summarise(avg_leaf_area = mean(.data$leaf_area),
                                                               std_dev_la = sd(.data$leaf_area),
                                                               avg_cell_area = mean(.data$cell_num),
                                                               std_dev_ca = sd(.data$cell_num),
                                                               avg_leaf_l = mean(.data$max_l),
                                                               std_dev_ll = sd(.data$max_l),
                                                               avg_leaf_w = mean(.data$max_w),
                                                             std_dev_lw = sd(.data$max_w))
# make a column for the color so that time can be used as numeric
summary$sum_fill <- as.character(summary$time)
# make factors
summary$cond <- factor(summary$cond, levels = c('wt', 'jawD'))

# log linear
summary$log_area <- log(summary$leaf_area)
summary$log_l <- log(summary$max_l)
# which values variables
ys <- colnames(summary)[4:8]

plot_small_scatter_helper(summary, time_palette, shapes, ys, add_p = F)
plot_small_scatter_helper(summary, time_palette, shapes, c('leaf_area', 'max_l'), add_p = F, is_log=T, identifier='_as_log')
summary$time <- as.numeric(as.character(summary$time))
plot_small_scatter_helper(summary, time_palette, shapes, c('log_area', 'log_l'), add_p = F, add_lm=T)
# if doing p values
# summary_sm <- summary[summary$time < '9',]
# plot_small_scatter_helper(summary_sm, time_palette, shapes, 'cell_dens', add_p = T)

#checking exponential
# make time numeric for running lm
#get estimates of linear fit
summary$time <- as.numeric(as.character(summary$time))

lm_cd <- lm(cell_dens ~ time, summary)
summary(lm_cd)
exp_cd <- lm(log(cell_dens) ~ time, summary)
summary(exp_cd)

# add line to scatter
plot_small_scatter(summary, time_palette, shapes, 'time', 'cell_dens', add_lm = T)

# get estimates of exp fit
lm_la <- lm(leaf_area ~time, summary)
summary(lm_la)
exp_la <- lm(log(leaf_area) ~ time, summary)
summary(exp_la)

lm_ml <- lm(max_l ~time, summary)
summary(lm_ml)
exp_ml <- lm(log(max_l) ~ time, summary)
summary(exp_ml)

# trying to guess parms
lm_cn <- lm(cell_num ~ time, summary)
summary(lm_cn)
exp_cn <- lm(log(cell_num) ~ time, summary)
summary(exp_cn)

sigmoid <- function(x, phi1, phi2, phi3) {phi1/(1 + exp(-phi3 * (x - phi2)))}
st <- list(phi1=1000, phi2=4, phi3=1)
sig_cn <- nls(cell_num ~ sigmoid(time, phi1, phi2, phi3), start = st, data = summary, algorithm = 'port')
summary(sig_cn)

# testing p-value of sig
library(sicegar)
summary_sm <- summary[summary$time < 9,]

norm <- sicegar::normalizeData(dataInput = data.frame(intensity = summary_sm$cell_num, time = summary_sm$time))
sig_cn2 <- sicegar::multipleFitFunction(dataInput = norm, model = 'sigmoidal')
sig_parms <- sicegar::parameterCalculation(sig_cn2)

library(drc)
# log logistic
# using drc http://www.darrenkoppel.com/2020/09/04/dose-response-modelling-and-model-selection-in-r/
# citation https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4696819/
sig_cn3 <- drm(cell_num ~ time, data = summary, fct = LL.3())
summary(sig_cn3)
plot(sig_cn3, type = 'all')
mselect(sig_cn3, linreg = T)
####

# plotting general big leaf characteristics
# remove last two columns 
big_leaf_d <- big_leaf_d[1:(length(big_leaf_d) - 2)]
# rename value column 
names(big_leaf_d)[length(big_leaf_d)] <- 'value'
# convert value to numeric
big_leaf_d$value <- as.numeric(as.character(big_leaf_d$value))
# remove rows w herbivory
big_leaf_d <- big_leaf_d[!(is.na(big_leaf_d$value)),]
# convert measures from in to mm
big_leaf_d <- big_leaf_d %>% mutate(value_mm =
                                      if_else(measure == 'length', value*25.4, value*645.16))
# rename measure to include region
big_leaf_d$measure <- paste0(big_leaf_d$region, '_', big_leaf_d$measure)
big_leaf_d$region <- NULL

# change format of df to calculate petiole measures
big_leaf_d$id <- paste0(big_leaf_d$filename, big_leaf_d$cond, big_leaf_d$leaf, big_leaf_d$number)
# reshape
big_leaf_d <- big_leaf_d %>% reshape(idvar = c('filename', 'cond', 'leaf', 'number'), timevar = 'measure', direction = 'wide')
# add column for petiole measures
big_leaf_d <- big_leaf_d %>% mutate(petiole_length = value_mm.whole_length - value_mm.lamina_length,
                                    petiole_area = value_mm.whole_area - value_mm.lamina_area,
                                    rel_lam_pet_length = value_mm.lamina_length/petiole_length,
                                    rel_lam_pet_area = value.lamina_area/petiole_area)
# for a given leaf, number
big_leaf_summary <- big_leaf_d %>% group_by(cond) %>% 
  summarise(avg_area = mean(value_mm.whole_area),
            avg_length = mean(value_mm.whole_length),
            avg_pet_length = mean(petiole_length),
            avg_lam_area = mean(value_mm.lamina_area),
            n= length(.data$cond)) 

big_leaf_d$cond <- factor(big_leaf_d$cond, levels = c('wt', 'jawD'))

small_box_helper(big_leaf_d, c('value_mm.whole_length', 'value_mm.whole_area', 'petiole_length',
                               'value_mm.lamina_area', 'value_mm.lamina_length'), T)

# look at relative area of leaves
# separate out arbitrary 'a' leaves
rel_leaves <- big_leaf_d[big_leaf_d$leaf == 'a',]
#simplify dataframe
rel_leaves <- rel_leaves[c("filename", 'cond', 'number', 'value_mm.whole_area', 'value_mm.whole_length')]
# separate out arbitrary 'b' leaves
rel_leaves_b <- big_leaf_d[big_leaf_d$leaf == 'b',]
# make sure entries match up (Some pairs lost to herbiv)
rel_leaves$id <- paste0(rel_leaves$cond, rel_leaves$filename, rel_leaves$number)
rel_leaves_b$id <- paste0(rel_leaves_b$cond, rel_leaves_b$filename, rel_leaves_b$number)
# filter based on same parent
rel_leaves <- rel_leaves %>% filter( rel_leaves$id %in% rel_leaves_b$id)
# add area and length from second leaf
rel_leaves$b_whole_area <- rel_leaves_b$value_mm.whole_area
rel_leaves$b_whole_length <- rel_leaves_b$value_mm.whole_length
# add cols for comparison that should be directional
rel_leaves <- rel_leaves %>% mutate(rel_area = if_else(b_whole_area > value_mm.whole_area, 
                                            b_whole_area/value_mm.whole_area, value_mm.whole_area/b_whole_area),
                                    rel_length = if_else(b_whole_length > value_mm.whole_length, 
                                                                 b_whole_length/value_mm.whole_length, value_mm.whole_length/b_whole_length))

small_box_helper(rel_leaves, c('rel_area', 'rel_length'), T)
# means for manu text
rel_leaves_summary <- rel_leaves %>% group_by(cond) %>% summarise(avg_a = mean(rel_area),
                                                                  med_a = median(rel_area))

#### calculate cv
cv <- function(dat) {
  return(sd(dat)/mean(dat) *100)
}
cv_wt <- cv(rel_leaves[rel_leaves$cond == 'wt',]$rel_area)
cv_jawD <- cv(rel_leaves[rel_leaves$cond == 'jawD',]$rel_area)

# testing equality
# https://cran.r-project.org/web/packages/cvequality/vignettes/how_to_test_CVs.html
library(cvequality)
with(rel_leaves, asymptotic_test(rel_area, cond))

# ripples quant
ripples <- read.csv('/Users/kateharline/workspace/finals/wt_jawd_big_data_ripples.csv')
ripples$cond <- factor(ripples$cond, levels = c('wt', 'jawD'))
small_box_helper(ripples, c('ripples'), add_p = T)
