library(mtconnectR)
library(janitor)
library(stringr)

library(tidyverse)
library(dplyr)

base_dirs <- c("0728_wt_2-4", "wt_2-4", "wt_1-5", "jawD_2-2", "jawD_2-4", "jawD_2-7")
dir_conds <- c("wt", "wt", "wt", "jawD", "jawD", "jawD", "jawD")
dot_dir <- "/Users/kateharline/workspace/finals"


####  open each file, do data cleaning, manipulation
big_d <- data.frame()

for (k in 1:length(base_dirs)){
  setwd(file.path(dot_dir, base_dirs[k], "attributes"))
  
  # make list of files in dir
  csv_names <- dir(pattern = ".csv")
  # dummy list to store the csv files
  csvs <- list()
  
  # ~~ Clean the data 
  # open each csv in the list
  for (j in 1:length(csv_names)) {
    csvs[[j]] <- read.csv(csv_names[j],check.names = F)
    
    # remove non-numeric label entries
    csvs[[j]] <- subset(csvs[[j]], grepl('^\\d+$', csvs[[j]]$Label))
    #remove parents column https://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
    #add parent and stomata column so day 1 can bind
    csvs[[1]]$Parent <- NA
    csvs[[1]]$Stomata_Distance <- NaN
    # set all to NA https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
    # remove columns where no data was collected https://rdrr.io/cran/janitor/man/remove_empty.html
    csvs[[j]] <- remove_empty(csvs[[j]], which = c("cols"), quiet=TRUE)
    # remove any entries lacking growth data https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
    #csvs[[j]] <- csvs[[j]][complete.cases(csvs[[j]]), ]
    # remove entries missing growth tracking https://stackoverflow.com/questions/7980622/subset-of-rows-containing-na-missing-values-in-a-chosen-column-of-a-data-frame

    if (j < length(csv_names)) {
      # check tracked (curate)
      csvs[[j]] <- csvs[[j]][!(is.na(csvs[[j]]$d_Area)), ]
      # adjust growth and prolif to typical representation
      # https://stackoverflow.com/questions/30020466/error-in-lisi-attempt-to-select-less-than-one-element
      csvs[[j]]$d_Proliferation <- csvs[[j]]$d_Proliferation -1 
      csvs[[j]]$d_Area <- (csvs[[j]]$d_Area -1)*100
    }
    
    if (j == length(csv_names)){
      # check tracked (curated)
      csvs[[j]] <- csvs[[j]][!(is.na(csvs[[j]]$Parent)), ]
      # add blank entries to last day for growth and prolif
      csvs[[length(csv_names)]]$d_Proliferation <-  rep(0, length(csvs[[length(csvs)]][[1]]))
      csvs[[length(csv_names)]]$d_Area <-  rep(0, length(csvs[[length(csvs)]][[1]]))
      csvs[[length(csv_names)]]$aniso_angle_max <-  rep(0, length(csvs[[length(csvs)]][[1]]))
      csvs[[length(csv_names)]]$StretchCustomX <-  rep(0, length(csvs[[length(csvs)]][[1]]))
      csvs[[length(csv_names)]]$StretchCustomY <-  rep(0, length(csvs[[length(csvs)]][[1]]))
    }
    # export cleaned
    #write.csv(csvs[[j]], paste0('cleaned/',tools::file_path_sans_ext(csv_names[j]),'_cleaned.csv'))
    # add time column
    csvs[[j]]$time  <- rep(j+2, length(csvs[[j]][[1]]))
    
    #add sample name column
    csvs[[j]]$sample_id <- rep(paste(base_dirs[k]), length(csvs[[j]][[1]]))
    
    # replace NA with 0 for distance measures https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
    #change_nas <- c("Medial-Lateral_Distance", "Proximal-Distal_lamina_Distance", "Proximal-Distal_Distance")
    csvs[[j]] <- csvs[[j]] %>% mutate_at(vars(colnames(csvs[[j]])), ~replace(., is.na(.), 0))
    csvs[[j]] <- csvs[[j]] %>% mutate(`Proximal-Distal_petiole_Distance` = 
                                        if_else(`Proximal-Distal_lamina_Distance` == 0, `Proximal-Distal_Distance`,
                                                NaN))
    
    # normalize the dists to max dist
    csvs[[j]][["Medial-Lateral_Distance_norm"]] <- heatmaply::normalize(csvs[[j]][["Medial-Lateral_Distance"]])
    csvs[[j]][["Proximal-Distal_Distance_norm"]] <- heatmaply::normalize(csvs[[j]][["Proximal-Distal_Distance"]])
    csvs[[j]][["Proximal-Distal_lamina_Distance_norm"]] <- heatmaply::normalize(csvs[[j]][["Proximal-Distal_lamina_Distance"]])
    csvs[[j]][["Proximal-Distal_petiole_Distance_norm"]] <- heatmaply::normalize(csvs[[j]][["Proximal-Distal_petiole_Distance"]])
    csvs[[j]] <- csvs[[j]] %>% mutate_at(vars(colnames(csvs[[j]])), ~replace(., is.na(.), 0))
    
    # remove cols with name conflict or otherwise dumb
    csvs[[j]] <- select(csvs[[j]], -contains(c('Cell Axis', 'CustomDirections', 'Common Bending')))

  }

  # combine data for each data category
  df <- do.call('rbind', csvs)
  
  # add col for cond
  df$cond <- rep(paste(dir_conds[k]), length(df$time))
  
  # append to master df
  big_d <- do.call('rbind', list(df, big_d))
  
}
# remove weird cells with 
big_d <- big_d[big_d$`Geometry/Perimeter` > 0,]

biggest_d <- big_d

# remove columns 
#big_d <- big_d[, -c(32:56)]

setwd(dot_dir)
write.csv(big_d, file.path(dot_dir, "big_d.csv"))

# don't need parent column
# big_d$Parent <- NULL
# 
# # get rid of measures don't need/have a lot of missing values
# big_d$`Location/Cell Distance` <- NULL
# big_d$`Shape/Common Bending` <- NULL
# big_d$`Geometry/Junction Distance` <- NULL
# 
# 
# # handle NA's for later
# big_d <- big_d[complete.cases(big_d), ]
# 
# write.csv(big_d, file.path(dot_dir, "smaller_d.csv"))
# 
# 
# # check which cells are being left out
# # select by day labels in biggest d not in big d
# # Label time
# 
# # time : Labels (list)
# 
# biggest_d_labels <- biggest_d[,c('Label', 'time', 'cond')]
# big_d_labels <- big_d[,c('Label', 'time', 'cond')]
# 
# # https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in
# missing <- setdiff(biggest_d_labels, big_d_labels)
# 
# # make time and cond one variable
# missing$time_cond <- paste0(missing$cond, '_', missing$time)
# missing$time <- NULL
# missing$cond <- NULL
# 
# # vignette("pivot") 
# missing <- missing %>% pivot_wider(names_from = time_cond,
#                                    values_from = Label)
# # transpose
# missing <- t(missing)
# 
# write.table(missing, file = "~/Desktop/missing_cells.txt")
