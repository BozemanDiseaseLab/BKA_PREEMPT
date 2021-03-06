#soft max pro reader function
library(tidyverse)

soft.max.clean <- function(row_start_of_data, file_path, num_of_time_points)
{
#data <- readxl::read_excel(file_path, col_names = FALSE, skip = row_start_of_data-1)
data <- readxl::read_excel(file_path, col_names = FALSE)
index <- which(grepl(pattern = "Time", unlist(data[,1]))== TRUE)
data <- data[-c(0:index), ]
data <- data[,1:14]

wells <- readxl::read_excel(file_path, col_names = FALSE, sheet = 2)
colnames(data) <- c("time", "temp_c", "col_1", "col_2", "col_3","col_4","col_5","col_6","col_7","col_8","col_9","col_10","col_11","col_12")

if(is.na(num_of_time_points))
{
  num_of_time_points <- nrow(data[!is.na(data$time), ])
  
}
   
end_of_data <- 9*num_of_time_points
data <- data[c(0:end_of_data),]

#remove blanks 
blanks <- seq(9,end_of_data,by=9)
data <- data[-c(blanks),]

#fix time variables
times <-  seq(1,end_of_data-9,by=8)
index <- 0

data$time_2 <- NA
for(i in (times))
{
  index <- index+1
  data[c(i:(i+7)),'time_2'] <- as.numeric(data[times[index],'time'])
}

data$time <- data$time_2
data$time_2 <- NULL
data$which_row <- LETTERS[1:8]

data.tidy <- data %>%
  select(-c(temp_c)) %>%
  group_by(time, which_row) %>%
  gather(key=column, value = measure, 2:13)

colnames(wells) <- c("col_1", "col_2", "col_3","col_4","col_5","col_6","col_7","col_8","col_9","col_10","col_11","col_12")
wells$which_row <- LETTERS[1:8]

wells.tidy <- wells %>%
  group_by(which_row) %>%
  gather(key=column, value = sample, 1:12)

data.tidy.join <- full_join(data.tidy, wells.tidy)

return(data.tidy.join)
}

control.ratio.plot <- function(data, positive.control)
{
  data_0 <- data[data$time == 0, ]
  data_n0 <- data[data$time != 0, ]
  
  data.join <- full_join(data_0, data_n0, by =c('which_row', 'column', 'sample'))
  
  data_pos <- data.join[data.join$sample == positive.control, ]
  data_sample <- data.join[data.join$sample != positive.control, ]
  
  data.join$neg_control <- data_pos$measure.y
  
plot<- data.join %>%
    unite(well, c(which_row, column)) %>%
    mutate(rate_ratio = as.numeric(measure.y)/as.numeric(neg_control)) %>%
    filter(sample != 'Blank') %>%
    ggplot() +
    geom_line(aes(x=time.y, y= as.numeric(rate_ratio), group = well, col = sample)) +
    xlim(0,1)+
    ylim(0,3)+
    facet_wrap(~sample) 
return(plot)
}
  





