source('R/clean_soft_max.R')

data <- soft.max.clean(row_start_of_data = 19,
                       file_path = '~/Box Sync/Research NSF Bat1Health Collaboration/bat_immunoassays/EB018_microplate_BKA_S.aureus_Mouse_and_Bat.xlsx',
                       num_of_time_points = 9)

control.ratio.plot(data=data, positive.control = "S.aureus postive control")
control.ratio.plot(data=data, positive.control = "PBS negative conrtol")

data$which_row_num <- rep ( c(1,2,3,4,4,3,2,1)) 
data <- data %>%
    mutate (col_num = str_replace_all(column, "col_*", "")) %>%
    mutate (col_num = as.numeric (col_num)) %>%
      mutate(col_num = replace (col_num, col_num == 7, 6))  %>%
      mutate(col_num = replace (col_num, col_num == 8, 5))  %>%
      mutate(col_num = replace (col_num, col_num == 9, 4))  %>%
      mutate(col_num = replace (col_num, col_num == 10, 3))  %>%
      mutate(col_num = replace (col_num, col_num == 11, 2))  %>%
      mutate(col_num = replace (col_num, col_num == 12, 1))  %>%
    mutate (edge_num = which_row_num * col_num)


 data %>%
  unite(well, c(which_row, column)) %>%
  filter(sample != 'Blank') %>%
  ggplot() + theme_bw() +
  geom_line(aes(x=time, y= as.numeric(measure), group = well, col =  edge_num)) +
  scale_y_continuous(0,3)+ facet_wrap(~sample)  + 
  scale_color_gradient(low="red", high="blue")
 


 
 