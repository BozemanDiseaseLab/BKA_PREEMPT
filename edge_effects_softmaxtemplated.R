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
 
 


# old bka dat that had edge effect problem ? different template
 data2 <- read_excel("data/Exp_014_E.coli_BKA_2.5.19.xlsx")
 data2$which_bat <-  rep(1:2, each=4, length.out=nrow(data2))
 data2$edge.specific <- c(1,2,3,4,4,3,2,1)
 colnames(data2)[(3:14)] <-  (c(c(1:6),c(6.001,5.001,4.001,3.001,2.001,1.001)))
 data2$which_row <- LETTERS[1:8]
 
 data2.tidy <- data2  %>%
   gather(variable, measurement, 3:14) %>%
   mutate(product =  edge.specific *as.numeric (variable) ) %>%
   select(-c(deg)) %>%
   group_by(Tme, which_row)  %>%
   mutate(tme2 =  rep (c(1:12), each = 9)) # change back to 8 if doesnt work
 
 wells <- readxl::read_excel( 'data/soft_max_template.xlsx', col_names = FALSE, sheet = 2)
 colnames(wells) <- c("col_1", "col_2", "col_3","col_4","col_5","col_6","col_7","col_8","col_9","col_10","col_11","col_12")
 wells$which_row <- NA
 wells$which_row[1:8] <- LETTERS[1:8]
 
 wells.tidy <- wells[1:8,] %>%
   group_by(which_row)  %>%
   gather(key=column, value = sample, 1:12)

 data.tidy.join <- full_join(data2.tidy, wells.tidy)
 
 data.tidy.join %>%
   ggplot() + theme_bw() +
   geom_point(aes(y= measurement, x= tme2, group = sample, col = (product) )) +
   scale_color_gradient(low="red", high="blue") 
 

 

