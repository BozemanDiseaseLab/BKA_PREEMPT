source('R/clean_soft_max.R')

data <- soft.max.clean(row_start_of_data = 19,
                       file_path = '~/Box Sync/Research NSF Bat1Health Collaboration/bat_immunoassays/EB018_microplate_BKA_S.aureus_Mouse_and_Bat.xlsx',
                       num_of_time_points = 9)

control.ratio.plot(data=data, positive.control = "S.aureus postive control")

control.ratio.plot(data=data, positive.control = "PBS negative conrtol")


data %>%
unite(well, c(which_row, column)) %>%
filter(sample != 'Blank') %>%
ggplot() +
  geom_line(aes(x=time, y= as.numeric(measure), group = well, col = sample)) +
  scale_y_continuous(0,3)+
  facet_wrap(~sample) 



