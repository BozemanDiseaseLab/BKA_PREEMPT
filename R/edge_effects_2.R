Corrected_BKA_Growth_Data <- read_excel("data/Corrected BKA Growth Data.xlsx", col_names = FALSE)

Corrected_BKA_Growth_Data$row <- c(1,2,3,4,5,6)
colnames(Corrected_BKA_Growth_Data)[(3:14)] <-  (c(c(1:6),c(6.001,5.001,4.001,3.001,2.001,1.001)))


x <- Corrected_BKA_Growth_Data  %>%
  dplyr::filter((X__1) ==  20) %>% # change time
  dplyr::filter(X__2 == '10^4') %>%
  select(-c(X__1)) %>%
  group_by(row) %>%
  gather(variable, measurement, 2:13) #%>%
  #mutate(product = row * as.numeric(variable)) 

x%>%
  ggplot() +
  geom_point(aes(x=(variable), y= (measurement), group = as.factor(row), col = (row))) +
  geom_abline(intercept = 1.82310, slope = 0.012744)

lm(measurement ~ as.numeric(variable), data = x) %>% summary()





Corrected_BKA_Growth_Data %>% 
  filter(X__2 == '10^5') %>%
  ggplot() +
  geom_line(aes(x = X__1, y=  X__3, col = row, group = row)) +
  geom_line(aes(x = X__1, y=  X__6, col = row, group = row), linetype = 2)
  
Corrected_BKA_Growth_Data %>% 
  filter(X__2 == '10^4') %>%
  ggplot() +
  geom_line(aes(x = X__1, y=  X__3, col = row, group = row)) +
  geom_line(aes(x = X__1, y=  X__6, col = row, group = row), linetype = 2)
