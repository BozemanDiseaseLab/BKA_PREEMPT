#edge effects 

library(tidyverse)

Exp_014_E_coli_BKA_2_5_19 <- read_excel("data/Exp_014_E.coli_BKA_2.5.19.xlsx")

Exp_014_E_coli_BKA_2_5_19$which_bat <-  rep(1:2, each=4, length.out=nrow(Exp_014_E_coli_BKA_2_5_19))
Exp_014_E_coli_BKA_2_5_19$edge <- rep(c(1,rep(0,each=6),1 ))
Exp_014_E_coli_BKA_2_5_19$which_row <- rep(c(1:8))

Exp_014_E_coli_BKA_2_5_19$which_comp  <- rep(c(rep(1,each=4), rep(0,each=4)))

Exp_014_E_coli_BKA_2_5_19  %>%
  select(-c(deg, PBS, `NA EDTA`)) %>% 
  group_by(Tme, which_row) %>%
  #gather(variable, measurement, 2:12) %>%
  ggplot() +
  geom_point(aes(x=Tme, y= E.coli, group = as.factor(which_row), col = as.factor(edge))) + 
  geom_line(aes(x=Tme, y= E.coli, group = as.factor(which_row), col = as.factor(edge))) + 
  geom_point(aes(x=Tme, y= `PBS EDTA`, group = as.factor(which_row), col = as.factor(edge))) + 
  geom_line(aes(x=Tme, y= `PBS EDTA`, group = as.factor(which_row), col = as.factor(edge)))
