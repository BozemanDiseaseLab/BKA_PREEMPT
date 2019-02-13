library(tidyverse)
Exp_014_E_coli_BKA_2_5_19$which_bat <-  rep(1:2, each=4, length.out=nrow(Exp_014_E_coli_BKA_2_5_19))

Exp_014_E_coli_BKA_2_5_19$which_bat

Exp_014_E_coli_BKA_2_5_19 <- Exp_014_E_coli_BKA_2_5_19 %>%
  mutate(bat_5 = ifelse(which_bat == 1, `Bat 5/8`, NA)) %>%
  mutate(bat_8 = ifelse(which_bat == 2, `Bat 5/8`, NA)) %>%
  mutate(bat_6 = ifelse(which_bat == 1, `Bat 6/9`, NA)) %>%
  mutate(bat_9 = ifelse(which_bat == 2, `Bat 6/9`, NA)) %>%
  mutate(bat_7 = ifelse(which_bat == 1, `Bat 7/10`, NA)) %>%
  mutate(bat_10 = ifelse(which_bat == 2, `Bat 7/10`, NA)) %>%
  select(-c(`Bat 5/8`, `Bat 6/9`, `Bat 7/10`))

Exp_014_E_coli_BKA_2_5_19 %>%
  ggplot() +
  geom_point(aes(x= Tme, y = bat_5))

Exp_014_E_coli_BKA_2_5_19 %>%
  dplyr::select(-c(deg, which_bat)) %>%
  group_by(Tme) %>%
  gather(variable, measurement, 2:16) %>%
  ggplot() +
  geom_point(aes(x=Tme, y= measurement, group = variable, col = variable)) + 
  facet_wrap(~variable)

Exp_014_E_coli_BKA_2_5_19 %>%
  dplyr::select(-c(deg, which_bat)) %>%
  select(c(Tme, bat_10, E.coli)) %>%
  group_by(Tme) %>%
  gather(variable, measurement, 2:3) %>%
  ggplot() +
  geom_point(aes(x=Tme, y= measurement, group = variable, col = variable))# + 
  #facet_wrap(~variable)

Exp_014_E_coli_BKA_2_5_19 %>%
  dplyr::select(-c(deg, which_bat)) %>%
  select(c(Tme, bat_10, E.coli)) %>%
  group_by(Tme) %>%
  gather(variable, measurement, 2:3) %>%
  group_by(Tme, variable) %>%
  summarise(measurement= mean(measurement, na.rm=TRUE)) %>%
  ggplot() +
  geom_point(aes(x=Tme, y= measurement, group = variable, col = variable))# + 
#facet_wrap(~variable)





#question 1: PBS serum vs Mouse Serum. Isn't mouse serum diluted in PBS? what is the difference here?   
# Bat samples, are these plasma or serum samples? oh w8 its serum 
  
  