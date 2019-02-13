#edge effects 

library(tidyverse)

Exp_014_E_coli_BKA_2_5_19 <- read_excel("data/Exp_014_E.coli_BKA_2.5.19.xlsx")

Exp_014_E_coli_BKA_2_5_19$which_bat <-  rep(1:2, each=4, length.out=nrow(Exp_014_E_coli_BKA_2_5_19))
#Exp_014_E_coli_BKA_2_5_19$edge <- rep(c(1,rep(0,each=6),1 ))
Exp_014_E_coli_BKA_2_5_19$edge.specific <- c(1,2,3,4,4,3,2,1)
Exp_014_E_coli_BKA_2_5_19$which_row <- rep(c(1:8))

#colnames(Exp_014_E_coli_BKA_2_5_19)[(3:14)] <-  (c(c(1:6),c(6.001,5.001,4.001,3.001,2.001,1.001)))
#Exp_014_E_coli_BKA_2_5_19$which_comp  <- rep(c(rep(1,each=4), rep(0,each=4)))

Exp_014_E_coli_BKA_2_5_19  %>%
  #select(-c(deg, PBS, `NA EDTA`)) %>% 
  group_by(Tme, which_row) %>%
  #gather(variable, measurement, 2:12) %>%
  ggplot() +
  geom_point(aes(x=Tme, y= E.coli, group = as.factor(which_row), col = as.factor(edge.specific))) + 
  geom_line(aes(x=Tme, y= E.coli, group = as.factor(which_row), col = as.factor(edge.specific))) 

ggsave('figures/Ecoli.jpeg', last_plot())

Exp_014_E_coli_BKA_2_5_19  %>%
  #select(-c(deg, PBS, `NA EDTA`)) %>% 
  group_by(Tme, which_row) %>%
  #gather(variable, measurement, 2:12) %>%
  ggplot() +
  geom_point(aes(x=Tme, y= `PBS EDTA`, group = as.factor(which_row), col = as.factor(edge.specific))) + 
  geom_line(aes(x=Tme, y= `PBS EDTA`, group = as.factor(which_row), col = as.factor(edge.specific)))

ggsave('figures/PBS EDTA.jpeg', last_plot())


Exp_014_E_coli_BKA_2_5_19 <- read_excel("data/Exp_014_E.coli_BKA_2.5.19.xlsx")

Exp_014_E_coli_BKA_2_5_19$which_bat <-  rep(1:2, each=4, length.out=nrow(Exp_014_E_coli_BKA_2_5_19))
#Exp_014_E_coli_BKA_2_5_19$edge <- rep(c(1,rep(0,each=6),1 ))
Exp_014_E_coli_BKA_2_5_19$edge.specific <- c(1,2,3,4,4,3,2,1)
colnames(Exp_014_E_coli_BKA_2_5_19)[(3:14)] <-  (c(c(1:6),c(6.001,5.001,4.001,3.001,2.001,1.001)))
#Exp_014_E_coli_BKA_2_5_19$which_row <- rep(c(1:8))
#Exp_014_E_coli_BKA_2_5_19$which_comp  <- rep(c(rep(1,each=4), rep(0,each=4)))

x <- Exp_014_E_coli_BKA_2_5_19  %>%
  select(-c(deg)) %>% 
  mutate(Tme = as.character(Tme)) %>%
  dplyr::filter((Tme) ==  "1899-12-31 16:00:00") %>%
  group_by(Tme, edge.specific) %>%
  gather(variable, measurement, 2:13) %>%
  mutate(product = edge.specific * as.numeric(variable)) 

x%>%
  ggplot() +
  geom_point(aes(x=log(product), y= (measurement), group = as.factor(product), col = (product))) +
  xlab('log (distance from side of plate * distance from top of plate)')

ggsave('figures/BKA_issue1.jpeg', last_plot())
lm(measurement ~ log(product), data = x) %>% summary()

#it doesnt make any sense to go 1--> 12 because row 1 and row 12 have the same edge effect 
#so go 1 to 6, then 6 to 1



