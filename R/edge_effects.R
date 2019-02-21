#edge effects 

library(tidyverse)
suppressMessages(library(readxl))

Exp_014_E_coli_BKA_2_5_19 <- read_xlsx("data/Exp_014_E.coli_BKA_2.5.19.xlsx")
head(Exp_014_E_coli_BKA_2_5_19)

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

# ggsave('figures/Ecoli.jpeg', last_plot())

Exp_014_E_coli_BKA_2_5_19  %>%
  #select(-c(deg, PBS, `NA EDTA`)) %>% 
  group_by(Tme, which_row) %>%
  #gather(variable, measurement, 2:12) %>%
  ggplot() +
  geom_point(aes(x=Tme, y= `PBS EDTA`, group = as.factor(which_row), col = as.factor(edge.specific))) + 
  geom_line(aes(x=Tme, y= `PBS EDTA`, group = as.factor(which_row), col = as.factor(edge.specific)))

# ggsave('figures/PBS EDTA.jpeg', last_plot())


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
  dplyr::filter((Tme) ==  "1899-12-31 16:00:00") %>% # change time
  group_by(Tme, edge.specific) %>%
  gather(variable, measurement, 2:13) %>%
  mutate(product = edge.specific * as.numeric(variable)) 

x%>%
  ggplot() +
  geom_point(aes(x=log(product), y= (measurement), group = as.factor(product), col = (product))) +
  xlab('log (distance from side of plate * distance from top of plate)')
# ggsave('figures/BKA_issue1.jpeg', last_plot())

# flip
x%>%
  ggplot() +
  geom_point(aes(y=(product), x= (measurement), group = as.factor(product), col = (product))) +
  ylab('distance from side * distance from top')

# ggsave('figures/BKAExp_014_hour08.jpeg', last_plot())
# ggsave('figures/BKAExp_014_hour10.jpeg', last_plot())
# ggsave('figures/BKAExp_014_hour12.jpeg', last_plot())
# ggsave('figures/BKAExp_014_hour16.jpeg', last_plot())

lm(measurement ~ log(product), data = x) %>% summary()

#it doesnt make any sense to go 1--> 12 because row 1 and row 12 have the same edge effect 
#so go 1 to 6, then 6 to 1


# E.coli and MRSA growth optimization #### 

go_MRSA <- read_xlsx("data/BKA_Growth_Optimization_Exp.xlsx",  "MRSA")
go_Ecoli <- read_xlsx("data/BKA_Growth_Optimization_Exp.xlsx",  "E.coli_001")


go_MRSA$conc <-  rep(1:3, each=4, length.out=nrow(go_MRSA))
#Exp_014_E_coli_BKA_2_5_19$edge <- rep(c(1,rep(0,each=6),1 ))
go_MRSA$edge.specific <- c(1,2,3,3,2,1)
go_MRSA$which_row <- rep(c(1:6))

head(go_MRSA)

go_MRSA  %>%
  select(-c("..16", Mean, SD, "..19", OD)) %>% 
  group_by(Time_hr, which_row) %>%
  #gather(variable, measurement, 2:12) %>%
  ggplot() +
  geom_point(aes(x=Time_hr, y= x1, group = as.factor(which_row), col = as.factor(edge.specific))) + 
  geom_line(aes(x=Time_hr, y= x1, group = as.factor(which_row), col = as.factor(edge.specific))) 

# ggsave('figures/    .jpeg', last_plot())


head(go_Ecoli)
go_Ecoli$conc <-  rep(1:3, each=4, length.out=nrow(go_Ecoli))
#Exp_014_E_coli_BKA_2_5_19$edge <- rep(c(1,rep(0,each=6),1 ))
go_Ecoli$edge.specific <- c(1,2,3,3,2,1)
go_Ecoli$which_row <- rep(c(1:6))

go_Ecoli  %>%
  #select(-c(deg, PBS, `NA EDTA`)) %>% 
  group_by(Time_hr, which_row) %>%
  #gather(variable, measurement, 2:12) %>%
  ggplot() +
  geom_point(aes(x=Time_hr, y= x1, group = as.factor(which_row), col = as.factor(edge.specific))) + 
  geom_line(aes(x=Time_hr, y= x1, group = as.factor(which_row), col = as.factor(edge.specific))) 


colnames(go_Ecoli)[(4:15)] <-  (c(c(1:6),c(6.001,5.001,4.001,3.001,2.001,1.001)))
#Exp_014_E_coli_BKA_2_5_19$which_row <- rep(c(1:8))
#Exp_014_E_coli_BKA_2_5_19$which_comp  <- rep(c(rep(1,each=4), rep(0,each=4)))



# only at first time point (hour = 6)
x <- go_Ecoli  %>%
  select(-c(DegC)) %>% 
  mutate(Tme = as.character(Time_hr)) %>%
  dplyr::filter((Time_hr) ==  "20") %>% # change to look at different hours
  group_by(Tme, edge.specific) %>%
  gather(variable, measurement, 3:14) %>%
  mutate(product = edge.specific * as.numeric(variable)) 

# w/o loess
x%>% 
  ggplot(aes (x=log(product), y= (measurement))) +
  geom_point(aes(group = as.factor(product), col = (product),shape = as.factor(Bact.Conc) )) +
  xlab('log (distance from side of plate * distance from top of plate)
       edge ------------> center')

# w loess
x%>% 
  ggplot(aes (x=log(product), y= (measurement))) +
  geom_point(aes(group = as.factor(product), col = (product),shape = as.factor(Bact.Conc) )) +
  xlab('log (distance from side of plate * distance from top of plate)
       edge ------------> center') + 
  stat_smooth(aes(group = as.factor(Bact.Conc), col = as.factor(product)),n = 4)

# reverse x & y
x%>% 
  ggplot(aes (y=log(product), x= (measurement))) +
  geom_point(aes(group = as.factor(product), col = (product),shape = as.factor(Bact.Conc) )) +
  ylab("log (distance from side of plate * distance from top of plate)")

# ggsave('figures/Ecoli_hour6.jpeg', last_plot()) # why is PBS different at all
# ggsave('figures/Ecoli_hour10.jpeg', last_plot())
# ggsave('figures/Ecoli_hour16.jpeg', last_plot())
ggsave('figures/Ecoli_hour20.jpeg', last_plot())





