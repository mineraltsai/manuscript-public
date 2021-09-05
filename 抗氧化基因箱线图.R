library(ggsci)
library(ggpubr)
library(readxl)
library(tidyverse)
library(dplyr)
fcbv <- read_excel("Desktop/饥饿文章/r包用Excel/抗氧化基因.xlsx")
fcbv

# 统计摘要mMnSOD
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(mMnSOD),
    sd = sd(mMnSOD),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
summ_fcbv


# 数据转换  
fcbv_plot <- fcbv %>% 
  mutate(Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
head(fcbv_plot)

my_comparisons <- list( c("S0F0","S0F3"),
                        c("S3F0","S3F3"),
                        c("S0F0", "S3F0")
)


p1<-  ggplot(fcbv_plot , aes(x = Species, y = mMnSOD, fill = Species))+
        geom_col(data=summ_fcbv,
           aes(x=Species,y = mean,group = Species, color = Species),
           width = 0.5,
           inherit.aes=TRUE
           )+
      geom_errorbar(data = summ_fcbv,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=0.3
               # position=position_nudge(x = .1, y = 0)
               ) +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+
  theme(legend.position = "none")+
  xlab("Species")+
  scale_y_continuous(expand = c(0,0))


p1

# 统计摘要cMnSOD
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(cMnSOD),
    sd = sd(cMnSOD),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
summ_fcbv

p2<- ggplot(fcbv_plot , aes(x = Species, y = cMnSOD, fill = Species))+
  geom_col(data=summ_fcbv,
           aes(x=Species,y = mean,group = Species, color = Species),
           width = 0.5,
           inherit.aes=TRUE
  )+
  geom_errorbar(data = summ_fcbv,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=0.3
                # position=position_nudge(x = .1, y = 0)
  ) +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+
  theme(legend.position = "none")+
  xlab("Species")+
  scale_y_continuous(expand = c(0,0))
p2

# 统计摘要CAT
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(CAT),
    sd = sd(CAT),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
summ_fcbv

p3<-  ggplot(fcbv_plot , aes(x = Species, y = CAT, fill = Species))+
  geom_col(data=summ_fcbv,
           aes(x=Species,y = mean,group = Species, color = Species),
           width = 0.5,
           inherit.aes=TRUE
  )+
  geom_errorbar(data = summ_fcbv,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=0.3
                # position=position_nudge(x = .1, y = 0)
  ) +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  theme(legend.position = "none")+
  xlab("Species")+
  scale_y_continuous(expand = c(0,0))
p3


# 统计摘要GR
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(GR),
    sd = sd(GR),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
summ_fcbv


# 数据转换  
fcbv_plot <- fcbv %>% 
  mutate(Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
head(fcbv_plot)

my_comparisons <- list( c("S0F0","S0F3"),
                        c("S3F0","S3F3"),
                        c("S0F0", "S3F0")
)


p4<- ggplot(fcbv_plot , aes(x = Species, y = GR, fill = Species))+
  geom_col(data=summ_fcbv,
           aes(x=Species,y = mean,group = Species, color = Species),
           width = 0.5,
           inherit.aes=TRUE
  )+
  geom_errorbar(data = summ_fcbv,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=0.3
                # position=position_nudge(x = .1, y = 0)
  ) +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  theme(legend.position = "none")+
  xlab("Species")+
  scale_y_continuous(expand = c(0,0))


p4
# 统计摘要gst
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(gst),
    sd = sd(gst),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
summ_fcbv

p5<- ggplot(fcbv_plot , aes(x = Species, y = gst, fill = Species))+
  geom_col(data=summ_fcbv,
           aes(x=Species,y = mean,group = Species, color = Species),
           width = 0.5,
           inherit.aes=TRUE
  )+
  geom_errorbar(data = summ_fcbv,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=0.3
                # position=position_nudge(x = .1, y = 0)
  ) +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  theme(legend.position = "none")+
  xlab("Species")+
  scale_y_continuous(expand = c(0,0))
p5

# 统计摘要gpx
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(gpx),
    sd = sd(gpx),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
summ_fcbv

p6<- ggplot(fcbv_plot , aes(x = Species, y = gpx, fill = Species))+
  geom_col(data=summ_fcbv,
           aes(x=Species,y = mean,group = Species, color = Species),
           width = 0.5,
           inherit.aes=TRUE
  )+
  geom_errorbar(data = summ_fcbv,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=0.3
                # position=position_nudge(x = .1, y = 0)
  ) +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+
  theme(legend.position = "none")+
  xlab("Species")+
  scale_y_continuous(expand = c(0,0))
p6

# 统计摘要Nrf2
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Nrf2),
    sd = sd(Nrf2),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
summ_fcbv

p7<- ggplot(fcbv_plot , aes(x = Species, y = Nrf2, fill = Species))+
  geom_col(data=summ_fcbv,
           aes(x=Species,y = mean,group = Species, color = Species),
           width = 0.5,
           inherit.aes=TRUE
  )+
  geom_errorbar(data = summ_fcbv,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=0.3
                # position=position_nudge(x = .1, y = 0)
  ) +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  theme(legend.position = "none")+
  xlab("Species")+
  scale_y_continuous(expand = c(0,0))
p7

# 统计摘要Keap1
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Keap1),
    sd = sd(Keap1),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3")))
summ_fcbv

p8<- ggplot(fcbv_plot , aes(x = Species, y = Keap1, fill = Species))+
  geom_col(data=summ_fcbv,
           aes(x=Species,y = mean,group = Species, color = Species),
           width = 0.5,
           inherit.aes=TRUE
  )+
  geom_errorbar(data = summ_fcbv,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=0.3
                # position=position_nudge(x = .1, y = 0)
  ) +
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  theme(legend.position = "none")+
  xlab("Species")+
  scale_y_continuous(expand = c(0,0))
p8

library(cowplot)
plot_grid(p7, p8, p7, p8,p7, p8,
          labels = c('A', 'B', 'A', 'B','A', 'B'),
          align="hv", ncol=3)

plot_grid(p1, p2,p3, p4 ,p5,p6,
          labels = c('A', 'B', 'C', 'D','E', 'F'),
          align="hv", ncol=3
)

