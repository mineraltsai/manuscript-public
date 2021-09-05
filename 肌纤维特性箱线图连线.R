library(ggsci)
library(ggpubr)
library(readxl)
library(tidyverse)
fcbv <- read_excel("Desktop/饥饿文章/r包用Excel/肌纤维箱线图.xlsx")


# 统计摘要Mfa
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Mfa),
    sd = sd(Mfa),
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

p1<- ggpaired(fcbv_plot, x = 'Species', y = 'Mfa',id='id',
              color = 'Species', palette = "jco", 
              line.color = "gray", line.size = 0.4,
              short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Mfa,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")


p1
# 统计摘要Mfd
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Mfd),
    sd = sd(Mfd),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3","S6F0","S0F6")))
summ_fcbv

p2<- ggpaired(fcbv_plot, x = 'Species', y = 'Mfd',id='id',
              color = 'Species', palette = "jco", 
              line.color = "gray", line.size = 0.4,
              short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Mfd,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")
p2

# 统计摘要Mfdi
summ_fcbv<- fcbv %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Mfdi),
    sd = sd(Mfdi),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3","S6F0","S0F6")))
summ_fcbv

p3<-  ggpaired(fcbv_plot, x = 'Species', y = 'Mfdi',id='id',
               color = 'Species', palette = "jco", 
               line.color = "gray", line.size = 0.4,
               short.panel.labs = FALSE)+
  stat_compare_means(aes(label = ..p.signif..),
                     method = "t.test",paired = TRUE, comparisons = my_comparisons)+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+  
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Mfdi,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  
  geom_point(data=summ_fcbv,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  theme(legend.position = "none")+
  xlab("Species")
p3




library(cowplot)
plot_grid(p1, p2, p3, p3,
          labels = c('A', 'B', 'C', 'D'),
          align="hv", ncol=3)

plot_grid(p4, p2,p1, p3 ,p5,p6,
          labels = c('A', 'B', 'C', 'D','E', 'F'),
          align="hv", ncol=3
)