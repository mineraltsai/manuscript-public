library(gghalves)
library(ggsignif)
library(ggsci)
library(ggpubr)
library(readxl)
library(ggsignif)
mf <- read_excel("Desktop/饥饿文章/r包用Excel/肌纤维箱线图.xlsx")
library(tidyverse)

# 统计摘要Mfdi
summ_mf<- mf %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Mfdi),
    sd = sd(Mfdi),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3","S6F0","S0F6")))
summ_mf


# 数据转换  
mf_plot <- mf %>% 
  mutate(Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3","S6F0","S0F6")))
head(mf_plot)



p1<- ggplot(mf_plot , aes(x = Species, y = Mfdi, fill = Species))+
  geom_half_violin(aes(fill = Species),
                   position = position_nudge(x = .15, y = 0),
                   adjust=1.5, trim=FALSE, colour=NA, side = 'r') +
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Mfdi,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  geom_boxplot(aes(x = Species,y = Mfdi, fill = Species),
               outlier.shape = NA,
               width = .05,
               color = "black")+
  geom_point(data=summ_mf,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  geom_errorbar(data = summ_mf,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=.05,
                position=position_nudge(x = .1, y = 0)
  ) +
  scale_color_aaas() + 
  scale_fill_aaas()+
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+
  
  stat_compare_means(label.y = 2.4, method="t.test")+
  geom_signif(
    comparisons = list(c("S0F0","S0F3"),
                       c("S0F0", "S3F0"),
                       c("S3F0","S3F3"),
                       c("S0F0", "S6F0"),
                       c("S0F0", "S0F6")
    ),
    step_increase = 0.1,
    map_signif_level = T,
    test = t.test)+
  theme(legend.position = "none",
    axis.title.x = element_blank())
p1



# 统计摘要Mfd
summ_mf<- mf %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Mfd),
    sd = sd(Mfd),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3","S6F0","S0F6")))
summ_mf

p2<- ggplot(mf_plot , aes(x = Species, y = Mfd, fill = Species))+
  geom_half_violin(aes(fill = Species),
                   position = position_nudge(x = .15, y = 0),
                   adjust=1.5, trim=FALSE, colour=NA, side = 'r') +
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Mfd,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  geom_boxplot(aes(x = Species,y = Mfd, fill = Species),
               outlier.shape = NA,
               width = .05,
               color = "black")+
  geom_point(data=summ_mf,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  geom_errorbar(data = summ_mf,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=.05,
                position=position_nudge(x = .1, y = 0)
  ) +
  scale_color_aaas() + 
  scale_fill_aaas() +
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+
  
  stat_compare_means( label.y = 2, method="t.test")+
  geom_signif(
    comparisons = list(c("S0F0","S0F3"),
                       c("S0F0", "S3F0"),
                       c("S3F0","S3F3"),
                       c("S0F0", "S6F0"),
                       c("S0F0", "S0F6")
    ),
    step_increase = 0.1,
    map_signif_level = T,
    test = t.test)+
  theme(legend.position = "none",
    axis.title.x = element_blank())
p2

# 统计摘要Mfa
summ_mf<- mf %>% 
  group_by(Species) %>% 
  summarise(
    
    mean = mean(Mfa),
    sd = sd(Mfa),
    n = n()
  ) %>% 
  mutate(se = sd/sqrt(n),
         Species = factor(Species, levels = c("S0F0", "S0F3", "S3F0","S3F3","S6F0","S0F6")))
summ_mf

p3<- ggplot(mf_plot , aes(x = Species, y = Mfa, fill = Species))+
  geom_half_violin(aes(fill = Species),
                   position = position_nudge(x = .15, y = 0),
                   adjust=1.5, trim=FALSE, colour=NA, side = 'r') +
  geom_point(aes(x = as.numeric(Species)-0.1,
                 y = Mfa,color = Species),
             position = position_jitter(width = .05),size = .25, shape = 20) +
  geom_boxplot(aes(x = Species,y = Mfa, fill = Species),
               outlier.shape = NA,
               width = .05,
               color = "black")+
  geom_point(data=summ_mf,
             aes(x=Species,y = mean,group = Species, color = Species),
             shape=18,
             size = 1.5,
             position = position_nudge(x = .1,y = 0)) +
  geom_errorbar(data = summ_mf,
                aes(x = Species, y = mean, group = Species, colour = Species,
                    ymin = mean-se, ymax = mean+se),
                width=.05,
                position=position_nudge(x = .1, y = 0)
  ) +
  scale_color_aaas() + 
  scale_fill_aaas() +
  theme(panel.background = element_rect(fill = "NA"), 
        axis.line = element_line(size = 0.5, colour = "black")
  )+
  
  stat_compare_means(label.y = 2.2, method="t.test")+
  geom_signif(
    comparisons = list(c("S0F0","S0F3"),
                       c("S0F0", "S3F0"),
                       c("S3F0","S3F3"),
                       c("S0F0", "S6F0"),
                       c("S0F0", "S0F6")
    ),
    step_increase = 0.1,
    map_signif_level = T,
    test = t.test)+
  theme(legend.position = "none",
    axis.title.x = element_blank())
p3

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(readxl)
library(ggsci)
# The dataset is provided in the gapminder library
library(gapminder)
data <- read_excel("Desktop/饥饿文章/r包用Excel/肌纤维气泡图.xlsx")
data$Mfd
Mfd<-1/data$Mfd
Mfd
mfd<-Mfd
data<-data%>%cbind(mfd)
data
# Most basic bubble plot
p13<-data %>%
  arrange(desc('Mfa')) %>%
  #mutate(sample = factor(sample, sample)) %>%
  ggplot(aes(x=`Mfdi`, y=`Mfd`, size=`Mfa`, color=Groups, position_stack(vjust = 1))) +
  geom_point(alpha=0.5) +
  scale_size(range = c(0.1, 15), name="Mfa") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme(legend.position="right") +
  theme_classic() +
  geom_smooth(aes(
    col=Groups
  ), method = "loess", se=F
  )+
  scale_color_aaas()+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
  )
p13
library(cowplot)
p14<-plot_grid(p1, p2, p3, 
               labels=c('A','B','C'),
               ncol=1, nrow = 3,align="hv")
p14
plot_grid(p14,p13,labels='AUTO',nrow = 1)

