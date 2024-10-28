library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggpie)
library(showtext)
showtext_auto()
df <- read_excel('df.xls')
df %>% drop_na(总得分) %>% distinct(种中名,.keep_all = T)-> df1   
range(df1$总得分)
df1 %>% slice_max(总得分,n=10)->dfmax
df1 %>% slice_min(总得分,n=10)->dfmin
dfhighlight <- rbind(dfmax,dfmin)
pzdf1 <- ggplot(df1,aes(种中名,总得分))+geom_point(aes(size=总得分,fill=总得分),
                                        shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = dfhighlight,
                  aes(种中名,总得分,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=c(5,10,14,18,21,25),
                    colors = brewer.pal(6, "Blues"))+
  labs(xlab='',ylab='估量值',size='估量值',fill='估量值')


df1 %>% mutate(总得分分类=cut(总得分,
                             breaks = c(5,10,14,18,21,25),include.lowest = T,
                             right = F))->dfnew1
pzdf2 <- ggpie(data=dfnew1, group_key = '总得分分类', label_info = "all",border_color=NA,
                count_type = "full",label_split=NULL,label_type = "circle",label_pos = "in",
                label_size = 3)+scale_fill_brewer(palette = 'Blues')+theme_void(base_size = 14)+
  labs(fill='估量值分类')

pzdfall <- pzdf1+pzdf2+plot_annotation(tag_levels = 'A')
tiff('估量值.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
pzdfall
dev.off()

pzdfall <- pzdf1+pzdf2+plot_annotation(tag_levels = 'A')
pdf('估量值.pdf',width = 15,height = 6)
pzdfall
dev.off()

#大于21分
df1 %>% filter(总得分>=21) %>% slice_sample(n=10)->df21sample
p21 <- df1 %>% filter(总得分>=21) %>% ggplot(aes(种中名,总得分))+geom_point(aes(size=总得分,fill=总得分),
                                             shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df21sample,
                  aes(种中名,总得分,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=c(21,22,23,24,25),
                    colors = brewer.pal(6, "Blues"))+
  guides(fill=guide_colorsteps(order = 1),
         size=guide_legend(order = 2))+
  labs(xlab='',ylab='估量值',size='估量值',fill='估量值')


#18-21
df1 %>% filter(between(总得分,18,21)) %>% slice_sample(n=10)->df1821sample
p1821 <- df1 %>% filter(between(总得分,18,21)) %>% ggplot(aes(种中名,总得分))+geom_point(aes(size=总得分,fill=总得分),
                                                                   shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df1821sample,
                  aes(种中名,总得分,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=c(18,19,20,21),
                    colors = brewer.pal(6, "Blues"))+
  guides(fill=guide_colorsteps(order = 1),
         size=guide_legend(order = 2))+
  labs(xlab='',ylab='估量值',size='估量值',fill='估量值')

#14-18
df1 %>% filter(between(总得分,14,18)) %>% slice_sample(n=10)->df1418sample
p1418 <- df1 %>% filter(between(总得分,14,18)) %>% ggplot(aes(种中名,总得分))+geom_point(aes(size=总得分,fill=总得分),
                                                                                shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df1418sample,
                  aes(种中名,总得分,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=c(14,15,16,17,18),
                    colors = brewer.pal(6, "Blues"))+
  guides(fill=guide_colorsteps(order = 1),
         size=guide_legend(order = 2))+
  labs(xlab='',ylab='估量值',size='估量值',fill='估量值')
#10-14
df1 %>% filter(between(总得分,10,14)) %>% slice_sample(n=10)->df1014sample
p1014 <- df1 %>% filter(between(总得分,10,14)) %>% ggplot(aes(种中名,总得分))+geom_point(aes(size=总得分,fill=总得分),
                                                                                shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df1014sample,
                  aes(种中名,总得分,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=c(10,11,12,13,14),
                    colors = brewer.pal(6, "Blues"))+
  guides(fill=guide_colorsteps(order = 1),
         size=guide_legend(order = 2))+
  labs(xlab='',ylab='估量值',size='估量值',fill='估量值')
#小于10
df1 %>% filter(总得分<10) %>% slice_sample(n=10)->df10sample
p10 <- df1 %>% filter(总得分<10) %>% ggplot(aes(种中名,总得分))+geom_point(aes(size=总得分,fill=总得分),
                                                                                shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df10sample,
                  aes(种中名,总得分,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=c(5,6,7,8,9,10),
                    colors = brewer.pal(6, "Blues"))+
  guides(fill=guide_colorsteps(order = 1),
         size=guide_legend(order = 2))+
  labs(xlab='',ylab='估量值',size='估量值',fill='估量值')


                    
psampleall <- (p21+p1821)/(p1418+p1014+p10)+plot_annotation(tag_levels = 'A')
tiff('估量值取样.tiff',res=600,width = 15,height = 10, units="in", compression="lzw")
psampleall
dev.off()

psampleall <- (p21+p10)/(p1821+p1418+p1014)+plot_annotation(tag_levels = 'A')
pdf('估量值取样.pdf',width = 15,height = 10)
psampleall
dev.off()
