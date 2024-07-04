library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
df <- read_excel('df.xls')
head(df)
dim(df)
df %>% drop_na(总得分) %>% distinct(种中名,.keep_all = T)-> df1  
dim(df1)
df1 %>% count(有无用途,sort = T) %>% view
df1 %>% drop_na(有无用途)->df2
  dfupset <- data.frame(药用=str_detect(df2$有无用途,'药用'),
  观赏=str_detect(df2$有无用途,'观赏|绿化观赏'),
  食用=str_detect(df2$有无用途,'食用|可食'),
  饮料=str_detect(df2$有无用途,'饮用|饮料'),
  香料=str_detect(df2$有无用途,'香料|香料用'),
  工业原料=str_detect(df2$有无用途,'工业原料'),
  用材=str_detect(df2$有无用途,'用材'),
  绿化=str_detect(df2$有无用途,'绿化|绿化观赏'),
  饲料=str_detect(df2$有无用途,'饲料|饲用'),
  油料=str_detect(df2$有无用途,'油料'),
  绿肥=str_detect(df2$有无用途,'绿肥'),
  保护植物=str_detect(df2$有无用途,'三级保护植物|三级保护|省级保护|二级保护植物'),
  农药=str_detect(df2$有无用途,'农药|杀虫'),
  有毒植物=str_detect(df2$有无用途,'有毒|有毒植物'))
library(ComplexUpset)  
pyt <- upset(dfupset,intersect = colnames(dfupset),
    name = '用途',width_ratio = 0.1,min_size=5,
    set_sizes = F)
tiff('用途.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
pyt
dev.off()

#药用
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'药用')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df3
colnames(df3)[2] <- '数量'
df3 %>% slice_max(数量,n=10)->df3max
df3 %>% slice_min(数量,n=10)->df3min

df3highlight <- rbind(df3max,slice_sample(df3min,n=20))
pyy <- ggplot(df3,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                           shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df3highlight,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,80,20),
                    colors = brewer.pal(6, "Blues"))+xlab('药用')
#观赏
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'观赏|绿化观赏')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df4
colnames(df4)[2] <- '数量'
df4 %>% slice_max(数量,n=10)->df4max
df4 %>% slice_min(数量,n=10)->df4min

df4highlight <- rbind(df4max,slice_sample(df4min,n=20))
pgs <- ggplot(df4,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                         shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df4highlight,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,35,10),
                    colors = brewer.pal(6, "Blues"))+xlab('观赏')
#食用
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'食用|饮用|饮料|香料|可食|香料用')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df5
colnames(df5)[2] <- '数量'
df5 %>% slice_max(数量,n=10)->df5max
df5 %>% slice_min(数量,n=10)->df5min

df5highlight <- rbind(df5max,slice_sample(df5min,n=20))
psy <- ggplot(df5,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                         shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df5highlight,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,20,5),
                    colors = brewer.pal(6, "Blues"))+xlab('食用')
#工业原料
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'工业原料')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df6
colnames(df6)[2] <- '数量'
df6 %>% slice_max(数量,n=10)->df6max
df6 %>% slice_min(数量,n=10)->df6min

df6highlight <- rbind(df6max,slice_sample(df6min,n=20))
pgy <- ggplot(df6,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                         shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df6highlight,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,20,5),
                    colors = brewer.pal(6, "Blues"))+xlab('工业原料')
#用材
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'用材')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df7
colnames(df7)[2] <- '数量'
df7 %>% slice_max(数量,n=10)->df7max
df7 %>% slice_min(数量,n=10)->df7min

df7highlight <- rbind(df7max,df7min)
pyc <- ggplot(df7,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                         shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df7highlight,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,35,10),
                    colors = brewer.pal(6, "Blues"))+xlab('用材')
#饲料
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'饲料|饲用')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df8
colnames(df8)[2] <- '数量'
df8 %>% slice_max(数量,n=10)->df8max
df8 %>% slice_min(数量,n=10)->df8min

df8highlight <- rbind(df8max,df8min)
psl <- ggplot(df8,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                         shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df8highlight,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,30,10),
                    colors = brewer.pal(6, "Blues"))+xlab('饲料')
#绿化
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'绿化|绿化观赏')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df9
colnames(df9)[2] <- '数量'
df9 %>% slice_max(数量,n=10)->df9max
df9 %>% slice_min(数量,n=10)->df9min

df9highlight <- rbind(df9max,df9min)
plh <- ggplot(df9,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                         shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df9highlight,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,10,5),
                    colors = brewer.pal(6, "Blues"))+xlab('绿化')
#油料
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'油料')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df10
colnames(df10)[2] <- '数量'
df10 %>% slice_max(数量,n=10)->df10max
df10 %>% slice_min(数量,n=10)->df10min

df10highlight <- rbind(df10max,df10min)
pyl <- ggplot(df10,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                         shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df10highlight,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,15,5),
                    colors = brewer.pal(6, "Blues"))+xlab('油料')


#农药
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'农药|杀虫')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
  as.data.frame()->df11
colnames(df11)[2] <- '数量'

pny <- ggplot(df11,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                          shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df11,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,10,5),
                    colors = brewer.pal(6, "Blues"))+xlab('农药')

# #药食两用
# df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'药用')&str_detect(有无用途,'食用|可食')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>% 
#   as.data.frame()->df12
# colnames(df12)[2] <- '数量'
#香料或饮料
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'饮用|饮料|香料|香料用')) %>% arrange(科名) %>% view() %>% count(科名,sort = T) %>%
  as.data.frame()->df12
colnames(df12)[2] <- '数量'
pxy <- ggplot(df12,aes(科名,数量))+geom_point(aes(size=数量,fill=数量),
                                          shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df12,
                  aes(科名,数量,label=科名),max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,5,2),
                    colors = brewer.pal(6, "Blues"))+xlab('香料或饮料')

df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'三级保护植物|三级保护|省级保护|二级保护植物')) %>% arrange(科名) %>% view()
df2 %>% select(科名,种中名,有无用途) %>% filter(str_detect(有无用途,'有毒|有毒植物')) %>% arrange(科名) %>% view()



library(patchwork)
pall <- (pyy+pgs)/(pyc+pgy)/(psy+psl)/(pyl+plh)+plot_annotation(tag_level = 'A')
tiff('用途分类.tiff',res = 600,width = 15,height = 15,units = 'in',compression = 'lzw')
pall
dev.off()