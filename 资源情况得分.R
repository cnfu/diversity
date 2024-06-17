library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggpie)
df <- read_excel('df.xls')
df %>% drop_na(总得分) %>% distinct(种中名,.keep_all = T)-> df1
range(df1$资源情况得分)
df1 %>% slice_max(资源情况得分,n=10)->dfmax
df1 %>% slice_min(资源情况得分,n=10)->dfmin
dfhighlight <- rbind(dfmax,dfmin)
pzyqk1 <- ggplot(df1,aes(种中名,资源情况得分))+geom_point(aes(size=资源情况得分,fill=资源情况得分),
                                             shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = dfhighlight,
                  aes(种中名,资源情况得分,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(5,20,5),
                    colors = brewer.pal(6, "Blues"))+xlab('')

df1 %>% mutate(资源情况得分分类=cut(资源情况得分,
                         breaks = c(3.5,5,10,15,20),include.lowest = T,
                         right = T))->dfnew1
pzyqk2 <- ggpie(data=dfnew1, group_key = '资源情况得分分类', label_info = "all",border_color=NA,
               count_type = "full",label_split=NULL,label_type = "circle",label_pos = "in",
               label_size = 3)+scale_fill_brewer(palette = 'Blues')+theme_void(base_size = 14)+
  labs(fill='资源情况得分分类')

pzyqkall <- pzyqk1+pzyqk2+plot_annotation(tag_levels = 'A')
tiff('资源情况得分.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
pzyqkall
dev.off()
