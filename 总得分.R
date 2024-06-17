library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggpie)
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
  scale_fill_stepsn(breaks=seq(5,25,5),
                    colors = brewer.pal(6, "Blues"))+xlab('')
df1 %>% mutate(总得分分类=cut(总得分,
                             breaks = c(5,10,15,20,25),include.lowest = T,
                             right = T))->dfnew1
pzdf2 <- ggpie(data=dfnew1, group_key = '总得分分类', label_info = "all",border_color=NA,
                count_type = "full",label_split=NULL,label_type = "circle",label_pos = "in",
                label_size = 3)+scale_fill_brewer(palette = 'Blues')+theme_void(base_size = 14)+
  labs(fill='总得分分类')

pzdfall <- pzdf1+pzdf2+plot_annotation(tag_levels = 'A')
tiff('总得分.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
pzdfall
dev.off()
