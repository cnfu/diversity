library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggpie)
df <- read_excel('df.xls')
df %>% drop_na(总得分) %>% distinct(种中名,.keep_all = T)-> df1
range(df1$频度)
df1 %>% slice_max(频度,n=10)->dfmax
df1 %>% slice_min(频度,n=10)->dfmin
dfhighlight <- rbind(dfmax,dfmin)
ppd1 <- ggplot(df1,aes(种中名,频度))+geom_point(aes(size=频度,fill=频度),
                                                 shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = dfhighlight,
                  aes(种中名,频度,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0.5,5,2),
                    colors = brewer.pal(6, "Blues"))+xlab('')+
  ylim(0,6)

df1 %>% mutate(频度分类=cut(频度,
                            breaks = c(0.5,1.5,2.5,3.5,4.5,5),include.lowest = T,
                            right = T))->dfnew1
ppd2 <- ggpie(data=dfnew1, group_key = '频度分类', label_info = "all",border_color=NA,
                count_type = "full",label_split=NULL,label_type = "circle",label_pos = "in",
                label_size = 3)+scale_fill_brewer(palette = 'Blues')+theme_void(base_size = 14)+
  labs(fill='频度分类')

ppdall <- ppd1+ppd2+plot_annotation(tag_levels = 'A')
tiff('频度.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
ppdall
dev.off()

ppdall <- ppd1+ppd2+plot_annotation(tag_levels = 'A')
pdf('频度.pdf',width = 15,height = 6)
ppdall
dev.off()