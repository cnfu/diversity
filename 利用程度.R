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
range(df1$利用程度)
df1 %>% slice_max(利用程度,n=10)->dfmax
df1 %>% slice_min(利用程度,n=10)->dfmin

plycd1 <- ggplot(df1,aes(种中名,利用程度))+
  geom_point(aes(size=利用程度,fill=利用程度),shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_stepsn(breaks=c(1,2,3,4,5),
                    colors = brewer.pal(5, "Blues"))+
  geom_text_repel(data = slice_sample(dfmax,n=20),aes(种中名,利用程度,
                                             label=种中名),
                  max.overlaps =Inf,color='grey20',
                  min.segment.length = 0,nudge_y = 0.5)+
  geom_text_repel(data = dfmin,aes(种中名,利用程度,
                                         label=种中名),
                  max.overlaps =Inf,color='grey20',
                  min.segment.length = 0)+
  guides(fill=guide_colorsteps(order = 1),
         size=guide_legend(order = 2))+xlab('')+ylim(0,6)

# dfnew7 %>% filter(利用程度>=3) %>% count()
# dfnew7 %>% filter(利用程度<3) %>% count()
# dfnew7 %>% slice_max(利用程度,n=10) %>% count()
df1 %>% mutate(利用程度分类=cut(利用程度,
                             breaks = c(1,2,3,4,5),include.lowest = T,
                              right = T))->dfnew1
# head(dfnew71)
# table(dfnew71$利用程度分类) %>% as.data.frame()->dfly

plycd2 <- ggpie(data=dfnew1, group_key = '利用程度分类', label_info = "all",border_color=NA,
                count_type = "full",label_split=NULL,label_type = "circle",label_pos = "in",
                label_size = 3)+scale_fill_brewer(palette = 'Blues')+theme_void(base_size = 14)+
  labs(fill='利用程度分类')

plycdall <- plycd1+plycd2+plot_annotation(tag_levels = 'A')
tiff('利用程度.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
plycdall
dev.off()

plycdall <- plycd1+plycd2+plot_annotation(tag_levels = 'A')
pdf('利用程度.pdf',width = 15,height = 6)
plycdall
dev.off()