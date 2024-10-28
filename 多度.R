library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggpie)
df <- read_excel('df.xls')
df %>% drop_na(总得分) %>% distinct(种中名,.keep_all = T)-> df1
range(df1$多度)
df1 %>% slice_max(多度,n=10)->dfmax
df1 %>% slice_min(多度,n=10)->dfmin
dim(dfmin)
dfhighlight <- rbind(dfmax,slice_sample(dfmin,n=20))
pdd1 <- ggplot(df1,aes(种中名,多度))+geom_point(aes(size=多度,fill=多度),
                                           shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = dfhighlight,
                  aes(种中名,多度,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(1,5,1),
                    colors = brewer.pal(6, "Blues"))+xlab('')

df1 %>% mutate(多度分类=cut(多度,
                        breaks = c(1,2,3,4,5),include.lowest = T,
                        right = T))->dfnew1
pdd2 <- ggpie(data=dfnew1, group_key = '多度分类', label_info = "all",border_color=NA,
              count_type = "full",label_split=NULL,label_type = "circle",label_pos = "in",
              label_size = 3)+scale_fill_brewer(palette = 'Blues')+theme_void(base_size = 14)+
  labs(fill='多度分类')

pddall <- pdd1+pdd2+plot_annotation(tag_levels = 'A')
tiff('多度.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
pddall
dev.off()

pddall <- pdd1+pdd2+plot_annotation(tag_levels = 'A')
pdf('多度.pdf',width = 15,height = 6)
pddall
dev.off()