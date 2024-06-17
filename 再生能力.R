library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggpie)
df <- read_excel('df.xls')
df %>% drop_na(总得分) %>% distinct(种中名,.keep_all = T)-> df1
dim(df1)
range(df1$再生能力)
df1 %>% slice_max(再生能力,n=10)->dfmax
df1 %>% slice_min(再生能力,n=10)->dfmin
dim(dfmin)

pzsnl1 <- ggplot(df1,aes(种中名,再生能力))+geom_point(aes(size=再生能力,fill=再生能力),
                                           shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = slice_sample(dfmax,n=20),
                  aes(种中名,再生能力,label=种中名),
                  max.overlaps = Inf,color='grey20',nudge_y = 0.5)+
  scale_fill_stepsn(breaks=seq(1,5,1),
                    colors = brewer.pal(6, "Blues"))+
  geom_text_repel(data = slice_sample(dfmin,n=20),
                  aes(种中名,再生能力,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  xlab('')+ylim(0,6)

df1 %>% mutate(再生能力分类=cut(再生能力,
                        breaks = c(1,2,3,4,5),include.lowest = T,
                        right = T))->dfnew1
pzsnl2 <- ggpie(data=dfnew1, group_key = '再生能力分类', label_info = "all",border_color=NA,
              count_type = "full",label_split=NULL,label_type = "circle",label_pos = "in",
              label_size = 3)+scale_fill_brewer(palette = 'Blues')+theme_void(base_size = 14)+
  labs(fill='再生能力分类')

pzsnlall <- pzsnl1+pzsnl2+plot_annotation(tag_levels = 'A')
tiff('再生能力.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
pzsnlall
dev.off()
