library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggpie)
df <- read_excel('df.xls')
df %>% drop_na(总得分) %>% distinct(种中名,.keep_all = T)-> df1
range(df1$生境)
df1 %>% slice_max(生境,n=10)->dfmax
df1 %>% slice_min(生境,n=10)->dfmin
dim(dfmin)
dfhighlight <- rbind(dfmax,slice_sample(dfmin,n=20))
psj1 <- ggplot(df1,aes(种中名,生境))+geom_point(aes(size=生境,fill=生境),
                                           shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = dfhighlight,
                  aes(种中名,生境,label=种中名),
                  max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(1,5,1),
                    colors = brewer.pal(6, "Blues"))+xlab('生境')

df1 %>% mutate(生境分类=cut(生境,
                        breaks = c(1,2,3,4,5),include.lowest = T,
                        right = T))->dfnew1
psj2 <- ggpie(data=dfnew1, group_key = '生境分类', label_info = "all",border_color=NA,
              count_type = "full",label_split=NULL,label_type = "circle",label_pos = "in",
              label_size = 3)+scale_fill_brewer(palette = 'Blues')+theme_void(base_size = 14)+
  labs(fill='生境分类')

psjall <- psj1+psj2+plot_annotation(tag_levels = 'A')
tiff('生境.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
psjall
dev.off()

psjall <- psj1+psj2+plot_annotation(tag_levels = 'A')
pdf('生境.pdf',width = 15,height = 6)
psjall
dev.off()