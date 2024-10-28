library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(showtext)
showtext_auto()
df <- read_excel('df.xls')
head(df)
dim(df)
df %>% drop_na(总得分) %>% distinct(种中名,.keep_all = T)-> df1  
dim(df1)
table(df1$科名)->df2
df2 <- as.data.frame(df2)
head(df2)
dim(df2)
names(df2) <- c('科','数量')
range(df2$数量)
df2 %>% slice_max(数量,n=10)->df2max
head(df2)
pk1 <- ggplot(df2,aes(科,数量))+geom_point(aes(size=数量,fill=数量),
                                       shape=21,color='grey')+
  theme_bw(base_size = 14)+
  theme(axis.text = element_text(color = 'black'),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())+
  geom_text_repel(data = df2max,
                          aes(科,数量,label=科),
                        max.overlaps = Inf,color='grey20')+
  scale_fill_stepsn(breaks=seq(0,160,30),
                    colors = brewer.pal(6, "Blues"))+xlab('')

options('digits'=2)
df2 %>% filter(数量<40) -> df2remain
sum(df2remain$数量)
others <- data.frame(科=c('其他'), 数量=c(1572))
dfnew <- rbind(filter(df2,数量>=40),others) %>%
  mutate(perc = 数量/ sum(数量)) %>%
  mutate(labels = paste(数量,formattable::percent(perc),sep='-')) %>%
  arrange(desc(perc)) %>%
  mutate(科 = fct_rev(fct_inorder(科))) %>% 
  mutate(text_y = cumsum(数量) - 数量/2)

color <- colorRampPalette(brewer.pal(9,'Blues'))(17) 
pk2 <- ggplot(dfnew, aes(x = "", y = 数量, fill = 科)) +
  geom_col(color='grey') +
  coord_polar(theta = "y")+
  theme_void(base_size = 14)+
  scale_fill_manual(values = color)+
  geom_text_repel(aes(label = labels, y = text_y),
                   nudge_x = 0.7,
                   nudge_y = 0.7,
                   max.overlaps = Inf,
                   segment.color = "grey50")

pkall <- pk1+pk2+plot_annotation(tag_levels = 'A')
tiff('科数.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
pkall
dev.off()

pkall <- pk1+pk2+plot_annotation(tag_levels = 'A')
pdf('科数.pdf',width = 15,height = 6)
pkall
dev.off()

