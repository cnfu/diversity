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
  食用=str_detect(df2$有无用途,'食用|饮用|饮料|香料'),
  工业原料=str_detect(df2$有无用途,'工业原料'),
  用材=str_detect(df2$有无用途,'用材'),
  绿化=str_detect(df2$有无用途,'绿化|绿化观赏'),
  饲料=str_detect(df2$有无用途,'饲料|饲用'),
  油料=str_detect(df2$有无用途,'油料'),
  绿肥=str_detect(df2$有无用途,'绿肥'),
  保护植物=str_detect(df2$有无用途,'三级保护植物|三级保护|省级保护|二级保护植物'),
  农药=str_detect(df2$有无用途,'农药|杀虫'))
library(ComplexUpset)  
pyt <- upset(dfupset,intersect = colnames(dfupset),
    name = '用途',width_ratio = 0.1,min_size=5,
    set_sizes = F)+theme(panel.spacing = element_blank())
tiff('用途.tiff',res=600,width = 15,height = 6, units="in", compression="lzw")
pyt
dev.off()
