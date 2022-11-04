library(tidyverse)
lgp <- read.csv("D:/대학원/연구과제/110322graph.csv", header = T)
lgp <- lgp %>% 
          select(Category, Description, LogP, Log.q.value.) %>% 
          na.omit() %>% 
          filter(Category != "Canonical Pathways") %>% unique()
lgp$Category <- lgp$Category %>% as.factor()

cate <- lgp$Category %>% unique()
bar_col <- c("#E69F00", "#56B4E9", "#009E73","#D55E00")

lgp_r <- lgp %>% filter(Category == "Reactome Gene Sets")
lgp_g <- lgp %>% filter(Category == "GO Biological Processes")
lgp_k <- lgp %>% filter(Category == "KEGG Pathway")
lgp_w <- lgp %>% filter(Category == "WikiPathways")

lgp_r$descript <- paste0(lgp_r$Description,"_r")
lgp_g$descript <- paste0(lgp_g$Description,"_g")
lgp_k$descript <- paste0(lgp_k$Description,"_k")
lgp_w$descript <- paste0(lgp_w$Description,"_w")

lgp <- bind_rows(lgp_r,lgp_g,lgp_k,lgp_w)
lgp %>% str()

## 통합
levels(lgp$Category) <- c("Reactome Gene Sets", "GO Biological Processes", "KEGG Pathway","WikiPathways")
lgp$order <- ordered(lgp$descript, levels = lgp$descript[order(lgp$Category,lgp$LogP)])

rep_col <- c(rep("#E69F00",times = (lgp %>% filter(Category == cate[1]) %>% dim())[1]),
  rep("#56B4E9",times = (lgp %>% filter(Category == cate[2]) %>% dim())[1]),
  rep("#009E73",times = (lgp %>% filter(Category == cate[3]) %>% dim())[1]),
  rep("#D55E00",times = (lgp %>% filter(Category == cate[4]) %>% dim())[1]))

lgp %>% 
  ggplot(aes(x = order, y = LogP, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, color = rep_col)) +
  xlab("Description") +
  scale_y_continuous(limit = c(min(lgp$LogP),0)) +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(colour="black", size = 10, face = "bold")) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = bar_col)



# 1X4 분할 그래프
library(gridExtra)

n <- 4
graph_num <- list()

for(i in 1:n){
  graph_num[[i]] <- print(lgp %>% filter(Category == cate[i]) %>% 
  ggplot(aes(x = reorder(Description,LogP), y = LogP)) +
  geom_bar(stat = "identity", fill = bar_col[i]) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1,colour = bar_col[i])) +
  xlab(cate[i]) + 
  scale_y_continuous(limit = c(min(lgp$LogP),0)))
}

grid.arrange(graph_num[[1]],graph_num[[2]],graph_num[[3]],graph_num[[4]], nrow = 1, ncol = 4)

library(ggpubr)
ggarrange(graph_num[[1]],graph_num[[2]],graph_num[[3]],graph_num[[4]], nrow = 1,  align = "h")


# x축<->y축
for(i in 1:n){
  graph_num[[i]] <- print(lgp %>% filter(Category == cate[i]) %>% 
                            ggplot(aes(x = reorder(Description,-LogP), y = LogP)) +
                            geom_bar(stat = "identity", fill = bar_col[i]) +
                            theme_classic() +
                            xlab(cate[i]) + 
                            scale_y_continuous(limit = c(min(lgp$LogP),0)) +
                            coord_flip())
}

ggarrange(graph_num[[1]],graph_num[[2]],graph_num[[3]],graph_num[[4]], nrow = 4,  align = "v")

#
library(ggpubr)
library(ggtext)

lgp %>% head()
lgp %>% str()
ggbarplot(lgp, x = "descript", y = "LogP",
          fill = "Category",               
          color = "white",           
          palette = "bar_col",         
          sort.val = "asc",           
          sort.by.groups = TRUE,      
          # x.text.angle = 60,
          ggtheme = theme_classic()) + 
  font("x.text", size = 5) +
  xlab("Description") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) + 
  theme(legend.position = "top") + 
  theme(legend.text = element_text(colour="black", size = 10, face="bold")) +
  theme(legend.title = element_blank())+
  scale_fill_manual(values = bar_col)
