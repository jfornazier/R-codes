library(vegan)
library(reshape2)
library(ggplot2)

<<<<<<< HEAD
=======
# Loading FAPROTAX table ####
func_table <- as.data.frame(t(read.table("func_table.tsv", check.names=F)))

rownames(func_table)
colSums(func_table) 

func_table <- subset(func_table, select=colSums(func_table) != 0) #eliminate ESV without sequences

func.table.ra <- decostand(func_table, method = "total", MARGIN = 2, na.rm = T)
func.table.ra <- as.data.frame(func.table.ra) * 100

func.table.ra$samples <- rownames(func.table.ra)

df_long_function <- melt(func.table.ra, id.vars = "samples", variable.name = "Function")

ggplot(df_long_function, aes(samples, Function, size = value, color=samples)) +
  geom_point(stat="identity") +
  guides(color = FALSE)+
  labs(colour="Isobata", size = "Rel. abundance")+
  theme(legend.text = element_text(size = 14)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(size = 11, angle =90)) +
  scale_size(range = c(1, 8), limits = c(1,100), breaks = c(1,25,50,75,100)) +
  ylab("Predicted function") +
  xlab("") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  theme(axis.title.x = element_text(face = "bold", size=12, color = "black"),
        legend.title = element_text(face = "bold", size=12, color = "black"),
        legend.position="right",
        axis.title.y = element_text(face = "bold", size=12, color = "black", angle = 90),
        axis.text.x = element_text(face = "plain", angle = 45, hjust = 1.1, vjust = 1.1, size=10, color = "black"),
        axis.text.y = element_text(face = "plain", size=10, vjust = 0.3, color = "black"),
        panel.grid.major = element_line(colour = "gray", size=0.5, linetype = 3),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size=0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1.1))
>>>>>>> 77d5566cd5ccdcb1cc6938779748142b4663bb7c
