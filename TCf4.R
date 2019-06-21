library(ggplot2)
distance <- read.csv("sequential IUE E14.5 gfp E15.5 RFP to P7 analysis total.csv")
colnames(distance) = c("group", "dis")
p <- ggplot(distance, aes(group, dis)) + geom_violin(aes(fill = group))
p
ggsave("sequential IUE E14.5 & E15.5 to P7 in control", dpi=300, P)

install.packages("beeswarm")

dis <- read.csv("sequential IUE E14.5 GFP E15.5 RFP.csv")
View(dis)
colnames(dis) = c("group", "distance")
plot1 <- ggplot(dis, aes(group, distance)) + geom_violin(aes(fill = group))
plot1

#修改坐标轴的顺序
install.packages("ggthemes")
library("ggthemes")
str(dis)
library(ggplot2)
dis$group <- factor (dis$group, levels=c('E14.5 to p1 GFP in ctr', 'E15.5 to P1 RFP in ctr', 'E14.5 to p1 Tcf4 ff 237 & gfp', 'E15.5 to p1 Tcf4 ff RFP', 'E14.5 to P7 GFP in ctr', 'E15.5 to P7 RFP in ctr'))
plot1 <- ggplot(dis, aes(group, distance)) + geom_violin(aes(fill = group), trim=TRUE)
plot1
ggsave("sequential IUE E14.5 & E15.5 .pdf", width = 20, height = 15, dpi = 300)

library("beeswarm")
distance <- read.csv("sequential IUE contorl p1.csv")
library("ggthemes")
distance$experiment <- factor (distance$experiment, levels = c('E14.5 to p1 GFP in ctr', 'E15.5 to P1 RFP in ctr', 'E14.5 to p1 Tcf4 ff 237 & gfp', 'E15.5 to p1 Tcf4 ff RFP', 'E14.5 to P7 GFP in ctr', 'E15.5 to P7 RFP in ctr'))
beeswarm(Length~experiment, data = distance, method = 'swarm', cex = 0.3, 
         spacing = 0.3, col=rainbow(6),log =TRUE, xlab ="neurons labeled at different time", ylab = "ditance from the pial surface")
title("Distance from the pial surface")
boxplot(Length~experiment, data = distance, log=TRUE, add = TRUE, col = rgb(0.5, 0.5, 0.5, 0.3))

library("beeswarm")
distance <- read.csv("C57 P1 sequential IUE E14.5 GFP E15.5 RFP analysis.CSV")
View(distance)
beeswarm(distance~group, data = distance, method = 'swarm', cex = 0.2, 
         spacing = 0.3, col=rainbow(6),log =TRUE, xlab ="neurons labeled at different time", ylab = "ditance from the pial surface")
title("C57 sequential IUE E14.5 GFP / E15.5 RFP to P1")
boxplot(distance~group, data = distance, log=TRUE, add = TRUE, col = rgb(0.5, 0.5, 0.5, 0.3))

dis <- read.csv("tcf4 f f Emx1Cre sequential IUE  p1.csv")
View(dis)
beeswarm(distance~group, data = dis, method = 'swarm', cex = 0.2, 
         spacing = 0.8, col=rainbow(6),log =TRUE, xlab ="neurons labeled at different time", ylab = "ditance from the pial surface")
title("Tcf4 f/f;Emx1Cre/+ sequential IUE E14.5 GFP / E15.5 RFP to P1")
boxplot(distance~group, data = dis, log=TRUE, add = TRUE, col = rgb(0.5, 0.5, 0.5, 0.3))

d <- read.csv("sequential IUE control p1 total.csv")
beeswarm(distance~group, data = d, method = 'swarm', cex = 0.2, 
         spacing = 0.5, col=rainbow(6),log =TRUE, xlab ="neurons labeled at different time", ylab = "ditance from the pial surface")
title("control sequential IUE E14.5 GFP / E15.5 RFP to P1")
boxplot(distance~group, data = d, log=TRUE, add = TRUE, col = rgb(0.5, 0.5, 0.5, 0.3))

di <- read.csv("sequential IUE E14.5 GFP E15.5 RFP to P1 control total.csv")
View(di)
beeswarm(lenghth~group, data = di, method = 'swarm', cex = 0.2, 
         spacing = 0.8, col=rainbow(6),log =TRUE, xlab ="neurons labeled at different time", ylab = "ditance from the pial surface")
title("Tcf4 f/+ sequential IUE E14.5 GFP / E15.5 RFP to P1")
boxplot(lenghth~group, data = di, log=TRUE, add = TRUE, col = rgb(0.5, 0.5, 0.5, 0.3))


#20180808 将对照的所有sequential IUE的数据和目前一个mutant的到P1的脑子整合到一起
library("beeswarm")
distance <- read.csv("sequential IUE E14.5 GFP E15.5 RFP 20180808.csv")
View(distance)
distance$experiment <- factor (distance$experiment, levels=c('Ctr E14.5 to P1 GFP', 'Ctr E15.5 to P1 RFP', 'E14.5 to p1 Tcf4 ff 237 & gfp', 'E15.5 to p1 Tcf4 ff RFP', 'Tcf4 ff Emx1Cre GFP E14.5 to p1','Tcf4 ff Emx1Cre RFP E15.5 to p1', 'E14.5 to P7 GFP in ctr', 'E15.5 to P7 RFP in ctr'))
beeswarm(Length~experiment, data = distance, method = 'swarm', cex = 0.2, 
         spacing = 0.3, col=rainbow(6),log =TRUE, xlab ="neurons labeled at different time", ylab="distance from the surface")
title("sequential IUE E14.5 GFP / E15.5 RFP")
grid(distance)
boxplot(Length~experiment, data = distance, log=TRUE, add = TRUE, col = rgb(0.5, 0.5, 0.5, 0.3))
help("Axis")

# 20180830
library(ggplot2)
dis1 <- read.csv("the distance between the radial glia process and leading process of migrating neuron.csv")
View(dis1)
qplot(lp, soma, data = dis1, geom = "density", formula = y~x, color= group, main = "distance between leading process and daidial glia process", xlab = "Radial fiber to leading process distance (px)", ylab = "Distance from the soma (px)")

#20180903
library(ggplot2)
dis1 <- read.csv("the distance between the radial glia process and leading process of migrating neuron.csv")
?ggplot
gg <- ggplot (dis1, aes(x=soma, y=lp, color = group))+ 
  geom_point()+
  geom_smooth(method = "loess", se = T) +
  xlim(c(0, 35)) + ylim(c(0, 1.5)) +
  labs(subtitile= "distance",
       x="Distance from the soma",
       y= "Radial fiber to leading process distance",
       title="distance between the soma and leading process") +
  coord_flip() # reverse X and Y axis
  
plot(gg)

#20180903 layer distribution 

#library(ggplot2)
position_data <- read.csv("Iue E14.5 to p7 ctr and mutant analysis.csv")

#plot
g <- ggplot(position_data, aes(position))+
              geom_density(aes(fill=factor(group)), alpha=0.8) + 
  labs(titile = "the distribution of GFP+ cells IUE at E14.5") +
  coord_flip() +
  scale_x_reverse() # invert the scale of y axis
g

#20180904
position_ctr <-position_data[which(position_data$group == 'ctr'),]

?geom_bar

position_data <- read.csv("Iue E14.5 to p7 ctr and mutant analysis.csv")

position_ctr <-position_data[which(position_data$group == 'ctr'),]

library(ggplot2)

g <- ggplot(position_ctr, aes(position))+
  geom_histogram(aes(fill=factor(group)), alpha=0.8,bins = 20, binwidth = 0.05) + 
  labs(titile = "the distribution of GFP+ cells IUE at E14.5") +
  coord_flip() +
  scale_x_reverse() 
g

#20180905
g <- ggplot(position_data, aes(group, position))
g + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = 0.05, 
               fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot + Dot plot", 
       subtitle="position of GFP+ cell",
       x="group",
       y="postion")

#20180905
library(ggplot2)
library(magrittr)
library(dplyr)

position_data <- read.csv("Iue E14.5 to p7 ctr and mutant analysis.csv")

position_ctr <-position_data[which(position_data$group == 'ctr'),]

data = read.csv("Iue E14.5 to p7 ctr and mutant analysis.csv")
n = dim(position_ctr)[1]
n
data = mutate(position_ctr, z = rep(-0.5,2031))
p = ggplot(position_ctr) + 
  geom_rect(data=NULL,aes(xmin= -0.4,xmax=1,ymin=-1,ymax=0),
            fill="lightyellow", alpha = 0.2) +
  geom_histogram(aes(x = position,y = ..count../2031,  fill = ..count..), bins =20, show.legend = FALSE) + 
  scale_fill_gradient(low = 'grey80', high = 'grey45') + 
  geom_point(aes(x = position, y = z), position = 'jitter', alpha = 0.04) +
  scale_x_continuous(limits = c(-0.4,1.1), position = 'right') +
  scale_y_continuous(limits = c(-1,0.2)) +
  coord_flip() 
p

# sequential IUE reanalyzed at 20180920 

library(ggplot2)

se1 <- read.csv("control p1 analysis 20180919.csv")
View(se1)
g <- ggplot(se1, aes(distance))+
  geom_density(aes(fill=factor(group)), alpha=0.7) + 
  labs(titile = "sequential IUE at P1 in control") +
  coord_flip() +
  scale_x_reverse() # invert the scale of y axis
g

se2 <- read.csv("control p7 analysis 20180919.csv")
View(se2)
f <- ggplot(se2, aes(distance))+
  geom_density(aes(fill=factor(group)), alpha=0.8) + 
  labs(titile = "sequential IUE at P7 in control") +
  coord_flip() +
  scale_x_reverse() # invert the scale of y axis
f

se3 <- read.csv("Tcf4 ff Cre P1 analysis 20180919.csv")
View(se3)

colnames(se3) = c("group", "distance")
e <- ggplot(se3, aes(length))+
  geom_density(aes(fill=factor(group)), alpha=0.8) + 
  labs(titile = "sequential IUE at P1 in Tcf4 f/f E14.5 Cre") +
  coord_flip() +
  scale_x_reverse() # invert the scale of y axis
e

se4 <- read.csv("sequential Tcf4 ff  Cre gfp rfp P7 analysis 20180919.csv")
View(se4)
h <- ggplot(se4, aes(length))+
  geom_density(aes(fill=factor(group)), alpha=0.8) + 
  labs(titile = "sequential IUE at P1 in Tcf4 f/f E14.5 Cre") +
  coord_flip() +
  scale_x_reverse() # invert the scale of y axis
h

library("beeswarm")

beeswarm(distance~group, data = se1, method = 'swarm', cex = 0.2, 
         spacing = 1.2, col=rainbow(6),log =TRUE, xlab ="neurons labeled at different time", ylab = "ditance from the pial surface")

?beeswarm()        
title("control sequential IUE E14.5 GFP / E15.5 RFP to P1")

boxplot(distance~group, data = se1, log=TRUE, add = TRUE, col = rgb(0.5, 0.5, 0.5, 0.3))

#20190621 ONLY RFP

library(ggplot2)

seqCtr <- read.csv("control 20190621.csv")
View(seqCtr)
h <- ggplot(seqCtr, aes(distance))+
  geom_density(aes(fill=factor(group)), alpha=0.8) +
  xlim(0,1) +
  coord_flip() +
  scale_y_continuous(breaks = c(0, 1.0)) +
  scale_x_reverse() # invert the scale of y axis
h
h + scale_fill_manual(values = c("#00BFC4", "#F8766D"))
h + ylim(0, 8)
h + xlim(1,0)

seqMut <- read.csv("mutant 20190621.csv")
View(seqCtr)
f <- ggplot(seqMut, aes(distance))+
  geom_density(aes(fill=factor(group)), alpha=0.8) + 
  coord_flip() +
  scale_x_reverse() # invert the scale of y axis
f
f + scale_fill_manual(values = c("#00BFC4", "#F8766D"))
f + ylim(0, 8)
f + xlim(1,0)
