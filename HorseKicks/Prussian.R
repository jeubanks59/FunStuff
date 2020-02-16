library("ggplot2")
library("ggthemes")

PrussianArmy <- read.table("PrussianArmy.dat", header = FALSE)

SumYi <- sum(PrussianArmy$V3)
N <- nrow(PrussianArmy)

lamHat <- SumYi/N
SElamHat <- sqrt(lamHat/N)

CorpsByKicks <- table(PrussianArmy$V2,PrussianArmy$V3)
pdf("PrussianPlot.pdf")
ggplot(PrussianArmy, aes(x = V1, y = V3, fill = V2, shapes = V2)) + 
	scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  geom_bar(stat="identity")+
  xlab("Year")+
  ylab("Horse Kick Related Deaths")+
  labs(fill='Corp')+
  scale_fill_stata()+
  theme_bw()
invisible(dev.off())

library('ggplot2')
army <- read.table("PrussianArmy.dat", header = FALSE)
lambdaHat <- mean(army$V3)
se <- sqrt(lambdaHat/nrow(army))

expectedNumber <- function(k)
  20*(lambdaHat^k)*exp(-lambdaHat)/factorial(k)
Kicks <- c(0,1,2,3,4)
Occurrences <- sapply(Kicks, expectedNumber)

kick <- c(rep(0, 14), rep(1,14), rep(2,14),
rep(3,14), rep(4,14))
corps <- rep(c('G','I','II','III','IV','IX','V',
'VI','VII','VIII','X','XI','XIV','XV'),5)
expected <- c(rep(Occurrences[1],14),rep(Occurrences[2],14),
rep(Occurrences[3],14),rep(Occurrences[4],14),
rep(Occurrences[5],14))
residuals <- c(table(army$V2, army$V3)[1:70] - expected)

df.graph <- data.frame(kick, corps, residuals)

pdf("ResidualsPlot.pdf")
ggplot(df.graph, aes(kick, residuals))+
  geom_point(aes(shape = factor(corps),colour = factor(corps)), size = 4) +
  scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))+
  scale_color_stata()+
  theme_bw()
invisible(dev.off())
