#Alexandra Reich
#Bayes project: egg diameter
#last updated: 11/03/22


#####Proposal stuff######
#first, let's load some data. I'll use the cleaned dataset from my other analyses
coho_eggs <- read.csv("Data/coho_clean_for_641_project")

#Let's get some exploratory graphs for the project proposal
hist(coho_eggs$Diameter..mm., prob=TRUE, main="")
lines(density(coho_eggs$Diameter..mm.))

library(ggplot2)
ggplot(coho_eggs) + aes(x=Diameter..mm., fill=Wild.or.Hatch) +# geom_histogram() + 
  geom_density(alpha=0.5) + theme_bw() +
  scale_fill_manual(values=c("orange", "purple"))+
  labs(y="Density", x="Egg diameter (mm)")+
  theme(legend.title = element_blank())

ggplot(coho_eggs) + aes(x=Diameter..mm., y= Wild.or.Hatch, fill=Wild.or.Hatch) +# geom_histogram() + 
  geom_boxplot(alpha=0.5) + 
  #geom_jitter(alpha=0.9) +
  theme_bw() +
  scale_fill_manual(values=c("orange", "purple"))+
  theme(legend.title = element_blank())+
  labs(y="Fish origin", x="Egg diameter (mm)")
