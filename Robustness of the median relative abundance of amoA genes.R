

setwd("F:/data7/") 
abundance <-read.table("AOA.txt", header =TRUE, sep = "\t" )

result = data.frame(ID=1:1000) 
for(j in 1:nrow(abundance)){
  sample_random = NULL
  for(i in 1:1000){
    temp = median(abundance[sample(nrow(abundance),j,replace=F),])
    sample_random = rbind(sample_random, temp) 
  }
  sample_random = as.data.frame(sample_random)
  names(sample_random)<-j  
  result = cbind(result, sample_random)
}


library(reshape)
result2 <-melt(result,id=c("ID")) 
library(scales)
library(ggplot2)
ggplot(result2, aes(x=variable,y=value*100))+  
  geom_boxplot(size =1, alpha=I(0.5),outlier.shape = NA,
               colour= hue_pal()(3)[1],fill=hue_pal()(3)[1])+ 
  xlab("Sample number")+  
  ylab("Relative abundance %")+
  theme(axis.title = element_text(size=20),    
        axis.text = element_text(size=18),  
        legend.position = "none")  
ggsave("boxplot_AOA.pdf")        


