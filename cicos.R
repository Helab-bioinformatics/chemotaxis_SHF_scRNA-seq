E8.25.data<- read.table("./E8.25.data.txt",row.names = 1,header = T)
names(E8.25.data)

e8.25.signal.wnt <- e8.25.signal[c("Wnt11","Fzd7"), ]

### 
factors <- as.factor(c("c1","c2","c3"))
library(circlize)
pdf("circos.pdf",width=3,height=3)
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 100))
circos.track(factors = factors, ylim = c(0, 1), bg.col = c("#FF61C3","#B49CFE", "#228B22"), 
             bg.border = NA, track.height = 0.05)
#######plot link without arrow######
circos.link("c1", c(0, c1_1/(c1_1+c1_2)*100), "c1", c(c1_1/(c1_1+c1_2)*100,100), border = 0,col="lightpink")
circos.link("c3", c(0, c3_1/(c3_1+c3_2)*100), "c1", c(c1_1/(c1_1+c1_2)*100,100), border = 0,col="lightgreen")
circos.link("c2", c(0, c2_1/(c2_1+c2_2)*100), "c1", c(c1_1/(c1_1+c1_2)*100,100), border = 0,col="mediumpurple")
dev.off()


###HEATMAP
m <- as.matrix(e8.25.signal.wnt)
scale_max <- 2
scale_min <- -2

m=m[!apply(m,1,sd)==0,]
m=Matrix::t(scale(Matrix::t(m),center=TRUE))
m=m[is.na(row.names(m)) == FALSE,]
plot(density(as.matrix(m)))

m[is.nan(m)] = 0
m[m>scale_max] = scale_max
m[m<scale_min] = scale_min
plot(density(as.matrix(m)))
#heatmap_matrix <- t(m)

palette.breaks <- seq(min(m), max(m), 0.1)
color.palette <- colorRampPalette(c("skyblue","white","red"))(length(palette.breaks)-1)
#color.palette = colorRampPalette(c("midnightblue","white","red"), space="Lab")
ColSideColors=c(rep("#FF61C3",45),rep("#B49CFE",112) ,rep("#228B22",14))

library(gplots)
pdf("Wnt11-Fzd7.heatmap.pdf",height=3,width=5)
heatmap.2(m, na.rm = TRUE, scale="none", dendrogram="none",
          Rowv=F, Colv=F, ColSideColors=ColSideColors, symbreaks=FALSE, key=F, symkey=FALSE,
          density.info="none", trace="none", labCol=FALSE, cexRow=1, col=color.palette,
          ColSideColorsSize=2, KeyValueName="value",keysize = 1)
dev.off()
