setwd("E:/project/heart_scRNA-seq/")
library(Seurat)
library(dplyr)
library(Matrix)

heart.data <-  read.table("./data/all.cells.6cluster.txt",header = T,row.names=1)

alldata <- new("seurat", raw.data =heart.data)
alldata <- Setup(alldata, min.cells = 10, min.genes = 4000, do.logNormalize = F, names.field = 4,names.delim = "_",
                 total.expr = 1e4, project = "heart",is.expr = 1)
alldata
alldata <- MeanVarPlot(alldata ,fxn.x = expMean, fxn.y = logVarDivMean, 
                       x.low.cutoff =0.5, x.high.cutoff = 10, y.cutoff =1.5, do.contour = F)

length(alldata@var.genes)

alldata <- PCA(alldata, pc.genes =alldata@var.genes, do.print = TRUE, pcs.print =5, genes.print = 15)

PCAPlot(alldata,1,2,pt.size = 1)

alldata <- RunTSNE(alldata,dims.use = c(1:6),perplexity =130,max_iter = 2000,theta=0.1,do.fast = T)
TSNEPlot(alldata,pt.size =1.3)
alldata@tsne.rot[2]
#FeaturePlot(alldata, c("Sfrp5"),cols.use = c("grey","yellow","red"),dim.1 = 1,dim.2 = 2,reduction.use = "tsne",no.legend=F)#,no.legend=F)

#####identify diffentially expressed gene for each cluster
markers.all <- FindAllMarkers(pbmc,  min.pct = 0.25, test.use = "roc",thresh.use = 1,only.pos = T)


cols.use =c("#FF4242", "#D8B11B", "#9D9DFF", "#00BA38","#F564E3","#0080FF")
pdf("allcell.tSNE.pdf",width=5,height = 3)
p <- ggplot(alldata@tsne.rot,aes(x= tSNE_1, y= tSNE_2,col=as.factor(alldata@ident)))
p <- p +  geom_point(size=1.3,alpha = 0.8) +theme_bw()+scale_color_manual(values=cols.use)
print(p)
dev.off()


