library(slingshot);
library(clusterExperiment);library(RColorBrewer)

CPC <-'cluster.1'
EC <- 'cluster.4' 
CM1 <- 'cluster.7'  
CM2 <- 'cluster.8'

plot3d(Isl1@pca.rot[,dplot] , t='p',col=cols.use[as.factor(Isl1@ident)], axes=NULL,pch = 19,alpha=1,
       cex = 2, size=3, xlab="PC 1", ylab="PC 3", zlab="PC 2", aspect="iso", box=FALSE)


for (i in 1:ncol(slingshot.data))
  identity.slingshot   <- c(identity.slingshot ,strsplit(names(slingshot.data[i]),"_")[[1]][4])
n <-20; sval <- 0
lineages <- get_lineages(Isl1@pca.rot[, c(1:3)], Isl1@ident, start.clus = c(CPC),end.clus = c(EC,CM1,CM2))
for (i in 1:ncol(lineages$C)) {print(lineages[[i]])}#see different lineage has consists of which cluster
lineages$lineage1 <-c("cluster.1","cluster.2","cluster.6","cluster.8" )
lineages$lineage2 <-c("cluster.1","cluster.2","cluster.5","cluster.7" )
lineages$lineage3 <-c("cluster.1","cluster.3","cluster.4")
curves <- get_curves(Isl1@pca.rot[,c(1:n)], Isl1@ident, lineages, extend="y", stretch=1, shrink = sval, reweight = FALSE, drop.multi = FALSE)



#############slingshot
dplot <- c(1,2,3)
centers <- t(sapply(rownames(lineages$forest),function(clID){
  x.sub <-Isl1@pca.rot[Isl1@ident == clID,dplot]
  return(colMeans(x.sub))
}))

center.col <- sapply(rownames(lineages$forest),function(clID){
  colpal[as.factor(Isl1@ident)][which.max(Isl1@ident == clID)]
})


plot3d(Isl1@pca.rot[,dplot] , t='p',col=cols.use[as.factor(Isl1@ident)], axes=NULL,pch = 19,alpha=0.9,
       cex = 2, size=6, xlab="PC 1", ylab="PC 3", zlab="PC 2", aspect="iso", box=FALSE)

#axes3d(tick=F,labels = F,edges=c('x-+', 'y-+', 'z--'))
#par3d(windowRect = c(20, 30, 500, 500))
for (i in c(1,2,3)){
  plot3d(curves[[i]]$s[order(curves[[i]]$lambda),dplot], type='l',add=TRUE, lwd=4,col=cols.use[which.max(tail(lineages[[i]],1)==levels(Isl1@ident))])
}
#col <- c("#B62D2D", "#3C7598", "yellow", "#8E6335","#00FDFD","#6C3B97","#B5D4E5","#FF61C3")
plot3d(centers, size = 15, add =T, pch=1, col = cols.use)
rgl.postscript("isl1.slingshot.pca.svg","svg",drawText = T)
rglwidget()
try(rgl.close())

dev.off()
