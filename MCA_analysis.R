newHaberman <- Haberman
max =0
for(age in 1:15){
  for(node in 1:15){
    newHaberman$Age <- cut(Haberman$Age, seq(30,90,age),right=F)
    newHaberman$`Operation Year` <- factor(Haberman$`Operation Year`)
    newHaberman$`Axillary Nodes` <- cut(Haberman$`Axillary Nodes`,seq(0,56,node),right=F)
    newHaberman$`Survival Status` <- factor(newHaberman$`Survival Status`)
    res.mca2 <- MCA(newHaberman,quali.sup = 4,graph = FALSE)
    res.hcpc.mca2 <- HCPC(res.mca2,nb.clust = 2,graph = FALSE)
    temp.survival <- res.hcpc.mca2$data.clust$`Survival Status`
    temp.clust <- res.hcpc.mca2$data.clust$clust
    temp <- length(temp.clust[temp.clust==temp.survival])/306.0
    if (temp>max || (1-temp)>max){
      Age <- age
      Node <- node
      if (temp>0.5){
        max<-temp
      }else{
        max <- 1-temp
      }
    }
  }
}
Age
Node
max
#5/3 3/2 4/8 4/11.12.13