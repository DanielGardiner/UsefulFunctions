
cluster.rand = function(data, n, pop, cluster.name){
  # function to randomly select numnber of cases from clusters 
  # (or to select number of sub-clusters from clusters)
  
  # add population ranges within each cluster 
  
  data$pop.c = cumsum(data[, pop])
  
  data$pop.low = c(0, data$pop.c[1:length(data$pop.c) - 1]) + 1
  
  data$pop.high = c(data$pop.c[1:length(data$pop.c)])
  
  # take a random sample between 1 and total pop in all clusters
  
  rands = sample(1:max(data$pop.c), n, replace = F)
  
  # identify which cluster the random number falls within
  
  x = NULL
  for(i in 1:length(rands)){
    y = data[(data$pop.low <= rands[i]) + (data$pop.high >= rands[i]) == 2, ]   
    x = rbind(x, y)
  }
  
  # present results in table
  
  table = as.data.frame(table(as.character(x[, cluster.name])))
  
  table = table[rev(order(table$Freq)), ]
  
  colnames(table)[1] = cluster.name
  
  table
  
}

################################################################################
# EXAMPLE
#
# data = data.frame(LA = c("A", "B", "C", "D"),
#                   pop = c(45, 9, 12, 29))
# 
# data
# 
# cluster.rand(data, n = 10, pop = "pop", cluster.name = "LA")



