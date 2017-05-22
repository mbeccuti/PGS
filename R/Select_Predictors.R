SelectPredictor <- function(Gene_expression_file){
  
  if(!file.exists(Gene_expression_file))
  {
    print(paste('File Gene_expression:', Gene_expression_file,'not found \n'))
    return(NULL);
  }
  
  Gene_expression <- read.table(file = Gene_expression_file, header = T,  sep= '\t');
  PredictorsMatrix <-  as.character(Gene_expression[,1]);
  
  x <- seq(2, ncol(Gene_expression)-1, 2)
  
  for(i in x)
  {
     FC_i <- Gene_expression[, i+1] - Gene_expression[ ,i]
     PredictorsMatrix = cbind(PredictorsMatrix,FC_i)
  }  
    
  
  
  colnames(PredictorsMatrix) <- colnames(Gene_expression)[seq(from = 1, to = ncol(Gene_expression), by = 2)]
  
  write.table(PredictorsMatrix,
              file = 'Predictors_Matrix',
              sep = '\t',
              col.names = T,
              row.names = F);
  
}