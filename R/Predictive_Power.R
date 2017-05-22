######## Predictive power of Predictors + definition of the Binary matrix

### input files: Predictors matrix, classification vector MUST contain samples ordered by labels. 
### output file: binary matrix of class-label prediction by logistic regression
Predictive_Power <- function(Predictors_file, classification_vector_file, intercept = T, pp_thr = .5, q_thr = .95)
{
  if(!file.exists(Predictors_file))
  {
    print(paste('File Predictors_file:', Predictors_file,'not found \n'))
    return(NULL);
  }
  
  Predictors <- read.table(file = Predictors_file, header = T,  sep= '\t');
  
  Predictors_expression <- Predictors[ ,-1];
  Predictors_expression <- as.matrix(Predictors_expression);
  
  
  if(nrow(Predictors) == 0){
    print('Error reading Predictors_file \n');
    return(NULL);
  }
  
  if(!file.exists(classification_vector_file))
  {
    print(paste('File classification_vector_file:', classification_vector_file,'not found \n'))
    return(NULL);
  }
  
  classification_vector <- read.table(file = 'classification_vector', header = F, sep= '\n');
  
  
  if(length(classification_vector) == 0){
    print('Error reading classification_vector_file \n');
    return(NULL);
  }
  
  
  Predictors_binary <- matrix(0,
                          nrow = nrow(Predictors),
                          ncol = ncol(Predictors)-1);
    
  Predictors_pp     <- rep(0, nrow(Predictors));
    
    ####### logistic regression on Predictors #########
    
if(intercept == T){ 
    
    for(i in 1:nrow(Predictors)) {
      
      Predictors_fit       <- glm(classification_vector$V1 ~ 0 + Predictors_expression[i,], 
                            family = binomial(link = "logit"), 
                            control = list(maxit = 100));
      
      pp              <- predict(Predictors_fit, type = 'response');
      pp[pp > pp_thr]     <- 1;
      pp[pp <= pp_thr]    <- 0;
      
      Predictors_pp[i]         <- length(which(pp == classification_vector$V1))/length(pp);
      Predictors_binary[i, ]   <- pp;      

    }
    rm(pp);
}
  else { 
    
    for(i in 1:nrow(Predictors)) {
    
    Predictors_fit       <- glm(classification_vector$V1 ~ Predictors_expression[i,], 
                                family = binomial(link = "logit"), 
                                control = list(maxit = 100));
    
    pp              <- predict(Predictors_fit, type = 'response');
    pp[pp > pp_thr]     <- 1;
    pp[pp <= pp_thr]    <- 0;
    
    Predictors_pp[i]         <- length(which(pp == classification_vector$V1))/length(pp);
    Predictors_binary[i, ]   <- pp;      
    
   }
    rm(pp);
}
    
  Predictors_pp <- as.data.frame(Predictors_pp);
  rownames(Predictors_pp) <- Predictors$Genes 
    
    hist(Predictors_pp$Predictors_pp);
    
  rownames(Predictors_binary) <- Predictors[, 1];
  colnames(Predictors_binary) <- colnames(Predictors)[-1];
    
    write.table(Predictors_pp, 
                file = 'Predictors_pp',
                col.names = FALSE,
                row.names = TRUE,
                sep = '\t',
                quote = F);
  
  predictors <- read.table('Predictors_pp', sep = '\t');
  
  good_predictors <- predictors[which(predictors$V2 > quantile(predictors$V2, probs = q_thr)), ]
  
  write.table(Predictors_binary[row.names(Predictors_binary) %in% good_predictors$V1, ], 
              file = 'Good_Predictors',
              row.names = T,
              col.names = T,
              sep = '\t',
              quote = F)
  

}


