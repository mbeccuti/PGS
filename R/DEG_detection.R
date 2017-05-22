############### Find DEG ############
## input files: gene expression matrix, contrasts.txt
## output file: matrix of expression with all DEGs found

### per test lettura contrast.txt: somma riga  == 1
### controlla che i nomi delle righe dei due files sono uguali
### test di controllo se hai il limma package:
DEG_detection <- function(gene_expression_file, contrast_file)
{
    if(require("limma")){
      print("limma is loaded correctly")
    } else {
      print("trying to install limma")
      install.packages("limma")
      if(require(limma)){
        print("limma installed and loaded")
      } else {
        return("could not install limma")
      }
    }

contrast        <- read.table(file = contrast_file, header = T);
gene_expression <- read.table(file = gene_expression_file, 
                              header = T,
                              sep = '\t')
                              
                            
mod_mat           <- model.matrix(~ 0 + class, data = contrast);
colnames(mod_mat) <- c('classC0', 'classC1');
contr_matrix      <- makeContrasts(classC1-classC0, levels = mod_mat);


fit_contrast   <- lmFit(gene_expression[ ,-1], mod_mat);
fit            <- contrasts.fit(fit_contrast, contr_matrix);
fit            <- eBayes(fit);
result         <- decideTests(fit);
vennDiagram(result);


Ndeg           <- summary(result)[1]+ summary(result)[3];
print(Ndeg)
DEGs           <- topTable(fit, number = Ndeg);


#pos_DEG_lungs  <- intersect(rownames(my_expression), rownames(DEG_cancer));
myGenes         <- gene_expression[rownames(DEGs), ]; ## selecting only day 1 data

write.table(myGenes,
            file = 'DEGs_file',
            row.names = F,
            col.names = T,
            quote = F,
            sep = '\t');

}