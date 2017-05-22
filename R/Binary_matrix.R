############# Binary Matrix exploration
#### input files: the binary matrix of prediction class labels, thrs
###  output file: most misclassified subjects, peculiar genes list

BinaryMatrix <- function(binary_matrix_file, thr_low, thr_high, classification_vector_file)
{

  if(!file.exists(binary_matrix_file))
  {
    print(paste('File binary_matrix:', binary_matrix_file,'not found \n'))
    return(NULL);
  }

  binary_matrix <- read.table(file = binary_matrix_file, header = T, sep= '\t');

  if(nrow(binary_matrix) == 0){
    print('Error reading binary_matrix_file \n');
    return(NULL);
  }

  if(!file.exists(classification_vector_file))
  {
    print(paste('File classification_vector_file:', classification_vector_file,'not found \n'))
    return(NULL);
  }

  classification_vector <- read.table(file = classification_vector_file, header = F, sep= '\t');

  if(length(classification_vector) == 0){
    print('Error reading classification_vector_file \n');
    return(NULL);
  }

  if(thr_low < 0)
  {
    print('Error, thr must be >= 0 \n ');
    return(NULL);
  }

  if(thr_high < 0)
  {
    print('Error, thr must be >= 0 \n ');
    return(NULL);
  }

  subject_low <- which(apply(binary_matrix[ , classification_vector$V1 == 0], 2, sum) > thr_low)

  top_signature_low <- which(binary_matrix[, subject_low[1]] == classification_vector$V1[subject_low[1]])

    for(i in 2:length(subject_low)){

      tmp_low <- intersect(top_signature_low, which(binary_matrix[, subject_low[i]] == classification_vector$V1[subject_low[i]]) )

      if(length(tmp_low) == 0)
      {
       top_signature_low <- c(top_signature_low, which(binary_matrix[, subject_low[i]] == classification_vector$V1[subject_low[i]]))
      }
      else top_signature_low <- tmp_low;
    }


  size_class_low <- length(which(classification_vector$V1 == 0));

  subject_high <-  which(apply(binary_matrix[ , classification_vector$V1 == 1], 2, sum) < thr_high);

  top_signature_high <- which(binary_matrix[, size_class_low + subject_high[1]] == classification_vector$V1[size_class_low + subject_high[1]]);

    for (i in 2:length(subject_high)) {

      tmp_high <- intersect(top_signature_high, which(binary_matrix[,size_class_low + subject_high[i]] == classification_vector$V1[size_class_low + subject_high[i]]))

      if(length(tmp_high) == 0)
      {
        top_signature_high <- c(top_signature_high, which(binary_matrix[,size_class_low + subject_high[i]] == classification_vector$V1[size_class_low + subject_high[i]]))
      }

       else top_signature_high <- tmp_high;


    }


  top_signature <- c(top_signature_low, top_signature_high);

  write.table(rownames(binary_matrix)[top_signature],
              file = 'Final_Signature',
              row.names = F,
              col.names = F,
              sep = '\t',
              quote = F);

  write.table(rownames(binary_matrix)[top_signature_high],
              file = 'Signature_High',
              row.names = F,
              col.names = F,
              sep = '\t',
              quote = F);

  write.table(rownames(binary_matrix)[top_signature_low],
              file = 'Signature_Low',
              row.names = F,
              col.names = F,
              sep = '\t',
              quote = F);


}

