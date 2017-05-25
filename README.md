## Synopsis

PGS is a 3 steps feature selection procedure for binary classification tasks in case of High-Throughput data. It generates .txt files containing signatures extracted by applying Peculiar Genes Selection procedure from gene expression matrix. 

# ## Code Example
# X <- DEG_detection(gene_expression_file, contrast_file)
# y <- c(rep(1, 6), rep(0,6))
# pp <- Predictive_Power(X, y, intercept = T, pp_thr = .5, q_thr = .95)
# B_M <- BinaryMatrix(pp, 25, 40, y)

## Motivation
PGS is a quick and useful tool to extract signature for classification tasks from high-throughput data sets. It is able to handle both imbalanced and balanced data and it usage is limited to binary classification problems. 

## Installation

install.packages(PGS)



