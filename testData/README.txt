Author: Eric Tulowetzke
Lab: Ge Lab
Project: ShinyGO Restructure
Purpose of project: rewrite ShinyGO61 code to be easier to develop
File: README.txt
Purpose of file: Read me file for testdata folder 
Start data: 01-03-2021 (mm-dd-yyyy)
Data last modified: N/A

Currently this is a text file, I will convert it to a markdown file when I know the language

geneList.txt Took these genes from the current ShinyGO, currently having issues with logic to find gene symbols and convertID.db. This needs further testing

geneList2.txt This is the file that has 90 genes in it, and this file was generated with the code below. This data set works for all cases that involve a gene list
Code:
userSpecie <- "Mouse"
userIDtype <- 'entrezgene'
df <- getUserDf(userSpecie = userSpecie, userIDtype = userIDtype)
geneList <- sample(df$id, 90)
write(geneList, "geneList2.txt")