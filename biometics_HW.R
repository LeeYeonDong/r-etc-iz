## HW 01
load("G:/대학원/생물통계학/HW01/DGRP.Rdata")
DGRP %>% str()
DGRP %>% dim()
DGRP %>% head(5) %>% View()
DGRP %>% tail(5)

# 1. Compute the frequency of heterozygous genotypes for each SNP. Draw a histogram of the frequencies. What is the maximum value of the frequencies of heterozygous genotypes? How many SNPs have the maximum value?
DGRP$het <- paste0(DGRP$ref,DGRP$alt)
DGRP$het %>% table()
DGRP$het <- gsub("CA","AC",DGRP$het)
DGRP$het <- gsub("GA","AG",DGRP$het)
DGRP$het <- gsub("TA","AT",DGRP$het)
DGRP$het <- gsub("GC","CG",DGRP$het)
DGRP$het <- gsub("TC","CT",DGRP$het)
DGRP$het <- gsub("TG","GT",DGRP$het)

het_df <- DGRP$het %>% table() %>% as.data.frame()  
names(het_df) <- c("het_gene","Freq")
het_df <- het_df %>% arrange(desc(Freq))

het_df %>% ggplot(aes(x=reorder(het_gene,-Freq), y=Freq)) + geom_bar(stat='identity')

# 2. Estimate minor allele frequency (MAF) for each SNP. Draw a histogram of MAFs for the entire SNPs. Compute the 5-number summary of MAFs, which includes minimum, Q1, median, Q3, and maximum.
DGRP$freq <- apply(DGRP[,5:209],1,sum)

DGRP %>% 
  group_by(het) %>% 
  summarise(MAF_min = quantile(freq)[1],
            MAF_Q1 = quantile(freq)[2],
            MAF_med = quantile(freq)[3],
            MAF_Q3 = quantile(freq)[4],
            MAF_max = quantile(freq)[5]) 

# 3. There are 6 different chromosomes in the data set. For each chromosome, compute the proportion of SNPs whose MAFs are less than 0.1.
arti.chr <- sample(1:6,1000,replace=TRUE) 
prop.each.chr <- tapply(snps.maf,arti.chr,function(x) sum(x < 0.1)/length(x))

# 4. Perform a Hardy-Weinberg Equilibrium (HWE) test. Compute a χ2 test statistic of HWE for each SNP, and draw a histogram of test statistics. Also, provide the 5-number summary of the test statistics.

#??
het_count <- table(DGRP$het)
het_Nobs <- sum(het_count)

het_FreqC <- (2*het_count[3] + het_count[2])/(2*het_Nobs)
het_ExpCount <- c(het_Nobs*(1-het_FreqC)^2, 2*het_Nobs*het_FreqC*(1-het_FreqC), Nobs*het_FreqC^2)

het_ChiSqStat <- sum((het_count - het_ExpCount)^2/het_ExpCount)
het_ChiSqStat %>% summary()

#??
DGRP_snp <- DGRP[,5]
DGRP_snp_count <- table(DGRP_snp)
DGRP_snp_Nobs <- sum(DGRP_snp_count)

DGRP_snp_FreqC <- (2*DGRP_snp_count[3] + DGRP_snp_count[2])/(2*DGRP_snp_Nobs)
DGRP_snp_ExpCount <- c(DGRP_snp_Nobs*(1-DGRP_snp_FreqC)^2, 2*DGRP_snp_Nobs*DGRP_snp_FreqC*(1-DGRP_snp_FreqC), Nobs*DGRP_snp_FreqC^2)

DGRP_snp_ChiSqStat <- sum((DGRP_snp_count - DGRP_snp_ExpCount)^2/DGRP_snp_ExpCount)
DGRP_snp_ChiSqStat %>% summary()

# 5. Open the phenotype data “pheno.csv”, which includes the ID number and phenotype value of 204 subjects. The ID number should be matched with the column names of genotype data, where the prefix “line ” has been already left out. Note that you should remove out one subject that does not have a phenotype value, so only 204 subjects should be included in analysis. Compute the sample mean of phenotype values for each genotype of the SNP ‘2L 921702’.
load("G:/대학원/생물통계학/HW01/DGRP.Rdata")
pheno <- read.csv("G:/대학원/생물통계학/HW01/pheno.csv", header=FALSE)

line_phe <- str_split(pheno$V1, "_")
line_phe_num <- c()
for (i in 1:length(line_phe)){
  line_phe.tmp <- line_phe[[i]][2]
  line_phe_num <- append(line_phe_num, line_phe.tmp)
}
#column : num
num <- line_phe_num
num <- num %>% as.character()

#column : 2L_921702
DGRP_phe <- DGRP[,num]
DGRP2L921702 <- DGRP_phe["2L_921702",]
DGRP2L921702 <- DGRP2L921702 %>% t()
DGRP2L_921702 <- DGRP2L921702[,1]
DGRP2L_921702 <- DGRP2L_921702 %>% as.numeric()

#column : value
phe_value <- pheno$V2

#df
DGRP2L921702_df <- data.frame(num,DGRP2L_921702,phe_value)

#Compute the sample mean
sum(DGRP2L_921702 * phe_value)/length(num)

# 6. Perform an allelic association test for each SNP, using a Fisher’s exact test. Suppose that the phenotype values greater than 29 are regarded as a ‘case’ group; otherwise, a ‘control’ group. If the cut-off of p-values is 5 × 10−7, how many SNPs are statistically significant?
library(genetics)
load("G:/대학원/생물통계학/HW01/DGRP.Rdata")
pheno <- read.csv("G:/대학원/생물통계학/HW01/pheno.csv", header=FALSE)
line_phe <- str_split(pheno$V1, "_")
line_phe_num <- c()
for (i in 1:length(line_phe)){
  line_phe.tmp <- line_phe[[i]][2]
  line_phe_num <- append(line_phe_num, line_phe.tmp)
}

#column : num
num <- line_phe_num
num <- num %>% as.character()
num
#column : DGRP_phe 
DGRP_phe <- DGRP[,num]
DGRP_phe <- DGRP_phe %>% t()
DGRP_phe %>% dim()

#column : value
phe_value <- pheno$V2

#df
DGRP_phe_df <- data.frame(num,DGRP_phe,phe_value)
DGRP_phe_df[,1]

# Fisher’s exact test
DGRP_phe_df$group <- with(DGRP_phe_df, ifelse(phe_value > 29, "CASE", "CONTROL"))

DGRP_case<- genotype(DGRP_phe_df$group[DGRP_phe_df$group=="CASE"], sep="")
DGRP_cont <- genotype(DGRP_phe_df$group[DGRP_phe_df$group=="CONTROL"], sep="")

alleleTable <- t(rbind(summary(DGRP_case)$allele.freq[,1],
                       summary(DGRP_case)$allele.freq[,1]))
colnames(alleleTable) <- c("CASE", "CONTROL")
alleleTable
chisq.test(alleleTable, correct=FALSE)
fisher.test(alleleTable)

# 7. Perform a logistic regression for each SNP, where three genotypes are considered as a categorical variable. For each SNP, compute an odds ratio (OR) of minor allele homozygous genotype, when a major allele homozygous genotype is reference. For example, suppose that ‘C’ and ‘T’ are major and minor alleles, respectively. Then, OR of the genotype ‘TT’ should be computed for the reference genotype ‘CC’. How many SNPs have ORs greater than 10? (Note that case and control groups are the same as Q6.)

# 8. Perform a QTL mapping, considering four different genetic models: additive, dominant, recessive,and 2-df models. Compute the p-values of four genetic models for each SNP and find statistically significant SNPs, when the cut-off of p-values is 5×10−7.For each genetic model, how many SNPs are statistically significant? Assume that an alternative allele is risk in dominant and recessive genetic models.
