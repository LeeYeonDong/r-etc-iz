install.packages("tidyverse")
library(tidyverse)

##### 02 Statistical Tests for Genetic Data
# r conductor install
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("ALL")

# ALL data
library(ALL)
data(ALL)
?ALL
ALL %>% print()

ALL %>% str()
ALL %>% dim()

ALL_exprs <- ALL %>% exprs()
ALL_exprs[1:10, 1:5]

ALL_exprs %>% dim()
ALL %>% varLabels()

ALL$diagnosis
ALL$sex
ALL$age
ALL$"t(9;22)"
ALL$BT #pivot data
ALL$mol.biol

## Test for Two Sample Means
# Does the gene expression of 1636 g at probe from the two molecular groups (BCR/ABL vs. NEG) has the same population mean?
# 1636 g at
ALL$mol.biol %>% table()
mol <- ALL$mol.biol %in% c("BCR/ABL", "NEG") 
# 벡터 내 특정 값 포함 여부 확인 연산자 %in%

y <- ALL$mol.biol[mol] %>% factor()
x <- t(exprs(ALL))[mol,]

length(y)
dim(x)

x1 <- x[,"1636_g_at"]

stripchart(x1 ~ y, method="jitter", cex.lab=1.5,
           vertical = TRUE, xlab="1636_g_at", pch=21,
           ylab="molecular types", col=c("orange","darkgreen"),
           main="Distribution of 1636_g_at probe by cancer molecular
subtypes")

data.frame(y, x1)

tapply(x1, y, mean)

t1 <- x1[y=="BCR/ABL"]
t2 <- x1[y=="NEG"]

t.test(t1, t2)$stat
wilcox.test(t1, t2)


## Permutation Tests
# permutation : find whether t-dist or not / permutation ��??
permute2 <- function(x, y, K=10000) {
  m <- matrix(y, length(y), K) # length(y) 행, K 열
  sm <- apply(m, 2, sample)
  R1 <- apply(sm, 2, function(t) t.test(x[t=="BCR/ABL"],
                                        x[t=="NEG"])$stat)
  R2 <- apply(sm, 2, function(t) wilcox.test(x[t=="BCR/ABL"],
                                             x[t=="NEG"])$stat)
  return(list(Welch=R1, Wilcoxon=R2))
}

y %>% str()
y %>% View()
sm %>% str()
sm %>% View()

result <- permute2(x1, y, K=100)

# histogram
p <- permute2(x1, y)
par(mfrow=c(1,2))
hist(p$Welch, nclass=50, col="orange", main="Welch")
hist(p$Wilcoxon, nclass=50, col="darkgreen", main="Wilcoxon")

# p-value
tobs <- t.test(t1, t2)$stat
wobs <- wilcox.test(t1, t2)$stat
(sum(tobs < p$Welch) + 1)/ (length(p$Welch)+1)
(sum(wobs < p$Wilcoxon) + 1)/ (length(p$Wilcoxon)+1)
1/10001

range(p$Welch)
range(p$Wilcoxon)

length(y)
y %>% table() # 1=BCR/ABL , 0=NEG

choose(length(y), length(y[y=="NEG"]))


## One-way Analysis of Variance
# We select the expression values for B-cell ALL patients in stage B1, B2, and B3 for probe set (row) name 1866 g at, which captures expression data for the SKI-like oncogene.
ALL$BT
ALL %>% str()

table(ALL$BT)
B1B2B3 <- ALL$BT %in% c("B1","B2","B3")

x1 <- t(exprs(ALL))[B1B2B3, "1866_g_at"]
y <- factor(ALL$BT[B1B2B3], labels=c("B1","B2","B3"))
# 벡터 내 특정 값 포함 여부 확인 연산자 %in%
par(mfrow=c(1,2))
stripchart(x1 ~ y, method="jitter", cex.lab=1.5, pch=21,
           vertical = TRUE, xlab="B-cell ALL stage",
           ylab="SKI-like oncogene expression",
           col=c("orange","darkgreen","blue"))
boxplot(x1 ~ y, cex.lab=1.5, main=NULL,
        xlab="B-cell ALL stage", boxwex=0.3,
        ylab="SKI-like oncogene expression",
        col=c("orange","darkgreen","blue"))
anova(lm(x1 ~ y))

# Next, we selected the expression values for probe 1242_at of the B-cell ALL patients in stage B1, B2, and B3 from the ALL data. This probe corresponds to the Ets2 repressor factor which plays a role in telomerase regulation in human cancer cells.
x2 <- t(exprs(ALL))[B1B2B3, "1242_at"]
stripchart(x2 ~ y, method="jitter", cex.lab=1.5, pch=21,
           vertical = TRUE, xlab="B-cell ALL stage",
           ylab="Ets2 expression",
           col=c("orange","darkgreen","blue"))
boxplot(x2 ~ y, cex.lab=1.5, main=NULL,
        xlab="B-cell ALL stage", boxwex=0.3,
        ylab="Ets2 expression",
        col=c("orange","darkgreen","blue"))
anova(lm(x2 ~ y))

# Kruskal-Wallis (K-W) Test
kruskal.test(x1, y)
kruskal.test(x2, y)

# Permutation Test
permute3 <- function(x, y, K=10000) {
  m <- matrix(y, length(y), K) # length(y) 행, K 열
  sm <- apply(m, 2, sample)
  R1 <- apply(sm, 2, function(t) anova(lm(x~t))$F[1])
  R2 <- apply(sm, 2, function(t) kruskal.test(x, t)$stat)
  return(list(Anova=R1, KW=R2))
}
m %>% dim()
m %>% View()

sm %>% dim()
sm %>% View()

permute3(x1, y, K=100)
permute3(x2, y, K=100)
p1 <- permute3(x1, y)
p2 <- permute3(x2, y)

par(mfrow=c(2,2))
hist(p1$Anova, nclass=50, col="orange", main="ANOVA")
hist(p1$KW, nclass=50, col="darkgreen", main="Kruskal-Wallis")
hist(p2$Anova, nclass=50, col="orange", main="ANOVA")
hist(p2$KW, nclass=50, col="darkgreen", main="Kruskal-Wallis")

F1 <- anova(lm(x1 ~ y))$F[1]
F2 <- anova(lm(x2 ~ y))$F[1]
KW1 <- kruskal.test(x1, y)$stat
KW2 <- kruskal.test(x2, y)$stat

#p-value
(sum(F1 < p1$Anova) + 1)/ (length(p1$Anova)+1)
(sum(KW1 < p1$KW) + 1)/ (length(p1$KW)+1)
(sum(F2 < p2$Anova) + 1)/ (length(p2$Anova)+1)
(sum(KW2 < p2$KW) + 1)/ (length(p2$KW)+1)


## FAMuSS Study
# The Functional SNPs Associated with Muscle Size and Strength (FAMuSS) study was conducted to identify the genetic determinants of skeletal muscle size and strength before and after exercise training.
# A total of n = 1397 college student volunteers participated in the study, and 225 SNPs across multiple genes were collected.
# The primary aim of the study was to identify genes associated with muscle performance.

fmsURL <- "http://stat-gen.org/book.e1/data/FMS_data.txt"
fms <- read.delim(file=fmsURL, header=TRUE, sep="\t")
fms %>% dim()
fms %>% head()
fms %>% tail()
fms %>% str()
attach(fms)
data.frame(id, actn3_r577x, actn3_rs540874, actn3_rs1815739,
           actn3_1671064, Term,Gender,Age,Race,NDRM.CH,DRM.CH)[1:30, ]

apply(fms[,2:226], 2, table)
actn3 <- actn3_rs540874
table(actn3)
summary(factor(actn3))

N <- sum(table(actn3))
table(actn3)/N
GenoFreq <- as.vector(table(actn3)/N)
FreqA <- (2*GenoFreq[1] + GenoFreq[2])/2
FreqA
FreqG <- (GenoFreq[2] + 2*GenoFreq[3])/2
FreqG
install.packages("genetics")
library(genetics)

Geno <- genotype(actn3, sep="")
# genotype function : GG -> G/G
summary(Geno)


## Linkage Disequilibrium
# Prior to conducting tests of association between markers and traits, it is common to examine the marker data for genetic correlation: linkage disequilibrium (LD)
attach(fms)
actn3_r577x[1:10]
actn3_rs540874[1:10]
library(genetics)
Actn3Snp1 <- genotype(actn3_r577x,sep="")
Actn3Snp2 <- genotype(actn3_rs540874,sep="")
Actn3Snp1[1:10]
class(Actn3Snp1)

LD(Actn3Snp1, Actn3Snp2)

Actn3Snp3 <- genotype(actn3_rs1815739,sep="")
Actn3Snp4 <- genotype(actn3_1671064,sep="")

Actn3AllSnps <- data.frame(Actn3Snp1, Actn3Snp2, Actn3Snp3, Actn3Snp4)
LD(Actn3AllSnps)
LD(Actn3AllSnps)$"D'"
LD(Actn3AllSnps)$"R^2"

# If we now compare this to the situation where we look at SNPs in different genes, we see the level of LD is much lower.
Esr1Snp1 <- genotype(esr1_rs1801132, sep="")
LD(Actn3Snp1,Esr1Snp1)

## Linkage Disequilibrium Block
# We can also make a heatmap to graphically illustrate the extent of LD using the LDheatmap package.
install.packages("LDheatmap")
library(LDheatmap)
LDheatmap(Actn3AllSnps, LDmeasure="D'")

# Population Substructure and LD
ObsCount <- matrix(c(136, 64, 64, 136), 2)
ObsCount
ExpCount <- chisq.test(ObsCount)$expected
ExpCount
D <- abs(ObsCount[1]-ExpCount[1])/sum(ObsCount)
PA <- PB <- Pa <- Pb <- 0.5
Dmax <- min(PA*Pb, Pa*PB)
LD <- D/Dmax

# Hardy-Weinberg Equilibrium

# The Human Genome Diversity Project
HGDPURL <- "http://stat-gen.org/book.e1/data/HGDP_AKT1.txt"
HGDP <- read.delim(file=HGDPURL, header=T, sep="\t")
HGDP %>% dim()
HGDP %>% head()
HGDP %>% tail()
HGDP %>% str()

lapply(HGDP[,-c(1,2)], table)

attach(HGDP)
Akt1Snp1 <- AKT1.C0756A
ObsCount <- table(Akt1Snp1)
Nobs <- sum(ObsCount)

FreqC <- (2*ObsCount[3] + ObsCount[2])/(2*Nobs)
ExpCount <- c(Nobs*(1-FreqC)^2, 2*Nobs*FreqC*(1-FreqC), Nobs*FreqC^2)
ExpCount
ChiSqStat <- sum((ObsCount - ExpCount)^2/ExpCount)
ChiSqStat

## HWE Example
library(genetics)
Akt1Snp1 <- genotype(AKT1.C0756A, sep="")
HWE.chisq(Akt1Snp1)

# Fisher’s exact test is more appropriate than the χ2-test for assessing departures from HWE, e.g. expected cell counts are < 5 which occurs in the presence of rare alleles.
Akt1Snp1Maya <- AKT1.C0756A[Population=="Maya"]
ObsCount <- table(Akt1Snp1Maya)
ObsCount

Nobs <- sum(ObsCount)
FreqC <- (2*ObsCount[3] + ObsCount[2])/(2*Nobs)
ExpCount <- c(Nobs*(1-FreqC)^2, 2*Nobs*FreqC*(1-FreqC),
              Nobs*FreqC^2)
ExpCount

# Since the expected count for the first cell is less than 5, using Fisher’s exact test to test for HWE is most appropriate.
# We can use the HWE.exact() function in the genetics package:
library(genetics)
Akt1Snp1Maya <- genotype(AKT1.C0756A[Population=="Maya"], sep="")
HWE.exact(Akt1Snp1Maya)

HWEGeo <- tapply(Akt1Snp1, Geographic.area, HWE.chisq)
HWEGeo
HWEGeo$"Central Africa"
HWEGeo$"South America"

## Pearson’s χ2 Test for Allelic Association
# H0 : there is no association between trait and allele.
# Ha : there is an association between trait and allele.
LHON <- read.table("G:/대학원/생물통계학/lhon.txt", header=TRUE)
LHON %>% head()
LHON %>% tail()
LHON %>% str()

library(genetics)
geno <- genotype(LHON$GENO, sep="")
geno %>% summary()
geno %>% str()

pheno <- LHON$PHENO

geno_case <- genotype(LHON$GENO[pheno=="CASE"], sep="")
geno_cont <- genotype(LHON$GENO[pheno=="CONTROL"], sep="")
summary(geno_case)
summary(geno_cont)
alleleTable <- t(rbind(summary(geno_case)$allele.freq[,1],
                       summary(geno_cont)$allele.freq[,1]))
colnames(alleleTable) <- c("Case", "Control")
alleleTable
chisq.test(alleleTable, correct=FALSE)
fisher.test(alleleTable)

# Cochran-Armitage Test Example
summary(geno_case)
summary(geno_cont)
GenoTable <- rbind(summary(geno_case)$genotype.freq[,1],
                   summary(geno_cont)$genotype.freq[,1])
GenoTable

install.packages("DescTools")
library(DescTools)

LHON <- read.table("G:/대학원/생물통계학/lhon.txt", header=TRUE)
library(genetics)
geno <- genotype(LHON$GENO, sep="")

GenoTable %>% CochranArmitageTest() #걍 이거 돌리면 다 나옴

g <- geno %>% as.numeric()
p <- as.numeric(factor(pheno))

r2 <- cor(g,p)^2
teststat <- length(geno)*r2
sign(cor(g,p))*sqrt(teststat) # x -> -1,0,1
1-pchisq(teststat, 1)

# Odds Ratios Example
LHON$newpheno <- with(LHON, ifelse(PHENO=="CASE", 1, 0))
LHON %>% head()
g <- glm(newpheno ~ GENO, family="binomial", data=LHON)
g %>% summary()
g %>% coef()
exp(coef(g))
g %>% confint()
exp(confint(g))

LHON$newgeno <- with(LHON, relevel(factor(GENO), ref = "TT"))
LHON %>% head()
g2 <- glm(newpheno ~ newgeno, family="binomial", data=LHON)
g2 %>% summary()

LHON$genoadd <- with(LHON, 0 + 1*(GENO=="CT") + 2*(GENO=="TT"))
g3 <- glm(newpheno ~ genoadd, family="binomial", data=LHON)
g3 %>% summary()


alleleTable <- t(rbind(summary(geno_case)$allele.freq[,1],
                       summary(geno_cont)$allele.freq[,1]))
colnames(alleleTable) <- c("Case", "Control")
alleleTable
n11 <- alleleTable[1,1]
n12 <- alleleTable[1,2]
n21 <- alleleTable[2,1]
n22 <- alleleTable[2,2]

# OR = 1 implies no association with the disease.
# OR != 1 implies an association with the disease.
OR <- (n11*n22)/(n12*n21)
OR
se <- sqrt(1/n11 + 1/n12 + 1/n21 + 1/n22)
alpha <- 0.05
CI <- log(OR) + c(qnorm(alpha/2)*se, qnorm(1-alpha/2)*se)
exp(CI)

# QTL Data Example
# Quantitative trait loci (QTL) is a genetic marker or a genetic region associated with the quantitative trait.
dsbp <- read.csv("G:/대학원/생물통계학/dsbp.txt", header=TRUE)

dsbp %>% head()
dsbp %>% str()
dsbp <- dsbp[,-1]
dsbp %>% head()

summary(dsbp[,c("dbp", "sbp")])
par(mfrow=c(1,2))
hist(dsbp$dbp, nclass=30, col="orange", xlab="",
     main="diastolic blood pressure")
hist(dsbp$sbp, nclass=30, col="darkblue", xlab="",
     main="systolic blood pressure")

dsbp$snp3 %>% table()
summary(factor(dsbp$snp3))
miss <- is.na(dsbp$snp3)
y <- dsbp$sbp[!miss]
snp <- factor(dsbp$snp3)[!miss]


##### 03 Multiple Testing Procedure
### A Single Test
### Multiple Testing
## If we accepted p < .05 as convincing evidence, what is the probability that we would reject at least one null hypothesis, even if all null hypotheses are true (assuming the tests are independent)?
### Family-wise Error Rate
### Bonferroni Adjustment

### Golub Data Example
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("golubEsets")
library(golubEsets)
data(Golub_Merge)
?Golub_Merge
Golub_Merge %>% str()
Golub_Merge %>% dim()
Golub_Merge %>% varLabels()

Golub_Merge$Gender
Golub_Merge$ALL.AML

table(Golub_Merge$ALL.AML)
dim(exprs(Golub_Merge))
head(exprs(Golub_Merge))
tail(exprs(Golub_Merge))

genes <- Golub_Merge %>% exprs()
pval <- NULL
m <- genes %>% nrow() # row's length

for (i in 1:m) {
  pval[i] <- t.test(genes[i, Golub_Merge$ALL.AML=="ALL"],
                    genes[i, Golub_Merge$ALL.AML=="AML"])$p.val
}

### Multiple Testing
sum(pval < 0.05)
sum(pval < 0.05/m)

### Family-wise Error Rate
sel <- which(pval < 0.05/m)
data.frame(genes=rownames(genes)[sel], pvalue=pval[sel])
oo <- order(pval)[1:length(sel)]
data.frame(genes=rownames(genes)[oo], pvalue=pval[oo])
cols <- rep("red", m)
cols[sel] <- "darkblue"
plot(-log10(pval), type="p", pch=20, col=cols, xlab="Gene")
abline(h=-log10(0.05/m), lty=2)

### Bonferroni Adjustment
pbon <- p.adjust(pval, method="bonferroni")
sum(pbon < 0.05)
sum(pval < 0.05/m)

data.frame(genes=rownames(genes)[oo], unadjusted=pval[oo],
           adjusted=pbon[oo])

round(cbind(summary(pval), summary(pbon)),5)

par(mfrow=c(1,2))
hist(pval, col="orange", nclass=50, xlab="",
     main="Un-adjusted p-values")
hist(pbon, col="purple", nclass=50, xlab="",
     main="Adjusted p-values")

cols <- rep("red", m)
cols[sel] <- "darkblue"
plot(-log10(pbon), type="p", pch=20, col=cols, xlab="Gene")
abline(h=-log10(0.05), lty=2)

### False Discovery Rate
# An alternative strategy for dealing with multiple testing is to estimate false discovery rates (FDR).
# FDR estimates the proportion of significant findings that are type I errors.
# False discovery rate is an expected proportion of null hypotheses that are true among those that are declared significant.

### False Discovery Rate Control
q <- 0.01
BHp <- sort(pval, decreasing=T)*m/seq(m,1)
BHp[order(pval, decreasing=T)] <- cummin(BHp)
sum(BHp < q)

BHp2 <- p.adjust(pval, method="BH")
sum(BHp2 < q)
sel2 <- which(BHp2 < q)
oo2 <- order(BHp2)[1:length(sel2)]
data.frame(genes=rownames(genes)[oo2], BH.adjusted=BHp2[oo2])

BYp <- p.adjust(pval, method="BY")
sum(BYp < q)

sel3 <- which(BYp < q)
oo3 <- order(BYp)[1:length(sel3)]
data.frame(genes=rownames(genes)[oo3], BH.adjusted=BHp2[oo3],
           BY.adjusted=BYp[oo3])
