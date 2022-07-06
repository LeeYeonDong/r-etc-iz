# 06 rare variant analysis

#  There are 3 covariates;
#  age
#  gender(Male and Female)
#  ethnic (Black, Hispanic, and White)

# DHS Data Example
load("C:/Users/statistics/Downloads/DHS.RData")

ls()

str(gene03)
dim(gene03)
str(gene04)
dim(gene04)

gene <- c(gene03, gene04, gene05, gene06)
MAF <- num.mut <- NULL
for (i in 1:4) {
  G <- get(paste("gene0", i+2, sep=""))
  X <- G[,14:ncol(G)]
  num.mut[[i]] <- apply(X, 2, function(t) sum(t!=0))
  MAF[[i]] <- apply(X, 2 , mean)/2
}
num.mut
MAF

lapply(MAF, length)
lapply(MAF, function(t) sum(t<0.01))
hist(unlist(MAF), nclass=20, col="orange")
table(round(unlist(MAF), 5))


pval <- array(0, c(9, 4, 3))
for (i in 1:4) {
  G <- get(paste("gene0", i+2, sep=""))
  Y <- G[,5:13]
  Z <- G[,2:4]
  X <- G[,14:ncol(G)]
  ww <- which(MAF[[i]] < 0.01)
  wt <- 1/sqrt(MAF[[i]][ww]*(1-MAF[[i]][ww]))
  C1 <- as.numeric(apply(X[,ww], 1, sum) > 0)
  C2 <- apply(X[,ww], 1, sum)
  C3 <- as.matrix(X[,ww]) %*% matrix(wt, length(wt), 1)
  for (j in 1:3) {
    C <- get(paste("C", j, sep=""))
    data <- data.frame(Z, C)
    pval[,i,j] <- apply(Y, 2, function(t)
      summary(lm(t~., data))$coef[6,4])
  }
}

rownames(pval) <- names(Y)
colnames(pval) <- c("ANGTPL3", "ANGTPL4", "ANGTPL5", "ANGTPL6")
pval # ,,1- sum ,,2-
sum(pval < 0.05/4)

univ.pval <- NULL
X <- gene05[,14:ncol(gene05)]
Z <- gene05[,2:4]
Y <- gene05[,5]
for (i in 1:ncol(X)) {
  data <- data.frame(Z, X[,i])
  univ.pval[i] <- summary(lm(Y~., data))$coef[6,4]
}
univ.pval
data.frame(Num=num.mut[[3]], MAF=MAF[[3]], pval=univ.pval)


# SKAT Example
install.packages("SKAT")
library(SKAT)
load("C:/Users/statistics/Downloads/DHS.RData")
X <- gene05[,14:ncol(gene05)]
Z <- gene05[,2:4]
Y <- gene05[,5]
miss <- is.na(Y)
data <- data.frame(Y, Z)[!miss,]
X1 <- as.matrix(X)[!miss,]

obj <- SKAT_Null_Model(Y~., data, out_type="C") #covariate를 포함시킬때 , 쓸려면 명시
SKAT(X1, obj, kernel = "linear.weighted")$p.value
SKAT(X1, obj, weights.beta=c(1,25))$p.value
SKAT(X1, obj, weights.beta=c(1,1))$p.value
SKAT(X1, obj, weights.beta=c(0.5,0.5))$p.value

dim(X1)

Q1 <- quantile(Y, 0.25, na.rm=TRUE)
Q3 <- quantile(Y, 0.75, na.rm=TRUE)

B <- rep(NA, length(Y))
B[Y < Q1] <- 0
B[Y > Q3] <- 1
Y2 <- B[!is.na(B)]
X2 <- as.matrix(X[!is.na(B), ])
ncount <- apply(X2, 2, sum)
ncount
X2 <- X2[, ncount>0]
dim(X2)
table(Y2)

obj <- SKAT_Null_Model(Y2~1, out_type="D")
SKAT(X2, obj, kernel = "linear.weighted")$p.value
SKAT(X2, obj, weights.beta=c(1,1))$p.value
SKAT(X2, obj, weights.beta=c(0.5,0.5))$p.value

pval <- matrix(0, 9, 4)

for (i in 1:4) {
  G <- get(paste("gene0", i+2, sep=""))
  Y <- G[,5:13]
  Z <- G[,2:4]
  X <- G[,14:ncol(G)]
  ww <- which(MAF[[i]] < 0.01)
  X <- as.matrix(X[,ww])
  for (j in 1:9) {
    YJ <- Y[,j]
    miss <- is.na(YJ)
    data <- data.frame(YJ, Z)[!miss,]
    G <- X[!miss,]
    obj <- SKAT_Null_Model(YJ~., data, out_type="C")
    pval[j, i] <- SKAT(G, obj, weights.beta=c(0.5,0.5))$p.value
  }
}
rownames(pval) <- names(Y)
colnames(pval) <- c("ANGTPL3", "ANGTPL4", "ANGTPL5", "ANGTPL6")
pval
sum(pval < 0.05/4)


# SKAT-O Example
library(SKAT)

X <- gene05[,14:ncol(gene05)]
Z <- gene05[,2:4]
Y <- gene05[,5]
miss <- is.na(Y)
data <- data.frame(Y, Z)[!miss,]
X1 <- as.matrix(X)[!miss,]

obj <- SKAT_Null_Model(Y~., data, out_type="C")
SKAT(X1, obj, r.corr=0)$p.value
SKAT(X1, obj, r.corr=1)$p.value
SKAT(X1, obj, r.corr=0.5)$p.value
SKAT(X1, obj, method="SKATO")$p.value

SKAT(X1, obj, r.corr=0, weights.beta=c(0.5,0.5))$p.value
SKAT(X1, obj, r.corr=1, weights.beta=c(0.5,0.5))$p.value
SKAT(X1, obj, method="SKATO", weights.beta=c(0.5,0.5))$p.value

pval <- matrix(0, 9, 4)

for (i in 1:4) {
  G <- get(paste("gene0", i+2, sep=""))
  Y <- G[,5:13]
  Z <- G[,2:4]
  X <- G[,14:ncol(G)]
  ww <- which(MAF[[i]] < 0.01)
  X <- as.matrix(X[,ww])
  for (j in 1:9) {
    YJ <- Y[,j]
    miss <- is.na(YJ)
    data <- data.frame(YJ, Z)[!miss,]
    G <- X[!miss,]
    obj <- SKAT_Null_Model(YJ~., data, out_type="C")
    pval[j, i] <- SKAT(G, obj, method="SKATO",
                       weights.beta=c(0.5,0.5))$p.value
  }
}

rownames(pval) <- names(Y)
colnames(pval) <- c("ANGTPL3", "ANGTPL4", "ANGTPL5", "ANGTPL6")
sum(pval < 0.05/4)
pval < 0.05/4
