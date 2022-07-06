#### 1.3 ?????Ð¼??? ???? ?Ú·?
### R-?Úµ? 1.3.1 : ???? ?×·ìº° Ã´?? ?À¿? ???? ?????? À±??
## ?????? ??Å°?? ?Ò·??À±?
```{r load_lpes)
library(tidyverse)
data(mice)
mice %>% str()
```

## Shapof 3 groups of T2 mouse with 6 landmarks
par(mfrow=c(1,3))
joins <- c(1, 6, 2, 3, 4, 5, 1)

mice_c <- grep("c",mice$group)
mice_l <- grep("l",mice$group)
mice_s <- grep("s",mice$group)

plotshapes(mice$x[,,mice_c], color=1:6, symbol=1, joinline=joins)
title("(a) ??Á¶ ?×·?")
plotshapes(mice$x[,,mice_l], color=1:6, symbol=1, joinline=joins)
title("(b) Å« ?×·?")
plotshapes(mice$x[,,mice_s], color=1:6, symbol=1, joinline=joins)
title("(c) ??Àº ?×·?")

## Shapes of 3 groups of T2 mouse with 60 points 
par(mfrow=c(1,3))
joins=c(1:60, 1)

plotshapes(mice$outlines[,,mice_c], color=4, joinline=joins)
title("(a) ??Á¶ ?×·?")
plotshapes(mice$outlines[,,mice_l], color=4, joinline=joins)
title("(b) Å« ?×·?")
plotshapes(mice$outlines[,,mice_s], color=4,  joinline=joins)
title("(c) ??Àº ?×·?")


### R-?Úµ? 1.3.2 ???? ?×·ìº° Ã´?? ?À¿? ???? ?Ù¸? ?????? ?Ú·á¿¡ ???? ????
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(qcet2.dat)
qcet2.dat %>% str()

## Shapes od 3 groups of T2 mouse with 6 landmarks
par(mfrow=c(1,3))
joins=c(1, 6, 2, 3, 4, 5, 1)
plotshapes(qcet2.dat, color=1:6, symbol=1, joinline=joins)
title("(a) ??Á¶ ?×·?")
plotshapes(qlet2.dat, color=1:6, symbol=1, joinline=joins)
title("(b) Å« ?×·?")
plotshapes(qset2.dat, color=1:6, symbol=1, joinline=joins)
title("(c) ??Àº ?×·?")

## Shapes of 3 groups of T2 mouse with 6 landmarks : text data 
mouseT2 <- readLines("C:/???Ð¿?/?Ùº??? ?????? ?????Ð¼?/R-codes-ShapeAnalysis/mouseT2.txt")
mouseT2 <- str_split(mouseT2," ")
mouseT2 <- mouseT2 %>% unlist()
mouseT2 <- mouseT2 %>% as.double()

mouseT2_dim_int <- length(mouseT2) / (6*2)
n <- mouseT2_dim_int %>% ceiling()

mouseT2_s <- split(mouseT2,rep(1:n,each=12))

mouseT2_a <- array(dim=c(6,2,n))

for (i in 1:n){
  mouseT2_a[,,i] <- array(mouseT2_s[[i]], dim = c(2,6)) %>% t()
}

cm <- mouseT2_a[,,1:30] # ??Á¶ ?×·? n1=30
sm <- mouseT2_a[,,31:53] # ??Àº ?×·? n2=23
lm <- mouseT2_a[,,54:76] # Å« ?×·? n3=23

par(mfrow=c(1,3))
plotshapes(cm,color=1:6, symbol=1, joinline=joins)
title("(a) ??Á¶ ?×·?")
plotshapes(lm,color=1:6, symbol=1, joinline=joins)
title("(b) ??Àº ?×·?")
plotshapes(sm,color=1:6, symbol=1, joinline=joins)
title("(c) Å« ?×·?")


### R-?Úµ? 1.3.3 ?? Á¾?? À¯?Î¿??? ?????? À±??
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(apes)
apes %>% str()

## Shapes of Gorillas, Chimpanzees, Orangutans with 8 landmarks
par(mfrow=c(3,2))
joins=c(1, 5, 4, 3, 2, 8, 7, 6, 1)

gorm <- grep("gorm",apes$group)
gorf <- grep("gorf",apes$group)
panm <- grep("panm",apes$group)
panf <- grep("panf",apes$group)
pongom <- grep("pongom",apes$group)
pongof <- grep("pongof",apes$group)

plotshapes(apes$x[,,gorm], color=1, joinline=joins)
title("(a) ???? ?í¸±??")
plotshapes(apes$x[,,gorf], color=1, joinline=joins)
title("(b) ???? ?í¸±??")

plotshapes(apes$x[,,panm], color=2, joinline=joins)
title("(c) ???? Ä§????")
plotshapes(apes$x[,,panf], color=2, joinline=joins)
title("(d) ???? Ä§????")

plotshapes(apes$x[,,pongom], color=3, joinline=joins)
title("(e) ???? ?À¶???Åº")
plotshapes(apes$x[,,pongof], color=3, joinline=joins)
title("(f) ???? ?À¶???Åº")


### R-?Úµ? 1.3.4 ??Á¶ ?×·??? Ã³???×·??? ?? ??Äµ ???? ??Äµ ????
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(schizophrenia)
schizophrenia %>% str()

## Shapes of Control and Treatment Groups with 13 landmarks
plotshapes(schizophrenia$x, symbol=as.integer(schizophrenia$group))
title("(a) ??Á¶?? Ã³???×·? ??Ã¼ ?? ???? ??Äµ")

par(mfrow=c(1,2))

con <- grep("con",schizophrenia$group)
scz <- grep("scz",schizophrenia$group)

plotshapes(schizophrenia$x[,,con])
title(" (b) ??Á¶ ?×·? ?? ???? ??Äµ")
plotshapes(schizophrenia$x[,,scz], symbol=2)
title(" (c) Ã³???×·? ?? ???? ??Äµ")


### R-?Úµ? 1.3.5 ??Ã¼?? ?? ?????? ??????È£ ???? 3 ????
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(digit3.dat)
digit3.dat %>% str()

## Shapes of Digit 3 from Postcode
par(mfrow=c(1,2))

plotshapes(digit3.dat, joinline=1:13)
title("(b) ??Ã¼ ????")
plotshapes(digit3.dat[,,2:3], joinline=1:13)
title("(c) ?? ???? ????")


### R-?Úµ? 1.3.6 ??È­???? ????
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(shells)
shells %>% str()

## Shapes of shells
par(mfrow=c(1,2))
plotshapes(shells$uv, joinline=c(1:3,1))
title("(b) ??Ã¼ ????")
plotshapes(shells$uv[,, c(1, 6, 12, 19, 20)], joinline=c(1:3,1))
title("(c) ?ß½?È­ Å©?â¿¡ ???? ??Ç¥ ????")


### R-?Úµ? 1.3.7 ???â¿µ?? ???? ?Ì¹???
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(gels)
gels %>% str()

## Shapes of Gel A and Gel B Images
par(mfrow=c(1,2))

plotshapes(gels[,,1])
title("(c) ?? A ???? ")
plotshapes(gels[,,2])
title("(d) ?? B ????")


### R-?Úµ? 1.3.8 ???Ý´? ??À§ ???í¼± ?Ì¹???
## ?????? ??Å°?? ?Ò·??À±?
## 150 Patients(56 male, 94 female)
library(shapes)
library(openxlsx)

## Inner Cortex
inner_36 <- read.xlsx("C:/???Ð¿?/?Ùº??? ?????? ?????Ð¼?/R-codes-ShapeAnalysis/mandible.xlsx", sheet = 1)
inner_36 <- inner_36[,-c(1,2)]

inner_36_len <- inner_36 %>% unlist() %>% length()
inner_36_dim <- inner_36 %>% dim()

inner_a <- array(dim=c(23,2,inner_36_dim[1]))

for (i in 1:inner_36_dim[1]){
  inner_row <- inner_36[i,] %>% as.double()
  inner_a[,,i] <- array(inner_row, dim = c(2,23)) %>% t()
}

## Outer Cortex
outer_36 <- read.xlsx("C:/???Ð¿?/?Ùº??? ?????? ?????Ð¼?/R-codes-ShapeAnalysis/mandible.xlsx", sheet = 2)
outer_36 <- outer_36[,-c(1,2)]

outer_36_dim <- outer_36 %>% dim()

outer_a <- array(dim=c(23,2,outer_36_dim[1]))

for (i in 1:outer_36_dim[1]){
  outer_row <- outer_36[i,] %>% as.double()
  outer_a[,,i] <- array(outer_row, dim = c(2,23)) %>% t()
}

## Shape of Inner and Outer Cortex of First Patient
par(mfrow=c(1,2))
joins <- c(1:23)

plotshapes(inner_a[,,1],color=1:6, joinline=joins)
title("(c) ?Üº? ????")
plotshapes(outer_a[,,1],color=1:6, joinline=joins)
title("(d) ???? ????")


### R-?Úµ? 1.3.9 ???Ý¼? ???? È¯???? ???????? ?Ì¹???
## ?????? ??Å°?? ?Ò·??À±?
## 100 Patients(60 males, 40 females)
library(shapes)
library(openxlsx)
palatal <- read.xlsx("C:/???Ð¿?/?Ùº??? ?????? ?????Ð¼?/R-codes-ShapeAnalysis/palatal.xlsx")

## Palatal Morphology
palatal <- palatal[,-1]

palatal_dim <- palatal %>% dim()

palatal_a <- array(dim=c(200,3,palatal_dim[1]))

for (i in 1:palatal_dim[1]){
  palatal_row <- palatal[i,] %>% as.double()
  palatal_a[,,i] <- array(palatal_row, dim = c(3,200)) %>% t()
}

m <- p[,,41:100] # ???? È¯?? 60??
f <- p[,,1:40] # ???? È¯?? 40??

shapes3d(palatal_a[,,1], color=3, axes3=T) # ??Ã¼ È¯?? ???? ???? ????
shapes3d(m[,,1], color=4, axes3=T) # ???? È¯?? ???? ???? ????
shapes3d(f[,,1], color=2, axes3=T) # ???? È¯?? ???? ???? ????


### R-?Úµ? 1.3.10 ???? ?Ë°????? ????
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(sand)
sand %>% str()

## Shapes of Sand Grains of Sea and River
par(mfrow=c(1,2))

sea <- grep("sea",sand$group)
river <- grep("river",sand$group)

plotshapes(sand$x[,,sea], joinline=c(1:50,1))
title("(a) ?Ù´? ???? À±??")
plotshapes(sand$x[,,river], joinline=c(1:50,1))
title("(b) ?? ???? À±??")


### R-?Úµ? 1.4.1 ???? ?Ë°????? ????
## ?????? ??Å°?? ?Ò·??À±?
## Centroid size and Centred Pre-Shape for Three Groups of Mices
library(shapes)
data(mice)
k <- dim(mice$x)[1]
m <- dim(mice$x)[2]
C <- diag(k)-matrix(1/k,k,k) # Centring matrix
joins <- c(1, 6, 2, 3, 4, 5, 1)

## 1st mice of Control group
data(qcet2.dat)

cX <- qcet2.dat[,,1]
cXc <- C%*%cX
csize <- sqrt(sum(diag(t(cXc)%*%cXc)))
cZc <- cXc/csize
list(csize,centroid.size(cZc))

# Original Shape
par(mfrow=c(3,2));par(pty="s")
plot(cX,xlab="",ylab="")
lines(cX[joins,])
title("(a) ??Á¶ ?×·? : ?? ????")

# Centred Pre-Shape
plot(cZc,xlab="",ylab="")
lines(cZc[joins,])
abline(h=0,v=0)
title("(b) ??Á¶ ?×·? : ?ß½?È­ ???? ????")

## 1st mice of Large group
lX <- qlet2.dat[,,1]
lXc <- C%*%lX
csize <- sqrt(sum(diag(t(lXc)%*%lXc)))
lZc <- lXc/csize
list(csize,centroid.size(lZc))

# Original Shape
plot(lX,xlab="",ylab="")
lines(lX[joins,])
title("(c) Å« ?×·? : ?? ????")

# Centred Pre-Shape
plot(lZc,xlab="",ylab="")
lines(lZc[joins,])
abline(h=0,v=0)
title("(d) Å« ?×·? ?ß½?È­ ???? ????")

## 1st mice of Small group
sX <- qset2.dat[,,1]
sXc <- C%*%sX
csize <- sqrt(sum(diag(t(sXc)%*%sXc)))
sZc <- sXc/csize
list(csize,centroid.size(sZc))

# Original Shape
plot(sX,xlab="",ylab="")
lines(sX[joins,])
title("(e) ??Àº ?×·? : ?? ????")

# Centred Pre-Shape
plot(sZc,xlab="",ylab="")
abline(h=0,v=0)
lines(sZc[joins,])
title("(f) ??Àº ?×·? : ?ß½?È­ ???? ????")


### R-?Úµ? 1.4.2 ???? ?×·ìº° ??Ã¼ Ã´?? ?? ?????? ?ß½?Å©?â¿¡ ???? ANOVA
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(mice)

# Centroid Sizes of Three Groups: Control, Large, Small
size <- centroid.size(mice$x)
plot(mice$group, size)# Box plot

## ANOVA : ?Ð»??Ð¼?
fit <- aov(size~mice$group)
fit %>% summary()

## Multiple Comparison: ???ßº???
# ?????? ??Å°?? ?Ò·??À±?
install.packages("agricolae")
library(agricolae)
duncan.test(fit, "mice$group", console=T)


### R-?Úµ? 1.4.3 ??Á¶?? Ã³?? ?????? Ã¹??Â° È¯?? ?? ???? ?Ì¹????? ???? ?ß½?È­ ?????? ?ß½?È­ ???? ????
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(schizophrenia)

k <- dim(schizophrenia$x)[1]
m <- dim(schizophrenia$x)[2]
C <- diag(k)-matrix(1/k,k,k) # Centring matrix

con <- grep("con",schizophrenia$group)
scz <- grep("scz",schizophrenia$group)
cX <- schizophrenia$x[,,con] # ??Á¶ ?×·?
tX <- schizophrenia$x[,,scz] # Ã³?? ?×·?

## ??Á¶ ???? Ã¹??Â° È¯??
cX <- cX[,,1]
cXc <- C%*%cX
csize <- sqrt(sum(diag(t(cXc)%*%cXc)))
cZc <- cXc/csize
list(csize,centroid.size(cZc))

# Original Shape
par(mfrow=c(2,2))
par(pty="s")

plot(cX,xlim=c(-0.7, 0.7), ylim=c(-0.7, 0.6))
lines(cX[c(1:13,1),], lty=1)
title("(a) ??Á¶ ???? : ?? ????")

# Centred Pre-Shape
plot(cZc,xlim=c(-0.5, 0.4), ylim=c(-0.4, 0.4))
lines(cZc[c(1:13,1),], lty=1)
abline(h=0,v=0)
title("(b) ??Á¶ ?×·? : ?ß½?È­ ???? ????")

## Ã³?? ?×·? Ã¹??Â°
tX <- tX[,,1]
tXc <- C%*%tX
csize <- sqrt(sum(diag(t(tXc)%*%tXc)))
tZc <- tXc/csize
list(csize,centroid.size(tZc))

# Original Shape
plot(tX,xlim=c(-0.7, 0.7), ylim=c(-0.7, 0.7))
lines(tX[c(1:13,1),], lty=1)
title("(c) Ã³?? ?×·?  : ?? ????")

# Centred Pre-Shape
plot(tZc,xlim=c(-0.5, 0.4), ylim=c(-0.4, 0.4))
lines(tZc[c(1:13,1),], lty=1)
abline(h=0,v=0)
title("(d) Ã³?? ?×·? : ?ß½?È­ ???? ????")


### R-?Úµ? 1.4.4 ??Á¶?? Ã³?? ?×·??? Ã¹??Â° È¯?? ?? ???? ?Ì¹??? ?????? ?ß½? Å©?â¿¡ ???? Á¤?Ô¼? ??Á¤?? t-??Á¤
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(schizophrenia)

## Centroid Sizes of Three Groups: Control, Large, Small
size <- centroid.size(schizophrenia$x)
plot(schizophrenia$group, size)# Box plot

## ??Á¶ ?×·?, Ã³?? ?×·?
con <- grep("con",schizophrenia$group)
scz <- grep("scz",schizophrenia$group)
x <- size[con]
y <- size[scz]

## Ç¥???? Å©??
n1 <- length(x)
n2 <- length(y)

## ????, Ç¥??????
mean(x)
sd(x)
mean(y)
sd(y)

## Á¤?Ô¼? ??Á¤
shapiro.test(x)
shapiro.test(y)

## t-test : ??Ç¥??
t.test(x,y, var.equal=T, alternative="greater")


### R-?Úµ? 1.4.5 ???Ý´? ??À§ ???Î¿? ?Üº? ???? ?ß½?Å©?â¿¡ ???? ???? t-??Á¤
## ?????? ??Å°?? ?Ò·??À±?
library(shapes) 
library(openxlsx) # 150 Patients(56 male, 94 female)

## Inner Cortex
inner_36 <- read.xlsx("C:/???Ð¿?/?Ùº??? ?????? ?????Ð¼?/R-codes-ShapeAnalysis/mandible.xlsx", sheet = 1)
inner_36 <- inner_36[,-c(1,2)]

inner_36_len <- inner_36 %>% unlist() %>% length()
inner_36_dim <- inner_36 %>% dim()

inner_a <- array(dim=c(23,2,inner_36_dim[1]))

for (i in 1:inner_36_dim[1]){
  inner_row <- inner_36[i,] %>% as.double()
  inner_a[,,i] <- array(inner_row, dim = c(2,23)) %>% t()
}

## Outer Cortex
outer_36 <- read.xlsx("C:/???Ð¿?/?Ùº??? ?????? ?????Ð¼?/R-codes-ShapeAnalysis/mandible.xlsx", sheet = 2)
outer_36 <- outer_36[,-c(1,2)]

outer_36_dim <- outer_36 %>% dim()

outer_a <- array(dim=c(23,2,outer_36_dim[1]))

for (i in 1:outer_36_dim[1]){
  outer_row <- outer_36[i,] %>% as.double()
  outer_a[,,i] <- array(outer_row, dim = c(2,23)) %>% t()
}

# ???? t-??Á¤
X <- centroid.size(inner_a) # ???? ???Ý´? ?ß½?Å©??
Y <- centroid.size(outer_a) # ?Üº? ???Ý´? ?ß½?Å©??
t.test(X, Y, paired=T, alternative="greater")


### R-?Úµ? 1.5.1 ??Àº ???? Ã´?? ?À¿? ???? ???? ?Ð¼?
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
joins <- c(1, 6, 2, 3, 4, 5, 1)

## Groups: Control, Large, Small 
cm <- qcet2.dat # Control n1=30
lm <- qlet2.dat # Large n2=23
sm <- qset2.dat # Samll n3=23

## Centred Pre-Shape : Small group
cent.preshape <- function(X){
  k <- dim(X)[1];m <- dim(X)[2];n <- dim(X)[3]
  C <- diag(k)-matrix(1/k,k,k)
  csize <- centroid.size(X) 
  if( is.na(n)){ shape <- C%*%X/csize }
  if(!is.na(n)){
    shape <- array(NA,c(k,m,n))
    for(i in 1:n){ shape[,,i] <- C%*%X[,,i]/csize[i] }
  }
  return(shape)
}
sX <- sm
sZc <- cent.preshape(sX)

## Bookstein Coordinates
bcm <- bookstein2d(cm)
blm <- bookstein2d(lm)
bsm <- bookstein2d(sm)

## Shape Plots 
par(mfrow=c(2,4))

plotshapes(sm, symbol=1, joinline=joins) # Original shape
plotshapes(sZc, symbol=1, joinline=joins) # Centred Pre-shape

# Control
plotshapes(bcm$bshpv, symbol=1, joinline=joins)#Bookstein shape
plotshapes(bcm$mshape, symbol=1, joinline=joins)# Bookstein mean shape

# Large
plotshapes(blm$bshpv, symbol=1, joinline=joins)#Bookstein shape
plotshapes(blm$mshape, symbol=1, joinline=joins)# Bookstein mean shape

# Small
plotshapes(bsm$bshpv, symbol=1, joinline=joins)#Bookstein shape
plotshapes(bsm$mshape, symbol=1, joinline=joins)# Bookstein mean shape


### R-?Úµ? 1.5.2 ??Àº ?×·? ?? Ã´?? ?? ?????? ?ß½?Å©???? ?Ï½????? ??Ç¥?? ???? ??Á¡??
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
install.packages("psych")
library(psych)

## Multiple scatter plot of centroid size and Bookstein coordinates for small group
# Groups: Control, Large, Small 
cm <- qcet2.dat # Control n1=30
lm <- qlet2.dat # Large n2=23
sm <- qset2.dat # Samll n3=23

## Bookstein Coordinates
bcm <- bookstein2d(cm)
blm <- bookstein2d(lm)
bsm <- bookstein2d(sm)

## correlation between centroid size and Bookstein coordinates
# control
csize <- centroid.size(cm)
cs <- matrix(csize,1,23)
u <- bsm$bshpv[3:6,,]
x <- rbind(cs,u[,1,],u[,2,])
x <- t(x)
pairs.panels(x, density=F, ellipses=F, 
             labels=c("csize", "u3", "u4", "u5", "u6", "v3", "v4", "v5", "v6"))

# Large
csize <- centroid.size(lm)
cs <- matrix(csize,1,23)
u <- bsm$bshpv[3:6,,]
x <- rbind(cs,u[,1,],u[,2,])
x <- t(x)
pairs.panels(x, density=F, ellipses=F, 
             labels=c("csize", "u3", "u4", "u5", "u6", "v3", "v4", "v5", "v6"))

# Small
csize <- centroid.size(sm)
cs <- matrix(csize,1,23)
u <- bsm$bshpv[3:6,,]
x <- rbind(cs,u[,1,],u[,2,])
x <- t(x)
pairs.panels(x, density=F, ellipses=F, 
             labels=c("csize", "u3", "u4", "u5", "u6", "v3", "v4", "v5", "v6"))


### R-?Úµ? 1.6.1 ??Àº ?×·? Ã¹??Â° ?? Ã´?? ?? ?????? ??????Ç¥?? ???? ?ß½?È­ ???? ????
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)

## Pre-shape based on the Complex coordinates for small group
joins <- c(1, 6, 2, 3, 4, 5, 1)
m1 <- qset2.dat[,,1]# 1st mouse od small gruop
k <- dim(m1)[1]
m <- dim(m1)[2]
C <- diag(k)-matrix(1/k,k,k) # Centring matrix

# Row coodinates 
x <- m1[,1]
y <- m1[,2]

# Orignal complex coordinates
z0 <- complex(real=x, imaginar=y)
H <- defh(k-1)
zh <- H%*%z0;round(zh, 2)
csize <- centroid.size(z0) # centroid size
z <- zh/csize;round(z,2)
zc <- t(H)%*%z;round(zc,2) # centred pre-shape
zcs=cbind(Re(zc),Im(zc))

## Plots of Original shape and centred pre-shape
par(mfrow=c(1,2))
par(pty="s")
plot(m1, xlim=c(-50,250), ylim=c(-50,250),
     xlab="", ylab="", pch=".")
lines(m1[joins,])
text(x, y, labels=c(1:6))
abline(v=0, h=0)
title("(a) ?? ????")

plotshapes(zcs, symbol=as.character(1:6),joinline=joins)
abline(v=0, h=0)
title("(b) ?ß½?È­ ???? ????")


### R-?Úµ? 2.1.1 ???? ?×·ìº° Ã¹ ??Â° ???? Ã´?? ?À¿? ???? ?×·ìº° ?????Å¸?
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)

## Shape Distances for Three Groups of Mices
# 1st mice of Control, Small, Large groups
cX <- qcet2.dat[,,1]
lX <- qlet2.dat[,,1]
sX <- qset2.dat[,,1]

# Reimannian distance
rho1 <- riemdist(cX,sX)
rho2 <- riemdist(cX,lX)
rho3 <- riemdist(sX,lX)
rho <- cbind(rho1,rho2,rho3) 
rho

# Partial Procrustes distance
dP1 <- 2*sin(rho1/2)
dP2 <- 2*sin(rho2/2)
dP3 <- 2*sin(rho3/2)
dP <- cbind(dP1,dP2,dP3)
dP

# Full Procrustes distance
dF1 <- sin(rho1)
dF2 <- sin(rho2)
dF3 <- sin(rho3)
dF <- cbind(dF1,dF2,dF3)
dF


### R-?Úµ? 2.1.2 ?? ?×·? ???? Ã´?? ???? ?Ï½????? ???????? ?? ?????Å¸?
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)

## Shape distances for three groups's mean shapes
joins <- c(1, 6, 2, 3, 4, 5, 1)

## Groups: Control, Large, Small 
cm <- qcet2.dat # Control n1=30
lm <- qlet2.dat # Large n2=23
sm <- qset2.dat # Samll n3=23

## Bookstein Coordinates
bcm <- bookstein2d(cm)
blm <- bookstein2d(lm)
bsm <- bookstein2d(sm)

## Plots of Bookstein mean shapes
par(mfrow=c(1,3))

plotshapes(bcm$mshape, symbol=1, joinline=joins)
title("(a) ??Á¶ ?×·?")
plotshapes(bsm$mshape, symbol=1, joinline=joins)
title("(b) ??Àº ?×·?")
plotshapes(blm$mshape, symbol=1, joinline=joins)
title("(c) Å« ?×·?")

## Reimannian distance
rho1 <- riemdist(bcm$mshape,bsm$mshape)
rho2 <- riemdist(bcm$mshape,blm$mshape)
rho3 <- riemdist(bsm$mshape,blm$mshape)
rho <- cbind(rho1,rho2,rho3) 
rho

## Partial Procrustes distance
dP1 <- 2*sin(rho1/2)
dP2 <- 2*sin(rho2/2)
dP3 <- 2*sin(rho3/2)
dP <- cbind(dP1,dP2,dP3)
dP

## Full Procrustes distance
dF1 <- sin(rho1)
dF2 <- sin(rho2)
dF3 <- sin(rho3)
dF <- cbind(dF1,dF2,dF3)
dF


### R-?Úµ? 2.2.1 ?×·ìº° Ã¹ ??Â° ???? Ã´?? ?À¿? ???? ?×·ìº° ?????Å¸??? MDS
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)

## MDS for Reimannian distance of Three Groups of Mices
# 1st mice of Control, Small, Large groups
cX <- qcet2.dat[,,1]
lX <- qlet2.dat[,,1]
sX <- qset2.dat[,,1]

# Reimannian distance
rho1 <- riemdist(cX,sX)
rho2 <- riemdist(cX,lX)
rho3 <- riemdist(sX,lX)
rho <- cdind(rho1,rho2,rho3) 
rho

# Metrtic MDS with Reimannian distance
distR <- matrix(c(0.0, rho1, rho2,
               rho1, 0.0, rho3,
               rho2, rho3, 0.0), 3,3)

con <- cmdscale(distR)
plot(con, xlab="Dim1",ylab="Dim2",  pch=".")
abline(h=0,v=0)
text(con[,1], con[,2], labels=c("C", "S", "L"))


### R-?Úµ? 2.2.2 ???? ?×·ìº° Ã´?? ?À¿? ???? ?????Å¸??? MDS
## ?????? ??Å°?? ?Ò·??À±?
library(MASS)
library(shapes)
data(mice)

## MDS for Reimannian distance of Three Groups of Mices
# Reimannian distance
distR <- matrix(0, 76, 76)

for (i in 1:76){
  for (j in 1:76){
    rho <- riemdist(mice$x[,,i],mice$x[,,j])
    distR[i, j] <- rho
  }
}

# Metrtic MDS with Reimannian distance
par(mfrow=c(1,2))

con <- cmdscale(distR)
plot(con, xlab="Dim1",ylab="Dim2",  pch=as.character(group))
abline(h=0,v=0)
title("(a) Metric MDS")

# Non-Metrtic MDS with Reimannian distance
con <- isoMDS(distR);con
plot(con$points, xlab="Dim1",ylab="Dim2", pch=as.character(group))
abline(h=0,v=0)
title("(b) Non-metric MDS")


### R-?Úµ? 2.2.3 ?À¶???Åº ?Ï¼? ?Î°??? ?????Å¸??? MDS
## ?????? ??Å°?? ?Ò·??À±?
library(MASS)
library(shapes)
data(pongof.dat) # 24 Females
data(pongom.dat) # 30 Males

## MDS for Orang Utan
orang <- array(data=c(pongof.dat,pongom.dat), dim=c(8,2,54)) 
group <- c(rep("f",24), rep("m",30))

# Reimannian distance
distR <- matrix(0, 54, 54)
for (i in 1:54){
  for (j in 1:54){
    rho <- riemdist(orang[,,i], orang[,,j])
    distR[i, j] <- rho
  }
}

# Metrtic MDS with Reimannian distance
con <- cmdscale(distR)
plot(con, xlab="Dim1",ylab="Dim2",  pch=as.character(group))
abline(h=0,v=0)

# ?À¶???Åº ???? ??????
plotshapes(pongof.dat)

# ?À¶???Åº ???? ??????
plotshapes(pongom.dat)


### R-?Úµ? 2.3.1 ???? ?×·ìº° Ã´?? ?À¿? ???? ?????Å¸??? MDS
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(mice)

## MDS for Reimannian size-and-shape distance of Three Groups of Mices
# Reimannian size-and-shape  distance
distS <- matrix(0, 76, 76)
for (i in 1:76){
  for (j in 1:76){
    rho <- ssriemdist(mice$x[,,i], mice$x[,,j])
    distS[i, j] <- rho
  }
}

# Metrtic MDS with Reimannian size-and-shape distance
par(mfrow=c(1,2))

con <- cmdscale(distS)
plot(con, xlab="Dim1",ylab="Dim2",  pch=as.character(mice$group))
abline(h=0,v=0)
title("(a) Metric MDS")

# Box plot for Centriod size
plot(mice$group, centroid.size(mice$x), xlab="Groups",ylab="Centroid size")
title("(b) Box plot")


### R-?Úµ? 2.3.2 ?À¶???Åº ?Ï¼? ?Î°??? ????Å©??-?????Å¸??? MDS
## ?????? ??Å°?? ?Ò·??À±?
library(MASS)
library(shapes)
data(pongof.dat) # 24 Females
data(pongom.dat) # 30 Males

## MDS for Reimannian size-and-shape distanc of Orang Utan
orang <- array(data=c(pongof.dat,pongom.dat), dim=c(8,2,54)) 
group <- c(rep("f",24), rep("m",30))

#Reimannian size-and-shape distance
distS <- matrix(0, 54, 54)
for (i in 1:54){
  for (j in 1:54){
    rho <- ssriemdist(orang[,,i], orang[,,j])
    distS[i, j] <- rho
  }
}

# Metrtic MDS with Reimannian size-and-shape distance
par(mfrow=c(1,2))

con <- cmdscale(distS)
plot(con, xlab="Dim1",ylab="Dim2",  pch=as.character(group))
abline(h=0,v=0)
title("(a) Metric MDS")

# Box plot for Centriod size
size <- centroid.size(orang)
size <- data.frame(size, group)
boxplot(size~group, data=size, xlab="Groups",ylab="Centroid size")
title("(b) Box plot")


### R-?Úµ? 2.4.1 ??????È£ ???? 3?? ????
## ?????? ??Å°?? ?Ò·??À±?
library(shapes)
data(digit3.dat)
join <- c(1:13)

## Mean shapes for 3 digits
win.graph()
par(mfrow=c(5,6))

# 30 handwritten digit 3
for(i in 1:30){
  plotshapes(digit3.dat[,,i], joinline=join) 
}

# ??Ã¼ ????
plotshapes(digit3.dat, joinline=join)

# Mean shpaes
f1 <- frechet(digit3.dat, mean="intrinsic")# h=1/3 : Reemannian dist
f2 <- frechet(digit3.dat, mean="partial.procrustes")# h=1/2
f3 <- frechet(digit3.dat, mean="full.procrustes")# h=1
f4 <- frechet(digit3.dat, mean=2)

# Joint Mean Shapes
plot(f1$mshape[,1], f1$mshape[,2], xlab="Dim1", ylab="Dim2")
lines(f1$mshape[join,], lty=1)
par(new=TRUE)
plot(f2$mshape[,1], f2$mshape[,2], xlab="Dim1", ylab="Dim2")
lines(f2$mshape[join,], lty=2)
par(new=TRUE)
plot(f3$mshape[,1], f3$mshape[,2], xlab="Dim1", ylab="Dim2")
lines(f3$mshape[join,], lty=3)
par(new=TRUE)
plot(f4$mshape[,1], f4$mshape[,2], xlab="Dim1", ylab="Dim2")
lines(f4$mshape[join,], lty=4)
legend("bottomright", c("intrinsic", "partial.p", "full.p", "h=2"), lty=c(1:4))

# Reimannian distances and RMSD
mshapes <- array(data=c(f1$mshape,f2$mshape,f3$mshape,f4$mshape),
              dim=c(13, 2, 4))
k <- dim(digit3.dat)[1]
distR <- matrix(0,4,4)
for (i in 1:4){
  for (j in 1:4){
    ds <- riemdist(mshapes[,,i], mshapes[,,j])
    distR[i, j] <- ds
  }
}
RMSD <- distR/sqrt(k)
name <- c("Int.(h1/3)", "Partial(h1/2)", "Full(h1)", "h2")
rownames(RMSD) <- name
colnames(RMSD) <- name
round(RMSD, 3)


### R-?Úµ? 2.4.2 ??????È£ ???? 3?? ?????????? RMSD?? ?è·®?? MDS
## ?????? ??Å°?? ?Ò·??À±?
## MDS for RMSD of Mean shapes of 3 digits
library(shapes)
data(digit3.dat)

# Mean shpaes
f0 <- frechet(digit3.dat, mean=0.0001)# h=0.0001
f1 <- frechet(digit3.dat, mean="intrinsic")# h=1/3 : Reemannian dist
f2 <- frechet(digit3.dat, mean="partial.procrustes")# h=1/2
f3 <- frechet(digit3.dat, mean="full.procrustes")# h=1
f4 <- frechet(digit3.dat, mean=2)# h=2
f5 <- frechet(digit3.dat, mean=3)# h=3
f6 <- frechet(digit3.dat, mean=5)# h=5
f7 <- frechet(digit3.dat, mean=10)# h=10
f8 <- frechet(digit3.dat, mean=20)# h=20

#Reimannian distances and RMSD
mshapes <- array(data=c(f0$mshape,f1$mshape,f2$mshape,f3$mshape,f4$mshape,
                     f5$mshape,f6$mshape,f7$mshape,f8$mshape),dim=c(13, 2, 9))
k <- dim(digit3.dat)[1]
distR <- matrix(0,9,9)
for (i in 1:9){
  for (j in 1:9){
    ds <- riemdist(mshapes[,,i], mshapes[,,j])
    distR[i, j] <- ds
  }
}

RMSD <- distR/sqrt(k)
name <- c("h0.001", "Int.(h1/3)", "Partial(h1/2)", "Full(h1)", "h2", "h3", "h5", "h10", "h20")
rownames(RMSD) <- name
colnames(RMSD) <- name
round(RMSD, 3)

# MDS for RMSD
con <- cmdscale(RMSD)
plot(con, xlab="Dim1",ylab="Dim2",  pch=".")
abline(h=0,v=0)
text(con, name)

# Hierarchical Cluster Analysis: Wards linkage
wards <- hclust(as.dist(RMSD), method="ward.D2")
plot(wards, labels=name)


### R-?Úµ? 3.1.1 ?? ?????? ??À¸?? ?? ???? 3?? OPA????
## ?????? ??Å°?? ?Ò·??À±?
## OPA of digit 3
library(shapes)
data(digit3.dat)
digit3.dat %>% head()
k <- dim(digit3.dat)[1]
C <- diag(k) - matrix(1/k, k ,k)

## centred pre-shapes of 1st and 2nd 
X <- digit3.dat[,,2:3]
cA <- C%*%X[,,1] # ???? ???Ï±? ?????? %*%
cB <- C%*%X[,,2]
A <- cA/centroid.size(cA)
B <- cB/centroid.size(cB)

par(mfrow=c(1,2))
plotshapes(A, B,symbol=1, joinline=1:13)

# OPA 
PAB <- procOPA(B, A) #matching A onto B
plotshapes(PAB$Bhat,symbol=1, joinline=1:13)
PBA <- procOPA(A, B) #matching B onto A
plotshapes(PBA$Bhat,symbol=1, joinline=1:13)


### R-?Úµ? 3.1.2 ?í¸±?? ?Ï¼? ?Î°??? ?????? GPA ????
## ?????? ??Å°?? ?Ò·??À±?
## GPA for female and male Gorillas
library(shapes)
data(gorf.dat)
data(gorm.dat)

join=c(1,5,4,3,2,8,7,6,1)
par(mfrow=c(1,2))
plotshapes(gorf.dat, gorm.dat, symbol=1,joinline=join)

# GPA
gorf<-procGPA(gorf.dat)
gorm<-procGPA(gorm.dat)
plotshapes(gorf$mshape, gorm$mshape, symbol=1, joinline=join)


### R-?Úµ? 3.2.1 ?? ?????? ??À¸?? ?? ???? 3?? OPA ???? ?? ?????×¸??? ????
## ?????? ??Å°?? ?Ò·??À±?
## Joint plots after OPA of digit 3
library(shapes)
data(digit3.dat)
k <- dim(digit3.dat)[1]
C <- diag(k) - matrix(1/k, k ,k)

## centred pre-shapes of 1st and 2nd 
X <- digit3.dat[,,2:3]
cA <- C%*%X[,,1]
cB <- C%*%X[,,2]
A <- cA/centroid.size(cA)
B <- cB/centroid.size(cB)

# OPA 
PAB <- procOPA(B, A) #matching A onto B
PBA <- procOPA(A, B) #matching B onto A


# Joint plot of OPA A -> B and B 
lim <- c(-0.5,0.5)
join <- c(1:13)
plot(B[,1],B[,2], xlim=lim,ylim=lim, xlab="Dim1", ylab="Dim2")
lines(B[join,], lty=1)

plot(PAB$Bhat[,1], PAB$Bhat[,2],xlim=lim,ylim=lim, xlab="Dim1", ylab="Dim2")
lines(PAB$Bhat[join,], lty=2)

# Joint plot of OPA B -> A and A
plot(A[,1],A[,2], xlim=lim,ylim=lim, xlab="Dim1", ylab="Dim2")
lines(A[join,], lty=1)

plot(PBA$Bhat[,1], PBA$Bhat[,2],xlim=lim,ylim=lim, xlab="Dim1", ylab="Dim2")
lines(PBA$Bhat[join,], lty=2)


### R-?Úµ? 3.2.2 ?? ?????? ??À¸?? ?? ???? 3?? OPA ????À» À§?? ??????Á¤?? OPSS?? RMSD
## ?????? ??Å°?? ?Ò·??À±?
## Joint plots after OPA of digit 3
## Estimated rotation, angle, scale, OPSS and RMSD in OPA of digit 3
library(shapes)
data(digit3.dat)
k <- dim(digit3.dat)[1]
C <- diag(k) - matrix(1/k, k ,k)

## centred pre-shapes of 1st and 2nd 
X <- digit3.dat[,,2:3]
cA <- C%*%X[,,1]
cB <- C%*%X[,,2]
A <- cA/centroid.size(cA)
B <- cB/centroid.size(cB)

# OPA 
PAB <- procOPA(B, A) #matching A onto B
PBA <- procOPA(A, B) #matching B onto A

# Rotaion matrix: Gamma and GammaR
cbind(PAB$R,PBA$R)

# Rotation angle: theta and thetaR
theta <- atan2(PAB$R[1,2], PAB$R[1,1])*180/pi
thetaR <- atan2(PBA$R[1,2], PBA$R[1,1])*180/pi
cbind(theta,thetaR)  

# Scale : beta and betaR
cbind(PAB$s,PBA$s)

# OPSS and RMSD : OPA A -> B
cbind(PAB$OSS,PAB$rmsd)

# OPSS and RMSD : OPA B -> A 
cbind(PBA$OSS,PBA$rmsd)


### R-?Úµ? 3.2.3 ?? ?????? ??À¸?? ?? ???? 3?? OPA ?Ë°í¸®???? ???? OPSS?? RMSD
## ?????? ??Å°?? ?Ò·??À±?
## OPA Algorithm :  digit 3
library(shapes)
data(digit3.dat)
k <- dim(digit3.dat)[1]
C <- diag(k) - matrix(1/k, k ,k)

## centred pre-shapes of 1st and 2nd 
X <- digit3.dat[,,2:3]
cA <- C%*%X[,,1]
cB <- C%*%X[,,2]
A <- cA/centroid.size(cA)
B <- cB/centroid.size(cB)

svd <- svd(t(A)%*%B) 
gamma <- svd$v%*%t(svd$u)
beta <- sum(diag((t(A)%*%B%*%gamma)))/sum(diag(t(B)%*%B))
OPSS <- sum(diag((t(A)%*%A))) + beta^2%*% sum(diag((t(B)%*%B))) - 2%*%beta%*%sum(diag(svd$d))
RMSD <- sqrt(OPSS/k)
list(gamma, beta, OPSS, RMSD)