# Data Exploration

## Read in the data ####
fish <- read.csv("data/siscowet.csv")
head(fish)

## Which variables have NAs?
TF <- apply(X=fish, MARGIN=2, FUN=is.na)
apply(X=TF, MARGIN=2, FUN=table)

## What is the spread of the data? ####
hist(fish$len)

hist(fish$age)

plot(x=fish$age, 
     y=fish$len,
     pch=19)

plot(x=fish$len,
     y=fish$wgt,
     pch=19)

out <- which(fish$wgt > 5000)

plot(x=fish$len[-out],
     y=fish$wgt[-out],
     pch=19)

age_m <- fish$age[fish$sex=="M"]
age_f <- fish$age[fish$sex=="F"]
len_m <- fish$len[fish$sex=="M"]
len_f <- fish$len[fish$sex=="F"]
wgt_m <- fish$wgt[fish$sex=="M"]
wgt_f <- fish$wgt[fish$sex=="F"]

## How do the age-length curves look by sex? ####
plot(x=age_m,
     y=len_m,
     pch=19,
     col="darkgray")

points(x=age_f,
       y=len_f,
       pch=19,
       col="darkgreen")

## How do the length-weight curves look by sex? ####
plot(x=len_m,
     y=wgt_m,
     pch=19,
     col="darkgray")

points(x=len_f,
       y=wgt_f,
       pch=19,
       col="darkgreen")

## Are there any repeat fish?
any(table(fish$fishID)>1)

## Locations ####
loca <- unique(fish$locID)
colr <- c("darkgray", "darkgreen", "darkblue", "pink")
counter <- 0

for (i in 1:length(loca)) {
  rindex <- which(fish$locID==loca[i])
  age <- fish[rindex,"age"]
  len <- fish[rindex,"len"]
  
  if (all(is.na(age) | is.na(len))) {
    print(paste(loca[i], "did not collect age or len data. Check."))
    
  } else {
    counter <- counter + 1
    
    if (counter==1) {
      plot(x=age,
           y=len,
           pch=19,
           col=colr[i])
      
    } else {
      points(x=age,
             y=len,
             pch=19,
             col=colr[i])
    }
  }
}

legend(x="bottomright",
       legend=loca,
       col=colr,
       pch=19,
       bty="n")

par(mfrow=c(4,2))
for (j in 1:length(loca)) {
  rindex <- which(fish$locID==loca[j])
  age <- fish[rindex,"age"]
  len <- fish[rindex,"len"]
  if (!all(is.na(age))) {
  hist(age, main=loca[j], xlim=c(0,25), ylim=c(0,40))
  hist(len, main=loca[j], xlim=c(0,800), ylim=c(0,150))
  } else {
    plot(0)
    hist(len, main=loca[j], ylim=c(0,150), xlim=c(0,800))
  }
}

## Gear ####
### Fish count by mesh size?
meshbins <- unique(fish$mesh)
hist(fish$mesh)

### Fish count by mesh size by location?
par(mfrow=c(4,1))
for (h in 1:length(loca)) {
  rindex <- which(fish$locID==loca[h])
  mesh <- fish[rindex,"mesh"]
  hist(mesh, main=loca[h], ylim=c(0,100))
}

### Fish count by depth?
hist(fish$pnldep,
     xlim=c(0,120),
     ylim=c(0,300))

### Fish count by depth by location?
par(mfrow=c(4,1))
for (m in 1:length(loca)) {
  rindex <- which(fish$locID==loca[m])
  dep <- fish[rindex,"pnldep"]
  hist(dep, main=loca[m],
       ylim=c(0,120), xlim=c(0,120))
}
