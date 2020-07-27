######################
##
##
#      START CODE ON LINE 85
##
##
######################

setwd("C:/Users/jriver58/Dropbox/Postdoc/Share Emilia/bite force")


#change directory to read in  the files 
setwd("C:/Users/jriver58/Dropbox/Postdoc/Share Emilia/bite force/data/not use")


## this code is to plot the 3 trials to see which one is the best
dat1 <- read.csv("GRA242-1.csv", skip=2)
names(dat1) <- c("time", "force", "na")
dat1 <- dat1[,-3]
dat2 <- read.csv("GRA242-2.csv", skip=2)
names(dat2) <- c("time", "force", "na")
dat2 <- dat2[,-3]
dat3 <- read.csv("GRA242-3.csv", skip=2)
names(dat3) <- c("time", "force", "na")
dat3 <- dat3[,-3]

plot(dat1$time, dat1$force, type="l", col="black", lwd=2, ylim=c(0,5), xlim=c(0,10))
lines(dat2$time, dat2$force, type="l", col="red", lwd=2)
lines(dat3$time, dat3$force, type="l", col="blue", lwd=2)

#############################################
## moved all the best trial to a new folder, need to find the max value for each of the best trials
#############################################

setwd("C:/Users/jriver58/Dropbox/Postdoc/Share Emilia/bite force/data/best bite")

#create a function to read data and get it in the right format
myfun <- function(x){

oo <- read.delim(x, skip=2, sep=",")
names(oo) <- c("time", "force", "na")
oo <- oo[,c(-1,-3)]

                    }

#read in all the data at once
temp <- list.files(pattern="*.csv")
bite <- lapply(temp, myfun)

#get max foce for each individual
bite.force <- lapply(bite, function(x) x[which.max(abs(x))])

bite.force <- cbind(temp, bite.force)
bite.force <- data.frame(bite.force)
names(bite.force) <- c("ID", "N")

#creating a data frame with all the data that is needed to merge with morphology later
foo <- data.frame(do.call('rbind', strsplit(as.character(bite.force$ID),'-',fixed=TRUE)))
bite.force2 <- cbind(foo, bite.force)
bite.force2 <- bite.force2[,-3]
names(bite.force2) <- c("ID", "trial", "force")

foo2 <- data.frame(do.call('rbind', strsplit(as.character(bite.force2$trial),'.',fixed=TRUE)))
bite.force2 <- cbind(bite.force2, foo2[,1])

bite.force3 <- bite.force2[,-c(2,4)]
names(bite.force3) <- c("ID", "force", "trial")

sp <- substr(bite.force3$ID, 1,3)
id <- substr(bite.force3$ID, 4,6)

infor <- cbind(sp,id)
bite.force4 <- cbind(infor, bite.force3)

bite.force4 <- bite.force4[,-3]
bite.force4 <- as.data.frame(bite.force4)
bite.force4$force <- as.numeric(bite.force4$force)


write.csv(bite.force4, file="bite force.csv")

###########################################################################
#
#
#                   START HERE
#
#
###########################################################################

setwd("C:/Users/jriver58/Dropbox/Postdoc/Share Emilia/bite force/data")
setwd("C:/Users/julio/Dropbox/Postdoc/Share Emilia/bite force/data")

forc <- read.csv("bite force.csv")
forc <- forc[,-1]
morph <- read.csv("Sceloporus morph.csv")

dat <- merge(morph, forc, by="ID")
dat <- dat[,c(1:5,7,8,9,10,19:24,41,42)]

plot(dat$force, dat$mass)
plot(dat$force, dat$svl)
plot(dat$force, dat$head.L)
plot(dat$force, dat$head.W)
plot(dat$force, dat$jaw.L)
plot(dat$force, dat$outlever)
plot(dat$force, dat$PL)
plot(dat$force, dat$PW)

par(mfrow=c(3,3))
plot(dat$force, dat$mass, col=dat$species, pch=16)
plot(dat$force, dat$svl, col=dat$species, pch=16)
plot(dat$force, dat$head.L/dat$svl, col=dat$species, pch=16)
plot(dat$force, dat$head.W/dat$svl, col=dat$species, pch=16) ##look more into this
plot(dat$force, dat$jaw.L/dat$svl, col=dat$species, pch=16)
plot(dat$force, dat$outlever/dat$svl, col=dat$species, pch=16)






##let's see if this pattern still holds when you do it by species

dat2 <- subset(dat, dat$sex=="M")
dat3 <- subset(dat2, dat2$age=="A")
dat4 <- na.omit(dat3)

dat4$MA <- dat4$inlever/dat4$outlever

#removing sites where grammicus was in allopatry
dat4 <- dat4[-c(1:22), ]

dat.sp <- split(dat4, dat4$species)

## get means
gra.mean <- colMeans(dat.sp$grammicus[,c(8:18)])
tor.mean <- colMeans(dat.sp$torquatus[,c(8:18)])
spi.mean <- colMeans(dat.sp$spinosus[,c(8:18)])

oo <- rbind(gra.mean, tor.mean)
mean.dat <- rbind(oo, spi.mean)

## calculate standard error
std <- function(x) {
sd(x)/sqrt(length(x))
}

std(dat.sp$grammicus[,c(18)])
##################
#make size corrected data frme

dat4$c.head.L <- dat4$head.L/dat4$svl
dat4$c.head.W <- dat4$head.W/dat4$svl
dat4$c.head.H <- dat4$head.H/dat4$svl
dat4$c.jaw.L <- dat4$jaw.L/dat4$svl
dat4$c.outlever <- dat4$outlever/dat4$svl
dat4$c.inlever <- dat4$inlever/dat4$svl
dat4$c.force <- dat4$force/dat4$head.L


### are morphologies different between species?


size.dat4 <- dat4[,c(1,3,4,6,7,8,9,16,18,19:25)]


aov.headL <- aov(size.dat4$c.head.L ~ size.dat4$species)
summary(aov.headL)
TukeyHSD(aov.headL)
plot(size.dat4$species, size.dat4$c.head.L) #spinosus has shorter head

aov.headW <- aov(size.dat4$c.head.W ~ size.dat4$species)
summary(aov.headW)
TukeyHSD(aov.headW)
plot(size.dat4$species, size.dat4$c.head.W) #torquatus has wider head

aov.headH <- aov(size.dat4$c.head.H ~ size.dat4$species)
summary(aov.headH)
TukeyHSD(aov.headH)
plot(size.dat4$species, size.dat4$c.head.H) #grammicus has shallow head

aov.jawL <- aov(size.dat4$c.jaw.L ~ size.dat4$species)
summary(aov.jawL)
TukeyHSD(aov.jawL)
plot(size.dat4$species, size.dat4$c.jaw.L) #torqutus > gramm > spinosus

aov.outlever <- aov(size.dat4$c.outlever ~ size.dat4$species)
summary(aov.outlever)
TukeyHSD(aov.outlever)
plot(size.dat4$species, size.dat4$c.outlever) #torqutus > gramm > spinosus

aov.inlever <- aov(size.dat4$c.inlever ~ size.dat4$species) 
summary(aov.inlever)
TukeyHSD(aov.inlever)
plot(size.dat4$species, size.dat4$c.inlever) #spi and toe > gram



aov.MA <- aov(size.dat4$MA ~ size.dat4$species)
summary(aov.MA)
TukeyHSD(aov.MA)
plot(size.dat4$species, size.dat4$MA) #spi > tor > gram




##### is bite force different between species and when you correct for size 

force.aov <- aov(size.dat4$force ~ size.dat4$species)
summary(force.aov)
TukeyHSD(force.aov)

c.force.aov <- aov(size.dat4$c.force ~ size.dat4$species)
summary(c.force.aov)
TukeyHSD(c.force.aov)

par(mfrow=c(2,3))
plot(size.dat4$species, size.dat4$c.head.L, xlab="Species", ylab="size-corrected head length", ylim=c(0.16,0.28))
text(x=c(1,3), y=0.27, labels="a")
text(x=2, y=0.27, labels="b")
plot(size.dat4$species, size.dat4$c.head.W, xlab="Species", ylab="size-corrected head width", ylim=c(0.14,0.27))
text(x=c(1,2), y=0.26, labels="a")
text(x=3, y=0.26, labels="b")
plot(size.dat4$species, size.dat4$c.head.H, xlab="Species", ylab="size-corrected head height", ylim=c(0.08,0.17))
text(x=1, y=0.16, labels="a")
text(x=c(2,3), y=0.16, labels="b")
plot(size.dat4$species, size.dat4$c.jaw.L, xlab="Species", ylab="size-corrected jaw length", ylim=c(0.16,0.31))
text(x=1, y=0.29, labels="a")
text(x=2, y=0.29, labels="b")
text(x=3, y=0.29, labels="c")
plot(size.dat4$species, size.dat4$c.inlever, xlab="Species", ylab="size-corrected inlever", ylim=c(0.05,0.18))
text(x=1, y=0.16, labels="a")
text(x=2, y=0.16, labels="b")
text(x=3, y=0.16, labels="b")
plot(size.dat4$species, size.dat4$c.outlever, xlab="Species", ylab="size-corrected outlever", ylim=c(0.15,0.27))
text(x=1, y=0.26, labels="a")
text(x=2, y=0.26, labels="b")
text(x=3, y=0.26, labels="c")



#### what morphologies correlated with wbite force - one for size, morph
require(MuMIn)

################ GRAMMICUS ######################
lm.morph.gra1 <- lm(force ~ head.L, data=dat.sp$grammicus)
lm.morph.gra2 <- lm(force ~ head.W, data=dat.sp$grammicus)
lm.morph.gra3 <- lm(force ~ head.H, data=dat.sp$grammicus)
lm.morph.gra4 <- lm(force ~ jaw.L, data=dat.sp$grammicus)
lm.morph.gra5 <- lm(force ~ head.L + head.W, data=dat.sp$grammicus)		
lm.morph.gra6 <- lm(force ~ head.L + head.W + head.H, data=dat.sp$grammicus)	
lm.morph.gra7 <- lm(force ~ head.L + head.W + head.H + jaw.L, data=dat.sp$grammicus)

#leave head L	
lm.morph.gra8 <- lm(force ~ head.L + head.H, data=dat.sp$grammicus)
lm.morph.gra9 <- lm(force ~ head.L + jaw.L, data=dat.sp$grammicus)
lm.morph.gra10 <- lm(force ~ head.L + head.W + jaw.L, data=dat.sp$grammicus)
lm.morph.gra11 <- lm(force ~ head.L + head.H + jaw.L, data=dat.sp$grammicus)
		  
#leave head W			  
lm.morph.gra12 <- lm(force ~ head.W + head.H, data=dat.sp$grammicus)	
lm.morph.gra13 <- lm(force ~ head.W + jaw.L, data=dat.sp$grammicus)
lm.morph.gra14 <- lm(force ~ head.W +  head.H + jaw.L, data=dat.sp$grammicus)
    			  
#leave head H		  
lm.morph.gra15 <- lm(force ~ head.H + jaw.L, data=dat.sp$grammicus)

#intercept only model
lm.morph.gra16 <- lm(force ~ 1, data=dat.sp$grammicus)


x <- c("lm.morph.gra1", "lm.morph.gra2", "lm.morph.gra3", "lm.morph.gra4", "lm.morph.gra5", "lm.morph.gra6",
"lm.morph.gra7", "lm.morph.gra8", "lm.morph.gra9", "lm.morph.gra10", "lm.morph.gra11", "lm.morph.gra12",
"lm.morph.gra13", "lm.morph.gra14", "lm.morph.gra15", "lm.morph.gra16")


y <- c(AICc(lm.morph.gra1), AICc(lm.morph.gra2), AICc(lm.morph.gra3), AICc(lm.morph.gra4), AICc(lm.morph.gra5)
,AICc(lm.morph.gra6), AICc(lm.morph.gra7), AICc(lm.morph.gra8), AICc(lm.morph.gra9), AICc(lm.morph.gra10)
,AICc(lm.morph.gra11), AICc(lm.morph.gra12), AICc(lm.morph.gra13), AICc(lm.morph.gra14), AICc(lm.morph.gra15)
, AICc(lm.morph.gra16))
	
gra.models <- cbind(as.character(x), as.numeric(y))
gra.models <- as.data.frame(gra.models)
names(gra.models) <- c("model", "AICc")
gra.models$AICc <- as.numeric(as.character(gra.models$AICc))


write.csv(gra.models, file="graAICc.csv")

gra.models2 <- gra.models[order(gra.models$AICc),]

### weighing the models and plotting only important variables

modelsgra <- list(lm.morph.gra1, lm.morph.gra2, lm.morph.gra3, lm.morph.gra4, lm.morph.gra5, lm.morph.gra6,
lm.morph.gra7, lm.morph.gra8, lm.morph.gra9, lm.morph.gra10, lm.morph.gra11, lm.morph.gra12,
lm.morph.gra13, lm.morph.gra14, lm.morph.gra15, lm.morph.gra16)

model.mean.Gram <- model.avg(modelsgra)
gra.summ <- summary(model.mean.Gram)
write.csv(gra.summ[[1]], file="graweights.csv")

mean(dat.sp$grammicus$head.L)
mean(dat.sp$grammicus$head.H)
mean(dat.sp$grammicus$head.W)
mean(dat.sp$grammicus$jaw.L)


#new interccept, no need to include the thing that you are plotting
new.inter.gra <- -9.273129 + ( mean(dat.sp$grammicus$head.L) * 0.028448) +  (mean(dat.sp$grammicus$head.W) * 0.217976 ) + (mean(dat.sp$grammicus$jaw.L) * -0.004041) 

plot(dat.sp$grammicus$head.H, dat.sp$grammicus$force, pch=16, xlab="Head height (mm)", ylab="Force (N)", main="S. grammicus")
abline(a=new.inter.gra, b=1.390582, lw=3)

########### TORQUATUS ##################
lm.morph.tor1 <- lm(force ~ head.L, data=dat.sp$torquatus)
lm.morph.tor2 <- lm(force ~ head.W, data=dat.sp$torquatus)
lm.morph.tor3 <- lm(force ~ head.H, data=dat.sp$torquatus)
lm.morph.tor4 <- lm(force ~ jaw.L, data=dat.sp$torquatus)
lm.morph.tor5 <- lm(force ~ head.L + head.W, data=dat.sp$torquatus)
lm.morph.tor6 <- lm(force ~ head.L + head.W + head.H, data=dat.sp$torquatus)
lm.morph.tor7 <- lm(force ~ head.L + head.W + head.H + jaw.L, data=dat.sp$torquatus)

#leave head L	
lm.morph.tor8 <- lm(force ~ head.L + head.H, data=dat.sp$torquatus)
lm.morph.tor9 <- lm(force ~ head.L + jaw.L, data=dat.sp$torquatus)
lm.morph.tor10 <- lm(force ~ head.L + head.W + jaw.L, data=dat.sp$torquatus)
lm.morph.tor11 <- lm(force ~ head.L + head.H + jaw.L, data=dat.sp$torquatus)
			  
#leave head W			  
lm.morph.tor12 <- lm(force ~ head.W + head.H, data=dat.sp$torquatus)	
lm.morph.tor13 <- lm(force ~ head.W + jaw.L, data=dat.sp$torquatus)
lm.morph.tor14 <- lm(force ~ head.W +  head.H + jaw.L, data=dat.sp$torquatus)
			  
#leave head H		  
lm.morph.tor15 <- lm(force ~ head.H + jaw.L, data=dat.sp$torquatus)

#intercept only model
lm.morph.tor16 <- lm(force ~ 1, data=dat.sp$torquatus)



x <- c("lm.morph.tor1", "lm.morph.tor2", "lm.morph.tor3", "lm.morph.tor4", "lm.morph.tor5", "lm.morph.tor6",
"lm.morph.tor7", "lm.morph.tor8", "lm.morph.tor9", "lm.morph.tor10", "lm.morph.tor11", "lm.morph.tor12",
"lm.morph.tor13", "lm.morph.tor14", "lm.morph.tor15", "lm.morph.tor16")


y <- c(AICc(lm.morph.tor1), AICc(lm.morph.tor2), AICc(lm.morph.tor3), AICc(lm.morph.tor4), AICc(lm.morph.tor5)
,AICc(lm.morph.tor6), AICc(lm.morph.tor7), AICc(lm.morph.tor8), AICc(lm.morph.tor9), AICc(lm.morph.tor10)
,AICc(lm.morph.tor11), AICc(lm.morph.tor12), AICc(lm.morph.tor13), AICc(lm.morph.tor14), AICc(lm.morph.tor15)
, AICc(lm.morph.tor16))	


tor.models <- cbind(as.character(x), as.numeric(y))
tor.models <- as.data.frame(tor.models)
names(tor.models) <- c("model", "AICc")
tor.models$AICc <- as.numeric(as.character(tor.models$AICc))

write.csv(tor.models, file="torAICc.csv")

tor.models2 <- tor.models[order(tor.models$AICc),]


### weighing the models and plotting only important variables

modelstor <- list(lm.morph.tor1, lm.morph.tor2, lm.morph.tor3, lm.morph.tor4, lm.morph.tor5, lm.morph.tor6,
lm.morph.tor7, lm.morph.tor8, lm.morph.tor9, lm.morph.tor10, lm.morph.tor11, lm.morph.tor12,
lm.morph.tor13, lm.morph.tor14, lm.morph.tor15, lm.morph.tor16)

model.mean.tor <- model.avg(modelstor)

tor.summ <- summary(model.mean.tor)
write.csv(tor.summ[[1]], file="torweights.csv")


#no trait has a strog influence so not plotting things.

######################## SPINOSUS ###################
lm.morph.spi1 <- lm(force ~ head.L, data=dat.sp$spinosus)
lm.morph.spi2 <- lm(force ~ head.W, data=dat.sp$spinosus)
lm.morph.spi3 <- lm(force ~ head.H, data=dat.sp$spinosus)
lm.morph.spi4 <- lm(force ~ jaw.L, data=dat.sp$spinosus)
lm.morph.spi5 <- lm(force ~ head.L + head.W, data=dat.sp$spinosus)				
lm.morph.spi6 <- lm(force ~ head.L + head.W + head.H, data=dat.sp$spinosus)
lm.morph.spi7 <- lm(force ~ head.L + head.W + head.H + jaw.L, data=dat.sp$spinosus)

#leave head L	
lm.morph.spi8 <- lm(force ~ head.L + head.H, data=dat.sp$spinosus)
lm.morph.spi9 <- lm(force ~ head.L + jaw.L, data=dat.sp$spinosus)
lm.morph.spi10 <- lm(force ~ head.L + head.W + jaw.L, data=dat.sp$spinosus)
lm.morph.spi11 <- lm(force ~ head.L + head.H + jaw.L, data=dat.sp$spinosus)
			  
#leave head W			  
lm.morph.spi12 <- lm(force ~ head.W + head.H, data=dat.sp$spinosus)	
lm.morph.spi13 <- lm(force ~ head.W + jaw.L, data=dat.sp$spinosus)
lm.morph.spi14 <- lm(force ~ head.W +  head.H + jaw.L, data=dat.sp$spinosus)
    		  
#leave head H		  
lm.morph.spi15 <- lm(force ~ head.H + jaw.L, data=dat.sp$spinosus)

#intercept only model
lm.morph.spi16 <- lm(force ~ 1, data=dat.sp$spinosus)

x <- c("lm.morph.spi1", "lm.morph.spi2", "lm.morph.spi3", "lm.morph.spi4", "lm.morph.spi5", "lm.morph.spi6",
"lm.morph.spi7", "lm.morph.spi8", "lm.morph.spi9", "lm.morph.spi10", "lm.morph.spi11", "lm.morph.spi12",
"lm.morph.spi13", "lm.morph.spi14", "lm.morph.spi15", "lm.morph.spi16")


y <- c(AICc(lm.morph.spi1), AICc(lm.morph.spi2), AICc(lm.morph.spi3), AICc(lm.morph.spi4), AICc(lm.morph.spi5)
,AICc(lm.morph.spi6), AICc(lm.morph.spi7), AICc(lm.morph.spi8), AICc(lm.morph.spi9), AICc(lm.morph.spi10)
,AICc(lm.morph.spi11), AICc(lm.morph.spi12), AICc(lm.morph.spi13), AICc(lm.morph.spi14), AICc(lm.morph.spi15)
, AICc(lm.morph.spi16))

	
spi.models <- cbind(as.character(x), as.numeric(y))
spi.models <- as.data.frame(spi.models)
names(spi.models) <- c("model", "AICc")
spi.models$AICc <- as.numeric(as.character(spi.models$AICc))

write.csv(spi.models, file="spiAICc.csv")

spi.models2 <- spi.models[order(spi.models$AICc),]

### weighing the models and plotting only important variables

modelsspi <- list(lm.morph.spi1, lm.morph.spi2, lm.morph.spi3, lm.morph.spi4, lm.morph.spi5, lm.morph.spi6,
lm.morph.spi7, lm.morph.spi8, lm.morph.spi9, lm.morph.spi10, lm.morph.spi11, lm.morph.spi12,
lm.morph.spi13, lm.morph.spi14, lm.morph.spi15, lm.morph.spi16)

model.mean.spi <- model.avg(modelsspi)
spi.summ <- summary(model.mean.spi)

write.csv(spi.summ[[1]], file="spiweights.csv")

#to get the new regression you have to average the intercept from all other traits because the slope is an average of all the traits
new.inter.spi <- -34.72991 + (mean(dat.sp$spinosus$head.L) * 0.34748) +  (mean(dat.sp$spinosus$head.W) * -0.05916) + (mean(dat.sp$spinosus$jaw.L) * 0.12630) 

plot(dat.sp$spinosus$head.H, dat.sp$spinosus$force, pch=16, xlab="Head height (mm)", ylab="Force (N)", main="S. spinosus")
abline(a=new.inter.spi, b=2.88730, lw=3)


##################################
### plot for paper

par(mfrow=c(1,3))
plot(size.dat4$species, size.dat4$c.force, xlab="Species", ylab="Size-corrected bite force", main="Corrected bite force")
text(x=1, y=1.05, labels="a")
text(x=c(2,3), y=1.05, labels="b")

plot(dat.sp$grammicus$head.H, dat.sp$grammicus$force, pch=16, xlab="Head height (mm)", ylab="Force (N)", main="S. grammicus")
abline(a=new.inter.gra, b=1.390582, lw=3)

plot(dat.sp$spinosus$head.H, dat.sp$spinosus$force, pch=16, xlab="Head height (mm)", ylab="Force (N)", main="S. spinosus")
abline(a=new.inter.spi, b=2.88730, lw=3)

########################
######## get means
########################

dat.sp.size <- split(size.dat4, size.dat4$species)
gra.mean <- colMeans(dat.sp.size$grammicus[,6:16])
tor.mean <- colMeans(dat.sp.size$torquatus[,6:16])
spi.mean <- colMeans(dat.sp.size$spinosus[,6:16])

oo <- rbind(gra.mean, tor.mean)
mean.dat <- rbind(oo, spi.mean)




##################################################
##################################################
#canonical correlation analysis
##################################################
##################################################

require(ggplot2)
require(GGally)
require(CCA)
require(CCP)
require(candisc)

morph.gra <- dat.sp$grammicus[c("head.L", "head.W", "head.H", "jaw.L")]
per.gra <- dat.sp$grammicus[c("force")]
names(per.gra)[1] <- "bite"
pairs(morph.gra)
cc1 <- cc(morph.gra, per.gra)
cc2 <- comput(morph.gra, per.gra, cc1)

s1 <- diag(sqrt(diag(cov(morph.gra))))
s1 %*% cc1$xcoef
cc1$ycoef

rho <- cc1$cor
n <- dim(morph.gra)
p <- length(morph.gra)[1]
q <- length(per.gra)
p.asym(rho, n, p, q, tstat="Wilks")

matcor(morph.gra, per.gra)


#in spinosus, bite force is correlated with headW and headH and negatively correlated with PL and head L/dat

morph.spi <- dat.sp$spinosus[c("head.L", "head.W", "head.H", "jaw.L")]
per.spi <- dat.sp$spinosus[c("force")]
morph.spi <- as.matrix(morph.spi)
per.spi <- as.matrix(per.spi)
names(per.spi)[1] <- "bite"
pairs(morph.spi)
matcor(morph.spi, per.spi)
cc1 <- cc(morph.spi, per.spi)
cc2 <- comput(morph.spi, per.spi, cc1)

s1 <- diag(sqrt(diag(cov(morph.spi))))
s1 %*% cc1$xcoef
cc1$ycoef

rho <- cc1$cor
n <- dim(morph.spi)
p <- length(morph.spi)[1]
q <- length(per.spi)
p.asym(rho, n, p, q, tstat="Wilks")

matcor(morph.spi, per.spi)

##plotting 


# in torquatus, bite foce is negatively correlated with head L, head W, and jaw L

morph.tor <- dat.sp$torquatus[c("head.L", "head.W", "head.H", "jaw.L")]
per.tor <- dat.sp$torquatus[c("force")]
names(per.tor)[1] <- "bite"
pairs(morph.tor)
matcor(morph.tor, per.tor)
cc1 <- cc(morph.tor, per.tor)
cc2 <- comput(morph.tor, per.tor, cc1)

s1 <- diag(sqrt(diag(cov(morph.tor))))
s1 %*% cc1$xcoef
cc1$ycoef

rho <- cc1$cor
n <- dim(morph.tor)
p <- length(morph.tor)[1]
q <- length(per.tor)
p.asym(rho, n, p, q, tstat="Wilks")

matcor(morph.tor, per.tor)


#######################
#### Does patch size predict bite force 

aov.PL <- aov(size.dat4$c.PL ~ size.dat4$species)
summary(aov.PL)
TukeyHSD(aov.PL)
plot(size.dat4$species, size.dat4$c.PL) #no difference in patch length

aov.PW <- aov(size.dat4$c.PW ~ size.dat4$species)
summary(aov.PW)
TukeyHSD(aov.PW)
plot(size.dat4$species, size.dat4$c.PW) #tor wide patches

glm.p.gra <- glm(force ~ PW + PL, data=dat.sp$grammicus)		 
summary(glm.p.gra)
glm.p.gra2 <- glm(force ~ PW, data=dat.sp$grammicus)		 
summary(glm.p.gra2)


glm.p.spi <- glm(force ~ PW + PL, data=dat.sp$spinosus)		 
summary(glm.p.spi)


glm.p.tor <- glm(force ~ PW + PL, data=dat.sp$torquatus)		 
summary(glm.p.tor)
glm.p.tor2 <- glm(force ~ PL, data=dat.sp$torquatus)		 
summary(glm.p.tor2)

############### FOR PAPER ###############

par(mfrow=c(3,2))

plot(dat.sp.size$grammicus$c.PL, dat.sp.size$grammicus$force, pch=16, xlab="Size-corrected patch length", ylab="Bite force (N)", main="S. grammicus")
PL.lm <- lm(dat.sp.size$grammicus$force ~ dat.sp.size$grammicus$c.PL)
summary(PL.lm)
abline(lm(dat.sp.size$grammicus$force ~ dat.sp.size$grammicus$c.PL), lwd=2)
plot(dat.sp.size$grammicus$c.PW, dat.sp.size$grammicus$force, pch=16, col="red", xlab="Size-corrected patch width", ylab="Bite force (N)")
PW.lm <- lm(dat.sp.size$grammicus$force ~ dat.sp.size$grammicus$c.PW)
summary(PW.lm)
abline(lm(dat.sp.size$grammicus$force ~ dat.sp.size$grammicus$c.PW), lwd=2, col="red")


plot(dat.sp.size$spinosus$c.PL, dat.sp.size$spinosus$force, pch=16, xlab="Size-corrected patch length", ylab="Bite force (N)", main="S. spinosus")
PL.lm <- lm(dat.sp.size$spinosus$force ~ dat.sp.size$spinosus$c.PL)
summary(PL.lm)
abline(lm(dat.sp.size$spinosus$force ~ dat.sp.size$spinosus$c.PL), lwd=2)
plot(dat.sp.size$spinosus$c.PW, dat.sp.size$spinosus$force, pch=16, col="red", xlab="Size-corrected patch width", ylab="Bite force (N)")
PW.lm <- lm(dat.sp.size$spinosus$force ~ dat.sp.size$spinosus$c.PW)
summary(PW.lm)
abline(lm(dat.sp.size$spinosus$force ~ dat.sp.size$spinosus$c.PW), lwd=2, col="red")



plot(dat.sp.size$torquatus$c.PL, dat.sp.size$torquatus$force, pch=16, xlab="Size-corrected patch length", ylab="Bite force (N)", main="S. torquatus")
PL.lm <- lm(dat.sp.size$torquatus$force ~ dat.sp.size$torquatus$c.PL)
summary(PL.lm)
abline(lm(dat.sp.size$torquatus$force ~ dat.sp.size$torquatus$c.PL), lwd=2)
plot(dat.sp.size$torquatus$c.PW, dat.sp.size$torquatus$force, pch=16, col="red", xlab="Size-corrected patch width", ylab="Bite force (N)")
PW.lm <- lm(dat.sp.size$torquatus$force ~ dat.sp.size$torquatus$c.PW)
summary(PW.lm)
abline(lm(dat.sp.size$torquatus$force ~ dat.sp.size$torquatus$c.PW), lwd=2, col="red")








