#MB2Finalproject Wintersemester 21/22 Suriya Elango
#Analyzing Light pollution with effect woth city population 2013-2018


install.packages('raster')
install.packages('rgdal')
library(raster)
library(rgdal)
library(sp)

setwd ("C:/R/MB2Final/Lightpollution")

## read images and set min and max vals and minus values to 0

r13 = raster('bangalore_2013.tif')
r13 = setMinMax(r13)
r13 = reclassify(r13, cbind(-Inf, 0, 0), right=FALSE)
r14 = raster('bangalore_2014.tif')
r14 = setMinMax(r14)
r14 = reclassify(r14, cbind(-Inf, 0, 0), right=FALSE)
r15 = raster('bangalore_2015.tif')
r15 = setMinMax(r15)
r15 = reclassify(r15, cbind(-Inf, 0, 0), right=FALSE)
r17 = raster('bangalore_2017.tif')
r17 = setMinMax(r17)
r17 = reclassify(r17, cbind(-Inf, 0, 0), right=FALSE)
r18 = raster('bangalore_2018.tif')
r18 = setMinMax(r18)
r18 = reclassify(r18, cbind(-Inf, 0, 0), right=FALSE)
s = stack(r13,r14,r15,r17,r18)
plot(s, col=grey(1:100/100))

range(r14[])
r13
##bortle scale reclassification
reclass_df = c(0, 0.15, 1,
              0.15, 0.25, 2,
              0.25, 0.5, 3,
              0.5, 1.5, 4,
              1.5, 10, 5,
              10, 50, 7,
              50, 75, 8,
              75, Inf, 9)
reclass_r = matrix(reclass_df, ncol = 3, byrow = TRUE)
reclass_r

install.packages('RColorBrewer')
library(RColorBrewer)

r13_classified = reclassify(r13, reclass_r)
r14_classified = reclassify(r14, reclass_r)
r15_classified = reclassify(r15, reclass_r)
r17_classified = reclassify(r17, reclass_r)
r18_classified = reclassify(r18, reclass_r)
par(mfrow=c(2,3))
plot(r13_classified, col=grey(1:100/100), main = 'Bangalore Bortle Scale Classification 2013')
plot(r14_classified, col=grey(1:100/100), main = 'Bangalore Bortle Scale Classification 2014')
plot(r15_classified, col=grey(1:100/100), main = 'Bangalore Bortle Scale Classification 2015')
plot(r17_classified, col=grey(1:100/100), main = 'Bangalore Bortle Scale Classification 2017')
plot(r18_classified, col=grey(1:100/100), main = 'Bangalore Bortle Scale Classification 2018')

## bortle scale percentage
sum(r13[] <= 0.15) / ncell(r13)
sum(0.15 < r13[] & r13[] < 0.25) / ncell(r13)
sum(0.25 < r13[] & r13[] < 0.5) / ncell(r13)
sum(0.5 < r13[] & r13[] < 1.5) / ncell(r13)
sum(1.5 < r13[] & r13[] < 10) / ncell(r13)
sum(10 < r13[] & r13[] < 50) / ncell(r13)
sum(50 < r13[] & r13[] < 75) / ncell(r13)
sum(r13[] >= 75) / ncell(r13)

sum(r14[] <= 0.15) / ncell(r14)
sum(0.15 < r14[] & r14[] < 0.25) / ncell(r14)
sum(0.25 < r14[] & r14[] < 0.5) / ncell(r14)
sum(0.5 < r14[] & r14[] < 1.5) / ncell(r14)
sum(1.5 < r14[] & r14[] < 10) / ncell(r14)
sum(10 < r14[] & r14[] < 50) / ncell(r14)
sum(50 < r14[] & r14[] < 75) / ncell(r14)
sum(r14[] >= 75) / ncell(r14)

sum(r15[] <= 0.15) / ncell(r15)
sum(0.15 < r15[] & r15[] < 0.25) / ncell(r15)
sum(0.25 < r15[] & r15[] < 0.5) / ncell(r15)
sum(0.5 < r15[] & r15[] < 1.5) / ncell(r15)
sum(1.5 < r15[] & r15[] < 10) / ncell(r15)
sum(10 < r15[] & r15[] < 50) / ncell(r15)
sum(50 < r15[] & r15[] < 75) / ncell(r15)
sum(r15[] >= 75) / ncell(r15)

sum(r17[] <= 0.15) / ncell(r17)
sum(0.15 < r17[] & r17[] < 0.25) / ncell(r17)
sum(0.25 < r17[] & r17[] < 0.5) / ncell(r17)
sum(0.5 < r17[] & r17[] < 1.5) / ncell(r17)
sum(1.5 < r17[] & r17[] < 10) / ncell(r17)
sum(10 < r17[] & r17[] < 50) / ncell(r17)
sum(50 < r17[] & r17[] < 75) / ncell(r17)
sum(r17[] >= 75) / ncell(r17)

sum(r18[] <= 0.15) / ncell(r18)
sum(0.15 < r18[] & r18[] < 0.25) / ncell(r18)
sum(0.25 < r18[] & r18[] < 0.5) / ncell(r18)
sum(0.5 < r18[] & r18[] < 1.5) / ncell(r18)
sum(1.5 < r18[] & r18[] < 10) / ncell(r18)
sum(10 < r18[] & r18[] < 50) / ncell(r18)
sum(50 < r18[] & r18[] < 75) / ncell(r18)
sum(r18[] >= 75) / ncell(r18)



## distribution of pixel vals
par(mfrow=c(2,3))
plot(r13[], type = 'l', col = 'blue', 
     xlab = 'Distribution of Pixel Values', ylab = 'Radiance Values', main = 'Bangalore 2013',
     xaxt='n')
axis(1, at = seq(0, 458834, by = 10000))

plot(r14[], type = 'l', col = 'red', 
     xlab = 'Distribution of Pixel Values', ylab = 'Radiance Values', main = 'Bangalore 2014',
     xaxt='n')
axis(1, at = seq(0, 458834, by = 10000))
plot(r15[], type = 'l', col = 'purple', 
     xlab = 'Distribution of Pixel Values', ylab = 'Radiance Values', main = 'Bangalore 2015',
     xaxt='n')
axis(1, at = seq(0, 458834, by = 10000))
plot(r17[], type = 'l', col = 'green', 
     xlab = 'Distribution of Pixel Values', ylab = 'Radiance Values', main = 'Bangalore 2017',
     xaxt='n')
axis(1, at = seq(0, 458834, by = 10000))
plot(r18[], type = 'l', col = 'orange', 
     xlab = 'Distribution of Pixel Values', ylab = 'Radiance Values', main = 'Bangalore 2018',
     xaxt='n')
axis(1, at = seq(0, 458834, by = 10000))


## plot differences between 16 and 15
## iiregular values in 2016
rindia16 = raster("2016_india.tif")
rindia15 = raster("2015_india.tif")
s16 = sd(as.matrix(rindia16))
m16 = mean(as.matrix(rindia16))
smp16 = m16+(s16*2)
smn16 = m16-(s16*2)
br16 = seq(smn16, smp16)
plot(rindia16, breaks = br16, col=grey(1:100/100), main= 'India Light Radiance Values 2016')

s15 = sd(as.matrix(rindia15))
m15 = mean(as.matrix(rindia15))
smp15 = m15+(s15*2)
smn15 = m15-(s15*2)
br15 = seq(smn15, smp15)
plot(rindia15, breaks = br15, col=grey(1:100/100), main= 'India Light Radiance Values 2015')

# stack
s = stack(r13,r14,r15,r17,r18)
plot(s, col=grey(1:100/100))

## statistics - check trends
## years vector
years = c("2013-01-01", "2014-01-01", "2015-01-01", "2017-01-01", "2018-01-01")
years = as.Date(years, format="%Y-%m-%d")
class(years)
#mean val
m13 = mean(r13[])
m14 = mean(r14[])
m15 = mean(r15[])
m17 = mean(r17[])
m18 = mean(r18[])
#sd val
sd13 = sd(r13[])
sd14 = sd(r14[])
sd15 = sd(r15[])
sd17 = sd(r17[])
sd18 = sd(r18[])
#max val
ma13 = max(r13[])
ma14 = max(r14[])
ma15 = max(r15[])
ma17 = max(r17[])
ma18 = max(r18[])
#median val
med13 = median(r13[])
med14 = median(r14[])
med15 = median(r15[])
med17 = median(r17[])
med18 = median(r18[])
#sum val
su13 = sum(r13[])
su14 = sum(r14[])
su15 = sum(r15[])
su17 = sum(r17[])
su18 = sum(r18[])

m = c(0 , su14-su13, su15-su13, su15-su14, su17-su13, su17-su14, su17-su15, su18-su13, su18-su14, su18-su15, su18-su17, su18-su13)
m
matrix(m, nrow = 5, ncol = 5, dimnames = years)

## create trend data.frame

tr = data.frame(RadianceMean = c(m13, m14, m15, m17, m18), 
                RadianceSd = c(sd13, sd14, sd15, sd17, sd18), 
                RadianceMax = c(ma13,ma14,ma15,ma17,ma18),
                RadianceMed = c(med13,med14,med15,med17,med18),
                RadianceSum = c(su13,su14,su15,su17,su18),
                Date = years)

tr

install.packages('ggplot2')
library(ggplot2)

install.packages('cowplot')
library(cowplot)

## plot stats

pl1 = ggplot(tr, aes(years, RadianceMean)) +
  geom_line(color='blue', size=1) +
  labs(ylab = 'Mean', title = 'Stats of Radiance Values between 2013-2018 (w/o 2016)')+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pl3 = ggplot(tr, aes(years, RadianceMax)) +
  geom_line(color='green', size=1) +
  ylab('Max')+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
  
pl4 = ggplot(tr, aes(years, RadianceSum)) +
geom_line(color='purple', size=1) +
  ylab('Sum') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pl5 = ggplot(tr, aes(years, RadianceSd)) +
geom_line(color='orange', size=1) +
  labs(ylab='Standard Deviation', xlab='Years')

tr$Date = c("2013-01-01", "2014-01-01", "2015-01-01", "2017-01-01", "2018-01-01")
plot(pl1)
plot_grid(pl1, pl4, pl5, align = 'v', ncol = 1)


## raster algebra - diff between every 2 years
ex = extent(r13)
dim(r13)
crs = crs(r13)
r13[]
s1413 = r14[]-r13[]
s1413 = matrix(s1413, nrow=599, ncol=766, byrow = TRUE)
s1413 = raster(s1413)
s1413@crs = crs
s1413@extent = ex
ms1413 = mean(s1413[])
sum(s1413[] > 0)/ ncell(s1413)
sum(s1413[] == 0 ) / ncell(s1413)
sum(s1413[] < 0 ) / ncell(s1413)

## classify between the more sig differences

s1514 = r15[]-r14[]
s1514 = matrix(s1514, nrow=599, ncol=766, byrow = TRUE)
s1514 = raster(s1514)
s1514@crs = crs
s1514@extent = ex
sum(s1514[] > 0)/ ncell(s1514)
sum(s1514[] == 0 ) / ncell(s1514)

sum(s1514[] <= 0 ) / ncell(s1514)


s1715 = r17[]-r15[]
s1715 = matrix(s1715, nrow=599, ncol=766, byrow = TRUE)
s1715 = raster(s1715)
s1715@crs = crs
s1715@extent = ex
sum(s1715[] > 0)/ ncell(s1715)
sum(s1715[] == 0 ) / ncell(s1715)

sum(s1715[] <= 0 ) / ncell(s1715)



s1817 = r18[]-r17[]
s1817 = matrix(s1817, nrow=599, ncol=766, byrow = TRUE)
s1817 = raster(s1817)
s1817@crs = crs
s1817@extent = ex
sum(s1817[] > 0)/ ncell(s1817)
sum(s1817[] == 0 ) / ncell(s1817)
sum(s1817[] <= 0 ) / ncell(s1817)



s1813 = r18[]-r13[]
s1813 = matrix(s1813, nrow=599, ncol=766, byrow = TRUE)
s1813 = raster(s1813)
s1813@crs = crs
s1813@extent = ex
sum(s1813[] > 0)/ ncell(s1813)
sum(s1813[] == 0 ) / ncell(s1813)
sum(s1813[] <= 0 ) / ncell(s1813)



## plots of raster algebra

s1413rec = s1413
s1413rec[s1413 < 0] = 0
s1413rec[s1413 == 0] = 1
s1413rec[s1413 > 0] = 2
plot(s1413rec, col=grey(1:100/100))

s1514rec = s1514
s1514rec[s1514 < 0] = 3
s1514rec[s1514 == 0] = 2
s1514rec[s1514 > 0]  = 1
plot(s1514rec, col=brewer.pal(n = 3, name = 'Greys'))
s1813[]
s1813rec = s1813
s1813rec[s1813 < 0] = 2
s1813rec[s1813 == 0] = 1
s1813rec[s1813 > 0]  = 0
s1813rec[]

colors = c( "black", "white")
plot(s1813rec, col=brewer.pal(n = 3, name = 'Greys'))

r14_classified = reclassify(r14, reclass_r)
r15_classified = reclassify(r15, reclass_r)
r17_classified = reclassify(r17, reclass_r)
r18_classified = reclassify(r18, reclass_r)

par(mfrow=c(2,2))
plot(s1413, col=brewer.pal(n = 11, name = 'RdYlGn'), main = 'Difference between 2013 and 2014')
plot(s1514,col=brewer.pal(n = 11, name = 'RdYlGn'), main = 'Difference between 2014 and 2015')
plot(s1715, col=brewer.pal(n = 11, name = 'RdYlGn'), main = 'Difference between 2015 and 2017')
plot(s1817,col=brewer.pal(n = 11, name = 'RdYlGn'), main = 'Difference between 2017 and 2018')
par(mfrow=c(1,1))
plot(s1813, col=brewer.pal(n = 11, name = 'RdYlGn'), main = 'Difference between 2013 and 2018')
plot(s1813, col=grey(1:100/100), main = 'Difference between 2013 and 2018')

sum(s1715[] == 0)

# par(mfrow=c(2,3))
# plot(d, col=c('black','grey90'), main = 'Classified Differences between 2014 and 2013')
# plot(d2, col=c('black','grey90'), main = 'Classified Differences between 2015 and 2014')
# plot(d3, col=c('black','grey90'), main = 'Classified Differences between 2017 and 2015')
# plot(d4, col=c('black','grey90'), main = 'Classified Differences between 2018 and 2017')
# par(mfrow=c(1,1))
# plot(d5, col=c('black','grey90'), main = 'Classified Differences between 2018 and 2013')


install.packages('dplyr')
library(dplyr)

## add population data
pop = read.csv('Copy of population growth data.csv', sep = ';')
pop_bangalore = filter(pop, pop$City == "Bangalore")
pop_bangalore = pop_bangalore[c(-1, -5),]
pop_bangalore


tr_pop = cbind(tr, pop_bangalore$Population)
tr_pop

pl0 = ggplot(tr_pop, aes(years, pop_bangalore$Population)) +
  geom_line(color='green', size=1) +
  labs(ylab = 'Population', title = 'Population and Radiance Trends 2013-2018 w/o (2016)')+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

pl01 = ggplot(tr_pop, aes(years, RadianceSum)) +
  geom_line(color='red', size=1) +
  labs(ylab='Sum of Radiance', xlab='Years')


plot_grid(pl0, pl01, align = 'v', ncol = 1, rel_heights = 2)

## check for normal distribution 
shapiro.test(tr$RadianceSum)
shapiro.test(pop_bangalore$Population)
qqnorm(tr$RadianceSum, pch = 1, frame = FALSE, main = "Normal Q-Q Plot Radiance Values Bangalore")
qqline(tr$RadianceSum, col = "blue", lwd = 2)
qqnorm(pop_bangalore$Population, pch = 1, frame = FALSE, main = "Normal Q-Q Plot Population Bangalore")
qqline(pop_bangalore$Population, col = "red", lwd = 2)

## correlation between population and radiance vals
cor.test(tr$RadianceSum, pop_bangalore$Population, method = 'pearson')
lm_bangalore = lm( pop_bangalore$Population ~ tr$RadianceSum )
lm_bangalore
?lm

plot(tr$RadianceSum, pop_bangalore$Population, pch = 16, cex = 1.3, col = "purple", main="Radiance Values and Population in Bangalore between 2013-2018",
     xlab = "Radiance Values (W cm-2 sr-1)", ylab= "Population in Bangalore")
abline(lm_bangalore)


## resample population raster

install.packages('stars')
library(stars)
grid = st_as_stars(st_bbox(r15), dx=  0.004166667, dy=0.004166667)
plot(grid)

pop_raster2 = st_warp(st_as_stars(pop_raster1), grid, method = "average", use_gdal = TRUE)
par(mfrow=c(1,1))
summary(pop_raster1[])
pop_raster2
write_stars(pop_raster2, 'resampledpop.tif', update = T )

## bortle scale range in population - 2015

sum(r15[] < 0) 
pop_resampled = raster('resampledpop.tif')

plot(pop_resampled)
plot(r15)
pop_resampled
r15

## each time change test to the required bortle scale range
## 0.25 - 1, 1 - 75, 75 and up

test = reclassify(r15, cbind(0, 75, NA), right=FALSE)
test = reclassify(test, cbind(75,Inf, 1), right=FALSE)
test = reclassify(test, cbind(75, Inf,NA), right=FALSE)
plot(test)
sum(is.na(test[]))
masked = mask(pop_resampled, test)
masked[] 
plot(masked)
writeRaster(masked, 'masked_75.tif', overwrite= TRUE)

pop_raster = raster("populationdense2015.tif")
pop_raster
setMinMax(pop_raster)

pop_raster1 = raster('extracted_pop.tif')
plot(pop_raster1)
setMinMax(pop_raster1)




## population prediction for 2017 and 2018 using radiance vals 
## change for r17 for suburban sky bortle range and then for r18

test = reclassify(r18, cbind(0,1.5, NA), right=FALSE)
test = reclassify(test, cbind(1.5 ,75, 1), right=FALSE)
test = reclassify(test, cbind(75, Inf,NA), right=FALSE)
plot(test)
sum(is.na(test[]))
masked = mask(pop_resampled, test)
masked[] 

writeRaster(masked, 'masked_18_sub.tif', overwrite= TRUE)
par(mfrow=c(1,2))
plot(r18_classified, col=grey(1:100/100), main = 'Bangalore Bortle Scale 2018')
plot(masked, col=grey(1:100/100), main = 'Predicted Population Density Suburban Sky 2018')
sum(masked[], na.rm = T) - 
  sum(masked1[], na.rm = T)


## process plots
install.packages('colorRamps')
library(colorRamps)
masked1 = raster("masked_0.25-1.5.tif")
masked2 = raster("masked_1.5-75.tif")
masked3 = raster("masked_75.tif")

#plot resampling
par(mfrow=c(1,2))
pop_raster1

plot(pop_raster1)
plot(pop_resampled)

#plot plot masked
par(mfrow=c(2,2))
plot(r15_classified, col=grey(1:100/100), main='Bangalore Bortle Scale 2015')
plot(masked1, col=grey(1:100/100), main='Population Density Rural Sky 2015')
plot(masked2, col=grey(1:100/100), main='Population Density Suburban Sky 2015')
plot(masked3, col=grey(1:100/100), main='Population Density Inner City Sky 2015')














