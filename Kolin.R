# Analysis code for Schwalb et al. 2022
# Distributed under CC BY 4.0 license: https://creativecommons.org/licenses/by/4.0/

# Packages
library(ggplot2)
library(reshape2)
library(patchwork)
library(RColorBrewer)

# Total TB cases - Total period
bacillarytb               <- read.csv("CSV/bacillarytb.csv", header=TRUE)
lmbacillarytb             <- lm(cases ~ year, data = bacillarytb)
expbacillarytb            <- lm(log(cases) ~ year, data = bacillarytb) 
decexpbacillarytb         <- (1-exp(-expbacillarytb$coefficients[2]))

bactbx <- seq(min(bacillarytb$year),max(bacillarytb$year),by=0.05)
bactbci <- as.data.frame(exp(predict(expbacillarytb, newdata = data.frame(year=bactbx), interval = "confidence", level = 0.95)))
bactbci$bactbx <- bactbx

plot(bacillarytb$year, bacillarytb$cases, xlab="Year", ylab="Cases", main="Regression")
lines(bactbx, bactbci[,1], col = "lightblue")
lines(bactbx, bactbci[,2], col="blue", lty=2)
lines(bactbx, bactbci[,3], col="blue", lty=2)

# Total TB cases - 1960-64
bacillarytbper1               <- read.csv("CSV/bacillarytbper1.csv", header=TRUE)
lmbacillarytbper1             <- lm(cases ~ year, data = bacillarytbper1)
expbacillarytbper1            <- lm(log(cases) ~ year, data = bacillarytbper1)
decexpbacillarytbper1         <- (1-exp(-expbacillarytbper1$coefficients[2]))

# Total TB cases - 1965-72
bacillarytbper2               <- read.csv("CSV/bacillarytbper2.csv", header=TRUE)
lmbacillarytbper2             <- lm(cases ~ year, data = bacillarytbper2)
expbacillarytbper2            <- lm(log(cases) ~ year, data = bacillarytbper2)
decexpbacillarytbper2         <- (1-exp(-expbacillarytbper2$coefficients[2]))

# New TB cases - Total period
newbacillarytb              <- read.csv("CSV/newbacillarytb.csv", header=TRUE)
lmnewbacillarytb            <- lm(cases ~ year, data = newbacillarytb)
expnewbacillarytb           <- lm(log(cases) ~ year, data = newbacillarytb)
decexpnewbacillarytb        <- (1-exp(-expnewbacillarytb$coefficients[2]))

newbactbx <- seq(min(newbacillarytb$year),max(newbacillarytb$year),by=0.05)
newbactbci <- as.data.frame(exp(predict(expnewbacillarytb, newdata = data.frame(year=newbactbx), interval = "confidence", level = 0.95)))
newbactbci$newbactbx <- newbactbx

plot(newbacillarytb$year, newbacillarytb$cases, xlab="Year", ylab="Cases", main="Regression")
lines(newbactbx, newbactbci[,1], col = "lightblue")
lines(newbactbx, newbactbci[,2], col="blue", lty=2)
lines(newbactbx, newbactbci[,3], col="blue", lty=2)

# New TB cases - 1960-64
newbacillarytbper1              <- read.csv("CSV/newbacillarytbper1.csv", header=TRUE)
lmnewbacillarytbper1            <- lm(cases ~ year, data = newbacillarytbper1)
expnewbacillarytbper1           <- lm(log(cases) ~ year, data = newbacillarytbper1)
decexpnewbacillarytbper1        <- (1-exp(-expnewbacillarytbper1$coefficients[2]))

# New TB cases - 1965-72
newbacillarytbper2              <- read.csv("CSV/newbacillarytbper2.csv", header=TRUE)
lmnewbacillarytbper2            <- lm(cases ~ year, data = newbacillarytbper2)
expnewbacillarytbper2           <- lm(log(cases) ~ year, data = newbacillarytbper2)
decexpnewbacillarytbper2        <- (1-exp(-expnewbacillarytbper2$coefficients[2]))

# Chronic TB cases - Total period
chronictb                 <- read.csv("CSV/chronictb.csv", header=TRUE)
lmchronictb               <- lm(cases ~ year, data = chronictb)
expchronictb              <- lm(log(cases) ~ year, data = chronictb)
decexpchronictb           <- (1-exp(-expchronictb$coefficients[2]))

chrbactbx <- seq(min(chronictb$year),max(chronictb$year),by=0.05)
chrbactbci <- as.data.frame(exp(predict(expchronictb, newdata = data.frame(year=chrbactbx), interval = "confidence", level = 0.95)))
chrbactbci$newbactbx <- chrbactbx

plot(chronictb$year, chronictb$cases, xlab="Year", ylab="Cases", main="Regression")
lines(chrbactbx, chrbactbci[,1], col = "lightblue")
lines(chrbactbx, chrbactbci[,2], col="blue", lty=2)
lines(chrbactbx, chrbactbci[,3], col="blue", lty=2)

# Kolin data set-up
kolin                 <- bacillarytb
kolin$newcases        <- newbacillarytb$cases
kolin$chronicases     <- chronictb$cases
kolinmelt             <- melt(kolin, id.vars = "year")

# Palettes
display.brewer.all(colorblindFriendly = T)
RColorBrewer::brewer.pal(8, 'Dark2')
RColorBrewer::brewer.pal(9, 'Greys')

# Figures
kolinlinear <- ggplot() +
  geom_line(kolinmelt, mapping = aes(x=year, y=value, color=variable)) +
  theme(panel.background = element_rect(fill = "#F0F0F0"),
        axis.text = element_text(size = 8)) +
  geom_line(bactbci, mapping = aes(x=bactbx, y=fit), color = "#1B9E77", linetype = 2, alpha = 0.5) +
  geom_ribbon(bactbci, mapping = aes(x=bactbx ,ymin=lwr, ymax=upr), color = "#1B9E77", fill = "#1B9E77", linetype = 2, alpha = 0.1) +
  geom_line(newbactbci, mapping = aes(x=newbactbx, y=fit), color = "#D95F02", linetype = 2, alpha = 0.5) +
  geom_ribbon(newbactbci, mapping = aes(x=newbactbx ,ymin=lwr, ymax=upr), color = "#D95F02", fill = "#D95F02", linetype = 2, alpha = 0.1) +
  geom_line(chrbactbci, mapping = aes(x=chrbactbx, y=fit), color = "#7570B3", linetype = 2, alpha = 0.5) +
  geom_ribbon(chrbactbci, mapping = aes(x=chrbactbx ,ymin=lwr, ymax=upr), color = "#7570B3", fill = "#7570B3", linetype = 2, alpha = 0.1) +
  scale_x_continuous("Year", expand=c(0, 0), limits = c(1960, 1972.2), breaks = seq(1960,1972.2,2)) +
  scale_y_continuous("Number of cases (linear scale)", expand=c(0, 0), limits=c(0,250), breaks = seq(0,250,50)) +
  scale_color_brewer(palette="Dark2", labels = c("Total cases","New cases","Chronic cases")) +
  annotate("segment", x = 1961, xend = 1961, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2) +
  annotate("segment", x = 1963, xend = 1963, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2) +
  annotate("segment", x = 1966, xend = 1966, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2) +
  annotate("segment", x = 1969, xend = 1969, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2) +
  annotate("segment", x = 1972, xend = 1972, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2)
  
kolinlog <- ggplot() +
  geom_line(kolinmelt, mapping = aes(x=year, y=value, color=variable)) +
  theme(panel.background = element_rect(fill = "#F0F0F0"),
        axis.text = element_text(size = 8)) +
  geom_line(bactbci, mapping = aes(x=bactbx, y=fit), color = "#1B9E77", linetype = 2, alpha = 0.5) +
  geom_ribbon(bactbci, mapping = aes(x=bactbx ,ymin=lwr, ymax=upr), color = "#1B9E77", fill = "#1B9E77", linetype = 2, alpha = 0.1) +
  geom_line(newbactbci, mapping = aes(x=newbactbx, y=fit), color = "#D95F02", linetype = 2, alpha = 0.5) +
  geom_ribbon(newbactbci, mapping = aes(x=newbactbx ,ymin=lwr, ymax=upr), color = "#D95F02", fill = "#D95F02", linetype = 2, alpha = 0.1) +
  geom_line(chrbactbci, mapping = aes(x=chrbactbx, y=fit), color = "#7570B3", linetype = 2, alpha = 0.5) +
  geom_ribbon(chrbactbci, mapping = aes(x=chrbactbx ,ymin=lwr, ymax=upr), color = "#7570B3", fill = "#7570B3", linetype = 2, alpha = 0.1) +
  scale_x_continuous("Year", expand=c(0, 0), limits = c(1960, 1972.2), breaks = seq(1960,1972.2,2)) +
  scale_y_log10("Number of cases (log scale)", limits = c(1,250), expand = c(0, 0), breaks = c(1,5,10,50,100,150,200,250)) +
  scale_color_brewer(palette="Dark2", labels = c("Total cases","New cases","Chronic cases")) +
  annotate("segment", x = 1961, xend = 1961, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2) +
  annotate("segment", x = 1963, xend = 1963, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2) +
  annotate("segment", x = 1966, xend = 1966, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2) +
  annotate("segment", x = 1969, xend = 1969, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2) +
  annotate("segment", x = 1972, xend = 1972, y = 0, yend = 250, alpha = 0.8, colour = "#E7298A", size = 0.4, linetype = 2)

(kolinlog + kolinlinear) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 14)) 
