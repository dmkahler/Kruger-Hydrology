# To analyze data for flow analysis

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(latex2exp)
# install_github("LimpopoLab/hydrostats")
library(hydrostats)

sandUp <- read_csv("X3H008.csv", col_names = FALSE)
sandUp <- sandUp %>%
     rename(dt=X1,UNIX=X2,lev_m=X3,levQC=X4,flow_m3s=X5,flowQC=X6) %>% # note UNIX date time is UTC
     mutate(lev_m=replace(lev_m, which(lev_m<=0),NA)) %>%
     mutate(flow_m3s=replace(flow_m3s, which(flow_m3s<=0),NA)) %>%
     group_by(UNIX) %>%
     summarize(lev_m=mean(lev_m,na.rm=TRUE), levQC=mean(levQC), flow_m3s=mean(flow_m3s,na.rm=TRUE), flowQC=mean(flowQC)) %>%
     mutate(dt=with_tz(as_datetime(UNIX), tzone = "Africa/Johannesburg")) %>%
     mutate(da=as_date(dt)) %>%
     group_by(da) %>%
     summarize(mean.flow=mean(flow_m3s,na.rm=TRUE)) %>%
     mutate(Site = "Sand River - upstream")
     
sabiUp <- read_csv("X3H021.csv", col_names = FALSE)
sabiUp <- sabiUp %>%
     rename(dt=X1,UNIX=X2,lev_m=X3,levQC=X4,flow_m3s=X5,flowQC=X6) %>% # note UNIX date time is UTC
     mutate(lev_m=replace(lev_m, which(lev_m<=0),NA)) %>%
     mutate(flow_m3s=replace(flow_m3s, which(flow_m3s<=0),NA)) %>%
     group_by(UNIX) %>%
     summarize(lev_m=mean(lev_m,na.rm=TRUE), levQC=mean(levQC), flow_m3s=mean(flow_m3s,na.rm=TRUE), flowQC=mean(flowQC)) %>%
     mutate(dt=with_tz(as_datetime(UNIX), tzone = "Africa/Johannesburg")) %>%
     mutate(da=as_date(dt)) %>%
     group_by(da) %>%
     summarize(mean.flow=mean(flow_m3s,na.rm=TRUE)) %>%
     mutate(Site = "Sabie River - upstream")

sabiDn <- read_csv("X3H015.csv", col_names = FALSE)
sabiDn <- sabiDn %>%
     rename(dt=X1,UNIX=X2,lev_m=X3,levQC=X4,flow_m3s=X5,flowQC=X6) %>% # note UNIX date time is UTC
     mutate(lev_m=replace(lev_m, which(lev_m<=0),NA)) %>%
     mutate(flow_m3s=replace(flow_m3s, which(flow_m3s<=0),NA)) %>%
     group_by(UNIX) %>%
     summarize(lev_m=mean(lev_m,na.rm=TRUE), levQC=mean(levQC), flow_m3s=mean(flow_m3s,na.rm=TRUE), flowQC=mean(flowQC)) %>%
     mutate(dt=with_tz(as_datetime(UNIX), tzone = "Africa/Johannesburg")) %>%
     mutate(da=as_date(dt)) %>%
     group_by(da) %>%
     summarize(mean.flow=mean(flow_m3s,na.rm=TRUE)) %>%
     mutate(Site = "Sabie River - downstream")

dat <- rbind(sabiDn,sabiUp,sandUp)
ggplot(dat) +
     geom_point(aes(x=da, y=mean.flow, color=Site)) +
     xlab("Date") + 
     ylab(TeX('Mean Discharge $(m^3/s)$')) + 
     ylim(c(0,3000)) +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14), 
           axis.title = element_text(face = "plain", size = 14)) +
     theme(legend.position="right", 
           panel.background = element_rect(fill = "white", colour = "black"),
           legend.text = element_text(face = "plain", size = 14), 
           legend.title = element_text(face = "plain", size = 14))

dat2 <- dat %>%
     mutate(dn = as.numeric(da))

s <- min(dat2$dn) -1 # set as the day before the first day - DATUM
n <- max(dat2$dn) - s
sabUP <- array(NA, dim = n)
sabDN <- sabUP
sanUP <- sabUP
daynumber <- array(NA, dim = n)

for (i in 1:n) {
     daynumber[i] <- i + s
}
for (i in 1:nrow(sabiUp)) {              # Sabie Upriver
     p <- as.numeric(sabiUp$da[i]) - s
     sabUP[p] <- sabiUp$mean.flow[i]
}
for (i in 1:nrow(sabiDn)) {              # Sabie Downriver
     p <- as.numeric(sabiDn$da[i]) - s
     sabDN[p] <- sabiDn$mean.flow[i]
}
for (i in 1:nrow(sandUp)) {              # Sand Upriver
     p <- as.numeric(sandUp$da[i]) - s
     sanUP[p] <- sandUp$mean.flow[i]
}

rm(dat2, p, i)
flow <- data.frame(daynumber) %>% # fails with tibble(), problem with preserving an array, even though it is only 1-D
     mutate(dt = as_date(daynumber, origin = lubridate::origin))
flow <- data.frame(flow,sabUP,sanUP,sabDN)
# write_csv(flow, "sabieRiverBalance.csv")

monthly <- flow %>%
     mutate(yr = year(dt), mo = month(dt)) %>%
     mutate(ym = 100 * yr + mo) %>%
     group_by(ym) %>%
     summarize(sabiUp = mean(sabUP, na.rm = TRUE), sandUp = mean(sanUP, na.rm = TRUE), sabiDn = mean(sabDN, na.rm = TRUE)) %>%
     mutate(net = sabiUp + sandUp - sabiDn) %>%
     mutate(yr = round(ym/100)) %>%
     mutate(mo = ym - (100 * yr)) %>%
     mutate(dt = ymd(paste0(yr, "-", mo, "-", "01"))) %>%
     filter(dt > ymd("1990-01-01"))
ggplot(monthly) +
     geom_line(aes(x = dt, y = net)) +
     xlab("Date") + 
     ylab(TeX('Net Monthly Mean Discharge $(m^3/s)$')) + 
     ylim(c(-150,150)) +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14), 
           axis.title = element_text(face = "plain", size = 14),
           panel.background = element_rect(fill = "white", colour = "black"))







# Remove duplicate dates
q <- x %>%
     mutate(lev_m=replace(lev_m, which(lev_m<=0),NA)) %>%
     mutate(flow_m3s=replace(flow_m3s, which(flow_m3s<=0),NA)) %>%
     group_by(UNIX) %>%
     summarize(lev_m=mean(lev_m,na.rm=TRUE), levQC=mean(levQC), flow_m3s=mean(flow_m3s,na.rm=TRUE), flowQC=mean(flowQC)) %>%
     mutate(dt=with_tz(as_datetime(UNIX), tzone = "Africa/Johannesburg"))

ggplot(q) +
     geom_point(aes(x=dt,y=flow_m3s)) +
     ylim(c(0,10000)) +
     xlab("Date") + 
     ylab(TeX('Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))

## Annual data
a <- q %>%
     mutate(hydro_year=hyd.yr(dt, s = "S")) %>%
     group_by(hydro_year) %>%
     summarize(ann.min.flow=min(flow_m3s,na.rm=TRUE),ann.mean.flow=mean(flow_m3s,na.rm=TRUE),ann.max.flow=max(flow_m3s,na.rm=TRUE)) %>%
     mutate(annual.tot=ann.mean.flow*3600*24*365.25) #%>%
     #na_if(Inf) %>%
     #na_if(-Inf) %>%
     #na_if(NaN)
# it appears that the warnings, length > 1, non-missing arguments, are taken care of.
hy <- array(NA, dim = nrow(q))
for (i in 1:nrow(q)) {
     hy[i] <- hyd.yr(q$dt[i])
}

b <- cbind(q, hy)
a <- b %>%
     group_by(hy) %>%
     summarize(ann.min.flow=min(flow_m3s,na.rm=TRUE),ann.mean.flow=mean(flow_m3s,na.rm=TRUE),ann.max.flow=max(flow_m3s,na.rm=TRUE)) %>%
     mutate(annual.tot=ann.mean.flow*3600*24*365.25) %>%
     rename(hydro_year = hy)

# Annual flow time series
# Average annual flow
# ave.ann.flow <- 365.25 * 24 * 3600 * mean(m$mon.mean.flow)
# Found the annual mean flow to be 1,689,908,081 m^3, that is, 1.7x10^9 m^3

modBeitbridge <- lm(a$annual.tot ~ a$hydro_year)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)   
#      (Intercept)   1.748e+11  5.308e+10   3.293  0.00168 **
#      a$hydro_year -8.566e+07  2.669e+07  -3.210  0.00215 **
#      ---
#      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#                         2.5 %       97.5 %
#      (Intercept)  68580428714 280986119165
#      a$hydro_year  -139060814    -32260553

ggplot(a) + #   ann_flow <- 
     geom_point(aes(x=hydro_year,y=(annual.tot/1e9))) +
     #geom_hline(yintercept = ave.ann.flow) +
     geom_smooth(aes(x=hydro_year,y=(annual.tot/1e9)), method = "lm", se = TRUE, color='blue') +
     xlim(c(1955,2022)) +
     ylim(c(0,2)) +
     #coord_cartesian(ylim=c(0,10000000000)) + # control y lim here!
     xlab("Hydrologic Year") + 
     ylab(TeX('Annual Total Discharge $(\\times 10^{9} m^3/y)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
ggsave("annual_flow_tall.eps", ann_flow, device = "eps", dpi = 72)

## Annual flood
ann_flood <- ggplot(a) +
     geom_col(aes(x=hydro_year,y=ann.max.flow)) +
     xlim(c(1955,2022)) +
     ylim(c(0,10000)) +
     xlab("Hydrologic Year") + 
     ylab(TeX('Annual Maximum Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_flood.eps", ann_flood, device = "eps", dpi = 72)

ann_flood_hist <- ggplot(a,aes(x=ann.max.flow)) +
     geom_histogram(breaks = (500*c(0:20)), color = "black", fill = "gray", na.rm = TRUE) +
     xlab(TeX('Annual Maximum Discharge $(m^3/s)$')) +
     ylab("Years") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_flood_hist.eps", ann_flood_hist, device = "eps", dpi = 72)

# 200 year flood
a$log_q <- log(a$ann.max.flow, base = 10) # log transform
T_R <- 200 # return period in years (in this case)
Fx <- 1 - (1/T_R)
mu <- mean(a$log_q, na.rm = TRUE)
s <- sd(a$log_q, na.rm = TRUE)
c <- skew(a$log_q) # NA removed by default
lp3.x <- pt3(c,Fx)
lp3.y <- (lp3.x * s) + mu
lp3.z <- 10^lp3.y # this is the T_R flood level 200-year flood is 13,667 m^3/s

ann_drought <- ggplot(a) +
     geom_col(aes(x=hydro_year,y=ann.min.flow)) +
     xlim(c(1955,2022)) +
     ylim(c(0,10)) +
     xlab("Hydrologic Year") + 
     ylab(TeX('Annual Minimum Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_drought.eps", ann_drought, device = "eps", dpi = 72)

ann_drought_hist <- ggplot(a,aes(x=ann.min.flow)) +
     geom_histogram(breaks = (c(0:10)), color = "black", fill = "gray", na.rm = TRUE) +
     xlab(TeX('Annual Minimum Discharge $(m^3/s)$')) +
     ylab("Years") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("annual_drought_hist.eps", ann_drought_hist, device = "eps", dpi = 72)

## Monthly averages
m <- q %>%
     mutate(mon=month(dt)) %>%
     group_by(mon) %>%
     summarize(mon.mean.flow=mean(flow_m3s,na.rm=TRUE),mon.sd.flow=stdev(flow_m3s),mon.med.flow=median(flow_m3s,na.rm=TRUE)) %>%
     na_if(Inf) %>%
     na_if(-Inf) %>%
     na_if(NaN)
m$label <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
m$label <- factor(m$label, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

mon_mean <- ggplot(m) +
     geom_col(aes(x=label,y=mon.mean.flow)) +
     ylim(c(0,200)) +
     xlab("Month") + 
     ylab(TeX('Monthly Mean Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("mon_mean.eps", mon_mean, device = "eps", dpi = 72)

mon_median <- ggplot(m) +
     geom_col(aes(x=label,y=mon.med.flow)) +
     ylim(c(0,100)) +
     xlab("Month") + 
     ylab(TeX('Monthly Median Discharge $(m^3/s)$')) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
ggsave("mon_med.eps", mon_median, device = "eps", dpi = 72)






