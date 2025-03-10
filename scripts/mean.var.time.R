#assume that on a log scale, amount of var stays the same as the mean shifts
#know that for a mouse and an elephant, on a log scale (prop amt), of var is the same
#is this true of a pop over time?
#does this change depending if there is a lot of change around a mean or a little or an OU process?
#if there is change, what does this imply about evolvability and rates of change?

#can look at 'average' trait (e.g., mean, median, or optimum est by an OU process)
#ask if var around mean of traits change depending on the distance away from the mean
#KLV expects no or a weak relationship; i.e., prop var stays the same
#another expectation is that as trait moves away from the mean, amount of var decreases
#this is maybe expected since we know that rates of evo decrease as it moves away from some mean
#we can also ask if this is asymmetrical: that is, is the amount of variation larger or smaller as the mean increases or decreases?

#1. Get pets data
#2. estimate a mean for each time series
# calculate distance from mean
#3. plot var (y) against distance from the mean (x)

#need to see if logged or not
#then need to use paleoTS from non-logged var into log
#remove anything missing or non-logged means negative
#retain minimum 6 steps
#need to think about which traits to use...

#### LOAD PACKAGES ----
require(dplyr)
require(reshape2)
require(tidyr)
require(ggplot2)
require(paleoTS)

#### ENVIRONMENT ----
# set to working directory

setwd("~/Documents/GitHub/megbalk/variation-time-series/")

plot.theme <- theme(text = element_text(size = 16),
                    legend.position = "none",
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    plot.background = element_rect(fill = 'transparent', 
                                                   color = NA))

#### LOAD DATA ----
#note that downloading it as a csv was weird (emailed Sunniva)
df <- read.table("data/timeseries/timeseries.txt", 
                 sep = "\t",
                 header = TRUE)
nrow(df) #12950
length(unique(df$tsID)) #671

#converted to a csv for ease of upload
meta <- read.csv("data/timeseries/metadata.csv",
                 header = TRUE)
nrow(meta) #671
colnames(meta)

#### MANIPULATE & FILTER DATA ----
setdiff(meta$tsID, df$tsID) #no differences
setdiff(df$tsID, meta$tsID) #no differences

df.meta <- merge(df, meta,
                 by = "tsID",
                 all.x = TRUE, all.y = TRUE)
nrow(df.meta) #12950
length(unique(df.meta$tsID)) #671

#let's use data that only have n = 30
df.meta <- df.meta %>%
    drop_na(trait_mean, trait_var) %>%
    filter(N >= 30)
nrow(df.meta) #6571
length(unique(df.meta$tsID)) #493

#filter data to avoid missing data and a time series of at least 6
n.ts <- df.meta %>%
    group_by(tsID) %>%
    summarise(n = n())
keep.ts <- n.ts$tsID[n.ts$n >= 6]
df.meta <- df.meta[df.meta$tsID %in% keep.ts,]

nrow(df.meta) #5945
length(unique(df.meta$tsID)) #236

#remove any with negative means
unique(df.meta$tsID[df.meta$trait_mean < 0])
df.meta <- df.meta[df.meta$trait_mean > 0,]
nrow(df.meta) #5399
length(unique(df.meta$tsID)) #228

##let's just do linear traits for now
df.meta.linear <- df.meta[df.meta$trait_type == "linear",]
nrow(df.meta.linear) #3407
length(unique(df.meta.linear$tsID)) #130

#select ones to convert
df.nonlog <- df.meta.linear[df.meta.linear$log_transformed == "no",]
nrow(df.nonlog) #3065; most
length(unique(df.nonlog$tsID)) #124

##### LOG TRANSFORM DATA -----
#need to see which are already logged and which aren't
for(i in 1:length(unique(df.nonlog$tsID))){
    y <- df.nonlog[df.nonlog$tsID == unique(df.nonlog$tsID)[i],]
    x <- as.paleoTS(mm = y$trait_mean,
                    vv = y$trait_var,
                    nn = y$N,
                    tt = y$age_MY)
    z <- ln.paleoTS(x)
    df.meta.linear$trait_mean[df.meta.linear$tsID == unique(df.nonlog$tsID)[i]] <- z$mm
    df.meta.linear$trait_var[df.meta.linear$tsID == unique(df.nonlog$tsID)[i]] <- z$vv
    df.meta.linear$log_transformed[df.meta.linear$tsID == unique(df.nonlog$tsID)[i]] <- "ln.paleoTS"
}
unique(df.meta.linear$log_transformed)

##### CALCULATE MEAN & MEDIANS ----
mid.traits <- df.meta.linear %>%
    group_by(tsID) %>%
    summarise(tot.mean = mean(trait_mean),
              tot.median = median(trait_mean)) %>%
    as.data.frame()

df.means <- merge(df.meta.linear, mid.traits,
                  by = "tsID")
nrow(df.means) #6272
length(unique(df.means$tsID)) #407

##### LOOK AT DATA ----
#what is size range
#what is time range
#what is taxonomic range
#what is geologic range


##### CALCULATE DISTANCE FROM MEAN -----
#want smaller to be negative, so then trait_mean - tot.mean
df.means$dist.mean = df.means$trait_mean - df.means$tot.mean
df.means$dist.median = df.means$trait_mean - df.means$tot.median

df.means$abs.dist.mean = abs(df.means$trait_mean - df.means$tot.mean)

df.means <- mutate(df.means, n.bin = cut(N, breaks = c(0, 10, 100, 1000)))

df.means$cat.dir <- "neg"
df.means$cat.dir[df.means$dist.mean > 0] <- "pos"

#which are really far away?
#unique(df.means$tsID[df.means$dist.mean > 5])
#357, 589
#what is going on with 357??
#df.means[df.means$tsID == 357,]
#let's remove for now...

#unique(df.means$tsID[df.means$trait_mean > 100])

#rm.tsID <- c(357, 589)
#df.means.trim <- df.means[!(df.means$tsID %in% rm.tsID),]

#which have really high variances?
unique(df.means$tsID[df.means$trait_var > 20])
df.means[df.means$tsID == 299,]
#let's remove this for now
df.means.trim <- df.means[df.means$tsID != 299,]

#### PLOTS ----

## mean v var
ggplot(df.means.trim,
       aes(trait_mean, trait_var)) +
    geom_point() + 
    plot.theme +
    scale_y_continuous(trans = 'log') + 
    scale_x_continuous(trans = 'log')
#looks wonky...
summary(lm(trait_var ~ trait_mean, df.means))
#significant; slightly neg; explains almost none of var

## distance from mean and variation
# color coded by sample size
ggplot(df.means.trim,
       aes(dist.mean, trait_var)) +
    geom_point(aes(fill = n.bin, col = n.bin, group = n.bin),
               alpha = 0.5)
#no trend

## distance from mean and variation
#group by if smaller or larger than mean
ggplot(df.means.trim,
       aes(dist.mean, trait_var,
           group = cat.dir, fill = cat.dir, col = cat.dir)) +
    geom_point() +
    geom_smooth(method = "lm") +
    plot.theme + 
    scale_y_continuous(name = expression(log~Variation)) +
    scale_x_continuous(name = expression(Distance~from~log~Mean))

summary(lm(trait_var ~ dist.mean, df.means.trim[df.means.trim$cat.dir == "neg",]))
summary(lm(trait_var ~ dist.mean, df.means.trim[df.means.trim$cat.dir == "pos",]))
#only slightly better, but drive by a few randos

## look at absolute distance
ggplot(df.means.trim,
       aes(abs.dist.mean, trait_var)) +
           #group = tsID, fill = tsID, col = tsID
           #fill = cat.dir, group = cat.dir, col = cat.dir
    geom_point() +
    geom_smooth(method = "lm") +
    plot.theme + 
    scale_y_continuous(name = expression(log~Variation),
                       trans = 'log') +
    scale_x_continuous(name = expression(Distance~from~log~Mean),
                       trans = 'log')
glob.lm = summary(lm(trait_var ~ abs.dist.mean, df.means.trim))
#sig, tight slope, positive (0.4); 8% var explained
#but we expect negative!!

df.means.trim$log.log.abs.dist.mean <- log(df.means.trim$abs.dist.mean)
df.means.trim$log.log.var <- log(df.means.trim$trait_var)
summary(lm(log.log.var ~ log.log.abs.dist.mean, df.means.trim))
#then we get 0.3 slope, 11% var explained
#if loglog it, then get more variance if go either way
#so one step away from mean gives .5 inc in var

#how many are really far away from mean
#here, really far is going to be more than 2 sigma (nb: var = sigma^2)
#i am probably doing something wrong, but I'm going to just do:
df.means.trim$se.two <- sqrt(df.means.trim$trait_var)*2

df.means.trim$far <- "no"
df.means.trim$far[df.means.trim$dist.mean > df.means.trim$se.two] <- "yes"
nrow(df.means.trim[df.means.trim$far == "yes",]) 

far.col = c("black", "red")

## distance from mean and variation
# color coded based on if they are 'far' or not from mean
ggplot(df.means.trim,
       aes(abs.dist.mean, trait_var)) +
    geom_point(aes(group = far, fill = far, col = far)) +
    plot.theme + 
    scale_y_continuous(name = expression(log~Variation)) +
    scale_x_continuous(name = expression(Distance~from~log~Mean)) +
    scale_fill_manual(values = far.col) + 
    scale_color_manual(values = far.col)
#most are close to mean

#### AVERAGE SLOPE ----
glob.lm$coefficients
glob.slope.min = glob.lm$coefficients[2] - glob.lm$coefficients[4]
glob.slope.max = glob.lm$coefficients[2] + glob.lm$coefficients[4]

stats.cols <- c("tsID", "n.steps", "age.range", "tot.mean", "total_N",
                "slope", "slope.se", "slope.p", "adj.r.sq", "df")
stats.df <- data.frame(matrix(ncol = length(stats.cols), nrow = length(unique(df.means.trim$tsID))))
colnames(stats.df) <- stats.cols
for(i in 1:length(unique(df.means.trim$tsID))){
    sub.df = df.means.trim[df.means.trim$tsID == unique(df.means.trim$tsID)[i],]
    x <- summary(lm(trait_var ~ abs.dist.mean,
                    sub.df))
    stats.df$tsID[i] = unique(sub.df$tsID)
    stats.df$n.steps[i] = nrow(sub.df)
    stats.df$age.range[i] = max(sub.df$age_MY) - min(sub.df$age_MY)
    stats.df$tot.mean[i] = unique(sub.df$tot.mean)
    stats.df$total_N[i] = unique(sub.df$total_N)
    stats.df$slope[i] = as.numeric(format(x$coefficients[2], scientific = FALSE))
    stats.df$slope.se[i] = as.numeric(format(x$coefficients[4], scientific = FALSE))
    stats.df$slope.p[i] = as.numeric(format(x$coefficients[8], scientific = FALSE))
    stats.df$adj.r.sq[i] = x$adj.r.squared
    stats.df$df[i] = x$df[2]
}
stats.df$sig <- stats.df$slope.p <= 0.005
stats.df$slope.min = stats.df$slope - stats.df$slope.se
stats.df$slope.max = stats.df$slope + stats.df$slope.se
stats.df$overlap.slope <- stats.df$slope.min <= glob.slope.min & stats.df$slope.max >= glob.slope.max

write.csv(stats.df,
          "results/slope.results.csv",
          row.names = FALSE)

## what percentage of slopes are around the global slope?
nrow(stats.df[stats.df$overlap.slope == TRUE,])/nrow(stats.df) #5%

#### TESTING ----

#let's just test if changing mean with same var is the same in log space
#x1 <- rnorm(n = 30, mean = 1, sd = 1)
x2 <- rnorm(n = 30, mean = 10, sd = 1)
x3 <- rnorm(n = 30, mean = 100, sd = 1)
x4 <- rnorm(n = 30, mean = 1000, sd = 1)
x5 <- rnorm(n = 30, mean = 10000, sd = 1)

#log.x1 <- log(x1)
log.x2 <- log(x2)
log.x3 <- log(x3)
log.x4 <- log(x4)
log.x5 <- log(x5)

mean.x <- c(mean(x2), mean(x3), mean(x4), mean(x5))
log.mean.x <- c(mean(log.x2), mean(log.x3), mean(log.x4), mean(log.x5))
sd.x <- c(sd(x2), sd(x3), sd(x4), sd(x5))
log.sd.x <- c(sd(log.x2), sd(log.x3), sd(log.x4), sd(log.x5))

ggplot() +
    plot.theme +
    geom_point(aes(x = mean.x, y = sd.x)) + 
    geom_smooth(aes(x = mean.x, y = sd.x),
                method = 'lm')

ggplot() +
    plot.theme +
    geom_point(aes(x = log.mean.x, y = log.sd.x)) + 
    geom_smooth(aes(x = log.mean.x, y = log.sd.x),
                method = 'lm')
summary(lm(log.sd.x ~ log.mean.x)) #no diff from zero
