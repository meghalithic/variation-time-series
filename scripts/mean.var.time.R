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
require(scales)

#### ENVIRONMENT ----
# set to working directory

setwd("~/Documents/GitHub/megbalk/variation-time-series/")

plot.theme <- theme(text = element_text(size = 16),
                    #legend.position = "none",
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

##### CALCULATE DISTANCE FROM MEAN -----
#want smaller to be negative, so then trait_mean - tot.mean
df.means$dist.mean = df.means$trait_mean - df.means$tot.mean
df.means$dist.median = df.means$trait_mean - df.means$tot.median

df.means$abs.dist.mean = abs(df.means$trait_mean - df.means$tot.mean)

#df.means <- mutate(df.means, n.bin = cut(N, breaks = c(0, 10, 100, 1000)))

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

#### LOOK AT DATA: SIZE ----
#what is size range
range(df.means.trim$trait_mean)
size.p <- ggplot(mid.traits, 
       aes(x = tot.mean)) +
    geom_histogram(binwidth = .25) + 
    plot.theme +
    scale_x_continuous(name = expression(log~Mean)) +
    scale_y_continuous(name = "Counts")

ggsave(size.p,
       file = "results/figures/size.hist.png",
       width = 20, height = 10, units = "cm")

#### LOOK AT DATA: TIME ----
#sampling over time of all time series
time.p <- ggplot(df.means.trim, 
                 aes(x = age_MY)) +
    geom_histogram() + 
    plot.theme +
    scale_x_continuous(name = "Age (MY)") +
    scale_y_continuous(name = "Counts")

ggsave(time.p,
       file = "results/figures/time.hist.png",
       width = 20, height = 10, units = "cm")

#all geologic periods sampled
##pie chart
uni.df <- df.means.trim[!duplicated(df.means.trim$tsID),]

geol.vec <- uni.df$epoch_start[uni.df$epoch_start == uni.df$epoch_end]
star.geol.vec <- uni.df$epoch_start[uni.df$epoch_start != uni.df$epoch_end]
end.geol.vec <- uni.df$epoch_end[uni.df$epoch_start != uni.df$epoch_end]

length(uni.df$tsID[uni.df$epoch_start == "Miocene"
                   & uni.df$epoch_end == "Pleistocene"])
#need to add this many "Pliocene"

length(uni.df$tsID[uni.df$epoch_start == "Pleistocene"
                   & uni.df$epoch_end == "modern"])
#need to add this many "Holocene"

length(uni.df$tsID[uni.df$epoch_start == "Pliocene"
                   & uni.df$epoch_end == "Holocene"])
#need to add this many Pleistocene

geol.vec2 <- c(geol.vec, star.geol.vec, end.geol.vec, 
               rep("Pliocene", 6), rep("Holocene", 3), rep("Pleistocene", 1))

#remove unknown
#make modern and present the same
geol.vec3 <- geol.vec2[geol.vec2 != "unknown"]
geol.vec3[geol.vec3 == "present"] <- "modern"
geol.vec3[geol.vec3 == "modern"] <- "Modern"
geol.vec3[geol.vec3 == "llandovery"] <- "Llandovery"
geol.vec3[geol.vec3 == "Upper-Cretaceaous"] <- "Late Cretaceous"
geol.vec3[geol.vec3 == "Upper-Cretaceous"] <- "Late Cretaceous"
geol.vec3[geol.vec3 == "Upper Cretaceous"] <- "Late Cretaceous"
geol.vec3[geol.vec3 == "Middle-Jurassic"] <- "Middle Jurassic"

per.stats <- as.data.frame(table(geol.vec3))
colnames(per.stats) <- c("Period", "n")
unique(per.stats$Period)
per.stats$Period <- factor(per.stats$Period, 
                           levels = c('Modern', 
                                      'Holocene', 
                                      'Pleistocene',
                                      'Pliocene',
                                      'Miocene',
                                      'Oligocene',
                                      'Eocene',
                                      'Paleocene',
                                      'Late Cretaceous',
                                      'Middle Jurassic',
                                      'Llandovery'))

period.p <- ggplot(per.stats,
                 aes(x = "", y = n,
                     fill = Period)) +
    geom_col(color = "black") +
    coord_polar(theta = "y") +
    plot.theme +
    theme_void() +
    geom_text(aes(label = n),
              position = position_stack(vjust = 0.5))

ggsave(period.p,
       file = "results/figures/period.sampled.pie.png",
       width = 20, height = 10, units = "cm")

#time ranges of each time series
#recreating total_MY since it doesn't seem to be in the metadata
df.range <- df.means.trim %>%
    group_by(tsID) %>%
    summarise(time.range = max(age_MY)-min(age_MY))

time.range.p <- ggplot(df.range, 
                 aes(x = time.range)) +
    geom_histogram() + 
    plot.theme +
    scale_x_continuous(name = "Time rage (MY)") +
    scale_y_continuous(name = "Counts")

ggsave(time.range.p,
       file = "results/figures/time.range.hist.png",
       width = 20, height = 10, units = "cm")

#### LOOK AT DATA: TAXA ----
#what is taxonomic range
#pie chart
taxa.stats <- df.means.trim[!duplicated(df.means.trim$tsID),] %>%
    group_by(taxa) %>%
    summarise(n = n())

unique(taxa.stats$taxa)

taxa.p <- ggplot(taxa.stats,
       aes(x = "", y = n,
           fill = taxa)) +
    geom_col(color = "black") +
    coord_polar(theta = "y") +
    plot.theme +
    theme_void() +
    geom_text(aes(label = n),
              position = position_stack(vjust = 0.5))

ggsave(taxa.p,
       file = "results/figures/taxa.pie.png",
       width = 20, height = 10, units = "cm")
    
#### PLOTS ----

##### MEAN AND VARIANCE -----
ggplot(df.means.trim,
       aes(trait_mean, trait_var)) +
    geom_point() + 
    plot.theme +
    scale_y_continuous(trans = 'log') + 
    scale_x_continuous(trans = 'log')
#looks wonky...
summary(lm(trait_var ~ trait_mean, df.means))
#significant; slightly neg; explains almost none of var

##### VARIANCE AND DISTANCE FROM MEAN -----
## look at absolute distance
glob.lm.p <- ggplot(df.means.trim,
       aes(abs.dist.mean, trait_var)) +
           #group = tsID, fill = tsID, col = tsID
           #fill = cat.dir, group = cat.dir, col = cat.dir
    geom_point() +
    geom_smooth(method = "lm") +
    plot.theme + 
    scale_y_continuous(name = expression(log~Variation),
                       trans = 'log',
                       breaks = trans_breaks('log', function(x) exp(x)),
                       labels = trans_format('log', math_format(exp(.x)))) +
    scale_x_continuous(name = expression(Distance~from~log~Mean),
                       trans = 'log',
                       breaks = trans_breaks('log', function(x) exp(x)),
                       labels = trans_format('log', math_format(exp(.x))))

glob.lm = summary(lm(trait_var ~ abs.dist.mean, df.means.trim))
#sig, tight slope, positive (0.4 +/- 0.02); 8% var explained
#but we expect negative!!

ggsave(glob.lm.p,
       file = "results/figures/global.relationship.png",
       width = 20, height = 10, units = "cm")

df.means.trim$log.log.abs.dist.mean <- log(df.means.trim$abs.dist.mean)
df.means.trim$log.log.var <- log(df.means.trim$trait_var)
summary(lm(log.log.var ~ log.log.abs.dist.mean, df.means.trim))
#then we get 0.3 slope, 11% var explained
#if loglog it, then get more variance if go either way
#so one step away from mean gives .5 inc in var

##### DIRECTIONALITY -----
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
#only slightly better for neg (11% var explained), seems to be driven by a few points
#not better for pos (4% of var explained)

##### HOW MANY ARE REALLY FAR AWAY FROM MEAN? -----
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

##### AVERAGE SLOPE -----
ts.lm.p <- ggplot(df.means.trim,
                    aes(abs.dist.mean, trait_var,
                        group = tsID)) + #fill = tsID, col = tsID
    geom_point() +
    geom_smooth(method = "lm") +
    plot.theme + 
    scale_y_continuous(name = expression(log~Variation),
                       trans = 'log',
                       breaks = trans_breaks('log', function(x) exp(x)),
                       labels = trans_format('log', math_format(exp(.x)))) +
    scale_x_continuous(name = expression(Distance~from~log~Mean),
                       trans = 'log',
                       breaks = trans_breaks('log', function(x) exp(x)),
                       labels = trans_format('log', math_format(exp(.x))))

ggsave(ts.lm.p,
       file = "results/figures/individ.relationship.png",
       width = 20, height = 10, units = "cm")

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

glob.stats <- c("global", nrow(df.means.trim), 
                max(df.means.trim$age_MY) - min(df.means.trim$age_MY),
                mean(df.means.trim$tot.mean),
                sum(stats.df$total_N),
                glob.lm$coefficients[2], glob.lm$coefficients[4],
                as.numeric(format(glob.lm$coefficients[8], scientific = FALSE)),
                glob.lm$adj.r.squared, glob.lm$df[2],
                as.numeric(format(glob.lm$coefficients[8], scientific = FALSE)) <= 0.005,
                glob.slope.min, glob.slope.max,
                glob.slope.min <= glob.slope.min & glob.slope.max >= glob.slope.max)

stats.df.with.glob <- rbind(stats.df, glob.stats)

write.csv(stats.df.with.glob,
          "results/slope.results.csv",
          row.names = FALSE)

mean(stats.df$slope) #0.02 
#this is expected!
#not close to the global slope
#which are driving the trend?

## what percentage of slopes are around the global slope?
nrow(stats.df[stats.df$overlap.slope == TRUE,])/nrow(stats.df) #5%

chk.ts <- stats.df$tsID[stats.df$overlap.slope == TRUE]
chk.ts
View(df.means.trim[df.means.trim$tsID %in% chk.ts,])
View(stats.df[stats.df$overlap.slope == TRUE,])
#all have huge errors around 0 except for three (460, 505, 550), only one of which is sig (460)
#460 has A LOT of data
mean(stats.df$total_N)
#460 has 1332 N, which is near the mean, so not more than
mean(stats.df$n.steps) #avg is 24, 460 has 26 and 505 has 6

df.means.trim %>%
    filter(tsID %in% chk.ts) %>%
ggplot(aes(abs.dist.mean, trait_var,
           group = factor(tsID), fill = factor(tsID), col = factor(tsID))) +
    geom_point() +
    geom_smooth(method = "lm") +
    plot.theme + 
    scale_y_continuous(name = expression(log~Variation),
                       trans = 'log',
                       breaks = trans_breaks('log', function(x) exp(x)),
                       labels = trans_format('log', math_format(exp(.x)))) +
    scale_x_continuous(name = expression(Distance~from~log~Mean),
                       trans = 'log',
                       breaks = trans_breaks('log', function(x) exp(x)),
                       labels = trans_format('log', math_format(exp(.x))))
#505 really stands out, only 6 time steps

##### CI OF EACH TIME STEP -----
#mean +/- z (1.96 for 95% CI) * sigma/sqrt(n)
df.means.trim$sd <- sqrt(df.means.trim$trait_var)
df.means.trim$ci.min <- df.means.trim$trait_mean - 1.96*(df.means.trim$sd/sqrt(df.means.trim$N))
df.means.trim$ci.max <- df.means.trim$trait_mean + 1.96*(df.means.trim$sd/sqrt(df.means.trim$N))

#Do these overlap with the tot mean?
df.means.trim$overlap.mean <- df.means.trim$ci.min <= df.means.trim$tot.mean & df.means.trim$ci.max >= df.means.trim$tot.mean
table(df.means.trim$overlap.mean) #50/50

df.trim <- df.means.trim[df.means.trim$overlap.mean == FALSE,]
samp.trim <- df.trim %>%
    group_by(tsID) %>%
    summarise(n = n())
keep.ts <- samp.trim$tsID[samp.trim$n >= 6]
df.trim <- df.trim[df.trim$tsID %in% keep.ts,]

#now let's look at slopes
glob.ci.lm.p <- ggplot(df.trim,
                    aes(abs.dist.mean, trait_var)) +
    #group = tsID, fill = tsID, col = tsID
    #fill = cat.dir, group = cat.dir, col = cat.dir
    geom_point() +
    geom_smooth(method = "lm") +
    plot.theme + 
    scale_y_continuous(name = expression(log~Variation),
                       trans = 'log',
                       breaks = trans_breaks('log', function(x) exp(x)),
                       labels = trans_format('log', math_format(exp(.x)))) +
    scale_x_continuous(name = expression(Distance~from~log~Mean),
                       trans = 'log',
                       breaks = trans_breaks('log', function(x) exp(x)),
                       labels = trans_format('log', math_format(exp(.x))))

glob.ci.lm = summary(lm(trait_var ~ abs.dist.mean, df.trim))
#sig, tight slope, positive (0.4); 15% var explained
#similar slope, more var explained (expected if removing noise)

ggsave(glob.ci.lm.p,
       file = "results/figures/global.ci.relationship.png",
       width = 20, height = 10, units = "cm")

glob.ci.lm$coefficients
glob.ci.slope.min = glob.ci.lm$coefficients[2] - glob.ci.lm$coefficients[4]
glob.ci.slope.max = glob.ci.lm$coefficients[2] + glob.ci.lm$coefficients[4]

stats.ci.cols <- c("tsID", "n.steps", "age.range", "tot.mean", "total_N",
                   "slope", "slope.se", "slope.p", "adj.r.sq", "df")
stats.ci.df <- data.frame(matrix(ncol = length(stats.ci.cols), nrow = length(unique(df.trim$tsID))))
colnames(stats.ci.df) <- stats.ci.cols
for(i in 1:length(unique(df.trim$tsID))){
    sub.df = df.trim[df.trim$tsID == unique(df.trim$tsID)[i],]
    x <- summary(lm(trait_var ~ abs.dist.mean,
                    sub.df))
    stats.ci.df$tsID[i] = unique(sub.df$tsID)
    stats.ci.df$n.steps[i] = nrow(sub.df)
    stats.ci.df$age.range[i] = max(sub.df$age_MY) - min(sub.df$age_MY)
    stats.ci.df$tot.mean[i] = unique(sub.df$tot.mean)
    stats.ci.df$total_N[i] = unique(sub.df$total_N)
    stats.ci.df$slope[i] = as.numeric(format(x$coefficients[2], scientific = FALSE))
    stats.ci.df$slope.se[i] = as.numeric(format(x$coefficients[4], scientific = FALSE))
    stats.ci.df$slope.p[i] = as.numeric(format(x$coefficients[8], scientific = FALSE))
    stats.ci.df$adj.r.sq[i] = x$adj.r.squared
    stats.ci.df$df[i] = x$df[2]
}
stats.ci.df$sig <- stats.ci.df$slope.p <= 0.005
stats.ci.df$slope.min = stats.ci.df$slope - stats.ci.df$slope.se
stats.ci.df$slope.max = stats.ci.df$slope + stats.ci.df$slope.se
stats.ci.df$overlap.slope <- stats.ci.df$slope.min <= glob.ci.slope.min & stats.ci.df$slope.max >= glob.ci.slope.max

glob.ci.stats <- c("global", nrow(df.trim), 
                   max(df.trim$age_MY) - min(df.trim$age_MY),
                   mean(df.trim$tot.mean),
                   sum(stats.ci.df$total_N),
                   glob.ci.lm$coefficients[2], glob.ci.lm$coefficients[4],
                   as.numeric(format(glob.ci.lm$coefficients[8], scientific = FALSE)),
                   glob.ci.lm$adj.r.squared, glob.ci.lm$df[2],
                   as.numeric(format(glob.ci.lm$coefficients[8], scientific = FALSE)) <= 0.005,
                   glob.ci.slope.min, glob.ci.slope.max,
                   glob.ci.slope.min <= glob.ci.slope.min & glob.ci.slope.max >= glob.ci.slope.max)

stats.ci.df.with.glob <- rbind(stats.ci.df, glob.ci.stats)

write.csv(stats.ci.df.with.glob,
          "results/slope.ci.results.csv",
          row.names = FALSE)

#how many overlap?
nrow(stats.ci.df[stats.ci.df$overlap.slope == TRUE,])/nrow(stats.ci.df) #1% (even fewer!)
#which are driving it?
nrow(stats.ci.df[stats.ci.df$overlap.slope == TRUE,]) #1
stats.ci.df$tsID[stats.ci.df$overlap.slope == TRUE] #78
df.trim[df.trim$tsID == 78,]
stats.ci.df[stats.ci.df$tsID == 78,] #nonsig

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
