setwd("C:/Users/jeane/Documents/HSLU/HS21/Customer Analytics/data_hs21")

library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)      # manipulating dates
library(tidyr)
library(directlabels)   # add labels within plot
library(gridExtra)      # draw multiple plots with ggplot2

##### Read in data files #####
for(i in 0:18) {
  oname = paste("article_", i, sep="")
  dfname = paste("df", i, sep="")
  assign(dfname, read.csv(paste("./train/", oname, ".csv", sep="")))
}

### Combine all dfs ##
df.list <- list(df0, df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18)
df.all <- bind_rows(df.list, .id="id")                                         # bind all df

### update id to match df source ###
for(i in 1:19) {                                                               # update id to match df
  df.all$id[df.all$id==i] <- i-1
}

### update data types ###
df.all$id <- factor(df.all$id, levels=c(0:18))                                 # convert id to factor
df.all$ts <- as.POSIXct(df.all$yrwk_start)
df.all$yrwk_start <- as.Date(df.all$yrwk_start, format="%Y-%m-%d")             # convert date format
df.all$yrwk_end <- as.Date(df.all$yrwk_end, format="%Y-%m-%d")
df.all$logSales <- log(df.all$sales)

df.all$category_1 <- as.factor(df.all$category_1)                              # convert to factor
df.all$category_2 <- as.factor(df.all$category_2)

### add size column ###
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

df.all <- df.all %>%
  mutate(
    size = substrRight(article_name, 4)
  )

table(df.all$size)                                                             # look at size groups

df.all$size[df.all$size == "CLAS"] <- "NA"                                     # replace unknown sizes with NA
df.all$size[df.all$size == "E NA"] <- "NA"

table(df.all$size)   

### update data type ###
df.all$article_name <- as.factor(df.all$article_name)                          # must be converted after size is stripped
df.all$size <- as.factor(df.all$size)

E## Cleanup Environment ##E
for(i in 0:18) {
  dfname = paste("df", i, sep="")
  dfs <- vector()
  dfs <- append(dfs, dfname)
  rm(list = dfs)
}

rm(oname, dfname, dfs, i, df.list, substrRight)

### Summarize Promos ###
df.all <- df.all %>% 
  group_by(id, yrwk_start) %>%
  mutate(num_promos = sum(promo_01, promo_02, promo_03, promo_04, promo_05))

table(df.all$num_promos)                                                       # look at number of simultaneous promos (per product)

df.all.nopromos <- df.all %>%
  filter(num_promos == 0) %>%
  mutate(no_promo = 1)

# check which combos are offered
df.all %>% filter(num_promos == 2) %>% group_by(promo_01, promo_02, promo_03, promo_04, promo_05) %>% summarise(n())

df.all.2promos.13 <- df.all %>%
  filter(num_promos == 2, promo_01 == 1, promo_03 == 1) %>%
  mutate(promo_01 = 0, promo_03 = 0, promo_1_3 = 1)

df.all.2promos.15 <- df.all %>%
  filter(num_promos == 2, promo_01 == 1, promo_05 == 1) %>%
  mutate(promo_01 = 0, promo_05 = 0, promo_1_5 = 1)

df.all.2promos.35 <- df.all %>%
  filter(num_promos == 2, promo_03 == 1, promo_05 == 1) %>%
  mutate(promo_03 = 0, promo_05 = 0, promo_3_5 = 1)

df.all.2promos <- full_join(df.all.2promos.13, df.all.2promos.15) %>% full_join(., df.all.2promos.35)

# check which combos are offered
df.all %>% filter(num_promos == 3) %>% group_by(promo_01, promo_02, promo_03, promo_04, promo_05) %>% summarise(n())

df.all.3promos <- df.all %>%
  filter(num_promos == 3) %>%
  mutate(promo_01 = 0, promo_03 = 0, promo_05 = 0, promos_1_3_5 = 1)

df.all.bind <- bind_rows(df.all %>% filter(num_promos == 1), df.all.nopromos, df.all.2promos, df.all.3promos)
df.all.bind <- df.all.bind[order(df.all.bind$id, df.all.bind$yrwk_start),]     # sort by id then by yrwk_start
df.all.bind[is.na(df.all.bind)] = 0                                            # fill in all NAs with 0

df.all.rebind <- bind_cols(
  df.all.bind[, 1:3], 
  ts = df.all.bind$ts, 
  df.all.bind[, 4:8],
  logSales = df.all.bind$logSales,
  df.all.bind[, 9:10],
  df.all.bind[, 18],
  no_promo = df.all.bind$no_promo,
  df.all.bind[, 11:15],
  promo_1_3 = df.all.bind$promo_1_3,
  promo_1_5 = df.all.bind$promo_1_5,
  promo_3_5 = df.all.bind$promo_3_5,
  promos_1_3_5 = df.all.bind$promos_1_3_5,
  num_promos = df.all.bind$num_promos)

### Pivot df ###
df.all.rebind.long <- pivot_longer(df.all.rebind, 
                               cols = c(promo_01, promo_02, promo_03, promo_04, promo_05, promo_1_3, promo_1_5, promo_3_5, promos_1_3_5, no_promo), ## variables that are to be put as results
                               names_to = "promo", ## new column with name of sport
                               values_to = "applied") ## new column with value (unquoted also works)

df.all.final <- df.all.rebind.long %>%
  filter(applied != 0) %>%
  select(., -c(num_promos, promo, applied), promo, num_promos)

df.all.final$promo <- factor(df.all.final$promo)

### Cleanup Environment ###
rm(df.all, df.all.2promos, df.all.2promos.13, df.all.2promos.15, df.all.2promos.35, df.all.3promos, df.all.bind, df.all.nopromos, df.all.rebind.long)


##### Data Analysis #####
ggplot(data=df.all.final, aes(x=ts, y=sales, group=article_name)) +
  labs(x="Year", y="Sales (units)", colour="Article") +
  geom_line(aes(col=article_name)) +
  scale_x_datetime(date_labels = "%Y") +
  theme(legend.text=element_text(size=7))

# split the plots
ggplot(data=df.all.final, aes(x=ts, y=sales, group=article_name)) +
  labs(x = "Year", y = "Sales (units)", colour="Article") +
  geom_line(aes(col=article_name)) +
  scale_x_datetime(date_labels = "%Y") +
  facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE)) +
  theme(strip.text=element_text(size=7), legend.text=element_text(size=7))

### Q. Is there seasonality? ###
ggplot(df.all.final, aes(x=factor(month), y=sales)) +
  geom_boxplot(aes(col=article_name), outlier.alpha = 0.1) +
  labs(x="Month", y="Sales (units)", colour="Article") +
  facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE)) +
  theme(strip.text=element_text(size=7), legend.text=element_text(size=7))
# Analysis: 
#   - spread for 7 (BIFIDUS JOGH. NATURE 150G), 8 (VALFLORA CREME FRAICHE NA), 11 (M-CLAS JOG. NATURE 200G), 13 (M-CLAS JOG. SCHOKOLA 200G), 14 (M-CLAS JOGHURT MOKKA 200G) seem to be bigger than other articles
#   - median for 13 (M-CLAS JOG. SCHOKOLA 200G) & 14 (M-CLAS JOGHURT MOKKA 200G) are much higher
#   - median for 7 (BIFIDUS JOGH. NATURE 150G), 8 (VALFLORA CREME FRAICHE NA), 11 (M-CLAS JOG. NATURE 200G) are also slightly higher than others
#   - there seems to be some sort of seasonality (i.e. some months are higher than others)


### Q. Is there YoY growth (trend)? ###
df.all.final %>%
  group_by(article_name, year) %>%
  mutate(mean_sales = mean(sales)) %>%
  select(article_name, year, mean_sales) %>%
  distinct() %>%
  ggplot(., aes(x=year, y=mean_sales)) + 
    labs(x="Year", y="Mean Sales (units)", colour="Article") +
    geom_line(aes(col=article_name)) + 
    geom_point(aes(col=article_name)) +
    theme(axis.text.x=element_text(color="black", size=11, angle=50, vjust=.8, hjust=0.8), strip.text=element_text(size=7), legend.text=element_text(size=7)) +
    facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE))
# Analysis:
#   - here we see again that mean sales is higher for 7 (BIFIDUS JOGH. NATURE 150G), 8 (VALFLORA CREME FRAICHE NA), 11 (M-CLAS JOG. NATURE 200G), 13 (M-CLAS JOG. SCHOKOLA 200G), 14 (M-CLAS JOGHURT MOKKA 200G)
#   - slight positive trend for some articles; article 8 (VALFLORA CREME FRAICHE NA) showed a strong increase in 2020 & 2021
#   - slight downward trend for 11 (M-CLAS JOG. NATURE 200G)


### Q. How do the number of promos affect sales? ###
ggplot(df.all.final, aes(x=factor(num_promos), y=sales)) +  
  labs(x = "Number of Promos", y = "Sales (units)", colour="Article") +
  geom_boxplot(aes(col=article_name), outlier.alpha = 0.1) +
  theme(strip.text=element_text(size=7), legend.text=element_text(size=7)) +
  facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE))
# Analysis: 
#   single promo
#   - increases spread & sales for 4 (EXC JOGHURT TRUFFES 150G), 7 (BIFIDUS JOGH. NATURE 150G), 8 (VALFLORA CREME FRAICHE NA)
#   - increases spread, but not really sales for 13 (M-CLAS JOG. SCHOKOLA 200G), 14 (M-CLAS JOGHURT MOKKA 200G)
#   2 promos
#   - increases spread & sales for 4 (EXC JOGHURT TRUFFES 150G), 8 (VALFLORA CREME FRAICHE NA), 13 (M-CLAS JOG. SCHOKOLA 200G), 14 (M-CLAS JOGHURT MOKKA 200G)
#   triple promo 
#   - small spread but increased sales for 5 (YOGOS GRECQUE NATURE 180G), 13 (M-CLAS JOG. SCHOKOLA 200G), 14 (M-CLAS JOGHURT MOKKA 200G)
#   - didn't really affect article 3 (BIO JOGHURT NATURE 180G ) (this is also seen in the other plot)


### Q. Which promos are applied? ###
promo_labs <- c("None", "1", "3", "4", "5", "1&3", "1&5", "3&5", "1,3&5")      # create a x-axis label

df.all.rebind %>% group_by(article_name, id) %>% summarise(nopromo = sum(no_promo), 
                                             promo1 = sum(promo_01), 
                                             promo2 = sum(promo_02), 
                                             promo3 = sum(promo_03), 
                                             promo4 = sum(promo_04), 
                                             promo5 = sum(promo_05), 
                                             promo13 = sum(promo_1_3), 
                                             promo15 = sum(promo_1_5), 
                                             promo35 = sum(promo_3_5), 
                                             promo135 = sum(promos_1_3_5)) %>%
  write.csv(., "df.promos.pivot.csv")
# here we see promo_02 is never applied to any articles, so we exclude from the labels
# also, promo_1_5, _3_5, and _1_3_5 are not applied very frequently

df.all.final %>% 
  group_by(article_name, promo) %>% summarise(count = n()) %>% 
  select(article_name, promo, count) %>%
  ggplot(., aes(x=promo, y=count)) +
    labs(x="Promo", y="Count", fill="Article") +
    ylim(0, 300) +
    geom_bar(aes(fill=article_name), stat="identity") +
    geom_text(stat="identity", aes(label=count), size=2.5, vjust=-0.5) +
    scale_x_discrete(labels=promo_labs) +
    theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), strip.text=element_text(size=7), legend.text=element_text(size=7)) +
    facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE))


### Q. How does each promo affect sales? ###
ggplot(df.all.final, aes(x=promo, y=sales)) +  
  labs(x="Promo", y="Sales (units)", colour="Article") +
  geom_boxplot(aes(col=article_name), outlier.alpha = 0.1) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), strip.text=element_text(size=7), legend.text=element_text(size=7)) +
  scale_x_discrete(labels=promo_labs) +
  facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE))
# Analysis:
#   promo_01 
#   - increases spread & higher median for: 0, 2, 3, 4, 6, 7, 8
#   - increases spread, but median about the same: 13, 14
#   promo_03 
#   - increases spread & median: 8, 11 (only median), 13, 14
#   - in general does not have much effect for most products
#   - decreases spread & median for 5
#   promo_04 (only applied to 0, 3, 4, 6 => exclude from regression for other articles)
#   promo_05 (only applied to 2, 4, 7, 8, 10, 12, 13, 14, 15, 16 => exclude from regression for other articles)
#   promo_1_3 (applied to all articles)
#     - increases spread for 3, 4, 6, 8, 14 but does not seem to increase median sales
#   promo_1_5 (only applied to 2, 3, 8 => exclude from regression for other articles)
#   promo_3_5 (only applied to 3, 5, 8, 13, 14 => exclude from regression for other articles)
#   promos_1_3_5 (only applied to 3, 5, 13, 14 => exclude from regression for other articles)


### Q. How does category_1 affect sales? ###
ggplot(df.all.final, aes(x=category_1, y=sales)) +
  labs(x="Category 1", y="Sales (units)", colour="Category 1") +
  geom_boxplot(aes(col=category_1), outlier.alpha = 0.1) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), strip.text=element_text(size=7), legend.text=element_text(size=7))

df.all.final %>% group_by(category_1) %>% select(category_1, id, article_name) %>% distinct() %>% arrange(category_1) %>% write.csv(., "yoghurt.category.csv")


### Q. How does category_2 affect sales? ###
ggplot(df.all.final, aes(x=category_2, y=sales)) +
  labs(x="Category 2", y="Sales (units)", colour="Category 2") +
  geom_boxplot(aes(col=category_2), outlier.alpha = 0.1) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), strip.text=element_text(size=7), legend.text=element_text(size=7))

df.all.final %>% group_by(category_2) %>% select(category_2, id, article_name) %>% distinct() %>% arrange(category_2) %>% write.csv(., "yoghurt.category2.csv")


### Q. How does size affect sales? ###
ggplot(df.all.final, aes(x=size, y=sales)) +
  labs(x="Size", y="Sales (units)", colour="Size") +
  geom_boxplot(aes(col=size), outlier.alpha = 0.1) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), strip.text=element_text(size=7), legend.text=element_text(size=7))

df.all.final %>% group_by(size) %>% select(size, id, article_name) %>% distinct() %>% arrange(size) %>% write.csv(., "yoghurt.size.csv")


### Q: What is the spread of sales? ###
ggplot(df.all.final, aes(x=article_name, y=sales)) +
  labs(x="Article", y="Sales (units)", colour="Article") +
  geom_boxplot(aes(col=article_name), outlier.alpha = 0.1) +
  theme(axis.text.x=element_text(color = "black", size=8.5, angle=55, vjust=.8, hjust=0.8), strip.text=element_text(size=7), legend.text=element_text(size=7))

mean(df.all.final$sales)     # 9393

df.all.final %>%
  group_by(id) %>%
  summarise(mean = mean(sales)) %$% .[order(mean, decreasing=TRUE),]

### Alternate view of promo effect ###
ggplot(df.all.final, aes(x=promo, y=sales, color=paste(article_name, " (", id, ")", sep=""), group=article_name)) +
  labs(x="Promo", y="Sales (units)", colour="Article (ID)") +
  scale_x_discrete(labels=promo_labs, drop=FALSE) +
  geom_dl(aes(label=id), method=list(dl.trans(x=x-0.2), "first.points", cex=0.8)) +
  geom_dl(aes(label=id), method=list(dl.trans(x=x+0.2), "last.points", cex=0.8)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=55, vjust=.8, hjust=0.8), strip.text=element_text(size=7), legend.text=element_text(size=7)) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(fun=mean, geom="line")

# replot, grouping articles that have similar reactions to promotions
plot1 <- ggplot(df.all.final %>% filter(id %in% c(0, 6, 13, 14, 10, 12, 15, 16, 18)), aes(x=promo, y=sales, color=paste(article_name, " (", id, ")", sep=""), group=article_name)) +
  labs(x="Promo", y="Sales (unit)", colour="Article (ID)") +
  ylim(3000, 33000) +
  scale_x_discrete(labels=promo_labs, drop=FALSE) +
  geom_dl(aes(label=id), method=list(dl.trans(x=x+0.2), "last.points", cex=0.8)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=55, vjust=.8, hjust=0.8)) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(fun=mean, geom="line")

plot2 <- ggplot(df.all.final %>% filter(id %in% c(1, 9, 11, 17)), aes(x=promo, y=sales, color=paste(article_name, " (", id, ")", sep=""), group=article_name)) +
  labs(x="Promo", y="Sales (unit)", colour="Article (ID)") +
  ylim(3000, 15000) +
  scale_x_discrete(labels=promo_labs, drop=FALSE) +
  geom_dl(aes(label=id), method=list(dl.trans(x=x+0.2), "last.points", cex=0.8)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=55, vjust=.8, hjust=0.8)) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(fun=mean, geom="line")

plot3 <- ggplot(df.all.final %>% filter(id %in% c(2, 3, 5, 8, 4, 7)), aes(x=promo, y=sales, color=paste(article_name, " (", id, ")", sep=""), group=article_name)) +
  labs(x="Promo", y="Sales (unit)", colour="Article (ID)") +
  ylim(3000, 17500) +
  scale_x_discrete(labels=promo_labs, drop=FALSE) +
  geom_dl(aes(label=id), method=list(dl.trans(x=x+0.2), "last.points", cex=0.8)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=55, vjust=.8, hjust=0.8)) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(fun=mean, geom="line")

grid.arrange(plot1, plot2, plot3, nrow=3)
# Analysis: 
#   - 1, 9, 17 are relatively horizontal => promos don't really have much effect
#   - interactivity between lines indicate combined effect between id & promos
#   - some lines are parallel indicating id doesn't play an effect, however there is additivity: (13, 14), (0, 6), (10, 12, 15, 16, 18)
#   - additional IVs are at play here

##### Regression #####
### Step-by-step forward method ###
reg <- regsubsets(sales ~ ., data=df.all.final %>% select(id, month, week, sales, category_1, category_2, size, promo), method="forward", nvmax=20)
reg.sum <- summary(reg)
reg.sum$which %>% write.csv(., "reg.sum.csv")

for(i in 0:18) {
  reg <- regsubsets(logSales ~ ., data=df.all.final %>% filter(id==i) %>% select(month, week, logSales, promo), method="forward", nvmax=10)
  reg.sum <- summary(reg)
  oname = paste("./regsumcheck/reg.sum.", i, ".csv", sep="")
  reg.sum$which %>% write.csv(., oname)
}

colors <- c("orig"="blue", "pred"="red")

for(i in 0:18) {
  assign(paste("train.", i, sep=""), df.all.rebind %>% filter(id==i, ts < "2021-01-01"))
  assign(paste("test.", i, sep=""), df.all.rebind %>% filter(id==i, ts >= "2021-01-01"))
}


# --- Article 0 ---
fit.0 <- lm(logSales ~ promo_01 + promo_04 + month + promo_1_3 + promo_03 + week, data=train.0)
summary(fit.0)
#fit.0.interact <- lm(logSales ~ promo_01 * promo_04 * month * promo_1_3 * promo_03 * week, data=train.0)
#summary(fit.0.interact)
#fit.0.mix <- lm(logSales ~ (promo_01 + promo_04 + promo_1_3 + promo_03) * week * month, data=df.all.rebind)
#summary(fit.0.mix)

# Validate simple Model
fit.0.pred <- predict(fit.0, newdata=test.0)
res.0 <- exp(fit.0.pred) - test.0$sales
ape.0 <- abs(res.0) / test.0$sales # Percentage Error
mean(ape.0) # Mean absolute percentage error                                   # 0.18

# Plot prediction Error
plot(test.0$ts, ape.0, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.0 <- data.frame(article_name=test.0$article_name, ts=test.0$ts, art.0_pred = exp(fit.0.pred))

# this is the predicted line of multiple linear regression
plot0 <- ggplot(data=test.0, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.0, aes(x=ts, y=art.0_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.0$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")

# --- Article 1 ---
fit.1 <- lm(logSales ~ month + promo_01 + promo_03 + week + promo_1_3, data=train.1)
summary(fit.1)

# Validate simple Model
fit.1.pred <- predict(fit.1, newdata=test.1)
res.1 <- exp(fit.1.pred) - test.1$sales
ape.1 <- abs(res.1) / test.1$sales # Percentage Error
mean(ape.1) # Mean absolute percentage error                                   # 0.05

# Plot prediction Error
plot(test.1$ts, ape.1, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.1 <- data.frame(article_name=test.1$article_name, ts=test.1$ts, art.1_pred = exp(fit.1.pred))

# this is the predicted line of multiple linear regression
plot1 <- ggplot(data=test.1, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.1, aes(x=ts, y=art.1_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.1$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 2 ---
fit.2 <- lm(logSales ~ promo_01 + month + promo_1_5 + promo_1_3 + promo_05 + week + promo_03, data=train.2)
summary(fit.2)

# Validate simple Model
fit.2.pred <- predict(fit.2, newdata=test.2)
res.2 <- exp(fit.2.pred) - test.2$sales
ape.2 <- abs(res.2) / test.2$sales # Percentage Error
mean(ape.2) # Mean absolute percentage error                                   # 0.20

# Plot prediction Error
plot(test.2$ts, ape.2, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.2 <- data.frame(article_name=test.2$article_name, ts=test.2$ts, art.2_pred = exp(fit.2.pred))

# this is the predicted line of multiple linear regression
plot2 <- ggplot(data=test.2, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.2, aes(x=ts, y=art.2_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.2$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 3 ---
fit.3 <- lm(logSales ~ promo_01 + month + promo_04 + promo_1_3 + promo_03 + week + promo_1_5 + promos_1_3_5 + promo_3_5, data=train.3)
summary(fit.3)

# Validate simple Model
fit.3.pred <- predict(fit.3, newdata=test.3)
res.3 <- exp(fit.3.pred) - test.3$sales
ape.3 <- abs(res.3) / test.3$sales # Percentage Error
mean(ape.3) # Mean absolute percentage error                                   # 0.06

# Plot prediction Error
plot(test.3$ts, ape.3, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.3 <- data.frame(article_name=test.3$article_name, ts=test.3$ts, art.3_pred = exp(fit.3.pred))

# this is the predicted line of multiple linear regression
plot3 <- ggplot(data=test.3, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.3, aes(x=ts, y=art.3_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.3$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 4 ---
fit.4 <- lm(logSales ~ promo_01 + promo_04 + promo_1_3 + week + promo_03 + month + promo_05, data=train.4)
summary(fit.4)

# Validate simple Model
fit.4.pred <- predict(fit.4, newdata=test.4)
res.4 <- exp(fit.4.pred) - test.4$sales
ape.4 <- abs(res.4) / test.4$sales # Percentage Error
mean(ape.4) # Mean absolute percentage error                                   # 0.17

# Plot prediction Error
plot(test.4$ts, ape.4, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.4 <- data.frame(article_name=test.4$article_name, ts=test.4$ts, art.4_pred = exp(fit.4.pred))

# this is the predicted line of multiple linear regression
plot4 <- ggplot(data=test.4, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.4, aes(x=ts, y=art.4_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.4$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 5 ---
fit.5 <- lm(logSales ~ month + promos_1_3_5 + promo_1_3 + promo_03 + week + promo_01 + promo_3_5, data=train.5)
summary(fit.5)

# Validate simple Model
fit.5.pred <- predict(fit.5, newdata=test.5)
res.5 <- exp(fit.5.pred) - test.5$sales
ape.5 <- abs(res.5) / test.5$sales # Percentage Error
mean(ape.5) # Mean absolute percentage error                                   # 0.22

# Plot prediction Error
plot(test.5$ts, ape.5, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.5 <- data.frame(article_name=test.5$article_name, ts=test.5$ts, art.5_pred = exp(fit.5.pred))

# this is the predicted line of multiple linear regression
plot5 <- ggplot(data=test.5, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.5, aes(x=ts, y=art.5_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.5$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 6 ---
fit.6 <- lm(logSales ~ promo_01 + month + promo_04 + promo_1_3 + promo_03 + week, data=train.6)
summary(fit.6)

# Validate simple Model
fit.6.pred <- predict(fit.6, newdata=test.6)
res.6 <- exp(fit.6.pred) - test.6$sales
ape.6 <- abs(res.6) / test.6$sales # Percentage Error
mean(ape.6) # Mean absolute percentage error                                   # 0.09

# Plot prediction Error
plot(test.6$ts, ape.6, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.6 <- data.frame(article_name=test.6$article_name, ts=test.6$ts, art.6_pred = exp(fit.6.pred))

# this is the predicted line of multiple linear regression
plot6 <- ggplot(data=test.6, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.6, aes(x=ts, y=art.6_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.6$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 7 ---
fit.7 <- lm(logSales ~ promo_01 + month + promo_05 + promo_1_3 + promo_03 + week, data=train.7)
summary(fit.7)

# Validate simple Model
fit.7.pred <- predict(fit.7, newdata=test.7)
res.7 <- exp(fit.7.pred) - test.7$sales
ape.7 <- abs(res.7) / test.7$sales # Percentage Error
mean(ape.7) # Mean absolute percentage error                                   # 0.05

# Plot prediction Error
plot(test.7$ts, ape.7, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.7 <- data.frame(article_name=test.7$article_name, ts=test.7$ts, art.7_pred = exp(fit.7.pred))

# this is the predicted line of multiple linear regression
plot7 <- ggplot(data=test.7, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.7, aes(x=ts, y=art.7_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.7$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 8 ---
fit.8 <- lm(logSales ~ promo_01 + promo_03 + promo_05 + promo_1_3 + promo_1_5 + week + month + promo_3_5, data=train.8)
summary(fit.8)

# Validate simple Model
fit.8.pred <- predict(fit.8, newdata=test.8)
res.8 <- exp(fit.8.pred) - test.8$sales
ape.8 <- abs(res.8) / test.8$sales # Percentage Error
mean(ape.8) # Mean absolute percentage error                                   # 0.31

# Plot prediction Error
plot(test.8$ts, ape.8, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.8 <- data.frame(article_name=test.8$article_name, ts=test.8$ts, art.8_pred = exp(fit.8.pred))

# this is the predicted line of multiple linear regression
plot8 <- ggplot(data=test.8, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.8, aes(x=ts, y=art.8_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.8$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 9 ---
fit.9 <- lm(logSales ~ month + promo_03 + promo_01 + week + promo_1_3, data=train.9)
summary(fit.9)

# Validate simple Model
fit.9.pred <- predict(fit.9, newdata=test.9)
res.9 <- exp(fit.9.pred) - test.9$sales
ape.9 <- abs(res.9) / test.9$sales # Percentage Error
mean(ape.9) # Mean absolute percentage error                                   # 0.09

# Plot prediction Error
plot(test.9$ts, ape.9, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.9 <- data.frame(article_name=test.9$article_name, ts=test.9$ts, art.9_pred = exp(fit.9.pred))

# this is the predicted line of multiple linear regression
plot9 <- ggplot(data=test.9, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.9, aes(x=ts, y=art.9_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.9$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 10 ---
fit.10 <- lm(logSales ~ week + promo_1_3 + month + promo_05 + promo_01 + promo_03, data=train.10)
summary(fit.10)

# Validate simple Model
fit.10.pred <- predict(fit.10, newdata=test.10)
res.10 <- exp(fit.10.pred) - test.10$sales
ape.10 <- abs(res.10) / test.10$sales # Percentage Error
mean(ape.10) # Mean absolute percentage error                                  # 0.07

# Plot prediction Error
plot(test.10$ts, ape.10, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.10 <- data.frame(article_name=test.10$article_name, ts=test.10$ts, art.10_pred = exp(fit.10.pred))

# this is the predicted line of multiple linear regression
plot10 <- ggplot(data=test.10, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.10, aes(x=ts, y=art.10_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.10$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 11 ---
fit.11 <- lm(logSales ~ week + promo_01 + promo_03 + promo_1_3 + month, data=train.11)
summary(fit.11)

# Validate simple Model
fit.11.pred <- predict(fit.11, newdata=test.11)
res.11 <- exp(fit.11.pred) - test.11$sales
ape.11 <- abs(res.11) / test.11$sales # Percentage Error
mean(ape.11) # Mean absolute percentage error                                  # 0.15

# Plot prediction Error
plot(test.11$ts, ape.11, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.11 <- data.frame(article_name=test.11$article_name, ts=test.11$ts, art.11_pred = exp(fit.11.pred))

# this is the predicted line of multiple linear regression
plot11 <- ggplot(data=test.11, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.11, aes(x=ts, y=art.11_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.11$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 12 ---
fit.12 <- lm(logSales ~ week + promo_1_3 + promo_01 + promo_03 + promo_05 + month, data=train.12)
summary(fit.12)

# Validate simple Model
fit.12.pred <- predict(fit.12, newdata=test.12)
res.12 <- exp(fit.12.pred) - test.12$sales
ape.12 <- abs(res.12) / test.12$sales # Percentage Error
mean(ape.12) # Mean absolute percentage error                                  # 0.06

# Plot prediction Error
plot(test.12$ts, ape.12, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.12 <- data.frame(article_name=test.12$article_name, ts=test.12$ts, art.12_pred = exp(fit.12.pred))

# this is the predicted line of multiple linear regression
plot12 <- ggplot(data=test.12, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.12, aes(x=ts, y=art.12_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.12$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 13 ---
fit.13 <- lm(logSales ~ week + promos_1_3_5 + promo_1_3 + month + promo_3_5 + promo_05 + promo_03 + promo_01, data=train.13)
summary(fit.13)

# Validate simple Model
fit.13.pred <- predict(fit.13, newdata=test.13)
res.13 <- exp(fit.13.pred) - test.13$sales
ape.13 <- abs(res.13) / test.13$sales # Percentage Error
mean(ape.13) # Mean absolute percentage error                                  # 0.10

# Plot prediction Error
plot(test.13$ts, ape.13, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.13 <- data.frame(article_name=test.13$article_name, ts=test.13$ts, art.13_pred = exp(fit.13.pred))

# this is the predicted line of multiple linear regression
plot13 <- ggplot(data=test.13, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.13, aes(x=ts, y=art.13_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.13$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 14 ---
fit.14 <- lm(logSales ~ week + promos_1_3_5 + month + promo_1_3 + promo_05 + promo_3_5 + promo_03 + promo_01, data=train.14)
summary(fit.14)

# Validate simple Model
fit.14.pred <- predict(fit.14, newdata=test.14)
res.14 <- exp(fit.14.pred) - test.14$sales
ape.14 <- abs(res.14) / test.14$sales # Percentage Error
mean(ape.14) # Mean absolute percentage error                                  # 0.10

# Plot prediction Error
plot(test.14$ts, ape.14, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.14 <- data.frame(article_name=test.14$article_name, ts=test.14$ts, art.14_pred = exp(fit.14.pred))

# this is the predicted line of multiple linear regression
plot14 <- ggplot(data=test.14, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.14, aes(x=ts, y=art.14_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.14$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 15 ---
fit.15 <- lm(logSales ~ week + promo_1_3 + month + promo_05 + promo_01 + promo_03, data=train.15)
summary(fit.15)

# Validate simple Model
fit.15.pred <- predict(fit.15, newdata=test.15)
res.15 <- exp(fit.15.pred) - test.15$sales
ape.15 <- abs(res.15) / test.15$sales # Percentage Error
mean(ape.15) # Mean absolute percentage error                                  # 0.09

# Plot prediction Error
plot(test.15$ts, ape.15, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.15 <- data.frame(article_name=test.15$article_name, ts=test.15$ts, art.15_pred = exp(fit.15.pred))

# this is the predicted line of multiple linear regression
plot15 <- ggplot(data=test.15, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.15, aes(x=ts, y=art.15_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.15$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 16 ---
fit.16 <- lm(logSales ~ week + promo_1_3 + promo_05 + promo_01 + promo_03 + month, data=train.16)
summary(fit.16)

# Validate simple Model
fit.16.pred <- predict(fit.16, newdata=test.16)
res.16 <- exp(fit.16.pred) - test.16$sales
ape.16 <- abs(res.16) / test.16$sales # Percentage Error
mean(ape.16) # Mean absolute percentage error                                  # 0.06

# Plot prediction Error
plot(test.16$ts, ape.16, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.16 <- data.frame(article_name=test.16$article_name, ts=test.16$ts, art.16_pred = exp(fit.16.pred))

# this is the predicted line of multiple linear regression
plot16 <- ggplot(data=test.16, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.16, aes(x=ts, y=art.16_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.16$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 17 ---
fit.17 <- lm(logSales ~ week + promo_1_3 + month + promo_03 + promo_01, data=train.17)
summary(fit.17)

# Validate simple Model
fit.17.pred <- predict(fit.17, newdata=test.17)
res.17 <- exp(fit.17.pred) - test.17$sales
ape.17 <- abs(res.17) / test.17$sales # Percentage Error
mean(ape.17) # Mean absolute percentage error                                  # 0.08

# Plot prediction Error
plot(test.17$ts, ape.17, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.17 <- data.frame(article_name=test.17$article_name, ts=test.17$ts, art.17_pred = exp(fit.17.pred))

# this is the predicted line of multiple linear regression
plot17 <- ggplot(data=test.17, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data = pred.df.17, aes(x=ts, y=art.17_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.17$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")


# --- Article 18 ---
fit.18 <- lm(logSales ~ week + promo_1_3 + promo_01 + promo_03 + promo_05 + month, data=train.18)
summary(fit.18)

# Validate simple Model
fit.18.pred <- predict(fit.18, newdata=test.18)
res.18 <- exp(fit.18.pred) - test.18$sales
ape.18 <- abs(res.18) / test.18$sales # Percentage Error
mean(ape.18) # Mean absolute percentage error                                  # 0.08

# Plot prediction Error
plot(test.18$ts, ape.18, type = "l")

# save predictions of the model in the new data frame together with the variable you want to plot against
pred.df.18 <- data.frame(article_name=test.18$article_name, ts=test.18$ts, art.18_pred = exp(fit.18.pred))

# this is the predicted line of multiple linear regression
plot18 <- ggplot(data=test.18, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data=pred.df.18, aes(x=ts, y=art.18_pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), plot.title=element_text(size=7)) +
  labs(title=test.18$article_name, x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side")

grid.arrange(plot9, plot7, plot2, plot0, plot3, 
             plot6, plot4, plot15, plot16, plot18, 
             plot10, plot12, plot11, plot13, plot17, 
             plot14, plot1, plot8, plot5, nrow=5)

## replot as a single ggplot
train.all <- df.all.rebind %>% filter(ts < "2021-01-01")
test.all <- df.all.rebind %>% filter(ts >= "2021-01-01")

v.pred.df <- vector()
for(i in 0:18) {
  temp <- paste("pred.df.", i, sep="")
  v.pred.df <- c(v.pred.df, temp)
}
cat(paste(v.pred.df, collapse=", "), "\n")

pred.df <- bind_rows(pred.df.0, pred.df.1, pred.df.2, pred.df.3, pred.df.4, pred.df.5, pred.df.6, pred.df.7, pred.df.8, pred.df.9, pred.df.10, pred.df.11, pred.df.12, pred.df.13, pred.df.14, pred.df.15, pred.df.16, pred.df.17, pred.df.18, .id="id") %>%
  mutate(art.pred = coalesce(art.0_pred, art.1_pred, art.2_pred, art.3_pred, art.4_pred, art.5_pred, art.6_pred, art.7_pred, art.8_pred, art.9_pred, art.10_pred, art.11_pred, art.12_pred, art.13_pred, art.14_pred, art.15_pred, art.16_pred, art.17_pred, art.18_pred)) %>%
  select(id, article_name, ts, art.pred)

for(i in 1:19) {                                                               # update id to match df
  pred.df$id[pred.df$id==i] <- i-1
}

ggplot(data=test.all, aes(x=ts, y=sales)) + 
  geom_line(aes(color="orig")) +
  geom_line(data=pred.df, aes(x=ts, y=art.pred, color="pred")) +
  theme(axis.text.x=element_text(color = "black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), strip.text=element_text(size=7)) +
  labs(x="2021", y="sales (units)") +
  scale_color_manual(values=colors) + 
  guides(legend.position = "side") +
  facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE), scales="free_y")


# summarize all mape values
df.mean.ape <- data.frame()
for(i in 0:18) {
  newdf <- data.frame(i, round(mean(get(paste("ape.", i, sep=""))), 2))
  names(newdf) <- c("id", "mape")
  df.mean.ape <- rbind(df.mean.ape, newdf)
}

df.mean.ape$id <- factor(df.mean.ape$id)

df.mean.ape <- df.all.final %>% 
  select(id, article_name) %>% 
  distinct()  %>%
  full_join(., df.mean.ape, by="id")

write.csv(df.mean.ape, "df.mean.ape.csv")

##### Prediction #####
### Read in all test data files ###
for(i in 0:18) {
  oname = paste("article_", i, "_test", sep="")
  dfname = paste("df", i, sep="")
  assign(dfname, read.csv(paste("./test/", oname, ".csv", sep="")))
}

##### Combine all dfs #####
df.list <- list(df0, df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18)
df.all.predict <- bind_rows(df.list, .id="id")                                 # bind all df

### update id to match df source ###
for(i in 1:19) {                                                               # update id to match df
  df.all.predict$id[df.all.predict$id==i] <- i-1
}

### update data types ###
df.all.predict$id <- factor(df.all.predict$id, levels=c(0:18))                 # convert id to factor
df.all.predict$ts <- as.POSIXct(df.all.predict$yrwk_start)
df.all.predict$yrwk_start <- as.Date(df.all.predict$yrwk_start, format="%Y-%m-%d")             # convert date format
df.all.predict$yrwk_end <- as.Date(df.all.predict$yrwk_end, format="%Y-%m-%d")
df.all.predict$article_name <- as.factor(df.all.predict$article_name)                       
df.all.predict$category_1 <- as.factor(df.all.predict$category_1)                              # convert to factor
df.all.predict$category_2 <- as.factor(df.all.predict$category_2)

### Cleanup Environment ###
for(i in 0:18) {
  dfname = paste("df", i, sep="")
  dfs <- vector()
  dfs <- append(dfs, dfname)
  rm(list = dfs)
}

rm(oname, dfname, dfs, i, df.list)

### Summarize Promos ###
sapply(df.all.predict[, c("promo_1", "promo_2", "promo_3", "promo_4", "promo_5")], sum)
# promos 2, 3, and 5 are not offered

df.all.predict <- df.all.predict %>% 
  group_by(id, yrwk_start) %>%
  mutate(num_promos = sum(promo_1, promo_2, promo_3, promo_4, promo_5))

table(df.all.predict$num_promos)                                                       # look at number of simultaneous promos (per product)
# Note: no promos run multiple times

df.all.predict <- df.all.predict %>%
  mutate(
    promo_1_3 = 0,
    promo_1_5 = 0,
    promo_3_5 = 0,
    promos_1_3_5 = 0
  )

df.no.promo <- df.all.predict %>%
  filter(num_promos == 0) %>%
  mutate(no_promo = 1)

df.promo <- df.all.predict %>%
  filter(num_promos == 1) %>%
  mutate(no_promo = 0)

df.all.predict <- bind_rows(df.no.promo, df.promo)

colnames(df.all.predict) <- c("id", "article_name", "year", "yrwk_start", "yrwk_end", "month", "week",
                              "promo_01", "promo_02", "promo_03", "promo_04", "promo_05", "category_1", "category_2",
                              "ts", "num_promos", "promo_1_3", "promo_1_5", "promo_3_5", "promos_1_3_5", "no_promo")


# --- Article 0 ---
fit.0 <- lm(logSales ~ promo_01 + promo_04 + month + promo_1_3 + promo_03 + week, data=df.all.rebind %>% filter(id==0))

# prediction
fit.0.test.pred <- predict(fit.0, newdata=df.all.predict %>% filter(id==0))
pred.df.test.0 <- df.all.predict %>% filter(id==0) %>% 
  data.frame(art.0_pred=exp(fit.0.test.pred))


# --- Article 1 ---
fit.1 <- lm(logSales ~ month + promo_01 + promo_03 + week + promo_1_3, data=df.all.rebind %>% filter(id==1))

# prediction
fit.1.test.pred <- predict(fit.1, newdata=df.all.predict %>% filter(id==1))
pred.df.test.1 <- df.all.predict %>% filter(id==1) %>% 
  data.frame(art.1_pred=exp(fit.1.test.pred))


# --- Article 2 ---
fit.2 <- lm(logSales ~ promo_01 + month + promo_1_5 + promo_1_3 + promo_05 + week + promo_03, data=df.all.rebind %>% filter(id==2))

# prediction
fit.2.test.pred <- predict(fit.2, newdata=df.all.predict %>% filter(id==2))
pred.df.test.2 <- df.all.predict %>% filter(id==2) %>% 
  data.frame(art.2_pred=exp(fit.2.test.pred))


# --- Article 3 ---
fit.3 <- lm(logSales ~ promo_01 + month + promo_04 + promo_1_3 + promo_03 + week + promo_1_5 + promos_1_3_5 + promo_3_5, data=df.all.rebind %>% filter(id==3))

# prediction
fit.3.test.pred <- predict(fit.3, newdata=df.all.predict %>% filter(id==3))
pred.df.test.3 <- df.all.predict %>% filter(id==3) %>% 
  data.frame(art.3_pred=exp(fit.3.test.pred))


# --- Article 4 ---
fit.4 <- lm(logSales ~ promo_01 + promo_04 + promo_1_3 + week + promo_03 + month + promo_05, data=df.all.rebind %>% filter(id==4))

# prediction
fit.4.test.pred <- predict(fit.4, newdata=df.all.predict %>% filter(id==4))
pred.df.test.4 <- df.all.predict %>% filter(id==4) %>% 
  data.frame(art.4_pred=exp(fit.4.test.pred))


# --- Article 5 ---
fit.5 <- lm(logSales ~ month + promos_1_3_5 + promo_1_3 + promo_03 + week + promo_01 + promo_3_5, data=df.all.rebind %>% filter(id==5))

# prediction
fit.5.test.pred <- predict(fit.5, newdata=df.all.predict %>% filter(id==5))
pred.df.test.5 <- df.all.predict %>% filter(id==5) %>% 
  data.frame(art.5_pred=exp(fit.5.test.pred))


# --- Article 6 ---
fit.6 <- lm(logSales ~ promo_01 + month + promo_04 + promo_1_3 + promo_03 + week, data=df.all.rebind %>% filter(id==6))

# prediction
fit.6.test.pred <- predict(fit.6, newdata=df.all.predict %>% filter(id==6))
pred.df.test.6 <- df.all.predict %>% filter(id==6) %>% 
  data.frame(art.6_pred=exp(fit.6.test.pred))


# --- Article 7 ---
fit.7 <- lm(logSales ~ promo_01 + month + promo_05 + promo_1_3 + promo_03 + week, data=df.all.rebind %>% filter(id==7))

# prediction
fit.7.test.pred <- predict(fit.7, newdata=df.all.predict %>% filter(id==7))
pred.df.test.7 <- df.all.predict %>% filter(id==7) %>% 
  data.frame(art.7_pred=exp(fit.7.test.pred))


# --- Article 8 ---
fit.8 <- lm(logSales ~ promo_01 + promo_03 + promo_05 + promo_1_3 + promo_1_5 + week + month + promo_3_5, data=df.all.rebind %>% filter(id==8))

# prediction
fit.8.test.pred <- predict(fit.8, newdata=df.all.predict %>% filter(id==8))
pred.df.test.8 <- df.all.predict %>% filter(id==8) %>% 
  data.frame(art.8_pred=exp(fit.8.test.pred))


# --- Article 9 ---
fit.9 <- lm(logSales ~ month + promo_03 + promo_01 + week + promo_1_3, data=df.all.rebind %>% filter(id==9))

# prediction
fit.9.test.pred <- predict(fit.9, newdata=df.all.predict %>% filter(id==9))
pred.df.test.9 <- df.all.predict %>% filter(id==9) %>% 
  data.frame(art.9_pred=exp(fit.9.test.pred))


# --- Article 10 ---
fit.10 <- lm(logSales ~ week + promo_1_3 + month + promo_05 + promo_01 + promo_03, data=df.all.rebind %>% filter(id==10))

# prediction
fit.10.test.pred <- predict(fit.10, newdata=df.all.predict %>% filter(id==10))
pred.df.test.10 <- df.all.predict %>% filter(id==10) %>% 
  data.frame(art.10_pred=exp(fit.10.test.pred))


# --- Article 11 ---
fit.11 <- lm(logSales ~ week + promo_01 + promo_03 + promo_1_3 + month, data=df.all.rebind %>% filter(id==11))

# prediction
fit.11.test.pred <- predict(fit.11, newdata=df.all.predict %>% filter(id==11))
pred.df.test.11 <- df.all.predict %>% filter(id==11) %>% 
  data.frame(art.11_pred=exp(fit.11.test.pred))


# --- Article 12 ---
fit.12 <- lm(logSales ~ week + promo_1_3 + promo_01 + promo_03 + promo_05 + month, data=df.all.rebind %>% filter(id==12))

# prediction
fit.12.test.pred <- predict(fit.12, newdata=df.all.predict %>% filter(id==12))
pred.df.test.12 <- df.all.predict %>% filter(id==12) %>% 
  data.frame(art.12_pred=exp(fit.12.test.pred))


# --- Article 13 ---
fit.13 <- lm(logSales ~ week + promos_1_3_5 + promo_1_3 + month + promo_3_5 + promo_05 + promo_03 + promo_01, data=df.all.rebind %>% filter(id==13))

# prediction
fit.13.test.pred <- predict(fit.13, newdata=df.all.predict %>% filter(id==13))
pred.df.test.13 <- df.all.predict %>% filter(id==13) %>% 
  data.frame(art.13_pred=exp(fit.13.test.pred))


# --- Article 14 ---
fit.14 <- lm(logSales ~ week + promos_1_3_5 + month + promo_1_3 + promo_05 + promo_3_5 + promo_03 + promo_01, data=df.all.rebind %>% filter(id==14))

# prediction
fit.14.test.pred <- predict(fit.14, newdata=df.all.predict %>% filter(id==14))
pred.df.test.14 <- df.all.predict %>% filter(id==14) %>% 
  data.frame(art.14_pred=exp(fit.14.test.pred))


# --- Article 15 ---
fit.15 <- lm(logSales ~ week + promo_1_3 + month + promo_05 + promo_01 + promo_03, data=df.all.rebind %>% filter(id==15))

# prediction
fit.15.test.pred <- predict(fit.15, newdata=df.all.predict %>% filter(id==15))
pred.df.test.15 <- df.all.predict %>% filter(id==15) %>% 
  data.frame(art.15_pred=exp(fit.15.test.pred))


# --- Article 16 ---
fit.16 <- lm(logSales ~ week + promo_1_3 + promo_05 + promo_01 + promo_03 + month, data=df.all.rebind %>% filter(id==16))

# prediction
fit.16.test.pred <- predict(fit.16, newdata=df.all.predict %>% filter(id==16))
pred.df.test.16 <- df.all.predict %>% filter(id==16) %>% 
  data.frame(art.16_pred=exp(fit.16.test.pred))


# --- Article 17 ---
fit.17 <- lm(logSales ~ week + promo_1_3 + month + promo_03 + promo_01, data=df.all.rebind %>% filter(id==17))

# prediction
fit.17.test.pred <- predict(fit.17, newdata=df.all.predict %>% filter(id==17))
pred.df.test.17 <- df.all.predict %>% filter(id==17) %>% 
  data.frame(art.17_pred=exp(fit.17.test.pred))


# --- Article 18 ---
fit.18 <- lm(logSales ~ week + promo_1_3 + promo_01 + promo_03 + promo_05 + month, data=df.all.rebind %>% filter(id==18))

# prediction
fit.18.test.pred <- predict(fit.18, newdata=df.all.predict %>% filter(id==18))
pred.df.test.18 <- df.all.predict %>% filter(id==18) %>% 
  data.frame(art.18_pred=exp(fit.18.test.pred))


### plot predictions ###
v.pred.test.df <- vector()
for(i in 0:18) {
  temp <- paste("pred.df.test.", i, sep="")
  v.pred.test.df <- c(v.pred.test.df, temp)
}
cat(paste(v.pred.test.df, collapse=", "), "\n")

pred.test.df <- bind_rows(pred.df.test.0, pred.df.test.1, pred.df.test.2, pred.df.test.3, pred.df.test.4, pred.df.test.5, pred.df.test.6, pred.df.test.7, pred.df.test.8, pred.df.test.9, pred.df.test.10, pred.df.test.11, pred.df.test.12, pred.df.test.13, pred.df.test.14, pred.df.test.15, pred.df.test.16, pred.df.test.17, pred.df.test.18, .id="id") %>%
  mutate(art.pred = coalesce(art.0_pred, art.1_pred, art.2_pred, art.3_pred, art.4_pred, art.5_pred, art.6_pred, art.7_pred, art.8_pred, art.9_pred, art.10_pred, art.11_pred, art.12_pred, art.13_pred, art.14_pred, art.15_pred, art.16_pred, art.17_pred, art.18_pred)) %>%
  select(id, article_name, ts, art.pred)

for(i in 1:19) {                                                               # update id to match df
  pred.test.df$id[pred.test.df$id==i] <- i-1
}

ggplot(data=df.all.rebind, aes(x=ts, y=sales)) + 
  geom_line(aes(color=df.all.rebind$article_name)) +
  geom_line(data=pred.test.df, aes(x=ts, y=art.pred)) +
  theme(axis.text.x=element_text(color="black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), strip.text=element_text(size=7)) +
  labs(x="2021", y="sales (units)") +
  #scale_color_manual(values=df.all.rebind$article_name) + 
  guides(legend.position = "side") +
  facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE), scales="free_y")


# export predictions
pred.test.df.b <- bind_rows(pred.df.test.0, pred.df.test.1, pred.df.test.2, pred.df.test.3, pred.df.test.4, pred.df.test.5, pred.df.test.6, pred.df.test.7, pred.df.test.8, pred.df.test.9, pred.df.test.10, pred.df.test.11, pred.df.test.12, pred.df.test.13, pred.df.test.14, pred.df.test.15, pred.df.test.16, pred.df.test.17, pred.df.test.18, .id=NULL) %>%
  mutate(sales=coalesce(art.0_pred, art.1_pred, art.2_pred, art.3_pred, art.4_pred, art.5_pred, art.6_pred, art.7_pred, art.8_pred, art.9_pred, art.10_pred, art.11_pred, art.12_pred, art.13_pred, art.14_pred, art.15_pred, art.16_pred, art.17_pred, art.18_pred)) %>%
  select(., -c(ts, num_promos, art.0_pred, art.1_pred, art.2_pred, art.3_pred, art.4_pred, art.5_pred, art.6_pred, art.7_pred, art.8_pred, art.9_pred, art.10_pred, art.11_pred, art.12_pred, art.13_pred, art.14_pred, art.15_pred, art.16_pred, art.17_pred, art.18_pred))

write.csv(pred.test.df.b, "./test/lm_prediction_Jeanette.csv", row.names=FALSE)

# export formulas
formula(fit.18)
df.all.final %>% select(id, article_name) %>% distinct()