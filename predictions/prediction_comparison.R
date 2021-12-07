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


##### compare predictions #####
df.compare <- read.csv("./test/prediction_comparison.csv")
df.compare$yrwk_start <- as.Date(df.compare$yrwk_start, format="%Y-%m-%d")
df.compare$ts <- as.POSIXct(df.compare$yrwk_start)

model <- c("lm1"="red", "lm2"="orange", "gam"="green4", "rf"="blue", "aa"="purple")

ggplot(data=df.all.rebind %>% filter(year %in% 2019:2021), aes(x=ts, y=sales)) + 
  geom_line(aes(color="black")) +
  geom_line(data=df.compare %>% filter(model=="lm1"), aes(x=ts, y=sales), color=model["lm1"], size=0.7) +
  geom_line(data=df.compare %>% filter(model=="lm2"), aes(x=ts, y=sales), color=model["lm2"], size=0.7) +
  geom_line(data=df.compare %>% filter(model=="gam"), aes(x=ts, y=sales), color=model["gam"], size=0.7) +
  geom_line(data=df.compare %>% filter(model=="rf"), aes(x=ts, y=sales), color=model["rf"], size=0.7) +
  geom_line(data=df.compare %>% filter(model=="aa"), aes(x=ts, y=sales), color=model["aa"], size=0.7) +
  theme(axis.text.x=element_text(color="black", size=10, angle=55, vjust=.8, hjust=0.8), legend.text=element_text(size=7), strip.text=element_text(size=7)) +
  labs(x="ts", y="sales (units)", color="Model") +
  scale_color_manual(values=model) + 
  guides(legend.position="side") +
  facet_wrap(~ article_name, labeller=label_wrap_gen(width=18, multi_line=TRUE), scales="free_y")
