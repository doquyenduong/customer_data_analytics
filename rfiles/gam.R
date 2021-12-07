# Packages -------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(caret)
library(stringr)

# Load data ------------------------------------------------------

data_train <- purrr::map_dfr(
  list.files("data_hs21", full.names = T),
  read.csv
) %>% 
  mutate(
    yrwk_start = as.Date(yrwk_start),
    yrwk_end = as.Date(yrwk_end)
  )

data_test <- purrr::map_dfr(
  list.files("test", full.names = T),
  read.csv
) %>% 
  mutate(
    yrwk_start = as.Date(yrwk_start),
    yrwk_end = as.Date(yrwk_end)) %>% 
  rename(article_name = artikel_name)


# Encoding ----------------------------------------------------------------

# encod <- function(x, var_name) {
#   
#   # Values
#   vals <- unique(x)
#   
#   # Compare vector with value and add numerized result to df 
#   res_enc <- purrr::map_dfc(vals, function(vals, x) as.numeric(x == vals), x = x)
#   names(res_enc) <- paste0(var_name, c(1:length(vals)))
#   
#   return(res_enc)
#   
# }
# 
# data_enc <- data %>% 
#   mutate(big = as.numeric(stringr::str_detect(article_name, "500G"))) %>%
#   mutate(mclas = as.numeric(stringr::str_detect(article_name, "M-CLAS"))) %>% 
#   mutate(bio = as.numeric(stringr::str_detect(article_name, "BIO"))) %>% 
#   #select(article, sales, year, month, week, starts_with("promo"), big, mclas, bio) %>%
#   bind_cols(encod(data$category_1, "cat1")) %>% 
#   bind_cols(encod(data$category_2, "cat2")) %>% 
#   mutate(ID = 1:n())


# GAM ------------------------------------------------------------- 

# Function to predict
gam_per_article <- function(article, x, z) {
  
  # Feedback
  cat(article, "\n")
  
  # Subset
  sales <- x %>% 
    filter(article_name == article) %>% 
    select(year, month, week, sales, promo_01, promo_02, promo_03, promo_04, promo_05)
  
  # Fit Model
  fit_gam_cc <- caret::train(
    form = as.formula("sales ~ year + month + week + promo_01 + promo_02 + promo_03 + promo_04 + promo_05"),
    data = sales,
    method = "gam"
  )
  

  # Predict
  nd <- z %>%
    filter(article_name == article) %>%
    mutate(sales = predict(fit_gam_cc,
                           newdata = z %>% filter(article_name == article)
                           %>% select(year, month, week, promo_01,
                                                   promo_02, promo_03, promo_04,
                                                   promo_05)))
  
  return(nd)
  
}

# Go
data_pred_gam <- purrr::map_dfr(
  unique(data_train$article_name),
  gam_per_article,
  x = data_train,
  z = data_test
)


# Plot time series incl. prediction
ggplot(data_train, mapping = aes(yrwk_start, sales, colour = article_name)) + 
  geom_line() + 
  geom_line(data = data_pred_gam, color = "black") +
  theme(strip.text=element_text(size=7), legend.text=element_text(size=7)) +
  facet_wrap(vars(article_name), scales = "free", labeller=label_wrap_gen(width=18, multi_line=TRUE))


# Create csv file
write.csv(data_pred_gam,"C:/Users/nyxan/OneDrive/Dokumente/NOT/HSLU/Studium/2. Sem/Customer Data Analytics\\sales_pred_gam.csv", row.names = FALSE)

