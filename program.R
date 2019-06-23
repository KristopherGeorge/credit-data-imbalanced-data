
library(tidyverse)
ds_raw <- data.table::fread('creditcard.csv')
glimpse(ds_raw)

ds_raw <- select_all(ds_raw, tolower)

# simple  check for missing values
map_if(ds_raw, is.numeric, mean)
check_dups <- function(x) sum(is.na(x))
map(ds_raw, check_dups) %>%
  unlist()

# ok class imbalance
ds_raw %>%
  group_by(class) %>%
  count() %>%
  ungroup %>%
  mutate(pcnt = n / sum(n)) ->
checking_for_imb

checking_for_imb %>%
  ggplot(aes(x = class, y = pcnt)) + 
  geom_bar(stat = 'identity')


# massive class imbalance exists in this data
# create mod, val and test data sets -> actaully no no need to -> will be over fitting but fine
# this is not as formal as I woudl be in a professional context!
ds_raw <- mutate(ds_raw, class = factor(class, levels = c(0, 1)))
model_formula <- paste0(colnames(ds_raw)[1:30], collapse = ' + ') %>% paste0("class ~ ", .) %>% as.formula()
mod0 <- glm(model_formula,
            data = ds_raw,
            family = binomial(link = 'logit'))

# now precision and recall curve! 
tibble(logit = predict(mod0)) %>%
  mutate(odds = exp(logit),
         prob = odds / (1 + odds)) ->
mod0_pred

mean(mod0_pred$prob)


mod0_pred$class = ds_raw$class

nbr_y1 <- sum(if_else(ds_raw$class == 1, 1L, 0L))
nbr_y0 <- nrow(ds_raw) - nbr_y1
mod0_pred %>%
  arrange(desc(prob)) %>%
  mutate(class_nbr = if_else(class == 1, 1L, 0L)) %>%
  mutate(y1_cumsum = cumsum(class_nbr),
         recall = y1_cumsum / !!nbr_y1) %>%
  mutate(predicted = row_number(),
         precision = y1_cumsum / predicted) ->
m0_proc_data

m0_proc_data %>%
  mutate(lag_recall = lag(recall),
         lag_recall = if_else(is.na(lag_recall), 0, lag_recall),
         change_recall = recall - lag_recall,
         avg_precision = precision * change_recall) ->
m0_proc_data

m0_proc_data %>%
  summarise(avg_precision  = sum(avg_precision)) %>%
  `$`('avg_precision') ->
m0_avg_precision



m0_proc_data %>%
  mutate(keep_cases = predicted %% 100) %>%
  group_by(keep_cases) %>%
  # sample_frac(0.2) %>%
  ungroup %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_point(aes(colour = prob)) + 
  scale_colour_gradientn('Probability', colours = RColorBrewer::brewer.pal(10, "RdYlGn") %>% rev) + 
  theme_bw() +
  ggtitle(paste0('Undersampling Frequent Class: avg precision = ', round(m1_avg_precision, 4))) ->
m0_recall_chart

m0_recall_chart
# so avg precision 




# 2. model down sampling! -------------------------------------------------

class <- table(ds_raw$class)
odds_infrequent <- class[2] / sum(class)
odds_infrequent <- odds_infrequent / (1 - odds_infrequent)

odds_frequent <- class[1] / sum(class)
odds_frequent <- odds_frequent / (1 - odds_frequent)

# suppose we had this situation: c(1, 1, 1, 0)
# then odds of frequent class is 3/4 / (1 - 3/4) = 3
# and odds of infrequent class is 1/4 / (1 - 1/4) = 1/3
# therefore downsample infrequent class by using odds of infreqent class



# ok now for adding weights
ds_raw %>%
  mutate(down_sample_frequent_class = if_else(class == 0, !!odds_infrequent, 1),
         up_sample_infrequent_class = if_else(class == 1, !!odds_frequent, 1)) ->
ds_raw


ds_raw %>%
  group_by(class) %>%
  summarise_at(vars('down_sample_frequent_class', 'up_sample_infrequent_class'), sum)


class

# excellent now let's fit downsample model first
# excellent now let's fit downsample model first
# excellent now let's fit downsample model first
# excellent now let's fit downsample model first
# excellent now let's fit downsample model first
# excellent now let's fit downsample model first
# excellent now let's fit downsample model first

mod1 <- glm(model_formula,
            data = ds_raw,
            family = binomial(link = 'logit'),
            weights = ds_raw$down_sample_frequent_class)
summary(mod1)

if(mod1$converged != TRUE){
  stop('did not converge')
}


# now precision and recall curve! 
tibble(logit = predict(mod1)) %>%
  mutate(odds = exp(logit),
         prob = odds / (1 + odds)) ->
mod1_pred
mean(mod1_pred$prob)



mod1_pred$class = ds_raw$class
glimpse(mod1_pred)

nbr_y1 <- sum(if_else(ds_raw$class == 1, 1L, 0L))
nbr_y0 <- nrow(ds_raw) - nbr_y1
mod1_pred %>%
  arrange(desc(prob)) %>%
  mutate(class_nbr = if_else(class == 1, 1L, 0L)) %>%
  mutate(y1_cumsum = cumsum(class_nbr),
         recall = y1_cumsum / !!nbr_y1) %>%
  mutate(predicted = row_number(),
         precision = y1_cumsum / predicted) ->
m1_proc_data

max(m1_proc_data$y1_cumsum)

m1_proc_data %>%
  mutate(lag_recall = lag(recall),
         lag_recall = if_else(is.na(lag_recall), 0, lag_recall),
         change_recall = recall - lag_recall,
         avg_precision = precision * change_recall) ->
m1_proc_data

m1_proc_data %>%
  summarise(avg_precision  = sum(avg_precision)) %>%
  `$`('avg_precision') ->
m1_avg_precision

m1_proc_data %>%
  mutate(keep_cases = predicted %% 100) %>%
  group_by(keep_cases) %>%
  # sample_frac(0.2) %>%
  ungroup %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_point(aes(colour = prob)) + 
  scale_colour_gradientn('Probability', colours = RColorBrewer::brewer.pal(10, "RdYlGn") %>% rev) + 
  theme_bw() +
  ggtitle(paste0('Undersampling Frequent Class: avg precision = ', round(m1_avg_precision, 4))) ->
m1_recall_chart

m1_recall_chart
# so avg precision 

# excellent now let's fit oversamnpling model next
# excellent now let's fit oversamnpling model next
# excellent now let's fit oversamnpling model next
# excellent now let's fit oversamnpling model next
# excellent now let's fit oversamnpling model next
# excellent now let's fit oversamnpling model next
# excellent now let's fit oversamnpling model next
# excellent now let's fit oversamnpling model next

mod2 <- glm(model_formula,
            data = ds_raw,
            family = binomial(link = 'logit'),
            weights = ds_raw$up_sample_infrequent_class)
summary(mod2)

# now precision and recall curve! 
tibble(logit = predict(mod2)) %>%
  mutate(odds = exp(logit),
         prob = odds / (1 + odds)) ->
mod2_pred
mean(mod2_pred$prob)
mean(mod1_pred$prob)


mod2_pred$class = ds_raw$class
glimpse(mod2_pred)

nbr_y1 <- sum(if_else(ds_raw$class == 1, 1L, 0L))
nbr_y0 <- nrow(ds_raw) - nbr_y1
mod2_pred %>%
  arrange(desc(prob)) %>%
  mutate(class_nbr = if_else(class == 1, 1L, 0L)) %>%
  mutate(y1_cumsum = cumsum(class_nbr),
         recall = y1_cumsum / !!nbr_y1) %>%
  mutate(predicted = row_number(),
         precision = y1_cumsum / predicted) ->
m2_proc_data


m2_proc_data %>%
  mutate(lag_recall = lag(recall),
         lag_recall = if_else(is.na(lag_recall), 0, lag_recall),
         change_recall = recall - lag_recall,
         avg_precision = precision * change_recall) ->
m2_proc_data

m2_proc_data %>%
  summarise(avg_precision  = sum(avg_precision)) %>%
  `$`('avg_precision') ->
m2_avg_precision



m2_proc_data %>%
  mutate(keep_cases = predicted %% 100) %>%
  group_by(keep_cases) %>%
  # sample_frac(0.2) %>%
  ungroup %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_point(aes(colour = prob)) + 
  scale_colour_gradientn('Probability', colours = RColorBrewer::brewer.pal(10, "RdYlGn") %>% rev) + 
  theme_bw() +
  ggtitle(paste0('Undersampling Frequent Class: avg precision = ', round(m2_avg_precision, 4))) ->
m2_recall_chart

m2_recall_chart
# so avg precision 

m1_avg_precision
m2_avg_precision



# compare models
m2_proc_data$avg_precision[20:30] == m1_proc_data$avg_precision[20:30]
m0_proc_data$avg_precision[20:30]

# interesting is that under or oversampling for logistic regression provides the same result which I wasn't expecting!

bind_rows(tibble(model = 'base',
                 prob = mod0_pred$prob,
                 class = mod0_pred$class),
          tibble(model = 'undersampliing',
                 prob = mod1_pred$prob,
                 class = mod1_pred$class),
          tibble(model = 'oversampling',
                 prob = mod2_pred$prob,
                 class = mod2_pred$class)) ->
comparing_model_scores


glimpse(comparing_model_scores)


comparing_model_scores %>%
  ggplot(aes(x = prob, fill = model)) + 
  geom_histogram() + 
  facet_wrap(model ~ class, nrow = 3, scale = "free_y")
