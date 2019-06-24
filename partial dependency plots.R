getwd()
library(tidyverse)
ds_raw <- data.table::fread('../creditcard.csv')
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


summary(mod0)


# let's do a grid search for time and plot it's influence
get_range <- function(ds, x, len = 100){
  x <- rlang::enexpr(x) %>% as.character
  mean_x <- mean(ds[[x]])
  var_x <- var(ds[[x]])
  seq(mean_x - 1.96 * sqrt(var_x), mean_x  + 1.96 * sqrt(var_x), length = len)
}

time <- get_range(ds_raw, time)
time

# now we want to change time for each of these cases!
get_pred <- function(ds = ds_raw, x, set_value, eq){
  x <- rlang::enexpr(x)
  set_value <- rlang::enexpr(set_value)
  eq <- enexpr(eq)
  mutate(ds, `:=`(!!x, !!set_value)) %>%
    mutate(`:=`(logit, !!eq),
           odds = exp(logit),
           prob = odds / (1 + odds)) %>%
    summarise(mean_prob = mean(prob),
              lower = quantile(prob, 0.025),
              upper = quantile(prob, 0.975))
}

coef(mod0) %>%
  enframe() ->
eq_s1

eq_s1 %>%
  filter(name != "(Intercept)") ->
eq_s2

map2(eq_s2$name %>% syms, eq_s2$value, function(x, y) expr(!!x * !!y)) %>%
  reduce(function(x, y) expr(!!x + !!y)) ->
eq_s3

eq_s1 %>%
  filter(name == "(Intercept)") %>%
  `$`('value') ->
intercept


eq <- expr(!!intercept + !!eq_s3)  


get_pred(x = time, set_value = 22420.819, eq = !!eq)

time_pred <- map_dfr(time, function(x) get_pred(x = time, set_value = !!x, eq = !!eq))
time_pred$time <- time

time_pred %>%
  ggplot(aes(x = time, y = mean_prob)) + 
  geom_line()


# now you have something that's worked for time -> you could implement it for other variables as well!!!

get_partial_prob_data <- function(x){
  x <- rlang::enexpr(x)
  x_range <- get_range(ds_raw, !!x, len = 10)
  x_pred <- map_dfr(x_range, function(y) get_pred(x = !!x, set_value = !!y, eq = !!eq))
  x_pred[[as.character(x)]] = x_range
  x_pred
}

time <- get_partial_prob_data(time)
v1 <- get_partial_prob_data(v1)
v1 %>%
  ggplot(aes(x = v1, y = mean_prob)) + 
  geom_line()

partial_vars_info <- names(coef(mod0)[-1]) %>% syms
all_vars <- map(partial_vars_info, function(x) get_partial_prob_data(!!x))

# now for all_vars I want to save as a flat file so we're able to plot it!

glimpse(all_vars[[1]])
# i.e. you have at the end the variable -> you want to change it to be var_val and add another column
# that's called var_name = time

all_vars_tibble <- tibble()
for(i in 1:length(partial_vars_info)){
  # i <- 1
  var_sym <-   partial_vars_info[[i]]
  var_char <- as.character(var_sym)
  table_loop <- all_vars[[i]]
  table_loop %>%
    rename(`:=`(var_value, !!var_sym)) %>%
    mutate(var_name = var_char) %>%
    bind_rows(all_vars_tibble, .) ->
  all_vars_tibble
  
}


all_vars_tibble %>%
  ggplot(aes(x = var_value, y = mean_prob, colour = var_name)) + 
  geom_line(show.legend = FALSE) + 
  facet_wrap(~var_name, scales = 'free_x') + 
  theme_bw()


# so you can imagine with categorical data this is easy to compute!!!! I love this makes sense!

