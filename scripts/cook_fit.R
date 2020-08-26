# This file contains function which fits the linear model

cook_fit <- function(x){
  rates_url = "xxx" # link to rates table
  recipes_recsys <- read_csv('recipes_recsys.csv') 
  recipes_id <- read_csv('id_from_rec_matrix.csv')
  recipes_recsys$recipe_id <- recipes_id$id
  recipes_recsys$likes <- c(scale(recipes_recsys$likes))
  recipes_recsys$dislikes <- c(scale(recipes_recsys$dislikes))
  recipes_recsys$added <- c(scale(recipes_recsys$added))
  recipes_recsys$time2 <- c(scale(recipes_recsys$time2))
  
  
  
  rates = read_sheet(rates_url)
  user_rates <- rates %>% filter(rates$login==x) %>% select(-login)
  joined_recipes <- left_join(user_rates, recipes_recsys, by='recipe_id') %>% select(-recipe_id)
  if (nrow(joined_recipes)==0){
    joined_recipes[1,]=0
  }
  lm <- lm(rating~., joined_recipes)
  return(lm)
}
