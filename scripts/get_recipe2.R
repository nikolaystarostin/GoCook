# This file contains function which recommends recipes


get_recipe = function(userid, recipes_filtered, recipes_id, recipes_recsys, recipes, nrecipe = 5){
  rates_url = "xxx" # link to rates
  
  user = read_sheet(rates_url) %>% filter(login == userid & (rating == 5 | rating == 4))
  user = user %>% filter(recipe_id %in% recipes_id$id)
  user = distinct(user)
  
  user_add = recipes_recsys %>% filter(recipes_recsys$recipe_id %in% user$recipe_id)
  recipes_filtered <- rbind(recipes_filtered, user_add)
  recipes_filtered <- distinct(recipes_filtered)
  
  rownames(recipes_filtered) = recipes_filtered$recipe_id
  sim = select(recipes_filtered,-recipe_id)
  sim = coop::cosine(t(as.matrix(recipes_filtered)))
  diag(sim) = 0
  
  
  if (nrow(user)==0) {
    print("Вам представлены самые популярные рецепты, так как вы не авторизованы или оценили слишком мало блюд")
    
    df = recipes %>% filter(recipe_id %in% recipes_filtered$recipe_id)
    df = df %>% arrange(-added) %>% select(recipe_id) %>% head(5)
    
    
  } else {
    mostSimilar = head(sort(sim[,as.character(user$recipe_id)], decreasing = T), n = nrecipe)
    a = which(sim[,as.character(user$recipe_id)] %in% mostSimilar, arr.ind = TRUE)
    rows = a %% dim(sim)[1]
    result = rownames(sim)[rows]
    recommend = filter(recipes, recipe_id %in% result) %>% dplyr::select(recipe_id)
    df = data.frame(recommend)
  }
  return(df$recipe_id)
}
