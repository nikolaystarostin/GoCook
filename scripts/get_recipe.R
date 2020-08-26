# This file contains function which recommends recipes


get_recipe = function(userid, recipes_filtered, recipes_id, recipes_recsys, recipes, nrecipe = 5){
  rates_url = "xxx" # link to rates 
  
  user = read_sheet(rates_url) %>% filter(login == userid & (rating == 5 | rating == 4))
  user = user %>% filter(recipe_id %in% recipes_id$id) # предостережение от потерянных рецептов
  user = distinct(user)
  
  user_add = recipes_recsys %>% filter(recipes_recsys$recipe_id %in% user$recipe_id)
  recipes_filtered <- rbind(recipes_filtered, user_add)
  recipes_filtered <- distinct(recipes_filtered)
  
  rownames(recipes_filtered) = recipes_filtered$recipe_id
  sim = select(recipes_filtered,-recipe_id)
  sim = coop::cosine(t(as.matrix(recipes_filtered)))
  diag(sim) = 0
  newlen = nrow(sim)-nrow(user) # new
  sim = sim[1:newlen,]
  
  if (nrow(user)==0) {
    print("Вам представлены самые популярные рецепты, так как вы не авторизованы или оценили слишком мало блюд")
    
    df = recipes %>% filter(recipe_id %in% recipes_filtered$recipe_id)
    df = df %>% arrange(-added) %>% select(recipe_id) %>% head(5)
    
    
  } else {
    mostSimilar = head(sort(sim[,as.character(user$recipe_id)], decreasing = T), n = 5) #change
    a = which(sim[,as.character(user$recipe_id)] %in% mostSimilar, arr.ind = TRUE)
    rows = a %% dim(sim)[1]
    rows = rows %>% str_replace("^0", as.character(dim(sim)[1])) %>% as.integer(rows)
    result = rownames(sim)[rows]
    result = unique(result) #new
    while (length(result)<5) {
      rn = rownames(sim) %>% as.data.frame()
      rn = rn %>% filter(!rn$. %in% as.character(as.data.frame(result)$result))
      sim = sim[as.character(rn$.),]
      mostSimilar = head(sort(sim[,as.character(user$recipe_id)], decreasing = T), n = 5-length(result))
      a = which(sim[,as.character(user$recipe_id)] %in% mostSimilar, arr.ind = TRUE)
      rows = a %% dim(sim)[1]
      rows = rows %>% str_replace("^0", as.character(dim(sim)[1])) %>% as.integer(rows)
      result1 = rownames(sim)[rows]
      result1 = unique(result1) #new
      result = c(result, result1)
    }
    recommend = filter(recipes, recipe_id %in% result) %>% dplyr::select(recipe_id)
    df = data.frame(recommend)
  }
  return(df$recipe_id)
}
