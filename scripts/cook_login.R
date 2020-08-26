# This file contains function which is used to log in into the account

cook_login <- function(x,y){
  table_url = "xxx" ### link to logins and passwords table
  auth = read_sheet(table_url)
  if (x %in% auth$login){
    auth_log = filter(auth,auth$login==x)
    if(y == auth_log$password){
      return('Вход выполнен')}
    else {return('Неверный пароль')}}
  else {return('Аккаунт не существует')}
}
