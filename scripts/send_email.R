# This file contains function which sends an email with account information

send_email <- function(x){
  table_url = "xxx" # link to logins and passwords table
  auth = read_sheet(table_url)
  if (x %in% auth$email){
    user_info <- read_sheet(table_url) %>% filter(email==x)
    msg <- paste('Здравствуйте! \n\n', 
                 'Вы получили это письмо, так как сделали запрос в приложении GoCook \n\n', 
                 'Ваши данные для входа:\n',
                 'Имя пользователя: ', user_info$login,'\n',
                 'Пароль: ',user_info$password,'\n\n',
                 'Всего наилучшего,\n',
                 'Команда GoCook',sep='')
    send.mail(from = 'Go Cook <gocook.recipe@gmail.com>',
              to = x,
              subject = 'GoCook. Ваши данные для входа',
              body = msg,
              smtp = list(host.name = 'smtp.gmail.com', port = 587,
                          user.name = 'gocook.recipe@gmail.com',
                          passwd = 'xxx', ssl = TRUE), # secret pass
              encoding = 'utf-8',
              authenticate = TRUE,
              send = TRUE)
    return('Письмо с паролем отправлено')}
  else {
    return('Аккаунта с такой почтой не существует')}
}
