

library(RSelenium)
library(httr)
library(rvest)
library(stringr)

usr_name <- "USERNAME"
password <- "PASSWORD"
rD <- rsDriver(browser="firefox", 
               port=netstat::free_port(), 
               verbose=F, 
               iedrver = NULL, 
               extraCapabilities = list(acceptInsecureCerts = TRUE))
remDr <- rD[["client"]]



# login to wordpress
wp_login <- function(usr, pswd){
  usr_name <- usr
  password <- pswd
  login_page <- "https://WORDPRESS_URL/wp-login.php"
  url <- login_page
  remDr$navigate(url = url) # Go to login page
  remDr$findElement(using = 'xpath', '//*[(@id = "user_login")]')$sendKeysToElement(list(usr_name)) # enter user details
  remDr$findElement(using = 'xpath', '//*[(@id = "user_pass")]')$sendKeysToElement(list(password)) # enter user details
  submit_button <- remDr$findElement(using = "name", "wp-submit") # locate login button
  submit_button$clickElement() # click login button
}



# replace post excerpts
replace_excerpt <- function(page_url){
  print(paste0("Currently editing page: ", page_url)) # print current url
  url <- page_url
  remDr$navigate(url = url) # go to post url
  description <- remDr$getPageSource()[[1]] # get page source
  description <- stringr::str_match(description, "item-preview-content\\s*(.*?)\\s*</p>")[1] # grep between strings `item-preview-content` and `</p>`
  description <- stringr::str_sub(description, start = 27, end = nchar(description) - 4) # keep only description text
  remDr$findElement("name", "excerpt")$clearElement() # Clear current text
  remDr$findElement("name", "excerpt")$sendKeysToElement(list(description)) # insert description var into textbox
  Sys.sleep(3)
  remDr$findElement("id", "remove-post-thumbnail")$clickElement() # while here, removing featured Image (Doesnt work)
}



edit_posts <- function(){
  remDr$findElement(using = 'link text', 'Posts')$clickElement() # Click Posts in WP menu
  
  
  # Change amount of links shown on WP (from 20, to 200)
  remDr$findElement("id", "show-settings-link")$clickElement()
  Sys.sleep(2)
  remDr$findElement("id", "edit_post_per_page")$clearElement()
  Sys.sleep(1)
  remDr$findElement("id", "edit_post_per_page")$sendKeysToElement(list("200"))
  Sys.sleep(2)
  remDr$findElement("name", "screen-options-apply")$clickElement()
  Sys.sleep(5)
  
  post_urls <- remDr$getPageSource()[[1]] # get page source
  post_urls <- post_urls %>% read_html() %>% html_nodes("a") %>% html_attr("href") %>% unique() # get all linkes
  post_urls <- post_urls[which(stringr::str_ends(post_urls, "action=edit") == TRUE)] # filter only on edit links
  Sys.sleep(2)

  # Call repalce excerpt function
  for(i in 1:length(post_urls)){
    replace_excerpt(post_urls[i])
    remDr$findElement("id", "publish")$clickElement() # Publish page
  }
}


wp_login(usr = usr_name,
         pswd = password)

edit_posts()
