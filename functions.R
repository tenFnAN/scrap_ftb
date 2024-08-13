
scrap_start_session = function(.browser = 'firefox', .port = 132L, .hide = F, ...){ 
  # .browser ='chrome'
  # driver <- RSelenium::rsDriver(browser=c("chrome"),  chromever="91.0.4472.101",  port=4545L,  verbose=F)
  
  if(.hide){
    driver        <- RSelenium::rsDriver(browser = .browser, port = .port ,
                                         # hide Browser 
                                         extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless')))
                                         # , check = F 
                                         ,...)  
  }else{
    driver        <- RSelenium::rsDriver(browser = .browser, port = .port,... )  
  }
  
  pid           <- driver$server$process$get_pid() # get the PID of the process you launched
  remote_driver <- driver[["client"]] 
  assign("pid",           pid,           envir = .GlobalEnv)
  assign("remote_driver", remote_driver, envir = .GlobalEnv)
  # driver$client
}

scrap_start_session = function(.port = 4444L, ...){ 
  # .browser ='chrome'
  # driver <- RSelenium::rsDriver(browser=c("chrome"),  chromever="91.0.4472.101",  port=4545L,  verbose=F)
  
  fprof <- RSelenium::makeFirefoxProfile(
    list(
      browser.download.dir = "/home/seluser/Downloads"
    )
  )
  driver        <- RSelenium::rsDriver(browser = 'firefox', port = .port, extraCapabilities = fprof, ...)
  pid           <- driver$server$process$get_pid() # get the PID of the process you launched
  remote_driver <- driver[["client"]] 
  assign("pid",           pid,           envir = .GlobalEnv)
  assign("remote_driver", remote_driver, envir = .GlobalEnv) 
  # driver$client
}
scrap_start_session = function(.port = 4444L){ 
  # .browser ='chrome'
  # driver <- RSelenium::rsDriver(browser=c("chrome"),  chromever="91.0.4472.101",  port=4545L,  verbose=F)
  
  fprof <- RSelenium::makeFirefoxProfile(
    list(
      browser.download.dir = "/home/seluser/Downloads"
    )
  )

  remote_driver <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = .port,
    browserName = "firefox",
    extraCapabilities = fprof
  )

  remote_driver$open(silent = TRUE) 
   
  assign("remote_driver", remote_driver, envir = .GlobalEnv)
  # driver$client
}
scrap_navigate = function(.link, .sleep = 1, .driver = remote_driver){
  .driver$navigate(.link)  
  Sys.sleep( .sleep )
}

scrap_kill_session = function(.pid = pid, .driver = remote_driver){
  tryCatch(.driver$close(), error = function(e) {  })
  if(exists('remote_driver', envir = .GlobalEnv)){  rm(remote_driver, envir = .GlobalEnv) ; gc() }
  # kill the processes
  if(.Platform$OS.type == 'windows'){
    system(paste0("Taskkill /F /T" ," /PID ", .pid))  
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  } else if(.Platform$OS.type  == 'unix'){
    system(paste0('kill -9 ', .pid))
  } 
  if(exists('pid', envir = .GlobalEnv)){  rm(pid, envir = .GlobalEnv) ; gc() } 
}

tidy_rownumber = function(ds, by_, col_name = 'id'){
  ds = as.data.table(ds) 
  ds[, (col_name) := as.integer(row.names(.SD)), by = by_] 
  as.data.frame(ds)
}
tidy_slice_rows <- function(ds, by_, iloc = 1){
  ds = data.table::as.data.table(ds)
  inx <- ds[, .I[1:min(.N, iloc)], by = by_]$V1
  as.data.frame(ds[inx])
}

change_polish_characters = function(x){
  gsub('ń', 'n', x, perl = T) %>% 
    gsub('ą', 'a', ., perl = T) %>% 
    gsub('ż|ź', 'z', ., perl = T) %>% 
    gsub('ć', 'c', ., perl = T) %>% 
    gsub('ó', 'o', ., perl = T) %>% 
    gsub('ł', 'l', ., perl = T) %>% 
    gsub('ę', 'e', ., perl = T) %>% 
    gsub('ś', 's', ., perl = T)
}
 
