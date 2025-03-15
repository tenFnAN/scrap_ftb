library(optparse) 
# 1.1. PREPARE PARAMETERS ##############################################################
optionsList <- list( 
  make_option(c("--type","-s"), default='schedule',help=" ") 
)  
parser = OptionParser(option_list = optionsList)
arguments = parse_args(parser) 
# 2.1. PARSE PARAMETERS ##############################################################
ARG_TYPE = as.character(arguments$type)  

#load libs
# Rscript scrap_ftb.R --type schedule
# Rscript scrap_ftb.R --type update

library(dplyr)  ; library(collapse)  ; library(data.table)   
library(rvest)  ; library(polite)    ; library(stringr)   ; library(readr)

source('functions.R')

# 1.0. CONST INIT ##############################################################
date_       = as.Date(Sys.time())
seq_sampler = seq(0.5, 1.5, 0.2)
SELE_HIDE   = if(Sys.info()['sysname'] == 'Windows'){F}else{T}
# scrap_start_session(.hide = T, check = F)
scrap_pilka_ = function(.url, .liga_nr = '1', .if_all_season = F, .time_break = 1.5, ...){
  # match.call zwraca nazwe wywolywanej funkcji
  # browser() 
  
  .log_file = ifelse(.if_all_season, stringr::str_glue('log/log_ftb_history_all.txt'), stringr::str_glue('log/log_ftb_history_update.txt')) 
  tryCatch({
    cat(paste0('\n',as.Date(Sys.time()), '|', .url) , file = stringr::str_glue('{.log_file}'), append = T)
    scrap_navigate(.url)
    
    if( .if_all_season  ){ 
      # kopiujesz XPATH  
      # xpaths_ = '/html/body/div[3]/div[1]/div/div/main/div[4]/div[2]/div[1]/div[1]/div/div/a'
      
      click_extend = tryCatch(remote_driver$findElement(using = 'link text', value = 'Pokaż więcej meczów'), error = function(e) { 'empty'})
      while( class(click_extend) == 'webElement' ){ 
        click_output = tryCatch(click_extend$sendKeysToElement(list("R Cran", key = "enter")), error = function(e) NA  )
        click_output = tryCatch(click_extend$clickElement(), error = function(e) NA  ) 
        Sys.sleep( sample(seq_sampler, 1) )
        
        click_extend = tryCatch(remote_driver$findElement(using = 'link text', value = 'Pokaż więcej meczów'), error = function(e) { 'empty'})
      } 
    }
    
    Sys.sleep( sample(seq_sampler, 1) )  
    www_       = remote_driver$getPageSource()[[1]] %>% read_html()  
    
    kraj_      = html_nodes(www_, '.breadcrumb__link') %>% html_text() %>% .[2]
    liga_      = html_nodes(www_, '.heading__name') %>% html_text() %>%
      gsub(x = ., pattern = paste0(kraj_, ': '), replacement = '') %>%
      gsub(x = ., pattern = 'Â', replacement = '') %>%
      gsub(x = ., pattern = ' -', replacement = '') %>%
      gsub(x = ., pattern = '- ', replacement = '') %>%
      str_trim() %>%
      gsub(x = ., pattern = ' ', replacement = '-') %>%
      tolower()
    season_    = html_nodes(www_, '.heading__info') %>% html_text() 
    # www_ %>% html_nodes('.event__round') %>% polite::html_attrs_dfr() %>% fselect(faza = .text)
    # www_ %>% html_element("h1")
    # length(www_ %>% html_nodes('.event__time'))
    
    n_match = length(www_ %>% html_nodes('.event__time'))
    cat(paste0('|nrow tb_', n_match), file = stringr::str_glue('{.log_file}'), append = T)
    if( n_match == 0 ){ return() }
    
    Sys.sleep( sample(seq_sampler, 1) ) 
    # 15.03.23 -> usuneli wynik z pierwszej polowy meczu
    # score_half_a = www_ %>% html_nodes('.event__participant--home') %>% polite::html_attrs_dfr() %>% fsubset(grepl('1$', class)) %>% fselect(team_score_half_a = .text)
    score_half_a = www_ %>% html_nodes('.event__participant--home') %>% polite::html_attrs_dfr() %>% fselect(team_score_half_a = .text)
    score_half_b = www_ %>% html_nodes('.event__participant--away') %>% polite::html_attrs_dfr() %>% fselect(team_score_half_b = .text)
    score_half_a = if(nrow(score_half_a) != n_match ){data.frame('team_score_half_a' = '')}else{score_half_a}
    score_half_b = if(nrow(score_half_b) != n_match ){data.frame('team_score_half_b' = '')}else{score_half_b}
    tb_ = bind_cols(
      www_ %>% html_nodes('.event__time') %>% polite::html_attrs_dfr() %>%fselect(data = .text) %>% tidyr::separate(col = 'data', sep = ' ', into = c('data', 'godzina')),
      www_ %>% html_nodes('.event__homeParticipant') %>% polite::html_attrs_dfr() %>% fselect(team_a = .text),
      www_ %>% html_nodes('.event__awayParticipant') %>% polite::html_attrs_dfr() %>% fselect(team_b = .text),
      www_ %>% html_nodes('.event__score--home') %>% polite::html_attrs_dfr() %>%  fselect(team_score_a = .text),
      www_ %>% html_nodes('.event__score--away') %>% polite::html_attrs_dfr() %>%  fselect(team_score_b = .text),
      score_half_a,
      score_half_b,
      www_ %>% html_nodes('.event__match') %>% polite::html_attrs_dfr() %>% fselect(link_kursy = id) %>% fmutate(link_kursy = substring(link_kursy, 5))
    ) %>%
      fmutate( month = substring(data, 4,5), year = NA) %>%  
      fmutate(across(grep('score_(a|b)', names(.), value = T), parse_number )) %>%
      fmutate(team_score_half_a = ifelse(str_length(team_score_half_a) >= 3 & is.character(team_score_half_a), NA, team_score_half_a),
              team_score_half_b = ifelse(str_length(team_score_half_b) >= 3 & is.character(team_score_half_b), NA, team_score_half_b))
    # 
    print('start loop')
    id_event_time = gregexpr('Zobacz szczegóły meczu', www_)[[1]]
    id_info_      = gregexpr('event__title--name', www_)[[1]]
    for(row in 1:nrow(tb_)){ 
      # print(row)
      # row = 5 
      i_ = ifelse(row==length(id_event_time),
                  id_event_time[row] + ifelse(length(id_event_time) >= 3 | id_event_time == -1, diff(id_event_time) %>% median, 555 ) , 
                  id_event_time[row+1])
      if( is.na(i_) ) { next }
      match_ = substring(www_, id_event_time[row] -111 , i_ )  
      
      # runda / kolejka
      match_   = substring(match_, 1, gregexpr('event__time', match_)[[1]][1])
      id_round = gregexpr('event__round', match_)[[1]] 
      
      #faza 
      id_info_tmp = tail(id_info_[id_info_ < id_event_time[row]],1)
      match_stage = substring(www_, id_info_tmp +27, id_info_tmp + 100)
      match_stage = substring(match_stage, 1, gregexpr('>', match_stage)[[1]][1]-2 ) 
      #kolejka
      if( length(id_round) >= 2 || id_round != -1 ){
        match_round = substring(match_, gregexpr('event__round', match_)[[1]]  ) 
        match_round = substring(match_round, gregexpr('static', match_round)[[1]][1]+8, gregexpr('</div', match_round)[[1]][1]-1  )[1]
      } else{
        match_round = NA
      } 
      tb_[row,'faza']    = match_stage 
      tb_[row,'kolejka'] = match_round  
    }
    print('end loop')
    #print(nrow(tb_))                            
    # 
    tryCatch(scrap_navigate( stringr::str_glue('https://www.flashscore.pl/mecz/{tb_$link_kursy[[1]]}/') ), 
             error = function(e){
               if(exists('pid')){scrap_kill_session( )}  
               if(.Platform$OS.type == 'windows'){
                 scrap_start_session(.hide = SELE_HIDE, check = F)
               }else{
                 # remote_driver$close()
                 # remote_driver$closeall()
                 print('start ses again')
                 scrap_start_session()
                 print('started ses again')
               } 
               scrap_navigate( stringr::str_glue('https://www.flashscore.pl/mecz/{tb_$link_kursy[[1]]}/') )
             })
    # scrap_navigate( stringr::str_glue('https://www.flashscore.pl/mecz/{tb_$link_kursy[[1]]}/') )
    year_1strow = remote_driver$getPageSource()[[1]] %>%
      read_html() %>% html_node('.duelParticipant__startTime') %>% html_text() %>% str_sub(7, 10) %>% as.numeric()
    tb_[1,'year'] = year_1strow
    
    tb_ = tb_ %>%
      fmutate(
        year     = zoo::na.locf0(year),
        kolejka  = zoo::na.locf0(kolejka),
        month    = as.numeric(substring(data, 4,5)),
        day      = as.numeric(substring(data, 1,2))
      ) %>%
      fmutate(
        new_year = as.integer(month > lag(month)),
        new_year_= new_year > 0) %>% 
      tidy_rownumber('new_year_', col_name = 'year_dif') %>% 
      fmutate(
        year_dif = as.integer(year_dif), 
        year2    = ifelse(new_year==1, year-year_dif, year),
        year2    = ifelse(is.na(year2), year, year2) %>% cummin(),
        data     = as.Date(paste(year2, month, day, sep = '-') )
      ) 
    cat(paste0('|change_year', sum(tb_$new_year, na.rm = T)), file = stringr::str_glue('{.log_file}'), append = T)
    
    tb_ = tb_ %>%
      fselect(-year, -year2, -new_year, -new_year_, -year_dif ,-month, -day) %>%
      fmutate(
        kraj    = kraj_,
        liga    = liga_,
        liga_nr = .liga_nr,
        liga_   = paste0(liga_, '|', season_)
      )
    
    # https://www.flashscore.pl/mecz/h6LUUFhp/#/zestawienie-kursow/powyzej-ponizej/koniec-meczu
    Sys.sleep(.time_break)
    tb_
  }, error = function(e){
    cat('|screw up, start again', file = stringr::str_glue('{.log_file}'), append = T)
    if(exists('pid')){scrap_kill_session( )} 
    Sys.sleep(1) 
    if(.Platform$OS.type == 'windows'){
      scrap_start_session(.hide = SELE_HIDE, check = F)
    }else{
      scrap_start_session()
    } 
    scrap_pilka_(
      .url          = link_ , 
      .liga_nr      = arch_$liga_nr[i],  
      .if_all_season= !IF_update,
      .time_break   = ifelse(IF_update, 1.5, 3) )
  } 
  )
}

scrap_schedule = function(.url, .liga_nr, ...){
  tryCatch({
    #print('inside func')
    cat(paste0('\n', date_, '|', .url) , file = str_glue('log/log_ftb_schedule.txt'), append = T) 
    
    scrap_navigate(.url)
    #  
    Sys.sleep( sample(seq_sampler, 1) )
    www_ = remote_driver$getPageSource()[[1]] %>%
      read_html() 
    cat('|read.html', file = str_glue('log/log_ftb_schedule.txt'), append = T)
    Sys.sleep( sample(seq_sampler, 1)  )
    # www_ = read_html(.url)  
    kraj_      = html_nodes(www_, '.breadcrumb__link') %>% html_text() %>% .[2]
    liga_      = html_nodes(www_, '.heading__name') %>% html_text() %>%
      gsub(x = ., pattern = paste0(kraj_, ': '), replacement = '') %>%
      gsub(x = ., pattern = 'Â', replacement = '') %>%
      gsub(x = ., pattern = ' -', replacement = '') %>%
      gsub(x = ., pattern = '- ', replacement = '') %>%
      str_trim() %>%
      gsub(x = ., pattern = ' ', replacement = '-') %>%
      tolower()
    season_    = html_nodes(www_, '.heading__info') %>% html_text() 
    faza_  = html_nodes(www_, '.container__livetable') %>%
      html_nodes('.container__fsbody') %>%
      html_nodes('.event__titleBox') %>%
      html_text() %>%
      .[1]
    if( length(www_ %>% html_nodes('.event__time')) == 0 ){
      cat('|nrow tb_0', file = str_glue('log/log_ftb_schedule.txt'), append = T)
      # return(invisible(NA))
      return()
    }
    #print('before tb_base')
    tb_base = data.frame(kraj    = kraj_,
                         liga    = liga_,
                         liga_nr = .liga_nr,
                         liga_   = paste0(liga_, '|', season_),
                         faza    = faza_)
    # www_ %>% html_nodes('.event__round') %>% polite::html_attrs_dfr() %>% fselect(faza = .text)
    # www_ %>% html_element("h1")  
    #print('before tb')
    #print(nrow(tb_base))
    
    tb_ = tryCatch(
      bind_cols(
        www_ %>% html_nodes('.event__time') %>% polite::html_attrs_dfr() %>% fselect(data = .text) %>% tidyr::separate(col = 'data', sep = ' ', into = c('data', 'godzina')),
        www_ %>% html_nodes('.event__participant--home') %>%polite::html_attrs_dfr() %>% fselect(team_a = .text),
        www_ %>% html_nodes('.event__participant--away') %>%polite::html_attrs_dfr() %>% fselect(team_b = .text), 
        www_ %>% html_nodes('.event__match') %>%polite::html_attrs_dfr() %>% fselect(link_kursy = id) %>% fmutate(link_kursy = substring(link_kursy, 5))
      ) %>%
        fmutate(month = substring(data, 4,5), year = NA), 
      error = function(e){ NA } )
    
    if(nrow(tb_) == 0 || is.na(tb_)){
      tb_ = bind_cols(
        www_ %>% html_nodes('.event__time') %>% polite::html_attrs_dfr() %>% fselect(data = .text) %>% tidyr::separate(col = 'data', sep = ' ', into = c('data', 'godzina')),
        www_ %>% html_nodes('.event__homeParticipant') %>%polite::html_attrs_dfr() %>% fselect(team_a = .text),
        www_ %>% html_nodes('.event__awayParticipant') %>%polite::html_attrs_dfr() %>% fselect(team_b = .text), 
        www_ %>% html_nodes('.event__match') %>%polite::html_attrs_dfr() %>% fselect(link_kursy = id) %>% fmutate(link_kursy = substring(link_kursy, 5))
      ) %>%
        fmutate(month = substring(data, 4,5), year = NA) 
    }
    
    cat(paste0('|nrow tb_', nrow(tb_)), file = str_glue('log/log_ftb_schedule.txt'), append = T)
    scrap_navigate( stringr::str_glue('https://www.flashscore.pl/mecz/{tb_$link_kursy[[1]]}/') ) 
    tb_[1,'year'] = remote_driver$getPageSource()[[1]] %>%
      read_html() %>% html_node('.duelParticipant__startTime') %>% html_text() %>% str_sub(7, 10) %>% as.numeric()
    #print('after year')
    #  
    tb_ = tb_ %>%
      fmutate(
        year  = zoo::na.locf0(year),
        month = as.numeric(substring(data, 4,5)),
        day   = as.numeric(substring(data, 1,2))
      ) %>% 
      fmutate(
        godzina  = gsub('TWK', '', godzina),
        new_year = as.integer(month > dplyr::lead(month)),
        year2    = ifelse(dplyr::lag(new_year)==1, year+1, year), 
        year2    = ifelse(is.na(year2), year, year2) %>% cummax(),
        data     = as.Date(paste(year2, month, day, sep = '-') )
      ) %>%
      fselect(-year, -year2, -new_year, -month, -day) %>% 
      fsubset(!grepl('Przeł.', godzina)) %>%
      bind_cols(tb_base)
    # print('after bind_cols') 
    tb_
  }, error = function(e) {
    cat('|fuck up, start again', file = str_glue('log/log_ftb_schedule.txt'), append = T)
    if(exists('pid')){scrap_kill_session( )} 
    Sys.sleep(1) 
    if(.Platform$OS.type == 'windows'){
      scrap_start_session(.hide = SELE_HIDE, check = F)
    }else{
      scrap_start_session()
    } 
    scrap_schedule(.url = link_ , .liga_nr = arch_$liga_nr[i])
  })
}

scrap_odds = function(.lnk, ...){
  
  tryCatch({       
    # 
    tb_odds = data.frame('link_kursy' = .lnk, 'odds_u_2_5' = NA, 'odds_u_3_5' = NA, 'odds_u_4_5' = NA)
    .url = paste0('https://www.flashscore.pl/mecz/', .lnk, '/#/zestawienie-kursow/powyzej-ponizej/koniec-meczu')
    # cat('\n start' , file = str_glue('log/log_ftb_odds.txt'), append = T) 
    # cat(paste0('\n', .url) , file = str_glue('log/log_ftb_odds.txt'), append = T) 
    scrap_navigate( .url )  
    
    Sys.sleep( sample(seq_sampler, 1) )
    
    remote_driver$deleteAllCookies()
    Sys.sleep( 0.3 )
    www_ = remote_driver$getPageSource()[[1]] %>% 
      read_html()
    Sys.sleep( sample(seq_sampler, 1) )
    
    tab_selected = www_ %>%  
      html_nodes('.detailOver') %>% 
      html_node('.selected') %>% 
      html_text()
    
    if(length(tab_selected) != 0 && tab_selected != 'Kursy'){
      xp_ = '/html/body/div[1]/div/div[6]/div/a[2]'
      
      click1       = tryCatch(remote_driver$findElement(using = 'xpath', value = xp_ ), error = function(e) 'error') 
      click_output = tryCatch(click1$sendKeysToElement(list("R Cran", key = "enter")), error = function(e) NA  ) 
      #  
      Sys.sleep( 0.3 )
      click1       = tryCatch(remote_driver$findElement(using = 'xpath', value = '/html/body/div[1]/div/div[7]/div[1]/div/a[2]/button' ), error = function(e) 'error') 
      click_output = tryCatch(click1$sendKeysToElement(list("R Cran", key = "enter")), error = function(e) NA  ) 
      #
      # webElem = remote_driver$findElement("css selector", "div.detail__detailOver.selected > button:nth-child(1)")  
      # webElem <-webElem$clickElement()
      # remote_driver$executeScript("document.querySelector('.detailOver').click();")
      www_ = remote_driver$getPageSource()[[1]] %>% 
        read_html()
    }
    odds_tabelka = www_ %>%  
      html_nodes('.oddsTab__tableWrapper')
    
    if( length(odds_tabelka) >= 1){
      print('odds TABLE')
      # naglowki  : RAZEM powyzej ponizej
      # odds_naglowki = www_ %>%  
      #   html_nodes('.ui-table__header')
      
      odds_kursy = www_ %>%  
        html_nodes('.ui-table__body')
      # linia pierwsza czesc
      # odds_kursy %>%  html_text()
      
      # dane z tabelek : kursy 
      ods = odds_kursy %>%
        html_nodes('.ui-table__row') %>%
        html_text() 
      
      # odds_linia_ = odds_tabelka  %>%     
      #   html_nodes( xpath=".//span[@class='oddsCell__noOddsCell']") %>%
      #   html_text()  
      # odds_id_poprawne = c( which( odds_linia_ != lead(odds_linia_)))
      # odds_id_poprawne = c(odds_id_poprawne, max(odds_id_poprawne)+1)
      # 
      # for( odds_line in c('2.5','3.5', '4.5')){
      #   id_linia = which( odds_linia_[odds_id_poprawne] == odds_line)[1]
      #   print(id_linia)
      #   print(paste0('linia ', odds_line))
      #   if( length(id_linia) >= 1 && !is.na(id_linia) && is.numeric(id_linia) && id_linia <= length(odds_kursy) ){ 
      #     print(paste0('before linia ', odds_line))
      #     print(paste0('len odds', length(odds_kursy)))
      #     tb_odds[,paste0('odds_u_',gsub('\\.', '_', odds_line))] =  
      #       odds_kursy[[ id_linia ]] %>%     
      #       html_nodes(  xpath=".//a[@class='oddsCell__odd oddsCell__highlight ']") %>%
      #       html_text() %>% as.numeric() %>% min
      #     # tb_odds[i,]
      #     print(paste0('after linia ', odds_line))
      #   }  
      # }
      for( odds_line in c('2.5','3.5', '4.5')){
        # id_linia = which( odds_linia_[odds_id_poprawne] == odds_line)[1]
        print(id_linia)
        print(paste0('linia ', odds_line))
        id_linia = ods[str_detect(ods, paste0('^',odds_line))]
        id_linia = gsub(paste0('^',odds_line), '', id_linia)
        # numbers <- str_extract_all(id_linia, "\\d+\\.\\d{1}")[[1]]
        odds_ = c()
        for( odd_ in id_linia){
          id_dot = gregexpr('\\.', odd_)[[1]][2]
          odd_min = c(
            as.numeric(substring(odd_, 1, id_dot-2)),
            as.numeric(substring(odd_, id_dot-1))
          ) %>%
            min(na.rm = T)
          odds_ = c(odds_, odd_min)
        } 
        tb_odds[,paste0('odds_u_',gsub('\\.', '_', odds_line))] = fmedian(odds_)
        # if( length(id_linia) >= 1 ){ 
        #   print(paste0('before linia ', odds_line))
        #   print(paste0('len odds ', length(id_linia)))
        #   tb_odds[,paste0('odds_u_',gsub('\\.', '_', odds_line))] =  
        #     odds_kursy[[ id_linia ]] %>%     
        #     html_nodes(  xpath=".//a[@class='oddsCell__odd oddsCell__highlight ']") %>%
        #     html_text() %>% as.numeric() %>% min
        #   # tb_odds[i,]
        #   print(paste0('after linia ', odds_line))
        # }  
      }
      
    }else{
      print('no odds')
    }
    # cat('\n end' , file = str_glue('log/log_ftb_odds.txt'), append = T) 
    Sys.sleep( 0.3 )
    # if(F){
    #   odds_kursy = odds_tabelka  %>%
    #     html_nodes(xpath = '//*[@class="oddsCell__odd  "]') %>%
    #     html_text()
    #   
    #   odds_tabelka  %>%
    #     html_nodes(xpath = '//*[@class="oddsCell__bookmakerPart"]')
    #   
    #   odds_book = odds_kursy %>%
    #     html_attr('href') %>%
    #     substr(1, 14) 
    # }
    print(nrow(tb_odds))
    tb_odds
  }, error = function(e) {
    assign('i_try', i_try+1,envir = globalenv())
    print(str_glue('try:{i_try}'))
    # cat('\n fuck up, start again', file = str_glue('log/log_ftb_odds.txt'), append = T)
    if(exists('pid')){scrap_kill_session( )} 
    Sys.sleep(1) 
    scrap_start_session()   
    scrap_odds(.lnk = link_ )
    
  })
}

# 2.3. SCRAP SCHEDULE ##############################################################

if(ARG_TYPE == 'schedule'){
  arch_ = read.csv('data/TAB_league_list.csv') %>%
    # fsubset(select==1) %>%
    fselect(wyniki, terminarz, kraj, liga, liga_nr)  %>% 
    fmutate(terminarz = gsub('spotkania','mecze', terminarz))  
  # 
  scrap_start_session() 
  # 
  TAB_sched = data.frame()
  for(i in 1:nrow(arch_) ){ # nrow(arch_) link_ = 'https://www.flashscore.pl/pilka-nozna/europa/liga-konfetrencji/mecze'
    link_ = arch_$terminarz[i]
    print(link_) ; print(paste0(which(arch_$terminarz == link_), '/', nrow(arch_)))
    
    TAB_sched = rbind(TAB_sched, scrap_schedule(.url = link_ , .liga_nr = arch_$liga_nr[i]) ) 
    
    Sys.sleep( sample(seq_sampler, 1) ) 
  }
  #  
  TAB_schedule_raw = arrow::read_parquet('data/ftb/TAB_terminarz.parquet') %>% tidy_slice_rows('link_kursy') %>% fmutate(update=Sys.time())
  # 
   
  funique(rowbind(
    TAB_schedule_raw,
    funique(fsubset(TAB_sched, !is.na(team_a) )) %>% fmutate(update=Sys.time(), odds_u_2_5=NA,odds_u_3_5=NA,odds_u_4_5=NA) 
  )) %>%
    roworderv(c('link_kursy', 'update'), decreasing = c(T, T)) %>%
    tidy_slice_rows('link_kursy') %>%
    fselect(-update) %>%
    fsubset(between(data, date_-3, date_ + 33 )) %>%
    arrow::write_parquet('data/ftb/TAB_terminarz.parquet') 
  
  # 2.4. SCRAP UPDATE ##############################################################
} else if(ARG_TYPE == 'update'){                          
  IF_update = T
  arch_ = read.csv('data/TAB_league_list.csv') %>%
    # fsubset(select==1) %>%
    fselect(wyniki, terminarz, kraj, liga, liga_nr)  %>% 
    fmutate(terminarz = gsub('spotkania','mecze', terminarz)) 
  #
  TAB_archiwum = 
    arrow::read_parquet('data/TAB_archiwum.parquet') %>%  
    group_by(liga) %>%
    mutate(id_season = row_number()) %>%
    ungroup() %>%
    fsubset(id_season %in% 1:3) %>% # How many recent seasons?
    mutate(#url = paste0('https://www.flashscore.pl/', url, 'wyniki/'), 
      id_ = row_number()) %>%
    join(arch_[,c('wyniki', 'kraj', 'liga')] %>% frename(ligaa = liga) %>% fmutate(wyniki = gsub('\\/wyniki', '', wyniki)), 
         on = c('liga' = 'wyniki'))
  
  if(IF_update){
    TAB_archiwum = fsubset(TAB_archiwum, id_season %in% c(1)) %>%
      fmutate(url = paste0(liga, '/', 'wyniki/')) #%>%
      #fsubset(!is.na(kraj))
  }else{
    TAB_archiwum = TAB_archiwum %>%  
      fmutate(
        tmp = gsub('https://www\\.flashscore\\.pl\\/pilka-nozna\\/', '', liga),
        n   = str_count(tmp, '/')
      ) %>%
      tidyr::separate(col = tmp, into = c('kraj', 'liga'), sep = '/') %>%
      fmutate(
        url = paste('https://www.flashscore.pl/pilka-nozna', '/', kraj, '/', url, '/wyniki', sep = '')
      ) %>%
      fselect(-kraj,-liga, -n) 
  }
  scrap_start_session()
  
  TAB_arch_tmp = data.frame()
  arch_        = TAB_archiwum   
  for(i in 1:nrow(arch_) ){ # i = 261   
    link_ = arch_$url[i] 
    print(link_) ; print(paste0(which(arch_$url == link_), '/', nrow(arch_)))
    # 
    TAB_arch_tmp = rowbind(
      TAB_arch_tmp,
      scrap_pilka_(
        .url          = link_ , 
        .liga_nr      = arch_$liga_nr[i],  
        .if_all_season= !IF_update,
        .time_break   = ifelse(IF_update, 1.5, 3) ) 
    ) 
  }  
  TAB_arch_tmp = TAB_arch_tmp %>% 
    fmutate(across(grep('team_(a|b)', names(.), value = T), function(x) {trimws(gsub('\\([^)]+\\)', '', x))} )) %>%
    fmutate(info = case_when(
      grepl('Walkower',  godzina) ~ 'Walkower',
      grepl('Pokarn.',   godzina) ~ 'Karne',
      grepl('Podogr.',   godzina) ~ 'Dogrywka',
      grepl('Anulowane', godzina) ~ 'Anulowane',
      T ~ '' ),
      godzina = gsub('(Walkower)|(Pokarn.)|(Podogr.)|(Anulowane)', '', godzina)
    ) 
  # save 
  if(IF_update){  
    TAB_arch     = arrow::read_parquet('data/ftb/TAB_historical.parquet')  
    TAB_arch     = rbind(TAB_arch, TAB_arch_tmp) %>% funique()
  }else{
    TAB_arch     = TAB_arch_tmp
  } 
  # 
  fsubset(TAB_arch, !is.na(team_a) & !is.na(team_score_a) & !is.na(team_score_b) & !is.na(data)) %>% 
    tidy_slice_rows(by_ = c('kraj', 'liga', 'liga_', 'liga_nr', 'data',  'team_a', 'team_b')) %>%
    funique() %>%
    arrow::write_parquet('data/ftb/TAB_historical.parquet')  
   
} else if(ARG_TYPE == 'odds'){
  # 2.5. SCRAP ODDS ##############################################################
  TAB_schedule_raw = arrow::read_parquet('data/ftb/TAB_terminarz.parquet') %>% tidy_slice_rows('link_kursy')
  TAB_odds_raw     = arrow::read_parquet('data/ftb/TAB_odds.parquet')  %>% tidy_slice_rows('link_kursy')
  
  TAB_sched =  
    fsubset(TAB_schedule_raw, 
            !is.na(link_kursy) & is.na(odds_u_3_5) & 
              between(data, date_, date_ + 3 ) & 
              !(!lubridate::month(data) %in% c(6,7,8) & liga %in% c('międzyklubowe-towarzyskie'))) %>%
    fsubset(
      !(kraj %in% c('Hiszpania', 'Włochy') & liga_nr %in% c('4', '5')) & 
        !(kraj %in% c('Algieria', 'Maroko') & liga_nr %in% c('2')) &
        !(kraj %in% c('Burundi', 'San Marino', 'Angola', 'Dominikana', 'Rosja'))
    ) 
  #
  scrap_start_session() 
  #
  TAB_odds = data.frame()
  for(i in 1:nrow(TAB_sched) ){
    link_ = TAB_sched$link_kursy[i]
    print(link_) ; print(paste0(which(TAB_sched$link_kursy == link_), '/', nrow(TAB_sched)))
    
    i_try = 1
    TAB_odds = rbind(TAB_odds, scrap_odds(.lnk = link_) )  
    
    Sys.sleep( sample(seq(0.5, 1, 0.2), 1) ) 
  } 
  #  
  TAB_odds_ = rowbind(
    fsubset(TAB_schedule_raw, !link_kursy %in% c(TAB_odds$link_kursy)),
    fsubset(TAB_schedule_raw, link_kursy %in% c(TAB_odds$link_kursy)) %>% 
      select(-matches('odds')) %>%
      dplyr::left_join(TAB_odds, by = 'link_kursy') 
  ) %>% 
    roworderv('data') %>% 
    funique() 
  # SAVE odds
  funique(rowbind(TAB_odds_raw, select(TAB_odds_, link_kursy, matches('odds')))) %>%
    fsubset(!is.na(odds_u_2_5) | !is.na(odds_u_3_5) | !is.na(odds_u_4_5)) %>% 
    tidy_slice_rows('link_kursy') %>%
    arrow::write_parquet('data/ftb/TAB_odds.parquet')  
  # SAVE schedule
  rowbind(
    fsubset(TAB_schedule_raw, !link_kursy %in% c(TAB_odds$link_kursy)),
    fsubset(TAB_schedule_raw, link_kursy %in% c(TAB_odds$link_kursy)) %>% 
      select(-matches('odds')) %>%
      dplyr::left_join(TAB_odds, by = 'link_kursy') 
  ) %>%
    arrow::write_parquet('data/ftb/TAB_terminarz.parquet') 
 
}

if(F){

# Update TAB_league_list --------------------------------------------------
  # update links [scrap_archiwum.R]
  TAB_archiwum %>%  
    fmutate(
      tmp = gsub('https://www\\.flashscore\\.pl\\/pilka-nozna\\/', '', liga),
      n   = str_count(tmp, '/')
    ) %>%
    tidyr::separate(col = tmp, into = c('kraj', 'liga'), sep = '/') %>%
    fmutate(
      wyniki    = paste('https://www.flashscore.pl/pilka-nozna', '/', kraj, '/', url, '/wyniki', sep = ''),
      terminarz = paste('https://www.flashscore.pl/pilka-nozna', '/', kraj, '/', url, '/mecze', sep = '')
    ) %>%
    fsubset(id_season==1) %>%
    fselect(wyniki, terminarz, kraj, liga, liga_nr) %>%
    write.csv('data/TAB_league_list.csv')
  
  # logs
  library(readr) ; library(purrr)
  fls = 'log/log_ftb_schedule.txt'
  fls = 'log/log_ftb_odds.txt'
  fls = 'log/log_ftb_history_update.txt'
  
  df_log = 
    read_lines(fls) %>% 
    keep(grepl('tb_0', .)) %>%
    as.data.frame() %>%
    setnames('path') %>%
    tidyr::separate(col = path, into = c('date', 'www', 'info'), sep = '\\|') %>%
    fmutate(n = parse_number(info)) %>%
    group_by(www) %>% tally %>% arrange(desc(n)) %>%
    pull(www) %>%
    substring(.,1, str_length(.)-1)
   
  arch_ = read.csv('data/TAB_league_list.csv') %>%
    fmutate(select = fifelse(wyniki %in% df_log, 0, 1)) 
  
  write.csv(arch_, 'data/TAB_league_list.csv')
  
 
 
}
 
 
