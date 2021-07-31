Sys.setlocale("LC_CTYPE","russian")
Sys.setlocale("LC_COLLATE","russian")
Sys.setlocale("LC_TIME", "English")
library(tidyverse)
library(scales)

# ---------------- директория, в которой хранится файл XML
dir <- "D:/Data/rkn/20210725/" 
file<-list.files(dir, full.names = TRUE) %>% .[grepl(".xml$",.)]

# ---------------- обработка 
data <-read_file(file)
data <- gsub(pattern="\\\r\\\n\\\t\\\t\\\t", replacement = "\\|",data) # takes 1 minutes
write_lines(data, paste0(dir, "/test.csv"))

# ---------------- рубим файл на менее крупные фрагменты
data <- readLines(paste0(dir, "/test.csv"), encoding = "UTF-8")
end_of_records <- rev(grep(pattern = '</rkn:record>', data))       # ~ 1 minute
article = length(end_of_records)

df <- data.frame(article = NA, rom = data, stringsAsFactors = FALSE)

k <- c(seq(article, 1, by = -5000),1)
v<- c(end_of_records[k])
v[1]<-1
f<-0

for (i in 1:(length(v)-1)){
  l <- df[(v[i]+1):v[i+1],]
  f<- f + NROW(l)
  write_excel_csv(l, paste0(dir, i,"_test.csv"))
}  
remove(df, data)
gc()

# в директории появились файлы с именем _test.csv

# ---------------- парсим их и объединяем их в 1 файл
data <- data.frame()
for (i in 1:30){
  l <- read_csv(paste0(dir,i,"_test.csv"))
  e <- rev(grep(pattern = '</rkn:record>', l$rom))
  a = length(e)
  
  for(z in e){
    l$article[1:z] <- a
    print(z)
    a = a - 1
  }
  l2 <- l %>%
    mutate(rom = gsub("<rkn:founders>\\|<rkn:founder>\\| <rkn:name>",
                      "<rkn:founder>\\<founder_name>", rom)) %>% 
    mutate(rom = gsub("</rkn:inn>\\| <rkn:id>", 
                      "</rkn:inn>\\|founder_id:", rom)) %>% 
    mutate(rom = gsub("</rkn:name>\\| <rkn:inn>", 
                      "</rkn:name>\\|inn:", rom)) %>% 
    mutate(rom = gsub('"true"', "true", rom)) %>% 
    mutate(rom = str_trim(gsub("\\\t","",rom))) %>% 
    mutate(tag = sapply(str_extract(rom, pattern = "^\\<[[:alnum:]\\_:=\\s]+\\>"), 
                        function(x) unlist(x[[1]]))
           ) %>% 
    mutate(value = sapply(str_replace_all(rom, 
                        pattern = "\\<[[:alnum:]\\/\\_:=\\s]+\\>", 
                        replacement =""), 
                        function(x) unlist(x))
    ) %>% 
    filter(!is.na(tag)) %>% 
    filter(tag!="<rkn:record>") %>% 
    mutate(article = paste0(i,"_",article))
  print(paste0("left - ", 30-i))
  data <- rbind(data, l2)
}  

data <- data %>% 
  mutate(tag = str_trim(tag), value = str_trim(value)) %>% 
  mutate(tag = gsub("<rkn:","",tag)) %>% 
  mutate(tag = gsub(">","",tag)) %>% 
  mutate(tag = sub(" xsi:nil=true","", tag))

x <- data %>% filter(tag == "id") %>% 
  select(article, id=value) %>% unique() %>% na.omit
data <- data %>% left_join(x)  

# данные в виде длинной (2,78 млн. строк) таблицы {id - атрибут - значение} 
data %>% select(id,tag,value) %>% 
  write_excel_csv(paste0(dir, "data_rkn.gz"))

# ---------------- превращаем в широкую таблицу 
data <- data %>% select(id, tag, value) %>% 
  pivot_wider(names_from = tag, values_from = value, id_cols = id, names_repair = "unique")

#  получилось 149,7 тыс строк
write_excel_csv(data, paste0(dir, "data_rkn_wide.gz"))

# удаляем ненужные test.csv файлы
list.files(dir, full.names = TRUE) %>% 
  .[grepl("_test.csv",.)] %>%
  walk(~file.remove(.x))
  
remove(x, data, l,l2)
gc()

# ---------------- убираем не-журналы
# если мы ищем только научные издания, то можем удалить газеты, каналы и программы
rkn_data <- read_csv(paste0(dir, "data_rkn_wide.gz"))

rkn_data <- rkn_data %>% 
  filter(!grepl("газета", form_spread)) %>% 
  filter(!grepl("программа", form_spread)) %>% 
  filter(!grepl("канал", form_spread))

# это сократило таблицу до 70,7 тыс строк
write_excel_csv(rkn_data, paste0(dir, "data_rkn_wide.gz"))

# ---------------- извлекаем информацию об учредителях
rkn_data <- read_csv(paste0(dir, "data_rkn_wide.gz"))

rkn_data$founder <- gsub('~', '', rkn_data$founder)

# заменяем ||| на |~
rkn_data$founder <- gsub('\\|\\|\\|', '~', rkn_data$founder)

rkn_data <- rkn_data %>% 
  mutate(founder = str_split(founder, "~")) %>% 
  unnest(founder) %>% 
  mutate(founder = str_replace(founder, "^[\\s\\|]|[\\s\\|]$", "")) 

#### выделяем информацию об издателе 
rkn_data <- rkn_data %>% 
  mutate(founder_id = str_extract(founder, "(?<=founder_id:)[^\\|]+")) %>% 
  mutate(founder_inn = str_extract(founder, "(?<=inn:)\\d+")) %>% 
  mutate(founder_name = str_extract(founder, "^.+?(?=\\|)")) %>% 
  mutate_at(c("name", "founder_name"), ~gsub("&quot;", "", .x))          

rkn_data <- rkn_data %>% 
  select(-rus_name, -founder, -1) %>% 
  rename(rkn_id = 1)

# файл вида 79,9 тыс строк
write_excel_csv(rkn_data, paste0(dir, "data_rkn_wide.gz"))

# Rows: 79,927
# Columns: 20
#   $ rkn_id           <dbl> 85840, 85841, 85841, 85841, 85847, 85847, 85847, 85847, 85869, 85870, 85870,~
#   $ name             <chr> "Домашняя коллекция", "Новая юстиция. Журнал судебных прецедентов", "Новая ю~
#   $ reg_number       <chr> "№ 0110638", "ПИ № ФС 77 - 30918", "ПИ № ФС 77 - 30918", "ПИ № ФС 77 - 30918~
#   $ reg_number_id    <dbl> 85854, 278045, 278045, 278045, 85861, 85861, 85861, 85861, 406913, 85884, 85~
#   $ status_id        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
#   $ reg_date         <date> 1993-05-06, 2008-01-18, 2008-01-18, 2008-01-18, 1993-05-12, 1993-05-12, 199~
#   $ langs            <chr> "русский", "русский", "русский", "русский", "английский, русский, немецкий, ~
#   $ form_spread      <chr> "печатное СМИ журнал", "печатное СМИ журнал", "печатное СМИ журнал", "печатн~
#   $ form_spread_id   <dbl> 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, ~
#   $ territory        <chr> "Санкт-Петербург (Российская Федерация)", "зарубежные страны, Российская Фед~
#   $ territory_ids    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
#   $ staff_address    <chr> "190000, Санкт-Петербург г.", "127006, Москва г., пер. Воротниковский, д. 7,~
#   $ domain_name      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
#   $ annulled_date    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
#   $ suspension_date  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
#   $ termination_date <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
#   $ status_comment   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
#   $ founder_id       <chr> NA, "709626", "709613", "709619", NA, NA, NA, NA, "1541170", "1977208", "197~
#   $ founder_inn      <chr> NA, "7710583536", NA, "7716082092", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
#   $ founder_name     <chr> NA, "Общество с ограниченной ответственностью Редакционно-издательское объед~
