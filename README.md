# parsing-rkn-register
Скрипт на R для парсинга XML-файла с реестром зарегистрированных СМИ (открытые данные Роскомнадзора)

Данные РКН: https://rkn.gov.ru/opendata/7705846236-ResolutionSMI/

Скрипт нарезает большой XML файл на 30 фрагментов по 5000 записей и парсит XML в простую таблицу вида 

Columns: 20
* $ rkn_id           <dbl> 85840, 85841, 85841, 85841, 85847, 85847, 85847, 85847, 85869, 85870, 85870,~
* $ name             <chr> "Домашняя коллекция", "Новая юстиция. Журнал судебных прецедентов", "Новая ю~
* $ reg_number       <chr> "№ 0110638", "ПИ № ФС 77 - 30918", "ПИ № ФС 77 - 30918", "ПИ № ФС 77 - 30918~
* $ reg_number_id    <dbl> 85854, 278045, 278045, 278045, 85861, 85861, 85861, 85861, 406913, 85884, 85~
* $ status_id        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
* $ reg_date         <date> 1993-05-06, 2008-01-18, 2008-01-18, 2008-01-18, 1993-05-12, 1993-05-12, 199~
* $ langs            <chr> "русский", "русский", "русский", "русский", "английский, русский, немецкий, ~
* $ form_spread      <chr> "печатное СМИ журнал", "печатное СМИ журнал", "печатное СМИ журнал", "печатн~
* $ form_spread_id   <dbl> 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, ~
* $ territory        <chr> "Санкт-Петербург (Российская Федерация)", "зарубежные страны, Российская Фед~
* $ territory_ids    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
* $ staff_address    <chr> "190000, Санкт-Петербург г.", "127006, Москва г., пер. Воротниковский, д. 7,~
* $ domain_name      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
* $ annulled_date    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
* $ suspension_date  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
* $ termination_date <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
* $ status_comment   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
* $ founder_id       <chr> NA, "709626", "709613", "709619", NA, NA, NA, NA, "1541170", "1977208", "197~
* $ founder_inn      <chr> NA, "7710583536", NA, "7716082092", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
* $ founder_name     <chr> NA, "Общество с ограниченной ответственностью Редакционно-издательское объед~
  
Время выполнения скрипта на ноутбуке AMD Quad-Core A10-9620P с 8 Гб памяти от 10 до 20 минут (точно не засекал).
