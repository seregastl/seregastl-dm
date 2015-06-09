library(dplyr)
require(dplyr)
library(ggplot2)

all_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/out-2015-06-08.csv', sep=";", header=TRUE)
#all_data <- mutate(all_data, book_day = gsub("\\s.*", "", Дата.бронирования))
all_data <- mutate(all_data, book_day = as.Date(as.character(gsub("\\s.*", "", Дата.бронирования)), '%d.%m.%Y'))

#all_data <- filter(all_data, as.POSIXct(book_day) >= as.POSIXct("2015-06-01"))
#all_data <- filter(all_data, as.POSIXct(book_day) >= as.POSIXct("2015-05-25")&as.POSIXct(book_day) < as.POSIXct("2015-06-01"))

all_data <- filter(all_data, !grepl("Отмена", Статус.брони))

all_data <- mutate(all_data, Способ.оплаты = gsub("\\s\\[.*", "", Способ.оплаты))


all_data <- mutate(all_data, banks = ifelse(Способ.оплаты == "Банк. карта", 1, 0))
all_data <- mutate(all_data, checkin = ifelse(Способ.оплаты == "При заселении", 1, 0))
all_data <- mutate(all_data, beznal1 = ifelse(Способ.оплаты == "Безнал для юр.лиц", 1, 0))
all_data <- mutate(all_data, beznal2 = ifelse(Способ.оплаты == "Банковский перевод для физ.лиц", 1, 0))
all_data <- mutate(all_data, other = ifelse(Способ.оплаты != "При заселении" & Способ.оплаты != "Банк. карта" & Способ.оплаты != "Безнал для юр.лиц" & Способ.оплаты != "Банковский перевод для физ.лиц", 1, 0))


ids <- group_by(all_data, ID, book_day)
#ids <- group_by(all_data, ID)
(per_day <- summarise(ids, bookings = n(), costs = sum(Стоимость..руб.), nights = sum(Ночей), guests = sum(Гостей), rooms = sum(Номеров)
                      , При.заселении = sum(checkin)
                      , banks = sum(banks)
                      , Безнал.для.юр.лиц = sum(beznal1)
                      , Банковский.перевод.для.физ.лиц = sum(beznal2)
                      , Другие = sum(other)
))

all_providers <- read.csv('C:/Users/sergey.sergeev/Documents/!/Аналитика/По дате выезда/all_providers.csv', sep=";", header=TRUE)
all_providers <- all_providers[1:7]

merged_table <- merge(x = per_day, y = all_providers,by="ID",all.x=TRUE)

merged_table <- filter(merged_table, status!= 0)

merged_table <- mutate(merged_table, avg_cost = costs/bookings)


ga_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf-2015-06-01-06-ga.csv', sep=";", header=TRUE)
#ga_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf-2015-05-25-31-ga.csv', sep=";", header=TRUE)

ga_data <- ga_data %>%
  group_by(ID) %>%
  select(ID, Views) %>%
  summarise(
    total_views = sum(Views)
  )

merged_table <- merge(x=merged_table, y=ga_data, by="ID",all.x=TRUE)
merged_table <- mutate(merged_table, conversion = bookings/total_views*100)

merged_table <- merged_table %>% select(ID:costs, avg_cost, total_views, conversion, nights:rooms, При.заселении:Другие, Гостиница:Регион)



merged_table$avg_cost <- format(round(merged_table$avg_cost, 2), nsmall=2, big.mark=" ")
merged_table$costs <- format(round(merged_table$costs, 2), nsmall=2, big.mark=" ")
merged_table$conversion <- format(round(merged_table$conversion, 2), nsmall=2, big.mark=" ")

merged_table %>%
  filter(ID  %in% c("322", "323", "1868")) %>%
  ggplot(aes(x=book_day, y=bookings, group=1)) + 
  geom_bar(stat="identity") +
  geom_smooth(method=loess, se=FALSE) + 
  facet_wrap(~ ID)  

#colnames(merged_table) <- c("ID","Броней","Стоимость броней","Ср чек","Визиты","Конверсия","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")
colnames(merged_table) <- c("ID","Дата бронирования","Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")

con <- file("res.csv",encoding="UTF-8")
write.csv(merged_table,con,row.names=FALSE)

