library(dplyr)
require(dplyr)

all_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/out-2015-06-03-all.csv', sep=";", header=TRUE)
all_data <- mutate(all_data, book_day = gsub("\\s.*", "", Дата.бронирования))

all_data <- filter(all_data, !grepl("Отмена", Статус.брони))

all_data <- mutate(all_data, Способ.оплаты = gsub("\\s\\[.*", "", Способ.оплаты))
#all_data <- all_data %>%
#  filter(Способ.оплаты == "Банк. карта")%>%
#  mutate(Банк.карта = 1)

all_data <- mutate(all_data, banks = ifelse(Способ.оплаты == "Банк. карта", 1, 0))
all_data <- mutate(all_data, checkin = ifelse(Способ.оплаты == "При заселении", 1, 0))
all_data <- mutate(all_data, beznal1 = ifelse(Способ.оплаты == "Безнал для юр.лиц", 1, 0))
all_data <- mutate(all_data, beznal2 = ifelse(Способ.оплаты == "Банковский перевод для физ.лиц", 1, 0))
all_data <- mutate(all_data, other = ifelse(Способ.оплаты != "При заселении" & Способ.оплаты != "Банк. карта" & Способ.оплаты != "Безнал для юр.лиц" & Способ.оплаты != "Банковский перевод для физ.лиц", 1, 0))




#tmp <- select(all_data, Способ.оплаты == "Банк. карта")
  
#all_data <- mutate(all_data, Банк.карта = 0)


#ids <- group_by(all_data, ID, book_day)
ids <- group_by(all_data, ID)
(per_day <- summarise(ids, bookings = n(), costs = sum(Стоимость..руб.), nights = sum(Ночей), guests = sum(Гостей), rooms = sum(Номеров)
                      , При.заселении = sum(checkin)
                      , banks = sum(banks)
                      , Безнал.для.юр.лиц = sum(beznal1)
                      , Банковский.перевод.для.физ.лиц = sum(beznal2)
                      , Другие = sum(other)
                      ))

#ids <- group_by(all_data, ID)
#(per_day <- summarise(ids, bookings = n(), costs = sum(Стоимость..руб.), nights = sum(Ночей), guests = sum(Гостей), rooms = sum(Номеров), banks = sum(banks)))

all_providers <- read.csv('C:/Users/sergey.sergeev/Documents/!/Аналитика/По дате выезда/all_providers.csv', sep=";", header=TRUE)
all_providers <- all_providers[1:7]

merged_table <- merge(x = per_day, y = all_providers,by="ID",all.x=TRUE)

merged_table <- filter(merged_table, status!= 0)

merged_table <- mutate(merged_table, avg_cost = costs/bookings)
merged_table <- merged_table %>% select(ID:costs, avg_cost, nights:rooms, При.заселении:Другие, Гостиница:Регион)



merged_table$avg_cost <- format(round(merged_table$avg_cost, 2), nsmall=2, big.mark=" ")
merged_table$costs <- format(round(merged_table$costs, 2), nsmall=2, big.mark=" ")

#colnames(merged_table) <- c("ID","Дата бронирования","Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров","Гостиница","Город","Сайт","Страна","Регион")
colnames(merged_table) <- c("ID","Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров","Гостиница","Город","Сайт","Страна","Регион")
colnames(merged_table) <- c("ID","Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")

con <- file("res.csv",encoding="UTF-8")
write.csv(merged_table,con,row.names=FALSE)