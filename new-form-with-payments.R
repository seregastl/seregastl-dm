library(dplyr)
require(dplyr)
library(ggplot2)

all_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/out-2015-06-11-14.csv', sep=";", header=TRUE)
all_data <- mutate(all_data, book_day = as.Date(as.character(gsub("\\s.*", "", Дата.бронирования)), '%d.%m.%Y'))

all_data <- mutate(all_data, id1 = gsub("(\\d+)-\\d+\\s*$", "!\\1", X..брони))
all_data <- mutate(all_data, ID = gsub("^.*!", "", id1))

all_data <- mutate(all_data, W1 = strftime(as.POSIXct(book_day),format="%W"))
all_data <- mutate(all_data, Week = as.numeric(W1) + 1)

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
#ids <- group_by(all_data, Week)
#ids <- group_by(all_data, ID, Week)
(per_day <- summarise(ids, bookings = n(), costs = sum(Стоимость..руб.), nights = sum(Ночей), guests = sum(Гостей), rooms = sum(Номеров)
#                      , Гостиниц = n_distinct(ID)
                      , При.заселении = sum(checkin)
                      , banks = sum(banks)
                      , Безнал.для.юр.лиц = sum(beznal1)
                      , Банковский.перевод.для.физ.лиц = sum(beznal2)
                      , Другие = sum(other)
))

per_day <- mutate(per_day, avg_cost = costs/bookings)
per_day$avg_cost <- format(round(per_day$avg_cost, 2), nsmall=2, big.mark=" ")
per_day$costs <- format(round(per_day$costs, 2), nsmall=2, big.mark=" ")

# p <- ggplot(per_day, aes(x=Week, y=Гостиниц, group=1)) + 
#   #    geom_bar(stat="identity")
#   geom_line(color="#aa0022", size=1.25) +
#   geom_point(color="#aa0022", size=3.5) +
#   ggtitle("Количество гостиниц с активными бронями") +
#   labs(x="Неделя", y="Количетво") +
#   geom_smooth(method='lm', se=FALSE)+
#   expand_limits(x = 10, y = 0) + 
#   scale_x_continuous(minor_breaks = seq(10,23)) +
#   theme(axis.title.y = element_text(size=8, family="Trebuchet MS", color="#666666")) +
#   theme(axis.text = element_text(size=10, family="Trebuchet MS")) +
#   theme(plot.title = element_text(size=14, family="Trebuchet MS", face="bold", hjust=0, color="#666666"))
# 
# ggsave("hotels.png", width=14, height=8, p)
#per_day <- per_day %>% select(Week:costs, avg_cost, nights:rooms, Гостиниц:Другие)
#colnames(per_day) <- c("Неделя", "Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров","Гостиниц", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие")
#con <- file("res.csv",encoding="UTF-8")
#write.csv(per_day,con,row.names=FALSE)



all_providers <- read.csv('C:/Users/sergey.sergeev/Documents/!/Аналитика/По дате выезда/all_providers.csv', sep=";", header=TRUE)
all_providers <- all_providers[1:7]

merged_table <- merge(x = per_day, y = all_providers,by="ID",all.x=TRUE)

merged_table <- filter(merged_table, status!= 0)

#ga_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf-2015-06-01-06-ga.csv', sep=";", header=TRUE)
#ga_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf-2015-05-25-31-ga.csv', sep=";", header=TRUE)

#ga_data <- ga_data %>%
#  group_by(ID) %>%
#  select(ID, Views) %>%
#  summarise(
#    total_views = sum(Views)
#  )
#merged_table <- merge(x=merged_table, y=ga_data, by="ID",all.x=TRUE)
#merged_table <- mutate(merged_table, conversion = bookings/total_views*100)
#merged_table <- merged_table %>% select(ID:costs, avg_cost, total_views, conversion, nights:rooms, При.заселении:Другие, Гостиница:Регион)


merged_table <- merged_table %>% select(ID:costs, avg_cost, nights:rooms, При.заселении:Другие, Гостиница:Регион)

# merged_table %>%
#   ggplot(aes(x=book_day, y=bookings, group=1)) + 
#   geom_bar(stat="identity") +
#   facet_wrap(~ ID)  

#colnames(merged_table) <- c("ID","Броней","Стоимость броней","Ср чек","Визиты","Конверсия","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")
colnames(merged_table) <- c("ID","Дата бронирования","Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")
#colnames(merged_table) <- c("ID","Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")
#colnames(merged_table) <- c("ID","Неделя", "Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")

con <- file("res.csv",encoding="UTF-8")
write.csv(merged_table,con,row.names=FALSE)

