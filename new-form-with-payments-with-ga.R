library(dplyr)
require(dplyr)
library(ggplot2)

all_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf_2015-07-06-12.csv', sep=";", header=TRUE)
all_data <- mutate(all_data, book_day = as.Date(as.character(gsub("\\s.*", "", Дата.бронирования)), '%d.%m.%Y'))

all_data <- mutate(all_data, id1 = gsub("(\\d+)-\\d+\\s*$", "!\\1", X..брони))
all_data <- mutate(all_data, ID = gsub("^.*!", "", id1))

all_data <- mutate(all_data, W1 = strftime(as.POSIXct(book_day),format="%W"))
all_data <- mutate(all_data, Week = as.numeric(W1) + 1)

#all_data <- mutate(all_data, Week = paste0("d", strftime(as.POSIXct(book_day),format="%m-%d")))

#all_data <- mutate(all_data, Week = strftime(as.POSIXct(book_day),format="%m"))
#all_data <- mutate(all_data, Week = strftime(as.POSIXct(as.character(Дата.бронирования), format = '%d.%m.%Y %H:%M'),format="%H"))
#all_data <- mutate(all_data, WeekDay = strftime(as.POSIXct(as.character(Дата.бронирования), format = '%d.%m.%Y %H:%M'),format="%a"))

#weekend_data <- filter(all_data, WeekDay == "Сб" | WeekDay == "Вс")
#all_data <- filter(all_data, WeekDay != "Сб" | WeekDay != "Вс")
#all_data <- filter(all_data, WeekDay == "Сб")
#all_data <- filter(all_data, as.POSIXct(book_day) != as.POSIXct("2015-06-12"))
#all_data <- filter(all_data, as.POSIXct(book_day) >= as.POSIXct("2015-05-25")&as.POSIXct(book_day) < as.POSIXct("2015-06-01"))

#all_data <- filter(all_data, ID != 11)

#all_data <- filter(all_data, !grepl("Отмена", Статус.брони))
all_data <- filter(all_data, Ночей > 0)

all_data <- mutate(all_data, Способ.оплаты = gsub("\\s\\[.*", "", Способ.оплаты))



all_data <- mutate(all_data, banks = ifelse(Способ.оплаты == "Банк. карта", 1, 0))
all_data <- mutate(all_data, checkin = ifelse(Способ.оплаты == "При заселении", 1, 0))
all_data <- mutate(all_data, beznal1 = ifelse(Способ.оплаты == "Безнал для юр.лиц", 1, 0))
all_data <- mutate(all_data, beznal2 = ifelse(Способ.оплаты == "Банковский перевод для физ.лиц", 1, 0))
all_data <- mutate(all_data, other = ifelse(Способ.оплаты != "При заселении" & Способ.оплаты != "Банк. карта" & Способ.оплаты != "Безнал для юр.лиц" & Способ.оплаты != "Банковский перевод для физ.лиц", 1, 0))


#ids <- group_by(all_data, ID, book_day)
#ids <- group_by(all_data, ID)
#ids <- group_by(all_data, Week)
ids <- group_by(all_data, ID, Week)
(per_day <- summarise(ids, bookings = n(), costs = sum(as.numeric(as.character(Стоимость..руб.)), na.rm=TRUE), nights = sum(Ночей), guests = sum(Гостей), rooms = sum(Номеров)
#                      , Гостиниц = n_distinct(ID)
                      , При.заселении = sum(checkin)
                      , banks = sum(banks)
                      , Безнал.для.юр.лиц = sum(beznal1)
                      , Банковский.перевод.для.физ.лиц = sum(beznal2)
                      , Другие = sum(other)
))

#tt <- filter(all_data, ID == 100 & Week == 12)
#colnames(tt)[6] <- "bCost"

#con <- file("res1.csv",encoding="UTF-8")
#write.csv(tt,con,row.names=FALSE)

#ff<- tt %>%
#  summarise(costs = sum(as.numeric(Стоимость..руб.), na.rm=TRUE), bookings = n())

  #max(as.numeric(Стоимость..руб.)



per_day <- mutate(per_day, avg_cost = costs/bookings)
per_day$avg_cost <- format(round(per_day$avg_cost, 2), nsmall=2, big.mark=" ")
per_day$costs <- format(round(per_day$costs, 2), nsmall=2, big.mark=" ")

#per_day <- per_day %>% select(Week:costs, avg_cost, nights:rooms, Гостиниц:Другие)
#colnames(per_day) <- c("Неделя", "Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров","Гостиниц", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие")
#per_day <- per_day %>% 
#mutate_each(funs(), "Броней","Стоимость броней")

#per_day[,"Броней"] <- per_day[,"Броней"]/10
#per_day[,"Броней"] <- per_day[,"Стоимость броней"]/10


#con <- file("res.csv",encoding="UTF-8")
#write.csv(per_day,con,row.names=FALSE)
#date_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/2015-06-16.csv', sep=";", header=TRUE, colClasses='character')

#date_data[, "Броней"] <- as.numeric(date_data[, "Броней"])

#p <- ggplot(per_day, aes(x=Неделя, y=Броней, group=1, colour = "Group1")) +
# p <- ggplot() + 
#   #    geom_bar(stat="identity")
# #  geom_line(color="#aa0022", size=1.25) +
#   geom_line(data=per_day, aes(x=Неделя, y=Броней, group=1, colour = "Среднее значение"), size=1.25) +
#   #geom_point(color="#aa0022", size=3.5) +
#   #geom_line(data=date_data, aes(x=Неделя, y=Броней),group=1, color="#E0790B", size=1.3) +
# #  geom_line(data=date_data, aes(x=Неделя, y=Броней, group = 1, colour = "16 июня"), size=1.3) + 
#   geom_smooth(data=per_day, aes(x=Неделя, y=Броней, group = 1, colour = "Линия тренда"), se=FALSE) +
#   scale_colour_manual("", 
#                       breaks = c("Среднее значение", "16 июня", "Линия тренда"),
#                       values = c("Среднее значение"="#aa0022", "16 июня"="#E0790B", "Линия тренда"="blue")) +
#   
#   ggtitle("Брони по часам") +
#   labs(x="Час", y="Количество") +
#   #geom_smooth(method='gam', se=FALSE)+
#   
#   expand_limits(x = 0, y = 0) + 
# #  scale_x_continuous(minor_breaks = seq(10,23)) +
#   theme(axis.title.y = element_text(size=8, family="Trebuchet MS", color="#666666")) +
#   theme(axis.text = element_text(size=10, family="Trebuchet MS")) +
#   theme(plot.title = element_text(size=14, family="Trebuchet MS", face="bold", hjust=0, color="#666666"))
# 
# ggsave("bookings-by-hour-working_days.png", width=14, height=8, p)


all_providers <- read.csv('C:/Users/sergey.sergeev/Documents/!/all_providers.csv', sep=";", header=TRUE)
all_providers <- all_providers[1:7]

merged_table <- merge(x = per_day, y = all_providers,by="ID",all.x=TRUE)

#merged_table <- filter(merged_table, status!= 0)

#merged_table <- mutate(merged_table, avg_cost = costs/bookings)


ga_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf_2015-07-06-12-ga.csv', sep=";", header=TRUE)
#ga_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf-ga-2015-05-25-06-21-sample.csv', sep=";", header=TRUE)
ga_data <- mutate(ga_data, id1 = gsub("/hotel/(\\d+)/.*$", "!\\1", url))
ga_data <- mutate(ga_data, ID = gsub("^.*!", "", id1))


ga_data <- ga_data %>%
  group_by(ID, Week) %>%
  select(ID, Week, Views) %>%
  #group_by(ID) %>%
  #select(ID, Views) %>%
  
  summarise(
    total_views = sum(Views)
  )
merged_table <- merge(x=merged_table, y=ga_data, by=c("ID", "Week"),all.x=TRUE)
#merged_table <- merge(x=merged_table, y=ga_data, by=c("ID"),all.x=TRUE)
merged_table <- mutate(merged_table, conversion = bookings/total_views)
merged_table <- merged_table %>% select(ID:costs, avg_cost, total_views, conversion, nights:rooms, При.заселении:Другие, Гостиница:Регион)


#merged_table <- merged_table %>% select(ID:costs, avg_cost, nights:rooms, При.заселении:Другие, Гостиница:Регион)


#merged_table$conversion <- format(round(merged_table$conversion, 2), nsmall=2, big.mark=" ")

# merged_table %>%
#   ggplot(aes(x=book_day, y=bookings, group=1)) + 
#   geom_bar(stat="identity") +
#   facet_wrap(~ ID)  
#colnames(merged_table)
#colnames(merged_table) <- c("ID", "Броней","Стоимость броней","Ср чек","Визиты","Конверсия","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")
colnames(merged_table) <- c("ID","Неделя", "Броней","Стоимость броней","Ср чек","Визиты","Конверсия","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")
#colnames(merged_table) <- c("ID","Дата бронирования","Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")
#colnames(merged_table) <- c("ID","Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")
#colnames(merged_table) <- c("ID","Неделя", "Броней","Стоимость броней","Ср чек","Ночей","Гостей","Номеров", "При заселении", "Банк карта", "Безнал юр лица", "Безнал физ лица", "Другие", "Гостиница","Город","Сайт","Страна","Регион")

con <- file("res.csv",encoding="UTF-8")
write.csv(merged_table,con,row.names=FALSE)