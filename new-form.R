library(dplyr)
require(dplyr)

all_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf-01.csv', sep=";", header=TRUE)
all_data <- mutate(all_data, book_day = gsub("\\s.*", "", Дата.бронирования))

ids <- group_by(all_data, ID, book_day)
(per_day <- summarise(ids, bookings = n(), costs = sum(Стоимость..руб.)))

all_providers <- read.csv('C:/Users/sergey.sergeev/Documents/!/Аналитика/По дате выезда/all_providers.csv', sep=";", header=TRUE)
all_providers <- all_providers[1:7]

merged_table <- merge(x = per_day, y = all_providers,by="ID",all.x=TRUE)