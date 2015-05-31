library(dplyr)
require(dplyr)

all_data <- read.csv('C:/Users/sergey.sergeev/Documents/!/nf-01.csv', sep=";", header=TRUE)
all_data <- mutate(all_data, book_day = gsub("\\s.*", "", Дата.бронирования))

ids <- group_by(all_data, ID, book_day)
(per_day <- summarise(ids, bookings = n(), costs = sum(Стоимость..руб.)))

all_providers <- read.csv('C:/Users/sergey.sergeev/Documents/!/Аналитика/По дате выезда/all_providers.csv', sep=";", header=TRUE)
all_providers <- all_providers[1:7]

merged_table <- merge(x = per_day, y = all_providers,by="ID",all.x=TRUE)

merged_table <- filter(merged_table, status!= 0)

merged_table <- mutate(merged_table, avg_cost = costs/bookings)
merged_table <- merged_table %>% select(ID:costs, avg_cost, Гостиница:Регион)

#merged_table <- sapply(merged_table, function(x) format(round(x, 2), nsmall=2))

merged_table$avg_cost <- format(round(merged_table$avg_cost, 2), nsmall=2, big.mark=" ")
merged_table$costs <- format(round(merged_table$costs, 2), nsmall=2, big.mark=" ")