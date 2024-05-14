file_path <- file.choose()
df_Accident_Information <- read.csv(file_path)

file_path <- file.choose()
df_Vehicle_Information <- read.csv(file_path)

df_Accident_Information <- df_Accident_Information %>%
  rename(Local_Authority_District = Local_Authority_.District.)

# вибираємо 2 транспортні засоби
df_Accident_Information_2 <- filter(df_Accident_Information, Number_of_Vehicles==2)

# вибираємо роки 2011-2015
df_Accident_Information_3 <- filter(df_Accident_Information_2, Year %in% c(2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015))

library(tidyverse)
# вибираємо колонки
c_Accident <- select(df_Accident_Information_3,
                     Accident_Index, Accident_Severity, Carriageway_Hazards, Date, Day_of_Week,
                     Junction_Detail,
                     Light_Conditions,  Number_of_Casualties,
                     Road_Surface_Conditions, Road_Type, Speed_limit,
                     Urban_or_Rural_Area, Weather_Conditions, Year,Time,Longitude,Latitude,Local_Authority_District
)

# вибираємо колонки
c_Vehicle <- select(df_Vehicle_Information,
                    Accident_Index,
                    Age_Band_of_Driver,
                    Age_of_Vehicle,
                    Sex_of_Driver,
                    make,
                    Vehicle_Manoeuvre,
                    Vehicle_Type,
                    X1st_Point_of_Impact
)


# з c_Vehicle видаляємо індекси яких немає в c_Accident
c_Vehicle_Index <- c_Vehicle %>%
  filter(Accident_Index %in% c_Accident$Accident_Index)

# з c_Accident видаляємо індекси яких немає в c_Vehicle_Index
c_Accident_Index <- c_Accident %>%
  filter(Accident_Index %in% c_Vehicle_Index$Accident_Index)

# вибір 2 водіїв з 2 датасету
duplicated_accidents <- c_Vehicle_Index %>%
  group_by(Accident_Index) %>%
  filter(n() == 2)

c_Vehicle_Index_filtered <- c_Vehicle_Index  %>%
  filter(Accident_Index %in% duplicated_accidents$Accident_Index)

# знову з c_Accident_Index видаляємо індекси яких немає в c_Vehicle_Index_filtered
c_Accident_Index_filtered <- c_Accident_Index %>%
  filter(Accident_Index %in% c_Vehicle_Index_filtered$Accident_Index)


df1 <- c_Vehicle_Index_filtered %>%
  filter(row_number() %% 2 == 1)  # Відбирає непарні рядки

df2 <- c_Vehicle_Index_filtered %>%
  filter(row_number() %% 2 == 0)  # Відбирає парні рядки

s_df1 <- df1 %>%
  rename(
    Age_Band_of_Driver1=Age_Band_of_Driver,
    Age_of_Vehicle1=Age_of_Vehicle,
    Sex_of_Driver1=Sex_of_Driver,
    Car_Brand1=make,
    Vehicle_Manoeuvre1=Vehicle_Manoeuvre,
    Vehicle_Type1=Vehicle_Type,
    Point_of_Impact1=X1st_Point_of_Impact
  )

s_df2 <- df2 %>%
  rename(
    Age_Band_of_Driver2=Age_Band_of_Driver,
    Age_of_Vehicle2=Age_of_Vehicle,
    Sex_of_Driver2=Sex_of_Driver,
    Car_Brand2=make,
    Vehicle_Manoeuvre2=Vehicle_Manoeuvre,
    Vehicle_Type2=Vehicle_Type,
    Point_of_Impact2=X1st_Point_of_Impact
    
  )

# Об'єднати за колонкою Accident_Index
merged_Vehicle <- merge(s_df1, s_df2, by = "Accident_Index", all = TRUE)

# Об'єднати за колонкою Accident_Index
all_data2 <- merge(c_Accident_Index_filtered , merged_Vehicle, by = "Accident_Index", all = TRUE)


filtered_data_3 <- all_data2  %>%
  rename(
    Area_Type=Urban_or_Rural_Area,
  )

library(lubridate)

# Перетворення колонки "Date" на тип дати
filtered_data_3$Date <- as.Date(filtered_data_3$Date)

# Додавання нових колонок для дня та місяця
filtered_data_3$Day <- day(filtered_data_3$Date)
filtered_data_3$Month <- month(filtered_data_3$Date)

# Видалення оригінальної колонки "Date"
filtered_data_3 <- subset(filtered_data_3, select = -Date)


rm(all_data2, c_Accident, c_Accident_Index, c_Accident_Index_filtered, c_Vehicle, c_Vehicle_Index,
   c_Vehicle_Index_filtered, df, df_Accident_Information, df_Accident_Information_2,
   df_Accident_Information_3, df_Vehicle_Information, df1, df2, s_df1, s_df2, merged_Vehicle)

#=============================== Розподіл тяжкості для марок автомоболів


selected_brands <- c("FORD", "VAUXHALL", "PEUGEOT", "VOLKSWAGEN", "RENAULT",
                     "HONDA", "TOYOTA", "MERCEDES", "CITROEN", "NISSAN")

accidents_by_brand_severity <- filtered_data_3 %>%
  filter(Car_Brand1 %in% selected_brands) %>%
  group_by(Car_Brand1, Accident_Severity) %>%
  summarise(count = n(), .groups = 'drop')

total_by_brand <- accidents_by_brand_severity %>%
  group_by(Car_Brand1) %>%
  summarise(total_count = sum(count))

accidents_by_brand_percentage <- accidents_by_brand_severity %>%
  left_join(total_by_brand, by = "Car_Brand1") %>%
  mutate(percentage = count / total_count * 100)

ggplot(accidents_by_brand_percentage, aes(x = reorder(Car_Brand1, -total_count), y = percentage, fill = Accident_Severity)) +
  geom_bar(stat = "identity") +
  labs(x = "Марка автомобіля", y = "Відсоток нещасних випадків", title = "Відсоток аварій за окремими марками автомобілів") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "lightgreen"),
                    labels = c("Фатальні", "Серйозні", "Легкі"),
                    name = "Тяжкість нещасного випадку")

#=================Відсоток ДТП зі смертельними наслідками для топ-10 автомобільних брендів
library(dplyr)
library(ggplot2)

fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal")

fatal_accidents_by_car <- fatal_accidents %>%
  group_by(Car_Brand1) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_10_cars_fatal <- fatal_accidents_by_car %>%
  slice(1:10)

total_fatal_accidents <- sum(top_10_cars_fatal$count)

top_10_cars_fatal <- top_10_cars_fatal %>%
  mutate(percentage = (count / total_fatal_accidents) * 100)

colors <- RColorBrewer::brewer.pal(n = nrow(top_10_cars_fatal), name = "Set3")

ggplot(top_10_cars_fatal, aes(x = reorder(Car_Brand1, -percentage), y = percentage, fill = Car_Brand1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5, size = 3.5, color = "black") +
  labs(x = "Марка автомобіля", y = "Відсоток нещасних випадків зі смертельними наслідками", title = "Відсоток ДТП зі смертельними наслідками для топ-10 автомобільних брендів") +
  scale_fill_manual(values = colors, name = "Марка автомобіля") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#============================"Топ-10 найстаріших автомобілів у ДТП зі смертельними наслідками"
library(ggplot2)

fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal")

fatal_accidents_by_car <- fatal_accidents %>%
  group_by(Age_of_Vehicle1) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_10_cars_fatal <- fatal_accidents_by_car %>%
  filter(!is.na(Age_of_Vehicle1)) %>%
  slice(1:10)

total <- sum(top_10_cars_fatal$count)
top_10_cars_fatal$percentage <- (top_10_cars_fatal$count / total) * 100

ggplot(top_10_cars_fatal, aes(x = reorder(Age_of_Vehicle1, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "salmon") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), vjust = -0.5, size = 3) +
  labs(x = "Вік транспортного засобу", y = "Відсоток нещасних випадків зі смертельними наслідками", title = "Топ-10 найстаріших автомобілів у ДТП зі смертельними наслідками") +
  theme(axis.text.x = element_text(hjust = 1))

#========================== Розподіл тяжкості за віком водіїв
library(ggplot2)
library(dplyr)

total_accidents <- filtered_data_3 %>%
  group_by(Age_Band_of_Driver1) %>%
  summarise(total = n())

percentages <- filtered_data_3 %>%
  group_by(Age_Band_of_Driver1, Accident_Severity) %>%
  summarise(count = n()) %>%
  left_join(total_accidents, by = "Age_Band_of_Driver1") %>%
  mutate(percentage = count / total * 100)

percentages <- percentages[percentages$Age_Band_of_Driver1 != "Data missing or out of range",]
percentages <- percentages[percentages$Age_Band_of_Driver1 != "0 - 5",]
percentages <- percentages[percentages$Age_Band_of_Driver1 != "6 - 10",]

ggplot(percentages, aes(x = Age_Band_of_Driver1, y = percentage, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Вікова категорія водія", y = "Відсоток", fill = "Тяжкість нещасного випадку") +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "lightgreen"), labels = c("Фатальні", "Серйозні", "Легкі"))

#========================= Порівняння маневрів транспортних засобів за статтю водія
library(ggplot2)

accidents_by_manoeuvre_sex <- filtered_data_3 %>%
  filter(Sex_of_Driver1 %in% c("Male", "Female")) %>%
  group_by(Vehicle_Manoeuvre1, Sex_of_Driver1) %>%
  summarise(count = n()) %>%
  arrange(Vehicle_Manoeuvre1)

accidents_by_manoeuvre_sex <- accidents_by_manoeuvre_sex %>%
  group_by(Vehicle_Manoeuvre1) %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100)

custom_labels <- c("Зміна смуги наліво", "Зміна смуги направо", "Попереду лівий поворот", "Обгін попереду", "Попереду правий поворот", "Зрушив з місця", "Ближній обгін", "Обгін автомобіля, що рухається праворуч", "Обгін автомобіля, що стоїть праворуч", "Парковка", "Реверс", "Уповільнення", "Поворот ліворуч", "Поворот праворуч", "Розворот", "Очікування на рух - затримка", "Очікування на лівий поворот", "Очікування на правий поворот", "Зупинка")

ggplot(accidents_by_manoeuvre_sex, aes(x = factor(Vehicle_Manoeuvre1, levels = unique(Vehicle_Manoeuvre1)), y = percentage, fill = Sex_of_Driver1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Маневр транспортного засобу", y = "Відсоток аварій", title = "Порівняння маневрів транспортних засобів за статтею водія") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"), name = "Стать водія",
                    labels = c("Чоловік", "Жінка")) +
  scale_x_discrete(labels = custom_labels)

#====================================== Розподіл тяжкості "перепона на полотні"

library(tidyr)

filtered_data_without_none <- filtered_data_3[filtered_data_3$Carriageway_Hazards != "None", ]

total_accidents <- filtered_data_without_none %>%
  group_by(Carriageway_Hazards) %>%
  summarise(total = n())

percentages <- filtered_data_without_none %>%
  group_by(Carriageway_Hazards, Accident_Severity) %>%
  summarise(count = n()) %>%
  left_join(total_accidents, by = "Carriageway_Hazards") %>%
  mutate(percentage = count / total * 100) %>%
  complete(Accident_Severity, fill = list(count = 0, total = 0, percentage = 0))

percentages <- percentages[percentages$Carriageway_Hazards != "Data missing or out of range",]

custom_labels <- c("Будь-яка тварина на проїджій частині", "Інший об'єкт на дорозі", "Пішохід на проїжджій частині - не постраждав", "Попередня аварія", "Навантаження автомобіля на дорозі")

ggplot(percentages, aes(x = Carriageway_Hazards, y = percentage, fill = Accident_Severity)) +
  geom_bar(stat = "identity") +
  labs(x = "Небезпеки на проїжджій частині", y = "Відсоток", fill = "Тяжкість аварії") +
  theme(legend.position = "top")+
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "lightgreen"), labels = c("Фатальна", "Серйозна", "Легка")) +
  scale_x_discrete(labels = custom_labels)
########################################################################################
########################################################################################

#=================================== ДТП Сільска/міська проценти
severity_by_area <- filtered_data_3 %>%
  filter(!is.na(Area_Type), Area_Type!= "Unallocated") %>%
  group_by(Area_Type, Accident_Severity) %>%
  summarize(Count = n()) %>%
  group_by(Area_Type) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)


ggplot(severity_by_area, aes(x = Area_Type, y = Percentage, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage,2), "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(x = "Міська або сільська місцевість", y = "Відсоток аварій", fill = "Серйозність аварії") +
  scale_fill_manual(values = c("lightcoral", "green", "skyblue"), labels = c("Фатальна", "Серйозна", "Легка")) +
  scale_x_discrete(labels = c("Сільська", "Міська")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 10))
#########################################
# ========================== Місяці невідсортоване кількісне
accidents_by_month <- filtered_data_3 %>%
  filter(!is.na(Accident_Severity)) %>%
  group_by(Month, Accident_Severity) %>%
  summarise(mean_accidents = mean(n())) %>%
  ungroup()

ggplot(accidents_by_month, aes(x = as.factor(Month), y = mean_accidents, fill = Accident_Severity)) +
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Average Accidents", title = "Average Number of Accidents by Month and Severity") +
  scale_fill_manual(values = c("Fatal" = "red", "Serious" = "orange", "Slight" = "lightblue")) +
  theme_minimal()
##########################################
# ================================ Місяці невідсортоване відсотки (3 категорії)
severity_counts <- filtered_data %>%
  group_by(Month, Accident_Severity) %>%
  summarise(count = n()) %>%
  ungroup()

total_counts <- severity_counts %>%
  group_by(Month) %>%
  summarise(total_count = sum(count))

merged_data <- merge(severity_counts, total_counts, by = "Month")


merged_data <- merged_data %>%
  mutate(percentage = (count / total_count) * 100)

ggplot(merged_data, aes(x = as.factor(Month), y = percentage, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Month", y = "Percentage of Accidents", title = "Distribution of Accident Severities by Month") +
  scale_fill_manual(values = c("red", "orange", "lightblue")) +
  theme_minimal()

merged_data <- merged_data %>% # =================== Місяці відсотки невідсортоване (серйозне/фатальне)
  filter(Accident_Severity != "Slight")

ggplot(merged_data, aes(x = as.factor(Month), y = percentage, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Mісяць", y = "Проценти", title = "Процентна частка серйозних і фатальних аварій за місяцями") +
  scale_fill_manual(values = c("red", "orange")) +
  theme_minimal() +
  geom_text(aes(label = paste(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3, color = "black")

#############################################################
#######(КІНЕЦЬ ГРАФІКИ ЛР1)##################################

# ======================================МАПА========================================
library(leaflet)

# Очистити дані від NA значень у стовпцях Latitude та Longitude
df <- filtered_data_3[complete.cases(filtered_data_3[c("Latitude", "Longitude")]), ]

# Перетворити стовпці Latitude та Longitude у числовий формат
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)

# Знайти середні координати для центру мапи
x_mean <- mean(df$Latitude)
y_mean <- mean(df$Longitude)


# Створення таблиці з підрахунком кількості рядків для кожної унікальної комбінації координат
coord_table <- df %>%
  group_by(Latitude, Longitude) %>%
  summarise(Count = n()) %>%
  ungroup()

# Перегляд перших кількох рядків таблиці
coord_table
library(dplyr)
library(leaflet)

# Вибірка випадкових рядків з таблиці
random_crashes_df <- coord_table %>% 
  sample_n(10000, replace = FALSE)  # Вибираємо 100000 випадкових рядків

# Побудова мапи
map <- leaflet() %>%
  setView(lng = y_mean, lat = x_mean, zoom = 10) %>%
  addTiles() %>%
  addCircleMarkers(data = random_crashes_df,
                   lng = ~Longitude, lat = ~Latitude,
                   radius = ~sqrt(Count) * 6,  # Радіус залежить від кількості аварій
                   color = "blue",
                   weight = ~Count / max(Count),
                   fill = TRUE,
                   fillColor = "red",
                   fillOpacity = 1,
                   popup = ~paste(Count, "Accidents Occured"))

# Відобразити мапу
#map

##########################################

# Підрахунок кількості аварій за кожним районом і вибірка топ-10 районів
district_top_10 <- head(sort(table(filtered_data_3$Local_Authority_District), decreasing = TRUE), 10)

district_top_10 <- as.data.frame(district_top_10)
colnames(district_top_10) <- c("city", "accidents")

# Виведення топ-10 районів
print(district_top_10)
# ==========================
# Фільтрація даних за фатальні аварії
fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal")

# Підрахунок кількості фатальних аварій за кожним районом
fatal_district_top_10 <- head(sort(table(fatal_accidents$Local_Authority_District), decreasing = TRUE), 10)

# Перетворення результатів у датафрейм
fatal_district_top_10 <- as.data.frame(fatal_district_top_10)
colnames(fatal_district_top_10) <- c("city", "fatal_accidents")

# Виведення топ-10 районів за кількістю фатальних аварій
print(fatal_district_top_10)
# ===================================================================



# Обчислення загальної кількості фатальних аварій
total_fatal_accidents <- sum(fatal_district_top_10$fatal_accidents)

# Додавання стовпчика з відсотковими значеннями
fatal_district_top_10$percentage <- (fatal_district_top_10$fatal_accidents / total_fatal_accidents) * 100

# Побудова графіку
ggplot(fatal_district_top_10, aes(x = reorder(city, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(round(percentage), "%")), vjust = -0.5, size = 3) +  # Додавання текстових міток з відсотками
  labs(x = "Район", y = "Відсоток фатальних аварій", title = "Топ-10 фатальних аварій за районами (у відсотках)") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.8, hjust = 0.8))


# Встановлення правильного порядку районів
filtered_top_10 <- filtered_data_3 %>%
  filter(Local_Authority_District %in% c("Aberdeenshire", "East Riding of Yorkshire", "Birmingham", "Cornwall", "Highland", 
                                         "Northumberland", "Powys", "Leeds", "Wiltshire", "Cheshire East"))

# Визначення порядку фактора "Local_Authority_District"
filtered_top_10$Local_Authority_District <- factor(filtered_top_10$Local_Authority_District,
                                                   levels = c("Aberdeenshire", "East Riding of Yorkshire", "Birmingham", 
                                                              "Cornwall", "Highland", "Northumberland", "Powys", "Leeds", 
                                                              "Wiltshire", "Cheshire East"))

# Побудова графіка з правильним порядком районів
ggplot(filtered_top_10, aes(x = Local_Authority_District, fill = Accident_Severity)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Район", y = "Кількість аварій", fill = "Серйозність аварії",
       title ="Вплив району на тяжкість і кількість ДТП") +
  scale_fill_manual(values = c("red", "orange", "lightblue"),
                    labels = c("Фатальна", "Серйозна", "Легка")) +
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust = 0.5))
###################################################################
#############################Кінець мапи###########################

# ОСТАТОЧНЕ ОСТАТОЧНОГО ЗАКИДУЮ В ОДНУ КОМІРКУ ВЕСЬ МІЙ КОД - ІННА
#1.Гіпотеза про рівність середньої кількості аварій в теплі та холодні місяці.- ВИБІРКА ВСІХ МІСЯЦІВ
# =====================================================ГРАФІК-2
# Фільтрація даних для фатальних аварій
fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal")

# Групування за місяцем і обчислення середньої кількості фатальних аварій
accidents_by_month <- fatal_accidents %>%
  group_by(Month) %>%
  summarise(mean_accidents = n()) %>%
  ungroup()

# Обчислення довірчих інтервалів
a_values <- numeric()
b_values <- numeric()
unique_Month <- unique(fatal_accidents$Month)
unique_Month <- sort(unique_Month)
for (m in unique_Month) {
  road_fatal <- fatal_accidents %>% filter(Month == m) %>% nrow()
  ci <- fatal_accidents %>%
    summarise(
      mean = mean(road_fatal),
      sd = sqrt(road_fatal * (1 - road_fatal/nrow(fatal_accidents))),
      n = nrow(fatal_accidents),
      a = mean + qnorm(0.025) * sd / sqrt(n),
      b = mean + qnorm(0.975) * sd / sqrt(n),
      a_t = mean + qt(0.025, df = n-1) * sd / sqrt(n),
      b_t = mean + qt(0.975, df = n-1) * sd / sqrt(n)
    )
  # Виведення результату
  print(paste("Довірчий інтервал для", m))
  print(ci)
  a_values <- c(a_values, ci$a)
  b_values <- c(b_values, ci$b)
}

# Додавання довірчих інтервалів до даних
accidents_by_month <- accidents_by_month %>%
  mutate(a = a_values, b = b_values)

# Створення вектора міток для місяців
month_labels <- c("Січень", "Лютий", "Березень", "Квітень", "Травень", "Червень", "Липень", "Серпень", "Вересень", "Жовтень", "Листопад", "Грудень")

ggplot(accidents_by_month, aes(x = factor(Month), y = mean_accidents)) +
  geom_errorbar(aes(ymin = a, ymax = b))+
  geom_point()+
  scale_x_discrete(labels = month_labels) +  # Встановлення міток для місяців
  labs(x = "Місяць", y = "Середня кількість фатальних аварій")

# =============================================================
# ============================================================ВИБІРКА ХОЛДНЕ І ТЕПЛЕ

fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal")


accidents_by_month <- fatal_accidents %>%
  group_by(Month) %>%
  summarise(mean_accidents = mean(n())) %>%
  ungroup()
a_values <- numeric()
b_values <- numeric()


ci1 <- accidents_by_month %>%
  filter(Month %in% c(1,2,3,4,10,11,12)) %>%
  summarize(
    mean = mean(mean_accidents),
    sd = sd(mean_accidents),
    n = n(),
    a = mean + qnorm(0.025) * sd / sqrt(n),
    b = mean + qnorm(0.975) * sd / sqrt(n),
    a_t = mean + qt(0.025, df = n-1) * sd / sqrt(n),
    b_t = mean + qt(0.975, df = n-1) * sd / sqrt(n)
  )
ci1

ci2 <- accidents_by_month %>%
  filter(Month %in% c(5,6,7,8,9))%>%
  summarize(
    mean = mean(mean_accidents),
    sd = sd(mean_accidents),
    n = n(),
    a = mean + qnorm(0.025) * sd / sqrt(n),
    b = mean + qnorm(0.975) * sd / sqrt(n),
    a_t = mean + qt(0.025, df = n-1) * sd / sqrt(n),
    b_t = mean + qt(0.975, df = n-1) * sd / sqrt(n)
  )
ci2


ci11 <-accidents_by_month %>%
  filter(accidents_by_month$Month %in% c(1,2,3,4,10,11,12))
ci22 <-accidents_by_month %>%
  filter(accidents_by_month$Month %in% c(5,6,7,8,9))


ggplot() +
  geom_errorbar(data = rbind(ci1, ci2), aes(x = c("Холодні місяці", "Теплі місяці"), ymin = a, ymax = b), color = "black", width = 0.1) +
  geom_pointrange(data = rbind(ci1, ci2), aes(x = c("Холодні місяці", "Теплі місяці"), y = mean, ymin = a, ymax = b), color = "black", size = 0.5) +
  
  theme_minimal() +
  labs(x = "Month",
       y = "Mean Accidents",
       title = "Average Number of Accidents in Warm and Cold Months",
       caption = "95% Confidence Interval") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

t.test(ci11$mean_accidents, ci22$mean_accidents)




# =====================================================Дисперсія
data <- data.frame(
  Month = 1:12,
  mean_accidents = c(161, 170, 181, 203, 192, 218, 238, 249, 206, 208, 213, 212)
)

warm_months <- c(5, 6, 7, 8, 9)
cold_months <- c(1, 2, 3, 4, 10, 11, 12)


variance_warm <- var(data[data$Month %in% warm_months, ]$mean_accidents)
variance_cold <- var(data[data$Month %in% cold_months, ]$mean_accidents)


F_statistic <- variance_warm / variance_cold


df1 <- length(warm_months) - 1
df2 <- length(cold_months) - 1
p_value <- 2 * pf(F_statistic, df1, df2, lower.tail = FALSE)

print(paste("F-статистика:", F_statistic))
print(paste("Ступені свободи:", df1, "та", df2))
print(paste("p-value:", p_value))
# =====================================================
data <- data.frame(
  Month = 1:12,
  mean_accidents = c(161, 170, 181, 203, 192, 218, 238, 249, 206, 208, 213, 212)
)

# Розділення на групи: теплі та холодні місяці
warm_months <- c(5, 6, 7, 8, 9)
cold_months <- c(1, 2, 3, 4, 10, 11, 12)

# Використання тесту Волда
welch_test <- t.test(mean_accidents ~ factor(Month %in% warm_months), data = data, var.equal = FALSE)

# Виведення результатів
print(welch_test)
# ======================================================
welch_test <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  mean1 <- mean(x)
  mean2 <- mean(y)
  var1 <- var(x)
  var2 <- var(y)
  df <- ((var1/n1 + var2/n2)^2) / ((var1/n1)^2 / (n1-1) + (var2/n2)^2 / (n2-1))
  t <- (mean1 - mean2) / sqrt(var1/n1 + var2/n2)
  p_value <- 2 * pt(-abs(t), df)
  return(list(t = t, df = df, p_value = p_value))
}

data <- data.frame(
  Month = 1:12,
  mean_accidents = c(161, 170, 181, 203, 192, 218, 238, 249, 206, 208, 213, 212)
)

# Розділення на групи: теплі та холодні місяці
warm_months <- c(5, 6, 7, 8, 9)
cold_months <- c(1, 2, 3, 4, 10, 11, 12)

# Обчислення статистик тесту Волда
results_welch <- welch_test(
  data[data$Month %in% warm_months, ]$mean_accidents,
  data[data$Month %in% cold_months, ]$mean_accidents
)

print("Результати тесту Велча:")
print(results_welch)

# =============================================
test_vold_mean <- function(group1, group2) {
  mean_hat_x <- mean(group1)
  mean_hat_y <- mean(group2)
  
  var_hat_x <- var(group1)
  var_hat_y <- var(group2)
  
  n1 <- length(group1)
  n2 <- length(group2)
  
  df <- ((var_hat_x/n1 + var_hat_y/n2)^2) / ((var_hat_x^2)/(n1^2 * (n1 - 1)) + (var_hat_y^2)/(n2^2 * (n2 - 1)))
  
  T <- (mean_hat_x - mean_hat_y) / sqrt((var_hat_x/n1) + (var_hat_y/n2))
  p_value <- 2 * pt(abs(T), df = df, lower.tail = FALSE)
  
  conf_int_lower <- mean_hat_x - mean_hat_y - qt(0.975, df) * sqrt((var_hat_x/n1) + (var_hat_y/n2))
  conf_int_upper <- mean_hat_x - mean_hat_y + qt(0.975, df) * sqrt((var_hat_x/n1) + (var_hat_y/n2))
  
  return(list(mean_x = mean_hat_x, mean_y = mean_hat_y, p_value = p_value, conf_int = c(conf_int_lower, conf_int_upper)))
}

# Передача колонок для теплих і холодних місяців
test_vold_mean(ci22$mean_accidents, ci11$mean_accidents)

########################################
#Гіпотеза 2
########################################
# По дням тижня серйозні
serious_accidents <- filtered_data_3 %>%
  filter(!is.na(Accident_Severity)) %>%
  filter(Accident_Severity == "Serious")


# Define the order of days of the week
days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Aggregate data to calculate the total number of "slight" accidents for each distinct day of the week (based on Day, Month, and Year)
serious_by_day <- serious_accidents %>%
  group_by(Day_of_Week, Day, Month, Year) %>%
  summarise(total_serious = n()) %>%
  group_by(Day_of_Week) %>%
  summarise(mean_serious = mean(total_serious),
            sd_serious= sqrt(mean_serious * (1 - mean_serious / n())))

# Calculate the number of records for each day of the week
serious_by_day <- serious_by_day %>%
  mutate(n = sapply(unique(Day_of_Week), function(day) sum(serious_accidents$Day_of_Week == day)))

# Calculate confidence intervals using normal distribution
serious_by_day <- serious_by_day %>%
  mutate(
    a = mean_serious + qnorm(0.025) * sd_serious / sqrt(n),
    b = mean_serious + qnorm(0.975) * sd_serious / sqrt(n)
  )

# Reorder the days of the week
serious_by_day <- serious_by_day[order(match(serious_by_day$Day_of_Week, days_order)),]

# Print out the result
print(serious_by_day)

#####################################################################################

# По дням тижня фатальні
fatal_accidents <- filtered_data_3 %>%
  filter(!is.na(Accident_Severity)) %>%
  filter(Accident_Severity == "Fatal")


# Define the order of days of the week
days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Aggregate data to calculate the total number of "slight" accidents for each distinct day of the week (based on Day, Month, and Year)
fatal_by_day <- fatal_accidents %>%
  group_by(Day_of_Week, Day, Month, Year) %>%
  summarise(total_fatal = n()) %>%
  group_by(Day_of_Week) %>%
  summarise(mean_fatal = mean(total_fatal),
            sd_fatal= sqrt(mean_fatal * (1 - mean_fatal / n())))

# Calculate the number of records for each day of the week
fatal_by_day <- fatal_by_day %>%
  mutate(n = sapply(unique(Day_of_Week), function(day) sum(fatal_accidents$Day_of_Week == day)))

# Calculate confidence intervals using normal distribution
fatal_by_day <- fatal_by_day %>%
  mutate(
    a = mean_fatal + qnorm(0.025) * sd_fatal / sqrt(n),
    b = mean_fatal + qnorm(0.975) * sd_fatal / sqrt(n)
  )

# Reorder the days of the week
fatal_by_day <- fatal_by_day[order(match(fatal_by_day$Day_of_Week, days_order)),]

# Print out the result
print(fatal_by_day)

# =====================================================ПЕРЕВІРКА ГІПОТЕЗИ - Іван, не знаю наскільки правильно
mean_sunday <- fatal_by_day$mean_fatal[fatal_by_day$Day_of_Week == "Sunday"]
sd_sunday <- fatal_by_day$sd_fatal[fatal_by_day$Day_of_Week == "Sunday"]
n_sunday <- fatal_by_day$n[fatal_by_day$Day_of_Week == "Sunday"]

mean_thursday <- fatal_by_day$mean_fatal[fatal_by_day$Day_of_Week == "Thursday"]
sd_thursday <- fatal_by_day$sd_fatal[fatal_by_day$Day_of_Week == "Thursday"]
n_thursday <- fatal_by_day$n[fatal_by_day$Day_of_Week == "Thursday"]

wald_test_statistic <- (mean_sunday - mean_thursday) / sqrt((sd_sunday^2 / n_sunday) + (sd_thursday^2 / n_thursday))
p_value_wald <- 1 - pnorm(wald_test_statistic)


welch_test_statistic <- (mean_sunday - mean_thursday) / sqrt((sd_sunday^2 / n_sunday) + (sd_thursday^2 / n_thursday))
df_welch <- ((sd_sunday^2 / n_sunday + sd_thursday^2 / n_thursday)^2) / ((sd_sunday^2 / n_sunday)^2 / (n_sunday - 1) + (sd_thursday^2 / n_thursday)^2 / (n_thursday - 1))
p_value_welch <- 1 - pt(welch_test_statistic, df_welch)

cat("Статистика тесту Волда:", wald_test_statistic, "\nP-значення (тест Волда):", p_value_wald)
cat("\n\nСтатистика тесту Велча:", welch_test_statistic, "\nP-значення (тест Велча):", p_value_welch)


days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
fatal_by_day <- fatal_by_day[order(match(fatal_by_day$Day_of_Week, days_order)),]

ggplot(fatal_by_day, aes(x = factor(Day_of_Week, levels = days_order), y = mean_fatal)) +
  geom_errorbar(aes(ymin = a, ymax = b), width = 0.2) +  # Add error bars for confidence intervals
  labs(title = "Mean Number of 'Fatal' Accidents per Day of the Week with 95% CI",
       x = "Day of the Week",
       y = "Mean Number of 'Fatal' Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#############################################
##########################Гіпотеза №3########
a_values_serious <- numeric()
b_values_serious <- numeric()
a_values_slight <- numeric()
b_values_slight <- numeric()
a_values_fatal <- numeric()
b_values_fatal <- numeric()

serious_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Serious" & !is.na(Age_Band_of_Driver1))

slight_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Slight" & !is.na(Age_Band_of_Driver1))

fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal" & !is.na(Age_Band_of_Driver1))

serious_accidents_by_age <- serious_accidents %>%
  group_by(Age_Band_of_Driver1) %>%
  summarise(mean_serious_accidents = mean(n())) %>%
  ungroup() %>%
  filter(Age_Band_of_Driver1 != "6 - 10" & Age_Band_of_Driver1 != "Data missing or out of range" & Age_Band_of_Driver1 != "0 - 5") # Exclude problematic age bands

slight_accidents_by_age <- slight_accidents %>%
  group_by(Age_Band_of_Driver1) %>%
  summarise(mean_slight_accidents = mean(n())) %>%
  ungroup() %>%
  filter(Age_Band_of_Driver1 != "6 - 10" & Age_Band_of_Driver1 != "Data missing or out of range" & Age_Band_of_Driver1 != "0 - 5") # Exclude problematic age bands

fatal_accidents_by_age <- fatal_accidents %>%
  group_by(Age_Band_of_Driver1) %>%
  summarise(mean_fatal_accidents = mean(n())) %>%
  ungroup() %>%
  filter(Age_Band_of_Driver1 != "6 - 10" & Age_Band_of_Driver1 != "Data missing or out of range" & Age_Band_of_Driver1 != "0 - 5") # Exclude problematic age bands

unique_Age_Band_serious <- unique(serious_accidents_by_age$Age_Band_of_Driver1)
unique_Age_Band_slight <- unique(slight_accidents_by_age$Age_Band_of_Driver1)
unique_Age_Band_fatal <- unique(fatal_accidents_by_age$Age_Band_of_Driver1)

for (age_band in unique_Age_Band_serious) {
  road_serious <- serious_accidents %>% filter(Age_Band_of_Driver1 == age_band) %>% nrow()
  
  mean_serious_accidents <- mean(road_serious)
  sd_serious_accidents <- sqrt(road_serious * (1 - road_serious / nrow(serious_accidents)))
  
  ci <- serious_accidents %>%
    summarise(
      mean = mean_serious_accidents,
      sd = sd_serious_accidents,
      n = nrow(serious_accidents),
      a = mean_serious_accidents - qnorm(0.975) * sd_serious_accidents / sqrt(n),
      b = mean_serious_accidents + qnorm(0.975) * sd_serious_accidents / sqrt(n),
      a_t = mean_serious_accidents - qt(0.975, df = n - 1) * sd_serious_accidents / sqrt(n),
      b_t = mean_serious_accidents + qt(0.975, df = n - 1) * sd_serious_accidents / sqrt(n)
    )
  
  a_values_serious <- c(a_values_serious, ci$a)
  b_values_serious <- c(b_values_serious, ci$b)
}

for (age_band in unique_Age_Band_slight) {
  road_slight <- slight_accidents %>% filter(Age_Band_of_Driver1 == age_band) %>% nrow()
  
  mean_slight_accidents <- mean(road_slight)
  sd_slight_accidents <- sqrt(road_slight * (1 - road_slight / nrow(slight_accidents)))
  
  ci <- slight_accidents %>%
    summarise(
      mean = mean_slight_accidents,
      sd = sd_slight_accidents,
      n = nrow(slight_accidents),
      a = mean_slight_accidents - qnorm(0.975) * sd_slight_accidents / sqrt(n),
      b = mean_slight_accidents + qnorm(0.975) * sd_slight_accidents / sqrt(n),
      a_t = mean_slight_accidents - qt(0.975, df = n - 1) * sd_slight_accidents / sqrt(n),
      b_t = mean_slight_accidents + qt(0.975, df = n - 1) * sd_slight_accidents / sqrt(n)
    )
  
  a_values_slight <- c(a_values_slight, ci$a)
  b_values_slight <- c(b_values_slight, ci$b)
}

for (age_band in unique_Age_Band_fatal) {
  road_fatal <- fatal_accidents %>% filter(Age_Band_of_Driver1 == age_band) %>% nrow()
  
  mean_fatal_accidents <- mean(road_fatal)
  sd_fatal_accidents <- sqrt(road_fatal * (1 - road_fatal / nrow(fatal_accidents)))
  
  ci <- fatal_accidents %>%
    summarise(
      mean = mean_fatal_accidents,
      sd = sd_fatal_accidents,
      n = nrow(fatal_accidents),
      a = mean_fatal_accidents - qnorm(0.975) * sd_fatal_accidents / sqrt(n),
      b = mean_fatal_accidents + qnorm(0.975) * sd_fatal_accidents / sqrt(n),
      a_t = mean_fatal_accidents - qt(0.975, df = n - 1) * sd_fatal_accidents / sqrt(n),
      b_t = mean_fatal_accidents + qt(0.975, df = n - 1) * sd_fatal_accidents / sqrt(n)
    )
  
  a_values_fatal <- c(a_values_fatal, ci$a)
  b_values_fatal <- c(b_values_fatal, ci$b)
}

serious_df <- data.frame(Age_Band_of_Driver1 = unique_Age_Band_serious, mean_serious_accidents = serious_accidents_by_age$mean_serious_accidents, a_serious = a_values_serious, b_serious = b_values_serious)
slight_df <- data.frame(Age_Band_of_Driver1 = unique_Age_Band_slight, mean_slight_accidents = slight_accidents_by_age$mean_slight_accidents, a_slight = a_values_slight, b_slight = b_values_slight)
fatal_df <- data.frame(Age_Band_of_Driver1 = unique_Age_Band_fatal, mean_fatal_accidents = fatal_accidents_by_age$mean_fatal_accidents, a_fatal = a_values_fatal, b_fatal = b_values_fatal)

library(ggplot2)

ggplot() +
  
  geom_point(data = serious_df, aes(x = Age_Band_of_Driver1, y = mean_serious_accidents), color = "blue") +
  geom_errorbar(data = serious_df, aes(x = Age_Band_of_Driver1, ymin = a_serious, ymax = b_serious), width = 0.2, color = "blue") +
  
  geom_point(data = slight_df, aes(x = Age_Band_of_Driver1, y = mean_slight_accidents), color = "green") +
  geom_errorbar(data = slight_df, aes(x = Age_Band_of_Driver1, ymin = a_slight, ymax = b_slight), width = 0.2, color = "green") +
  
  geom_point(data = fatal_df, aes(x = Age_Band_of_Driver1, y = mean_fatal_accidents), color = "red") +
  geom_errorbar(data = fatal_df, aes(x = Age_Band_of_Driver1, ymin = a_fatal, ymax = b_fatal), width = 0.2, color = "red") +
  labs(title = "Середня кількість ДТП за віковими групами водіїв та рівнем тяжкості",
       x = "Вікова група",
       y = "Нещасні випадки") +
  scale_color_manual(values = c("blue", "green", "red"), labels = c("Serious", "Slight", "Fatal"))
################################

set.seed(123)
n <- 1000
younger_ages <- c(rep("18-25", n))
older_ages <- c(rep("40+", n))
accidents_younger <- rpois(n, lambda = 5)
accidents_older <- rpois(n, lambda = 3)

data <- data.frame(
  Age_Group = c(younger_ages, older_ages),
  Accidents = c(accidents_younger, accidents_older)
)

wald_test <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  mean1 <- mean(x)
  mean2 <- mean(y)
  var1 <- var(x)
  var2 <- var(y)
  W <- (mean1 - mean2)^2 / (var1 / n1 + var2 / n2)
  p_value <- 1 - pchisq(W, df = 1)
  return(list(W = W, p_value = p_value))
}

welch_test <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  mean1 <- mean(x)
  mean2 <- mean(y)
  var1 <- var(x)
  var2 <- var(y)
  df <- ((var1/n1 + var2/n2)^2) / ((var1/n1)^2 / (n1-1) + (var2/n2)^2 / (n2-1))
  t <- (mean1 - mean2) / sqrt(var1/n1 + var2/n2)
  p_value <- 2 * pt(-abs(t), df)
  return(list(t = t, df = df, p_value = p_value))
}

results_wald <- wald_test(accidents_younger, accidents_older)
results_welch <- welch_test(accidents_younger, accidents_older)

print("Результати тесту Волда:")
print(results_wald)
print("Результати тесту Велча:")
print(results_welch)
################
young_drivers <- all_accidents %>% filter(Age_Band_of_Driver1 %in% c("11 - 15", "16 - 20", "21 - 25", "26 - 35"))
older_drivers <- all_accidents %>% filter(Age_Band_of_Driver1 %in% c("36 - 45", "46 - 55", "56 - 65", "Over 75", "66 - 75"))

total_young <- nrow(young_drivers)
total_older <- nrow(older_drivers)

n_fatal_young <- sum(young_drivers$Accident_Severity == "Fatal")
n_fatal_older <- sum(older_drivers$Accident_Severity == "Fatal")

expected_fatal_young <- (n_fatal_young + n_fatal_older) * (total_young / (total_young + total_older))
expected_fatal_older <- (n_fatal_young + n_fatal_older) * (total_older / (total_young + total_older))

chi_squared_statistic <- ((n_fatal_young - expected_fatal_young)^2 / expected_fatal_young) +
  ((n_fatal_older - expected_fatal_older)^2 / expected_fatal_older)

degrees_of_freedom <- 1

p_value <- pchisq(chi_squared_statistic, df = degrees_of_freedom, lower.tail = FALSE)

print("Chi-Squared Test Results:")
print(paste("Chi-Squared Statistic:", chi_squared_statistic))
print(paste("Degrees of Freedom:", degrees_of_freedom))
print(paste("p-value:", p_value))


#########

#4. Гіпотеза про рівність фатальних аварій автомобілей з більшим та меншим віком за роками
filtered_data_3_new <- filtered_data_3 %>%
  filter(Age_of_Vehicle1 != "Data missing or out of range",Age_of_Vehicle1 != "Unallocated")

filtered_data_3 <- na.omit(filtered_data_3_new)


unique_Area_Type <- sort(unique(filtered_data_3$Age_of_Vehicle1))



results <- data.frame()

for (Area_Type1 in unique_Area_Type) {
  if (Area_Type1 <= 20) {
    fatal_accidents <- filtered_data_3_new %>%
      filter(Accident_Severity == "Fatal")
    
    fatal_accidents2 <- fatal_accidents %>% filter(Age_of_Vehicle1== Area_Type1)
    
    accidents_by_month <- fatal_accidents2 %>%
      group_by(Year) %>%
      summarise(mean_accidents = mean(n())) %>%
      ungroup()
    
    a_values <- numeric()
    b_values <- numeric()
    
    ci2 <- accidents_by_month %>%
      filter(Year %in% c(2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015))%>%
      summarize(
        mean = mean(mean_accidents),
        sd = sd(mean_accidents),
        n = n(),
        a = mean + qnorm(0.025) * sd / sqrt(n),
        b = mean + qnorm(0.975) * sd / sqrt(n),
      )
    results <- rbind(results, ci2)
  }
}
results



library(dplyr)

fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal")

median_age <- median(fatal_accidents$Age_of_Vehicle1, na.rm = TRUE)

n <- sum(!is.na(fatal_accidents$Age_of_Vehicle1))
se <- 1.253 * mad(fatal_accidents$Age_of_Vehicle1, na.rm = TRUE) / sqrt(n)
lower_ci <- median_age - qnorm(0.975) * se
upper_ci <- median_age + qnorm(0.975) * se

cat("Медіана віку автомобілів у фатальних аваріях:", median_age, "\n")
cat("95% довірчий інтервал медіани:", lower_ci, "-", upper_ci)


ggplot(results, aes(x=1:nrow(results), y=mean, ymin=a, ymax=b)) +
  geom_point() +
  geom_errorbar() +
  scale_x_continuous(breaks=1:nrow(results)) +
  scale_y_continuous(breaks = seq(0, 3000, by = 10), labels = seq(0, 3000, by = 10)) +
  labs(x = "Вік", y = "Середнє значення фатальних аварій", title = "Довірчі інтервали віку транспорту")

# Функція для обчислення коефіцієнта Спірмана без вбудованих функцій
spearman_rank_corr <- function(x, y) {
  n <- length(x)
  
  # Ранги для x та y
  rank_x <- rank(x)
  rank_y <- rank(y)
  
  # Різниця у рангах
  d <- rank_x - rank_y
  
  # Квадрат різниці у рангах
  d_square <- d^2
  
  # Сума квадратів різниці у рангах
  sum_d_square <- sum(d_square)
  
  # Обчислення коефіцієнта Спірмана
  rho <- 1 - (6 * sum_d_square) / (n * (n^2 - 1))
  
  return(rho)
}

cleaned_data <- na.omit(filtered_data_3)

# Вибираємо дані про вік автомобіля і кількість фатальних аварій
age <- cleaned_data$Age_of_Vehicle1
fatal_accidents <- cleaned_data$Accident_Severity == "Fatal"

unique(cleaned_data$Accident_Severity)

# Створюємо таблицю, яка показує кількість фатальних аварій для кожного віку
data_summary <- aggregate(fatal_accidents, by=list(Age=age), FUN=sum)

# Перейменовуємо стовпець з кількістю фатальних аварій
names(data_summary) <- c("Age", "Fatal_Accidents")

# Сортуємо дані за віком
data_summary <- data_summary[order(data_summary$Age), ]

data_summary <- data_summary[1:10, ]

# Обчислення суми для першого рядка
row1_sum <- sum(data_summary$Fatal_Accidents[1:10])

# Обчислення суми для другого рядка
row2_sum <- sum(data_summary$Fatal_Accidents[10:length(data_summary$Fatal_Accidents)])

# Створення першого рядка нового датасету з обчисленими сумами
row1 <- data.frame(Age = 1, Fatal_Accidents = row1_sum)

# Створення другого рядка нового датасету з обчисленими сумами
row2 <- data.frame(Age = 2, Fatal_Accidents = row2_sum)

# Об'єднання рядків у новий датасет
new_dataset <- rbind(row1, row2)

# Виведення датасету
print(new_dataset)

x <- new_dataset$Age
y <- new_dataset$Fatal_Accident

# x <- data_summary$Age
# y <- data_summary$Fatal_Accidents

spearman_corr_manual2 <- spearman_rank_corr(x, y)
# Виведення результату
print(paste("Коефіцієнт кореляції Спірмана між віком автомобіля і кількістю фатальними аваріями :", spearman_corr_manual2))



fatal_accidents <- cleaned_data$Accident_Severity == "Fatal"


# Створюємо таблицю, яка показує кількість фатальних аварій для кожного віку
data_summary <- aggregate(fatal_accidents, by=list(Age=age), FUN=sum)

# data_summary <- aggregate(fatal_accidents, by=list(Age=age), FUN=sum)
age_and_fatal_accidents <- table(cleaned_data$Age_of_Vehicle1)
# Перетворення на dataframe

age_and_fatal_accidents_df <- data.frame(Age_of_Vehicle = names(age_and_fatal_accidents), All_Accidents = as.vector(age_and_fatal_accidents))

# Об'єднання даних
merged_data <- merge(data_summary, age_and_fatal_accidents_df, by.x = "Age", by.y = "Age_of_Vehicle", all.x = TRUE)

merged_data$Percentage <-(merged_data$x / merged_data$All_Accidents)* 100

# Виведення результату
print(merged_data)


# Побудова стовпчикової діаграми
barplot(merged_data$Percentage, 
        names.arg = data_summary$Age,
        xlab="Вік", ylab="Відсоток фатальних",
        main="Відсоток фатальних аварій за віком",
        col="skyblue",
        ylim=c(0, max(merged_data$Percentage) * 1.1))  # Задаємо максимальне значення осі y з невеликим маржем










test_vold_mean <- function(group1, group2) {
  mean_hat_x <- mean(group1)
  mean_hat_y <- mean(group2)
  
  var_hat_x <- var(group1)
  var_hat_y <- var(group2)
  
  n1 <- length(group1)
  n2 <- length(group2)
  
  df <- ((var_hat_x/n1 + var_hat_y/n2)^2) / ((var_hat_x^2)/(n1^2 * (n1 - 1)) + (var_hat_y^2)/(n2^2 * (n2 - 1)))
  
  T <- (mean_hat_x - mean_hat_y) / sqrt((var_hat_x/n1) + (var_hat_y/n2))
  p_value <- 2 * pt(abs(T), df = df, lower.tail = FALSE)
  
  conf_int_lower <- mean_hat_x - mean_hat_y - qt(0.975, df) * sqrt((var_hat_x/n1) + (var_hat_y/n2))
  conf_int_upper <- mean_hat_x - mean_hat_y + qt(0.975, df) * sqrt((var_hat_x/n1) + (var_hat_y/n2))
  
  return(list(mean_x = mean_hat_x, mean_y = mean_hat_y, p_value = p_value, conf_int = c(conf_int_lower, conf_int_upper)))
}


first_10_means <- head(results$mean, 10)
print(first_10_means)

second_10_means <- slice(results, 11:20)$mean
print(second_10_means)


test_vold_mean(first_10_means,second_10_means )


t.test(first_10_means,second_10_means)

# 5 нова гіпотеза точно!!!!!!!!!!!!!!!!!!! ТУТ УСе довірчі графік тестування
results <- data.frame()

fatal_accidents <- filtered_data_3_new %>%
  filter(Accident_Severity == "Fatal")

fatal_accidents1 <- fatal_accidents %>% filter(Area_Type == "Urban")

accidents_by_month <- fatal_accidents1 %>%
  group_by(Year) %>%
  summarise(mean_accidents = mean(n())) %>%
  ungroup()

a_values <- numeric()
b_values <- numeric()

ci1 <- accidents_by_month %>%
  filter(Year %in% c(2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015)) %>%
  summarize(
    mean = mean(mean_accidents),
    sd = sd(mean_accidents),
    n = n(),
    a = mean + qnorm(0.025) * sd / sqrt(n),
    b = mean + qnorm(0.975) * sd / sqrt(n),
  )
ci1
results <- rbind(results, ci1)

fatal_accidents2 <- fatal_accidents %>% filter(Area_Type == "Rural")

accidents_by_month <- fatal_accidents2 %>%
  group_by(Year) %>%
  summarise(mean_accidents = mean(n())) %>%
  ungroup()

a_values <- numeric()
b_values <- numeric()

ci2 <- accidents_by_month %>%
  filter(Year %in% c(2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015))%>%
  summarize(
    mean = mean(mean_accidents),
    sd = sd(mean_accidents),
    n = n(),
    a = mean + qnorm(0.025) * sd / sqrt(n),
    b = mean + qnorm(0.975) * sd / sqrt(n),
  )
results <- rbind(results, ci2)


ci1
ci2

ggplot(results, aes(x=1:nrow(results), y=mean, ymin=a, ymax=b)) +
  geom_point() +
  geom_errorbar() +
  scale_x_continuous(breaks=1:nrow(results), labels=c("Міська","Сільська")) +
  scale_y_continuous(breaks = seq(0, 3000, by = 10), labels = seq(0, 3000, by = 10)) +
  labs(x = "Тип місцевості", y = "Середнє значення фатальних аварій", title = "Довірчі інтервали для типу місцевості")

# Обчислення середнього значення фатальних аварій за роками для всієї місцевості
fatal_accidents_by_year <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal") %>%
  group_by(Year, Area_Type) %>%
  summarise(mean_accidents = mean(n()), .groups = 'drop')
fatal_accidents_by_year

# Обчислення середнього значення фатальних аварій за роками для кожного типу місцевості
mean_fatal_by_year <- fatal_accidents_by_year %>%
  group_by(Year, Area_Type) %>%
  summarise(mean_accidents = mean(mean_accidents))

# Результат
mean_fatal_by_year


# Визначення даних: середня кількість фатальних аварій за роками для кожного типу місцевості
mean_fatal_rural <- mean_fatal_by_year$mean_accidents[mean_fatal_by_year$Area_Type == "Rural"]
mean_fatal_urban <- mean_fatal_by_year$mean_accidents[mean_fatal_by_year$Area_Type == "Urban"]
# ++++++++++++тестування
test_vold_mean <- function(group1, group2) {
  mean_hat_x <- mean(group1)
  mean_hat_y <- mean(group2)
  
  var_hat_x <- var(group1)
  var_hat_y <- var(group2)
  
  n1 <- length(group1)
  n2 <- length(group2)
  
  df <- ((var_hat_x/n1 + var_hat_y/n2)^2) / ((var_hat_x^2)/(n1^2 * (n1 - 1)) + (var_hat_y^2)/(n2^2 * (n2 - 1)))
  
  T <- (mean_hat_x - mean_hat_y) / sqrt((var_hat_x/n1) + (var_hat_y/n2))
  p_value <- 2 * pt(abs(T), df = df, lower.tail = FALSE)
  
  conf_int_lower <- mean_hat_x - mean_hat_y - qt(0.975, df) * sqrt((var_hat_x/n1) + (var_hat_y/n2))
  conf_int_upper <- mean_hat_x - mean_hat_y + qt(0.975, df) * sqrt((var_hat_x/n1) + (var_hat_y/n2))
  
  return(list(mean_x = mean_hat_x, mean_y = mean_hat_y, p_value = p_value, conf_int = c(conf_int_lower, conf_int_upper)))
}

# Передача колонок для теплих і холодних місяців
test_vold_mean(mean_fatal_rural, mean_fatal_urban )

#########################################################################################
# По дням тижня легкі
slight_accidents <- filtered_data_3 %>%
  filter(!is.na(Accident_Severity)) %>%
  filter(Accident_Severity == "Slight")


# Define the order of days of the week
days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Aggregate data to calculate the total number of "slight" accidents for each distinct day of the week (based on Day, Month, and Year)
slight_by_day <- slight_accidents %>%
  group_by(Day_of_Week, Day, Month, Year) %>%
  summarise(total_slight = n()) %>%
  group_by(Day_of_Week) %>%
  summarise(mean_slight = mean(total_slight),
            sd_slight = sqrt(mean_slight * (1 - mean_slight / n())))

# Calculate the number of records for each day of the week
slight_by_day <- slight_by_day %>%
  mutate(n = sapply(unique(Day_of_Week), function(day) sum(slight_accidents$Day_of_Week == day)))

# Calculate confidence intervals using normal distribution
slight_by_day <- slight_by_day %>%
  mutate(
    a = mean_slight + qnorm(0.025) * sd_slight / sqrt(n),
    b = mean_slight + qnorm(0.975) * sd_slight / sqrt(n)
  )

# Reorder the days of the week
slight_by_day <- slight_by_day[order(match(slight_by_day$Day_of_Week, days_order)),]

# Print out the result
print(slight_by_day)



ggplot(slight_by_day, aes(x = factor(Day_of_Week, levels = days_order), y = mean_slight)) +
  geom_errorbar(aes(ymin = a, ymax = b), width = 0.2) +  # Add error bars for confidence intervals
  labs(title = "Mean Number of 'Slight' Accidents per Day of the Week with 95% CI",
       x = "Day of the Week",
       y = "Mean Number of 'Slight' Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
############################################################################################
# По дням тижня серйозні
serious_accidents <- filtered_data_3 %>%
  filter(!is.na(Accident_Severity)) %>%
  filter(Accident_Severity == "Serious")


# Define the order of days of the week
days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Aggregate data to calculate the total number of "slight" accidents for each distinct day of the week (based on Day, Month, and Year)
serious_by_day <- serious_accidents %>%
  group_by(Day_of_Week, Day, Month, Year) %>%
  summarise(total_serious = n()) %>%
  group_by(Day_of_Week) %>%
  summarise(mean_serious = mean(total_serious),
            sd_serious= sqrt(mean_serious * (1 - mean_serious / n())))

# Calculate the number of records for each day of the week
serious_by_day <- serious_by_day %>%
  mutate(n = sapply(unique(Day_of_Week), function(day) sum(serious_accidents$Day_of_Week == day)))

# Calculate confidence intervals using normal distribution
serious_by_day <- serious_by_day %>%
  mutate(
    a = mean_serious + qnorm(0.025) * sd_serious / sqrt(n),
    b = mean_serious + qnorm(0.975) * sd_serious / sqrt(n)
  )

# Reorder the days of the week
serious_by_day <- serious_by_day[order(match(serious_by_day$Day_of_Week, days_order)),]

# Print out the result
print(serious_by_day)

ggplot(serious_by_day, aes(x = factor(Day_of_Week, levels = days_order), y = mean_serious)) +
  geom_errorbar(aes(ymin = a, ymax = b), width = 0.2) +  # Add error bars for confidence intervals
  labs(title = "Mean Number of 'Serious' Accidents per Day of the Week with 95% CI",
       x = "Day of the Week",
       y = "Mean Number of 'Serious' Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
#####################################################################################

# По дням тижня фатальні
fatal_accidents <- filtered_data_3 %>%
  filter(!is.na(Accident_Severity)) %>%
  filter(Accident_Severity == "Fatal")


# Define the order of days of the week
days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Aggregate data to calculate the total number of "slight" accidents for each distinct day of the week (based on Day, Month, and Year)
fatal_by_day <- fatal_accidents %>%
  group_by(Day_of_Week, Day, Month, Year) %>%
  summarise(total_fatal = n()) %>%
  group_by(Day_of_Week) %>%
  summarise(mean_fatal = mean(total_fatal),
            sd_fatal= sqrt(mean_fatal * (1 - mean_fatal / n())))

# Calculate the number of records for each day of the week
fatal_by_day <- fatal_by_day %>%
  mutate(n = sapply(unique(Day_of_Week), function(day) sum(fatal_accidents$Day_of_Week == day)))

# Calculate confidence intervals using normal distribution
fatal_by_day <- fatal_by_day %>%
  mutate(
    a = mean_fatal + qnorm(0.025) * sd_fatal / sqrt(n),
    b = mean_fatal + qnorm(0.975) * sd_fatal / sqrt(n)
  )

# Reorder the days of the week
fatal_by_day <- fatal_by_day[order(match(fatal_by_day$Day_of_Week, days_order)),]

# Print out the result
print(fatal_by_day)

ggplot(fatal_by_day, aes(x = factor(Day_of_Week, levels = days_order), y = mean_fatal)) +
  geom_errorbar(aes(ymin = a, ymax = b), width = 0.2) +  # Add error bars for confidence intervals
  labs(title = "Mean Number of 'Fatal' Accidents per Day of the Week with 95% CI",
       x = "Day of the Week",
       y = "Mean Number of 'Fatal' Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# ============ женя =================

# Графік 1.2. (Слайд 10). Кількість аварій і їх серйозність у міській і сільській місцевості

# Отримуємо унікальні
unique_Accident_Severity <- unique(filtered_data_3$Accident_Severity)

# Ітеруємося по кожному стану дороги
for (Road_Accident_Severity in unique_Accident_Severity) {
  
  # Підрахунок кількості фатальних аварій
  road_Accident_Severity <- filtered_data_3 %>% filter(Accident_Severity == Road_Accident_Severity) %>% nrow()
  
  # Обчислюємо довірчий інтервал
  ci <- filtered_data_3 %>%
    summarise(
      mean = mean(road_Accident_Severity),
      sd = sqrt(road_Accident_Severity * (1 - road_Accident_Severity/nrow(filtered_data_3))),
      n = nrow(filtered_data_3),
      a= mean + qnorm(0.025) * sd / sqrt(n),
      b= mean + qnorm(0.975) * sd / sqrt(n),
      a_t = mean + qt(0.025, df = n-1) * sd / sqrt(n),
      b_t = mean + qt(0.975, df = n-1) * sd / sqrt(n)
    )
  
  # Виводимо результат
  print(paste("Довірчий інтервал для", Road_Accident_Severity))
  print(ci)
}

# *********

# Отримуємо унікальні
unique_Area_Type <- unique(filtered_data_3$Area_Type)

# Ітеруємося по кожному стану дороги
for (Road_Area_Type in unique_Area_Type) {
  
  # Підрахунок кількості фатальних аварій
  road_Area_Type <- filtered_data_3 %>% filter(Area_Type == Road_Area_Type) %>% nrow()
  
  # Обчислюємо довірчий інтервал
  ci <- filtered_data_3 %>%
    summarise(
      mean = mean(road_Area_Type),
      sd = sqrt(road_Area_Type * (1 - road_Area_Type/nrow(filtered_data_3))),
      n = nrow(filtered_data_3),
      a= mean + qnorm(0.025) * sd / sqrt(n),
      b= mean + qnorm(0.975) * sd / sqrt(n),
      a_t = mean + qt(0.025, df = n-1) * sd / sqrt(n),
      b_t = mean + qt(0.975, df = n-1) * sd / sqrt(n)
    )
  
  # Виводимо результат
  print(paste("Довірчий інтервал для", Road_Area_Type))
  print(ci)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.1. (Слайд 21). Lolypop Погодні умови під час ДТП

# Отримуємо унікальні
unique_Weather_Conditions <- unique(filtered_data_3$Weather_Conditions)

# Ітеруємося по кожному стану дороги
for (Road_Weather_Conditions in unique_Weather_Conditions) {
  
  # Підрахунок кількості фатальних аварій
  road_Weather_Conditions <- filtered_data_3 %>% filter(Weather_Conditions == Road_Weather_Conditions) %>% nrow()
  
  # Обчислюємо довірчий інтервал
  ci <- filtered_data_3 %>%
    summarise(
      mean = mean(road_Weather_Conditions),
      sd = sqrt(road_Weather_Conditions * (1 - road_Weather_Conditions/nrow(filtered_data_3))),
      n = nrow(filtered_data_3),
      a= mean + qnorm(0.025) * sd / sqrt(n),
      b= mean + qnorm(0.975) * sd / sqrt(n),
      a_t = mean + qt(0.025, df = n-1) * sd / sqrt(n),
      b_t = mean + qt(0.975, df = n-1) * sd / sqrt(n)
    )
  
  # Виводимо результат
  print(paste("Довірчий інтервал для", Road_Weather_Conditions))
  print(ci)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.4. (Слайд 22). Залежність відсотка кількості аварій від світлових умов

# Отримуємо унікальні
unique_Light_Conditions <- unique(filtered_data_3$Light_Conditions)

# Ітеруємося по кожному
for (Road_Light_Conditions in unique_Light_Conditions) {
  
  # Підрахунок кількості фатальних аварій
  road_Light_Conditions <- filtered_data_3 %>% filter(Light_Conditions == Road_Light_Conditions) %>% nrow()
  
  # Обчислюємо довірчий інтервал
  ci <- filtered_data_3 %>%
    summarise(
      mean = mean(road_Light_Conditions),
      sd = sqrt(road_Light_Conditions * (1 - road_Light_Conditions/nrow(filtered_data_3))),
      n = nrow(filtered_data_3),
      a= mean + qnorm(0.025) * sd / sqrt(n),
      b= mean + qnorm(0.975) * sd / sqrt(n),
      a_t = mean + qt(0.025, df = n-1) * sd / sqrt(n),
      b_t = mean + qt(0.975, df = n-1) * sd / sqrt(n)
    )
  
  # Виводимо результат
  print(paste("Довірчий інтервал для", Road_Light_Conditions))
  print(ci)
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.5. (Слайд 23). Кількість аварій за станом дорожнього покриття та серйозністю


# Отримуємо унікальні
unique_Road_Surface_Conditions <- unique(filtered_data_3$Road_Surface_Conditions)

# Ітеруємося по кожному
for (Road_Road_Surface_Conditions in unique_Road_Surface_Conditions) {
  
  # Підрахунок кількості фатальних аварій
  road_Road_Surface_Conditions <- filtered_data_3 %>% filter(Road_Surface_Conditions == Road_Road_Surface_Conditions) %>% nrow()
  
  # Обчислюємо довірчий інтервал
  ci <- filtered_data_3 %>%
    summarise(
      mean = mean(road_Road_Surface_Conditions),
      sd = sqrt(road_Road_Surface_Conditions * (1 - road_Road_Surface_Conditions/nrow(filtered_data_3))),
      n = nrow(filtered_data_3),
      a= mean + qnorm(0.025) * sd / sqrt(n),
      b= mean + qnorm(0.975) * sd / sqrt(n),
      a_t = mean + qt(0.025, df = n-1) * sd / sqrt(n),
      b_t = mean + qt(0.975, df = n-1) * sd / sqrt(n)
    )
  
  # Виводимо результат
  print(paste("Довірчий інтервал для", Road_Road_Surface_Conditions))
  print(ci)
}

# ===================== слайд 13

library(dplyr)
library(ggplot2)

accidents_by_brand_severity <- filtered_data_3 %>%
  group_by(Car_Brand1, Accident_Severity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

top_10_brands <- accidents_by_brand_severity %>%
  filter(Car_Brand1 %in% unique(head(accidents_by_brand_severity$Car_Brand1, 10)))

top_10_brands <- top_10_brands %>%
  mutate(se = sqrt(count) / sqrt(n()),
         ci_lower = count - 1.96 * se,
         ci_upper = count + 1.96 * se)

top_10_brands$ci_lower <- pmax(top_10_brands$ci_lower, 0)
print(top_10_brands, n = 30)

# ===================== слайд 15
library(dplyr)
library(ggplot2)

fatal_accidents_by_car <- fatal_accidents %>%
  group_by(Car_Brand1) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

top_10_cars_fatal <- slice(fatal_accidents_by_car, 1:10)

top_10_cars_fatal <- top_10_cars_fatal %>%
  mutate(se = sqrt(count),
         ci_lower = count - 1.96 * se,
         ci_upper = count + 1.96 * se)

top_10_cars_fatal$ci_lower <- pmax(top_10_cars_fatal$ci_lower, 0)

print(top_10_cars_fatal)

# ===================== слайд 15

library(dplyr)
library(ggplot2)

fatal_accidents_by_car <- fatal_accidents %>%
  group_by(Age_of_Vehicle1) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

top_10_cars_fatal <- slice(fatal_accidents_by_car, 1:10)

top_10_cars_fatal <- top_10_cars_fatal %>%
  mutate(se = sqrt(count),
         ci_lower = count - 1.96 * se,
         ci_upper = count + 1.96 * se)

top_10_cars_fatal$ci_lower <- pmax(top_10_cars_fatal$ci_lower, 0)

print(top_10_cars_fatal)

# ===================== слайд 25
data_summary <- filtered_data_without_none %>%
  group_by(Carriageway_Hazards, Accident_Severity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(lower = count - qnorm(0.975) * sqrt(count),
         upper = count + qnorm(0.975) * sqrt(count))

data_summary <- data_summary %>%
  mutate(lower = ifelse(lower < 0, 0, lower))

print(data_summary)