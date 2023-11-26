library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr) # For Making Text to toTitleCase.
library(scales) # For Making Short Number of Axis Text

sales_data <- read.csv("C:\\Users\\HP\\OneDrive\\Google Data Analytics\\Project\\Amazon Sales Analysis\\data\\Amazon Sale Report.csv")

palettes <- list(
  MediumVioletRed = "#C71585",
  Tomato = "#FF6347",
  Gold = "#FFD700",
  Yellow = "#FFFF00",
  RebeccaPurple = "#663399",
  Indigo = "#4B0082",
  LimeGreen = "#32CD32",
  MediumSeaGreen = "#3CB371",
  DarkGreen = "#006400",
  SpringGreen = "#00FF7F",
  GreenYellow = "#ADFF2F",
  Lime = "#00FF00",
  Teal = "#008080",
  CadetBlue = "#5F9EA0",
  MidnightBlue = "#191970",
  White = "#FFFFFF",
  DarkOrange = "#FF8C00",
  Coral = "#FF7F50",
  OrangeRed = "#FF4500",
  LightSalmon = "#FFA07A"
)

# Removing NA or Null Values from the Sales Data.
sales_data <- na.omit(sales_data)

# Cleaning and Organizing the Sales Data.
clean_data <- unique(sales_data) %>%
  mutate(
    Date = mdy(Date),
    Status = Status,
    Fulfilment = Fulfilment, # Amazon, Merchant
    Sales_Channel = Sales.Channel, # Aamazon.in
    Ship_Service_Level = ship.service.level, # Expedited (Fast), Standard
    Category = Category, # Product Category
    Size = Size,
    Courier_Status = Courier.Status, # The count of "" category is 5133, make sure. 
    Amount = as.double(Amount),
    Ship_City = str_to_title(ship.city),
    Ship_State = str_to_title(ship.state),
    Ship_Country = ship.country,
    B2B = ifelse(B2B == "True", 1, 0)
  ) %>%
  select(Date, Status, Fulfilment, Sales_Channel, Ship_Service_Level, 
         Category, Size, Courier_Status, Amount, Ship_City, Ship_State,
         Ship_Country, B2B)

# Understand the Attribute of Clean Data.
know_data <- clean_data %>%
  group_by(Status) %>%
  summarise(Count = length(Status)) %>%
  arrange(Count)





# ***************************************** Solving Question
# Key Performance Indicator
kpi <- clean_data %>%
  summarise(Revenue = sprintf("%0.1fM", sum(Amount) / 1e6),
            Unshipped_Count = length(Courier_Status[Courier_Status == "Unshipped"]),
            B2B_Sales = sum(B2B),
            B2B_Ratio = paste(round((sum(B2B) / n()) * 100, 2), "%", sep = "")
  )
kpi_measures <- data.frame(Labels = c("Revenue", "Unshipped Count", "B2B Sales", "B2B Ratio"),
                           Shapes = c(1),
                           Values = c(kpi$Revenue, kpi$Unshipped_Count, kpi$B2B_Sales, kpi$B2B_Ratio)
)
ggplot(data = kpi_measures, aes(x = Shapes, y = Labels)) + 
  geom_bar(stat = "identity", fill = palettes$White) + 
  geom_text(aes(label = Values), hjust = 1, size = 12, 
            fontface = "bold", color = palettes$Gold) + 
  scale_x_continuous(breaks = NULL) + 
  theme_minimal() + 
  theme(
    axis.text.y = element_text(colour = palettes$RebeccaPurple),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y.left = element_text(size = 15)
  )





# 1.  Sales Over the Period of Months.
sales_over_months <- clean_data %>%
  group_by(Months = format(Date, "%B")) %>%
  summarise(Revenue = sum(Amount))

sales_over_months <- sales_over_months[order(-sales_over_months$Revenue), ]
sales_over_months$Months <- factor(sales_over_months$Months, levels = sales_over_months$Months)

ggplot(data = sales_over_months, aes(x = Months, y = Revenue, fill = Months)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = label_number_si(scale = 1e-6, suffix = "M", accuracy = 0.01)) +
  scale_fill_manual(values = c("March" = palettes$Tomato, "April" = palettes$LimeGreen,
                               "May" = palettes$Gold, "June" = palettes$DarkOrange)) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color = palettes$RebeccaPurple)
  )





# 2. Un-shipped Product
status_unshipped <- clean_data %>%
  group_by(Courier_Status, Status) %>%
  summarise(Count = n()) %>%
  filter(Courier_Status == "Unshipped")

ggplot(data = status_unshipped, aes(x = "", y = -Count, fill = status_unshipped$Status)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Count), 
            position = position_stack(vjust = 0.5)) + 
  coord_polar(theta = "y", start = 0, direction = -1) + 
  scale_y_continuous(breaks = NULL) + 
  scale_fill_manual(values = c("Cancelled" = palettes$OrangeRed, "Pending" = palettes$Coral,
                               "Pending - Waiting for Pick Up" = palettes$LightSalmon)) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank()
  )





# 3.  Top 10 City place ordered
top_cities_placed_ordered <- clean_data %>%
  group_by(Ship_City) %>%
  summarise(Orders_Count = n(), 
            Revenue = sum(Amount)) %>%
  arrange(desc(Revenue)) %>%
  head(10)

top_cities_placed_ordered <- top_cities_placed_ordered[order(-top_cities_placed_ordered$Revenue), ]
top_cities_placed_ordered$Ship_City <- factor(top_cities_placed_ordered$Ship_City, 
                                              levels = rev(top_cities_placed_ordered$Ship_City))

ggplot(data = top_cities_placed_ordered, aes(y = Ship_City)) + 
  geom_bar(aes(x = Revenue, fill = "Revenue"), stat = "identity") + 
  geom_text(aes(x = Revenue, label = sprintf("%0.1fM", (Revenue / 1e6))), 
            hjust = -0.2) + 
  geom_bar(aes(x = Orders_Count*100, fill = "Orders_Count"), stat = "identity") + 
  geom_text(aes(x = Orders_Count, label = sprintf("%0.1fK", (Orders_Count / 1000))), 
            vjust = 0.5, hjust = 0, size = 3, color = "#000456") + 
  scale_x_continuous(labels = label_number_si(scale = 1e-6, suffix = "M", accuracy = 0.01), 
                     "Revenue in Million",
                     sec.axis = sec_axis(~./100, name = "Orders Count in Thousand")) + 
  scale_fill_manual(values = c("Revenue" = palettes$LimeGreen, 
                               "Orders_Count" = palettes$Gold), name = "") + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    aspect.ratio = 0.5
  )





# 4.  Maximum Sizes Ordered
sizes_ordered <- clean_data %>%
  group_by(Size) %>%
  summarise(Ordered_Count = n()) %>%
  arrange(desc(Ordered_Count)) %>%
  head(5)

sizes_ordered <- sizes_ordered[order(sizes_ordered$Ordered_Count), ]
sizes_ordered$Size <- factor(sizes_ordered$Size, level = rev(sizes_ordered$Size))

ggplot(data = sizes_ordered, aes(x = Size, y = Ordered_Count, fill = Ordered_Count)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Ordered_Count), vjust = -0.5) + 
  scale_y_continuous(breaks = NULL) + 
  scale_fill_gradient(low = palettes$DarkGreen, high = palettes$LimeGreen) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank()
  ) + 
  guides(fill = FALSE)





# 5.  B2B Sales Count
B2B_Sales <- clean_data %>%
  group_by(Category) %>%
  summarise(Order_Count = sum(B2B), Revenue = sum(Amount)) %>%
  arrange(desc(Order_Count)) %>%
  head(5)

B2B_Sales <- B2B_Sales[order(B2B_Sales$Order_Count), ]
B2B_Sales$Category <- factor(B2B_Sales$Category, levels = rev(B2B_Sales$Category))

ggplot(data = B2B_Sales, aes(x = Category)) + 
  geom_line(aes(y = Revenue, group = 1), linewidth = 1, color = palettes$Gold) + 
  geom_text(aes(y = Order_Count, label = Order_Count), vjust = -4) + 
  geom_bar(aes(y = Order_Count*10000, fill = Order_Count), stat = "identity") +
  scale_y_continuous("Revenue in Million",
                     labels = label_number_si(scale = 1e-6, suffix = "M", accuracy = 0.01),
                     sec.axis = sec_axis(~./10000, name = "Orders Count in Thousand")) +
  scale_fill_gradient(low = palettes$OrangeRed, high = palettes$Lime) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(colour = palettes$Indigo),
    axis.text = element_text(colour = palettes$RebeccaPurple)
  ) + 
  guides(fill = FALSE)
