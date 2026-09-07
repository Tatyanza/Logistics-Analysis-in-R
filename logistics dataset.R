# Importing the data
logistics <- read.csv("data/logistics_dataset.csv")

# Load libraries
library(dplyr)

# Loading ggplot2 for our visualizations 

library(ggplot2)

View(logistics)

# Selecting comparative variables

# Variable 1 - Creating a demand table
demand <- aggregate(`daily_demand` ~ category, logistics, 
                          FUN = function(x) c(Min = min(x),
                                              Mean = mean(x),
                                              Max = max(x)))
View(demand)

# Variable 2 - Stock level vs. Reorder point

# Find rows where stock level is less than the reorder point

logistics$under_stocked <- as.numeric(logistics$stock_level) < as.numeric(logistics$reorder_point)

stock_risk_table <- aggregate(under_stocked ~ zone, logistics,
                              FUN = sum)
names(stock_risk_table) <- c("Zone", "Items Needing Reorder")


# Variable 3 - Zone table
zones <- aggregate(cbind(`stock_level`, `reorder_point`,
                         `daily_demand`) ~ zone, 
                        logistics, FUN = mean)


# Step 1 - Map the data with aes
ggplot(logistics, aes(x = 'daily_demand',
                               y = zone,
                               color = 'reorder_point'))+
  geom_point(alpha = 0.3, size = 3)

# Visualization 1 - Histogram

# This graph shows the daily demand requirement for stock levels

# The second graph shows the demand per category

daily_demand_hist <- ggplot(logistics, aes(x = `daily_demand`)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Daily Demand", x = "Daily Demand Volume", y = "Count of Items")


daily_demand_hist2 <- ggplot(logistics, aes(x = `daily_demand`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  # Splits the chart into separate panels by category
  facet_wrap(~ category) + 
  labs(title = "Daily Demand Distribution across Categories", 
       x = "Daily Demand Volume", 
       y = "Count of Items") +
  theme_classic()

# Save the histogram

ggsave("daily_demand_hist2.png")

ggsave("daily_demand_hist.png", plot = daily_demand_hist, width = 8, height= 5, dpi = 300)

# Visualization 2 - Scatter plot

# Current Stock Levels vs Reorder Points Scatter Plot to determine 

# which products are leading to a stockout and need to be reordered

inventory_risk <- ggplot(logistics, aes(x = reorder_point,
                                        y = stock_level,
                                        color = under_stocked)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  # Custom high-contrast colors (Red for at risk, Grey for safe)
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  labs(title = "Inventory Risk Assessment", x = "Reorder Point",
       y = "Current Stock", color = "Under Stocked?") +
  theme_minimal()

# Save scatter plot
ggsave("inventory_risk.png")


# Visualization 3 - Reorder levels per zone

# This graph shows at which point stock needs to be reorder

# or re-shelved to avoid running out of stock

reorder_levels <- ggplot(stock_risk_table, 
       aes(x = reorder(Zone, -`Items Needing Reorder`), 
           y = `Items Needing Reorder`, 
           fill = Zone)) +                             
  geom_bar(stat = "identity", show.legend = FALSE) +    
  labs(x = "Zone") +                                    
  theme_classic()

print(reorder_levels)

# Save the bar chart

ggsave("reorder_levels.png")

# Correlation Analysis

correlation_test <- cor.test(logistics$daily_demand,
                             logistics$reorder_point)

correlation_test

# Linear regression model
regression_model <- lm(reorder_point ~ daily_demand,
                       data = logistics)

summary(regression_model)

regression_plot <- ggplot(logistics,
       aes(x = daily_demand,
           y = reorder_point)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm",
              se = TRUE,
              color = "blue") +
  labs(
    title = "Linear Regression of Daily Demand and Reorder Point",
    x = "Daily Demand",
    y = "Reorder Point"
  ) +
  theme_minimal()

ggsave("regression_plot.png")
