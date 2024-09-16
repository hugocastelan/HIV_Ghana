library(readxl)
library(reshape)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(ggmosaic)


Metadata <- read_excel("~/Documents/vih_billal/Metadata.xlsx")
                                                    
 View(Metadata)
# Rename columns to match the data frame in previous code
data <- Metadata %>%
  rename(
    Subtypes = `Subtypes`,
    Marital_status = `Marital status`,
    Education = `Educational level`,
    Risk_factor = `Risk factor`
  )
library(ggplot2)
library(gridExtra)

# Crear gráficos individuales mostrando porcentajes
plot_subtypes <- ggplot(data, aes(x = Year, fill = Subtypes)) +
  geom_bar(position = "fill") +  # Apilar las barras para mostrar porcentajes
  labs(title = "A)", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +  # Convertir el eje Y a porcentaje
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

plot_marital_status <- ggplot(data, aes(x = Year, fill = Marital_status)) +
  geom_bar(position = "fill") +
  labs(title = "B)", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

plot_education <- ggplot(data, aes(x = Year, fill = Education)) +
  geom_bar(position = "fill") +
  labs(title = "C)", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

plot_risk_factor <- ggplot(data, aes(x = Year, fill = Risk_factor)) +
  geom_bar(position = "fill") +
  labs(title = "D)", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# Crear gráfico de porcentaje de hombres y mujeres por año
plot_gender <- ggplot(data, aes(x = Year, fill = Sex)) +
  geom_bar(position = "fill") +
  labs(title = "E)", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"), labels = c("Male", "Female")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

# Combinar gráficos en una sola disposición
grid.arrange(plot_subtypes, plot_marital_status, plot_education, plot_risk_factor, plot_gender, 
             nrow = 3, ncol = 2)


# 
plot_subtypes <- ggplot(data, aes(x = Year, fill = Subtypes)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Subtypes by Year", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +  # Convertir eje Y a porcentaje
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 14),  # Tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 12)    # Tamaño del texto de los ejes
  ) +
  ggtitle("Panel A")

plot_marital_status <- ggplot(data, aes(x = Year, fill = Marital_status)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Marital Statuses by Year", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  ggtitle("Panel B")

plot_education <- ggplot(data, aes(x = Year, fill = Education)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Education Levels by Year", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  ggtitle("Panel C")

plot_risk_factor <- ggplot(data, aes(x = Year, fill = Risk_factor)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Risk Factors by Year", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  ggtitle("Panel D")

# Combinar gráficos en una sola disposición
grid.arrange(plot_subtypes, plot_marital_status, plot_education, plot_risk_factor, 
             nrow = 2, ncol = 2)




# Crear gráficos individuales con etiquetas de ejes más grandes, en porcentaje, y facetas por sexo
plot_subtypes <- ggplot(data, aes(x = Year, fill = Subtypes)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Subtypes by Year and Sex", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +  # Convertir eje Y a porcentaje
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  facet_wrap(~Sex) +  # Facetas por sexo
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 10),  # Tamaño de las etiquetas de los ejes
    axis.text = element_text(size = 10)    # Tamaño del texto de los ejes
  ) +
  ggtitle("Panel A")

plot_marital_status <- ggplot(data, aes(x = Year, fill = Marital_status)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Marital Statuses by Year and Sex", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  facet_wrap(~Sex) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)
  ) +
  ggtitle("Panel B")

plot_education <- ggplot(data, aes(x = Year, fill = Education)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Education Levels by Year and Sex", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  facet_wrap(~Sex) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)
  ) +
  ggtitle("Panel C")

plot_risk_factor <- ggplot(data, aes(x = Year, fill = Risk_factor)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Distribution of Risk Factors by Year and Sex", x = "Year", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
  facet_wrap(~Sex) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)
  ) +
  ggtitle("Panel D")

# Combinar gráficos en una sola disposición
grid.arrange(plot_subtypes, plot_marital_status, plot_education, plot_risk_factor, 
             nrow = 2, ncol = 2)


# Ensure that categorical variables are factors
data$Tribe <- as.factor(data$Tribe)
data$Sex <- as.factor(data$Sex)
data$Region <- as.factor(data$Region)
data$Subtypes <- as.factor(data$Subtypes)

# Define the color palette
palette_colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")

# Mosaic plot by Tribe
plot_mosaic_tribe <- ggplot(data) +
  geom_mosaic(aes(x = product(Tribe), fill = Subtypes), color = "black") +
  labs(title = "Subtype Distribution by Tribe", x = "Tribe", y = "Proportion") +
  scale_fill_manual(values = palette_colors) +
  theme_light() +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  ggtitle("Panel A")

# Mosaic plot by Sex
plot_mosaic_sex <- ggplot(data) +
  geom_mosaic(aes(x = product(Sex), fill = Subtypes), color = "black") +
  labs(title = "Subtype Distribution by Sex", x = "Sex", y = "Proportion") +
  scale_fill_manual(values = palette_colors) +
  theme_light() +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  ggtitle("Panel B")

# Mosaic plot by Region
plot_mosaic_region <- ggplot(data) +
  geom_mosaic(aes(x = product(Region), fill = Subtypes), color = "black") +
  labs(title = "Subtype Distribution by Region", x = "Region", y = "Proportion") +
  scale_fill_manual(values = palette_colors) +
  theme_light() +
  theme(plot.title = element_text(face = "bold", size = 14)) +  # Corregido 'bolXd' por 'bold'
  ggtitle("Panel C")

# Combine the plots into one layout
grid.arrange(plot_mosaic_tribe, plot_mosaic_sex, plot_mosaic_region, nrow = 3)

