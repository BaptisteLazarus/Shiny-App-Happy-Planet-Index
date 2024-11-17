# Introduction
This project analyzes data from the 2016 Happy Planet Index (HPI), a global measure of sustainable well-being. Using advanced statistical techniques and visualization tools, the project explores key indicators such as life expectancy, well-being, inequality, and ecological footprint to understand global patterns and disparities.

# Background
The Happy Planet Index ranks countries based on their ability to deliver long, happy, and sustainable lives for their citizens. Traditional metrics like GDP often overlook sustainability and well-being. This project aims to:

- Identify patterns in well-being and sustainability across regions.
- Explore the relationship between happiness and ecological impact.
- Group countries into clusters based on shared characteristics.

# Tools Used
**R** and multiple librairies:
- FactoMineR and factoextra for PCA (Principal Component Analysis)
- leaflet for interactive maps
- dplyr, ggplot2, and data.table for data manipulation and visualization
- cluster, fpc, and stats for clustering analysis

**Data Visualization:**
- Bar plots, box plots, scatter plots
- Interactive geographical maps
- Dendrograms and k-means clustering visualizations

**Dashboard:** shiny and shinydashboard for an interactive web application

## The Analysis
1. Data Cleaning and Preparation:
- Removed missing values for specific indicators (e.g., GINI Index).
- Scaled data to standardize variances.

2. Geographical Patterns:

- Created interactive maps to highlight countries with the highest and lowest HPI scores.
- Analyzed regional distributions of life expectancy, well-being, and ecological footprints.

3. Principal Component Analysis (PCA):
- Reduced dimensionality to identify major contributing factors to happiness.
- Visualized relationships among variables, such as the negative correlation between ecological impact and happiness.

4. Clustering Analysis:
- Conducted hierarchical clustering (CAH) using Ward’s method.
- Applied k-means to divide countries into two distinct clusters:

Cluster 1: High well-being, life expectancy, ecological footprint, and GDP per capita.

Cluster 2: Low well-being, life expectancy, and GDP, but lower ecological impact.

5. Dashboard
- Built an interactive Shiny dashboard for data exploration, comparison, and visualization.

## Results
**Geographical Insights:**

- Costa Rica ranked highest on the HPI due to balanced well-being and low ecological footprint.
- Chad ranked lowest, reflecting high inequality and low life expectancy.

**Key Findings from PCA:**

- Axis 1 revealed a trade-off between well-being and inequality.
- Axis 2 highlighted an inverse relationship between happiness and GDP per capita.

**Clustering:**
Two clusters emerged: one representing high-performing countries in terms of well-being, and the other encompassing countries with challenges in sustainability and happiness.

**Sustainability vs. Happiness:**
Countries with higher happiness often had greater ecological footprints, raising questions about sustainable growth.
