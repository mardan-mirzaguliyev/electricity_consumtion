# Global Electricity Consumption Dashboard

## Overview
This Shiny application provides an interactive visualization of global electricity consumption patterns. Users can explore both total electricity consumption and per-capita consumption across different countries, as well as a weighted composite view that combines both metrics.

## Features

### Multiple View Types
- **Total Consumption View**: Displays total electricity consumption in TWh/year by country
- **Per Capita Consumption View**: Shows electricity consumption per person in kWh/year
- **Weighted Composite View**: Combines both total and per-capita metrics with adjustable weights

### Interactive Controls
- Select between different view types (Total, Per Capita, or Composite)
- Adjust the number of top countries to display (5-50)
- For composite view: Adjust the weighting between total and per-capita consumption
- Interactive tooltips with detailed country information

### Visualization
- Responsive bar charts using plotly
- Flip-coordinated layout for better readability
- Detailed tooltips showing multiple metrics for each country

## Data
The dashboard uses a dataset containing the following information for each country:
- Location (country name)
- Total electricity consumption (TWh/year)
- Per capita electricity consumption (kWh/year)
- Population

## Requirements
- R (>= 4.0.0)
- Required R packages:
  - shiny
  - tidyverse
  - bslib
  - plotly

## Installation

```r
# Install required packages
install.packages(c("shiny", "tidyverse", "bslib", "plotly"))


