# ecotourism-gsoc-tests

This repository contains my practice work and test artifacts for the **R Project for Statistical Computing – GSoC 2026** idea:

> **ecotourism: update data package and create Shiny app**.

The main goal of this repo is to demonstrate that I can:

- Work with the `ecotourism` data package (occurrence, weather, and tourism tables)
- Design clear, teaching‑oriented tutorials
- Build interactive, web‑ready documents using Quarto, R, and HTML widgets

## Contents

- `ecotourism_tutorial.qmd`  
  Quarto source file for an interactive tutorial titled **“Ecotourism Tutorial: Joining occurrence, weather, and tourism data”**.  
  It focuses on orchid sightings in Western Australia during September and shows how to:
  - Filter occurrence data by state and month  
  - Join orchid records to daily weather using `ws_id` and `date`  
  - Join to quarterly tourism counts using `ws_id`, `year`, and `quarter`  
  - Assess data completeness for weather and tourism variables  
  - Explore the relationship between tourism volume and recorded sightings

- `ecotourism_tutorial.html`  
  Rendered HTML version of the tutorial, including:
  - An interactive `DT::datatable()` view of the orchid dataset  
  - A `leaflet` map of sampled orchid locations with informative popups  
  - An interactive `plotly` scatter plot relating tourism trips to yearly orchid sightings  

## How to run the tutorial locally

1. Install R (and optionally RStudio).
2. Install the `ecotourism` package and required dependencies:

   ```r
   # install.packages("pak")
   pak::pak("vahdatjavad/ecotourism")

   install.packages(c(
     "tidyverse",
     "plotly",
     "DT",
     "leaflet",
     "quarto"
   ))
