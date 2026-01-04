# Spatial-Competitive Analytics in Professional Bass Fishing

A Network-Based Framework for Location Strategy Analysis

**Presented at:** HICSS 2026 - Game Changing Analytics in Sport Mini-Track<br>
**Author:** Bradley Congelio, Ph.D. | Kutztown University of Pennsylvania<br>  
**Contact:** congelio@kutztown.edu

---

## Overview

This repository contains the code, data, and presentation materials for a novel analytical framework that applies spatial-competitive analytics to professional bass fishing tournaments. The research introduces four new performance metrics that quantify location-based strategic advantages and competitive differentiation patterns in the Bassmaster Elite Series.

**Key Innovation:** Develops the foundational "middleware" metrics necessary for building a "Weight Over Expected" (WOE) performance evaluation system for bass fishing, analogous to the NFL's "Rushing Yards Over Expected."

---

## Research Abstract

Professional bass fishing is a competitive sport with substantial prize money and media coverage, yet lacks the advanced analytical frameworks that have transformed other professional sports. This study introduces a comprehensive spatial-competitive framework specifically designed for tournament bass fishing that addresses unique challenges including dynamic spatial strategies, temporal performance variations, and competitor interactions.

Drawing from methodologies in sport analytics, ecological network theory, and spatial analysis, we present four novel performance metrics:

- **PWC (Productive Water Coefficient)** - Integrates location productivity with network centrality
- **CAM (Competitive Advantage Metric)** - Quantifies elite performer advantages at specific locations  
- **ILIM (Integrated Location Importance Metric)** - Synthesizes multiple dimensions of location value
- **PI (Performance Index)** - Composite assessment of angler performance

The framework is demonstrated using data from the 2024 Bassmaster Elite Series tournament at Toledo Bend Reservoir, comprising 1,587 individual fish catch records across 102 professional anglers over four tournament days.

---

## Key Findings

### Tournament Performance Patterns

- **Spatial Concentration:** 68% of catches occurred within 25% of grid cells, indicating significant spatial clustering
- **Elite Differentiation:** Top performers demonstrated markedly different spatial strategies from lower-tier competitors
- **Temporal Dynamics:** Peak fishing activity occurred 8:00-9:00 AM (tournament hour 2), accounting for 16.8% of catches

### Metric Performance

| Metric | Top Performer | Score | Interpretation |
|--------|--------------|-------|----------------|
| PWC | Grid Cell 14-12 | 0.841 | High fish quality + strategic positioning |
| CAM | Pat Schlapper (2nd) | 8.35 | 8.35× better than non-elite at same locations |
| ILIM | Pat Schlapper (2nd) | 1.39 | Optimal balance across all dimensions |
| PI | - | - | 8 of top 10 in top quartile |

### Validation Results

- **Exact Match Accuracy:** 53% (55/104 anglers correctly predicted to exact performance tier)
- **Within One Tier Accuracy:** 86% (88/104 anglers predicted within one performance tier)
- **Elite Identification:** 8 of top 10 finishers ranked in top Performance Index quartile

---

## Dataset

### Tournament Information

- **Event:** 2024 Bassmaster Elite Series - Toledo Bend Reservoir
- **Dates:** February 22-25, 2024
- **Location:** Louisiana/Texas border
- **Participants:** 102 professional anglers
- **Total Catches:** 1,587 individual fish with GPS coordinates

### Data Structure

Each catch record includes:
- GPS coordinates (latitude/longitude)
- Timestamp (accurate to the second)
- Individual fish weight
- Angler identification
- Tournament day
- Keeper status (top 5 fish per day)
- Distance from launch point

### Spatial Framework

- **Grid Structure:** 20×20 uniform grid (400 discrete cells)
- **Cell Assignment:** Coordinate-based membership evaluation
- **Network Construction:** Pairwise connections between locations visited by same angler
- **Edge Weights:** Number of shared angler visits between location pairs

---

## Methodology

### 1. Data Collection

Custom R-based web scraping process interfacing with Bassmaster's JSON API endpoints:

```r
fetch_catch_data(tournament_id, date_str)
fetch_leaderboard_data(tournament_id, date_str)
```

**Ethics:** 2-second delay between API calls to respect server load

### 2. Spatial Discretization

**Grid Creation:**
- Calculate bounding box from all catch coordinates
- Partition into 20×20 uniform grid (400 cells)
- Assign each catch to grid cell via coordinate evaluation

**Mathematical Assignment:**

For catch coordinates $(lat_c, lon_c)$ assigned to cell $(j,k)$ where:

$$lat_j \leq lat_c < lat_{j+1}$$
$$lon_k \leq lon_c < lon_{k+1}$$

### 3. Network Analysis

**Construction Process:**
1. Identify all grid cells visited by each angler
2. Create pairwise connections between co-visited locations
3. Weight edges by frequency of shared visits
4. Calculate betweenness centrality for each location

**Betweenness Centrality:** Identifies "bridge" locations that connect different productive zones, providing tactical flexibility and efficient access to multiple areas.

### 4. Performance Metrics

#### Productive Water Coefficient (PWC)

Combines location productivity with strategic network positioning:

$$PWC_i = B_i \times \frac{W_i}{W_{max}} \times \frac{K_i}{K_{max}}$$

Where:
- $B_i$ = Normalized betweenness centrality
- $W_i$ = Average keeper weight at location
- $K_i$ = Keeper ratio (proportion of keepers)

#### Competitive Advantage Metric (CAM)

Quantifies elite performer advantages:

$$CAM_i = \frac{W_{elite,i} / \sum W_{elite,j}}{W_{bottom,i} / \sum W_{bottom,j}}$$

Where:
- $W_{elite,i}$ = Weight by top 10 finishers at location $i$
- $W_{bottom,i}$ = Weight by bottom tier at location $i$

**Interpretation:**
- CAM > 1.0 indicates elite advantage location
- CAM < 1.0 indicates uniform productivity across skill levels

#### Integrated Location Importance Metric (ILIM)

Synthesizes multiple dimensions:

$$ILIM_i = \frac{PWC_{norm,i} + CAM_{norm,i} + SPI_{norm,i}}{3}$$

Where all components are normalized to [0,1] scale for equal weighting.

#### Performance Index (PI)

Composite angler performance assessment:

$$PI_i = 0.2 \cdot PWC_{norm} + 0.4 \cdot CAM_{norm} + 0.3 \cdot ILIM_{norm}$$

**Differential Weighting Rationale:**
- CAM (0.4): Highest weight - primary skill discriminator
- ILIM (0.3): Strategic positioning importance
- PWC (0.2): Baseline productivity necessity

---

## Installation & Usage

### Prerequisites

```r
# Required R packages
install.packages(c(
  "tidyverse",      ### data manipulation
  "sf",             ### spatial analysis
  "igraph",         ### network analysis
  "ggraph",         ### network visualization
  "httr2",          ### API requests
  "jsonlite",       ### JSON parsing
  "leaflet",        ### interactive maps
  "viridis"         ### Color palettes
))
```

### Running the Analysis

1. **Data Collection:**
```r
source("scripts/01_data_collection.R")
tournament_data <- fetch_tournament_data(tournament_id = "your_id", 
                                         dates = c("2024-02-22", "2024-02-25"))
```

2. **Spatial Framework:**
```r
source("scripts/02_spatial_framework.R")
grid <- create_grid(bbox, grid_size = 20)
tournament_data <- assign_to_grid(tournament_data, grid)
```

3. **Network Analysis:**
```r
source("scripts/03_network_analysis.R")
location_network <- build_spatial_network(tournament_data)
betweenness <- calculate_betweenness(location_network)
```

4. **Metric Calculation:**
```r
source("scripts/04_metric_calculation.R")
metrics <- calculate_all_metrics(tournament_data, grid, betweenness)
```

---

## Theoretical Contributions

### 1. Spatial-Competitive Framework

Establishes the foundational analytical infrastructure for context-aware performance evaluation in bass fishing, addressing the unique challenge of analyzing a sport with sparse traditional data.

### 2. Network-Based Strategic Analysis

Introduces betweenness centrality as a measure of strategic positioning in competitive fishing, revealing that elite success depends on hub control, not just productive water access.

### 3. Skill vs. Luck Differentiation

CAM metric proves elite performers extract disproportionate value from the same locations as lower-tier competitors, demonstrating skill-based spatial decision-making.

### 4. Bridge to "Over Expected" Metrics

Creates the necessary middleware for developing "Weight Over Expected" performance metrics, analogous to advanced analytics in traditional sports (NFL's "Rushing Yards Over Expected", MLB's "Wins Above Replacement").

---

## Future Research Directions

### Phase 2: Environmental Integration

- **Bathymetric Mapping:** Incorporate underwater structure data (depth contours, creek channels, bottom composition)
- **Structure Identification:** Quantify effects of boulders, laydowns, and man-made structures
- **Weather Variables:** Integrate real-time weather and water temperature data

### Phase 3: Technology Quantification

- **Forward-Facing Sonar:** Measure LiveScope/ActiveTarget deployment and proficiency
- **Technology Asymmetry:** Quantify differential skill in exploiting modern fish-finding technology
- **Equipment Effects:** Analyze impact of boat speed, trolling motor efficiency, etc.

### Phase 4: Weight Over Expected Development

Build comprehensive Expected Weight models:

$$WOE_i = W_{actual,i} - E[W_i | PWC, CAM, ILIM, Structure, Weather, Technology]$$

This will enable true context-aware performance evaluation that separates skill from environmental luck.

### Full-Season Analysis

Expand framework to complete Bassmaster Elite Series season:
- Cross-venue validation of metrics
- Angler consistency analysis
- Venue-specific strategic patterns
- Season-long performance rankings

---

## Citations

If you use this framework or code in your research, please cite:

```
Congelio, B. (2026). Spatial-Competitive Analytics in Professional Bass Fishing: 
A Network-Based Framework for Location Strategy Analysis. 
Proceedings of the 59th Hawaii International Conference on System Sciences (HICSS).
```

---

## Related Work

### Sport Analytics Literature

- Lewis, M. (2003). *Moneyball: The Art of Winning an Unfair Game*. W.W. Norton & Company.
- Hollinger, J. (2007). What is PER? ESPN Analytics.

### Spatial Analysis Methods

- Pebesma, E., & Bivand, R. (2023). *Spatial Data Science: With Applications in R*. Chapman and Hall/CRC.
- Wickham, H. (2014). Tidy data. *Journal of Statistical Software*, 59, 1-23.

### Bass Fishing Research

- Settlage, D. M., & Settlage, M. E. (2024). Using Orthodromic Distance to Determine Homefield Advantage in Professional Bass Fishing Tournaments. *Journal of Sports Economics*, 25(7), 844-865.

---

## Acknowledgments

- **Bassmaster/B.A.S.S.** for tournament data availability via public API
- **HICSS 2026** Game Changing Analytics in Sport mini-track organizers
- **Kutztown University of Pennsylvania** for research support

---

## License

This project is licensed under the MIT License - see LICENSE file for details.

Data from Bassmaster.com is used for academic research purposes under their "publicly" available API. Please respect their terms of service when using this data.

---

## Contact

**Bradley Congelio, Ph.D.**  
Assistant Professor of Sport Management<br>
Kutztown University of Pennsylvania

📧 congelio@kutztown.edu  
🐙 GitHub: [@bcongelio](https://github.com/bcongelio)  
🔗 LinkedIn: [Brad Congelio](https://linkedin.com/in/bradcongelio)

---

## Conference Presentation

**HICSS 2026 - Game Changing Analytics in Sport**  
📍 Hyatt Regency - Maui, HI  
📅 January 6-10, 2026


---

*Last Updated: January 2026*
