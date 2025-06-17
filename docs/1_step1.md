## Step 1 - Estimating the Impact of Weather on Welfare

The first step relies on quantifying the relationship between welfare and weather. Users can specify several inputs at this step:

- **The sample:** The household survey data used for the analysis. The sample could be selected by country, survey years, or population group (e.g., rural households).

- **The welfare outcome:** The welfare variable of interest available from GMD. This could be the welfare aggregate, another dimension of welfare captured by GMD, or a derived binary outcome such as poverty status.

- **The weather variable(s):** One or more weather variables of interest, typically derived from remote sensing or weather stations. Options will be selected based on use in the literature, such as the number of days above a temperature threshold, number of days of excess rainfall, and drought indices for the previous growing season.

- **The model specification:** The default is to use a fixed effects specification that can capture non-linearity by binning or transforming weather observations (Baquié and Foucault, 2023). This requires survey data collected from the same location in at least two survey rounds. It is important to have sufficient variation in the weather variable. Results depend critically on the goodness of fit. Machine learning models to predict poverty status could also be offered (Iran CCDR). Other options include:
  - **Interactions** – to estimate heterogeneity in impacts
  - **Lags** – include weather conditions in the previous period
  - **Displacement** – include nearby weather conditions
  - **Controls** – household and spatial characteristics to improve the model fit
  - **Fixed effects** – region, sub-region, cluster, household, year

### Default Fixed Effects Specification

One can regress log consumption on weather using a specification like below:

\[ \ln(C_{hkt}) = \beta X_{hkt} + \delta W_{kt} + \gamma W_{kt} \times X'_{hkt} + \alpha_h + \eta_t + \epsilon_{hkt} \]

Where:
- \( C_{hkt} \) is household \( h \)'s consumption when living in region \( k \) at time \( t \).
- \( X_{hkt} \) are household characteristics.
- \( \alpha_h \) and \( \eta_t \) are household and time fixed effects.
- \( W_{kt} \) is a vector of binned weather variables, potentially complemented with variables describing idiosyncratic shocks at the household level.
- \( \delta \) and \( \gamma \) are the parameters of interest that quantify the impact of weather on household welfare for the years in the dataset (past years with both weather and survey data).

Often, household panel data is not available, in which case \( \alpha_h \) is replaced by fixed effects at a more aggregated level, such as region fixed effects (\( \alpha_k \)). The error term could be higher in this specification.

### Methodological Decisions

Dell et al. (2014) highlight important considerations when estimating the impact of weather shocks. 


Outputs from step 1 describe the relationship between welfare and the weather variable of interest. These can include regression tables, figures, maps.
