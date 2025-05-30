## Step 2 - Simulating Welfare Over the Observed Historical Distribution of Weather

This step simulates welfare over the distribution of weather, based on historical observations. It uses equation 1 to predict consumption in various weather conditions that could occur in the same location (\( W_{kt} \)). Some studies do not consider \( W_{kt} \) but assume that consumption is lognormal conditional on \( X \) and assess its mean and variance to estimate the probability of falling below the poverty line. We prefer an approach that uses the empirical distribution of weather conditions to explain changes in welfare related to the weather.

### Using Historical Weather Observations

As a starting point, welfare is simulated for \( T \) states of the world corresponding with \( T \) years of weather observations. \( W_{kt} \) in each state of the world \( t \) are substituted into equation (1) to predict \( C_{hkt} \). Often at least 40 years of historical weather data is available. The same year of weather is applied to all households in each state of the world to account for the spatial correlation in weather conditions – if one area experiences drought, it is likely that nearby areas also experience drought. In this simple baseline scenario, the probability that a household falls below a poverty line \( z \) conditional on the weather can be calculated as:

\[ \Pr[C_{hk} \leq z | \text{WeatherProbabilityDistribution}_k] = \frac{1}{T} \sum_{t=1}^{T} [C_{hkt} < z] \quad (2) \]

The share of households with at least \( X/T \) probability of being poor can be calculated:

\[ \frac{1}{N} \sum_{h=1}^{N} \left[ \frac{1}{T} \sum_{t=1}^{T} [C_{hkt} < z] \geq X \right] \quad (3) \]

This metric has previously been used to estimate “vulnerability to poverty” using an annual threshold \( X/T = 0.29 \), or 50% probability over two years (Gunther and Harttgen, 2009).

The probability at least \( P/N \) households are poor in a future period \( t \) can be calculated as:

\[ \frac{1}{T} \sum_{t=1}^{T} \left[ \frac{1}{N} \sum_{h=1}^{N} [C_{hkt} < z] \geq P \right] \quad (4) \]

Results can be plotted on an exceedance probability curve to communicate risk.

Probabilistic estimates of any other relevant statistics can be derived from the simulated consumption distributions (FGT1, FGT2, Gini, Prosperity gap…). Welfare outcomes other than consumption and income, such as food security, health, hours worked, etc., can also be considered using the same framework.

### Simulation Using a Stochastic Catalog Based on Historical Observations

An extension to this basic setup involves generating a stochastic “catalog” of weather for many years, e.g., 10,000, based on the historical distribution (Gascoigne et al, 2024). Stochastic catalogs are designed to provide a view of a hazard that goes beyond the data available in the historical record. The idea is to simulate welfare in many possible states of the weather consistent with their probability of occurring.

In general, this involves adding random noise to the historical observations, consistent with the expected distribution of the weather variable (Kochar and Knippenberg, 2023). Some care is needed to realistically represent the physical processes governing the natural phenomena being modeled. The synthetic weather data can be checked for statistical robustness by inspecting how well it: (i) preserves intrinsic properties in terms of spatial and temporal correlations; (ii) is consistent with the empirical distributions at each location; and (iii) produces a realistic view of the weather consistent with historical observations while allowing for the creation of new extremes.

The process of simulating welfare distributions and calculating probabilistic welfare outcomes is the same as described above, only many more states of the world \( T \) can be represented. This allows the shape of an exceedance probability curve to be more precisely defined, especially in the tail (corresponding with rare events and few historical weather observations). However, it is important to remember that the vulnerability function derived in step 1 might not perform well for extreme events since it is based only on the weather conditions when survey data was collected.

Step 2 allows us to estimate the likelihood of different welfare distributions based on the historical distribution of weather.
