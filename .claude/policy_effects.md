# Decomposing policy effects in wiseapp simulations

1. **Main effect** The direct welfare impact of the policy, independent of weather. This answers the question - how much does the policy shift a household's position in the welfare distribution?

The size of a cash transfer may vary by welfare quantile of an individual ($τ_i$) or other characteristics ($x_i$), due to targeting etc. With RIF approach, the shift is allowed to vary across the distribution. The coefficient $\beta_x(τ)$ on any variable $x$ in the model that a policy changes tells us the marginal effect on the τ-th quantile of the unconditional welfare distribution:

- A cash transfer raises household consumption directly: $\Delta y_{main} = cash_i(τ_i, x_i)$

  - If cash is targeted to the poor or decreases with income, it is progressive

- Access to electricity is associated with higher income: $\Delta y_{main} = \beta_{elec}(τ_i) \cdot \Delta elec_i$

  - If $\beta$ is larger at low quantiles, the policy is progressive. It lifts the bottom more than the middle.

2. **Resilience effect** This is the primary interest and contribution. It answers the question: how does the policy change how badly a weather shock hurts? There are two mechanisms:

- **Repositioning** Independent of any interaction term (cash transfer case), a household that the policy has moved from the 20th to the 25th percentile now experiences the weather shock as a 25th-percentile household. If weather damages are concave across the distribution — i.e. poorer households lose proportionally more from the same shock — then the transfer provides implicit weather protection purely by moving the household up. This is captured by the difference in the hazard coefficient evaluated at the post-policy versus pre-policy quantile:

$$\Delta y_{res1} = [\beta_{Haz}(\tau_{i,policy}) - \beta_{Haz}(\tau_i)] \cdot Haz_i$$

   - **Direct interaction** If the policy variable explicitly moderates weather sensitivity (e.g., electricity reducing heat impacts) and is interacted with weather in the model:

$$\Delta y_{res2} = \beta_{Haz:elec}(\tau_i) \cdot Haz_i \cdot \Delta elec_i$$

Total resilience effect:

$$\Delta y_{res} = \underbrace{[\beta_{Haz}(\tau_{i,policy}) - \beta_{Haz}(\tau_i)] \cdot Haz_i}_{\text{repositioning}} + \underbrace{\beta_{Haz:elec}(\tau_i) \cdot Haz_i \cdot \Delta elec_i}_{\text{direct interaction}}$$

Total policy effect:

$$\Delta y_{total} = \Delta y_{main} + \Delta y_{res}$$

**Why the distinction matters for policy**

A policy could have a large main effect and no resilience effect — unconditional cash transfers that are spent on non-agricultural consumption. Or a small main effect but a large resilience effect — index insurance or early warning systems that don't raise average income much but dramatically reduce vulnerability to downside weather risk. Or both — a productive asset transfer that raises baseline income and also reduces weather sensitivity because the assets are drought-tolerant inputs.

The decomposition allows us to distinguish ex-ante resilience-building interventions from ex-post consumption-smoothing ones.