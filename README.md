# Settlement Reclassification to Estimate India’s Urban Transition (2011-2026)

This note outlines the methodology for projecting population growth and reclassifying settlements in India from 2011 to 2026, with particular focus on identifying emerging Census Towns (CTs) and Statutory Towns (STs). The approach combines official census data with labour force projections to estimate settlement transitions from rural to urban classification based on standard demographic and economic thresholds.

## Data Sources
* Census of India 2011: Primary Census Abstract and Town/Village Directory for baseline population, non-agricultural worker shares, and settlement areas
*	Technical Group on Population Projections (2019) Report: State-level population growth rates for 2011-2026 period. 2026 March estimates are used.
*	National Sample Survey (NSS) Employment Unemployment Survey 2011-12 (Round 68): Baseline non-agricultural workforce composition
*	Periodic Labour Force Survey (PLFS) rounds up to 2023-24: Male non-agricultural workforce data for projection calculations
*	Local Government Directory Portal: Updated counts of urban local bodies by state (as of June 2025)

## Methodology
**Step 1:** Baseline Data Compilation
Extract 2011 population figures, non-agricultural worker shares, and geographical area measurements for all settlements across India from Census 2011 Primary Census Abstract and Town/Village Directory datasets.
**Step 2:** Population Projection
Apply state-specific population growth rates (calculated from the Technical Group on Population Projections, 2019) to inflate 2011 settlement populations to current estimates. This approach provides reasonable approximations for mid-sized transitioning settlements while acknowledging potential underestimation for rapidly growing urban centres and overestimation for small rural settlements. In other words, we assume the large villages and small towns to have a population growth trajectory that is determined mostly by fertility and mortality rates, and only to a small extent by in-migration or out-migration.
**Step 3:** Workforce Composition Projection
Calculate state-level changes in non-agricultural workforce share using data from NSS Employment Unemployment Survey 2011-12 and different rounds of PLFS up to 2023-24. Estimate the workforce share number for the year 2026 using a linear trend-based projection. From this state-level data, calculate a non-agricultural workforce inflation factor, and apply that to all the settlements in the state, to estimate current employment composition. However, it is worth noting that in certain states with a large agricultural labour force, the state-level inflation number does not truly reflect the economic shift taking place in the transitioning settlements.
**Step 4:** Settlement Reclassification
Reclassify settlements using projected demographic and economic indicators against standard Census thresholds of 5,000 persons, 400 persons per square kilometre and 75% male non-agricultural workforce share. Additionally, settlements that had 65% male non-agricultural workforce share in 2011 are considered to meet the 2026 workforce threshold. Settlement boundaries remain constant at 2011 definitions for consistency. This step generates an updated list of CTs for 2026, identifying settlements that meet urban criteria but lacked ST or CT status in 2011.
**Step 5:** Statutory Town Approximation
Due to the absence of comprehensive national data on specific settlement transitions to Statutory Town status, the following approximation method is employed:
1. Calculate the net increase in urban local bodies per state from the Local Government Directory.
2. From the pool of 2026 Census Towns in each state, select the calculated number of settlements using Probability Proportionate to Size (PPS) sampling.
3. Designate these selected settlements as having transitioned to Statutory Town status.
This randomization approach acknowledges that rural local body to urban local body transitions involve administrative and political considerations beyond purely demographic thresholds.

## Limitations and Considerations
This method provides a simple and pragmatic framework for estimating the urban transition in India, in the absence of updated Census data. However, please note that the results should be interpreted as estimates suitable for macro-level analysis rather than definitive settlement classifications for administrative purposes. Some of the key limitations of the method are:
*	Population projections may not capture fine-grained spatial growth patterns, particularly in rapidly developing or declining areas, as a state-level growth rate is assumed at the settlement level as well.
*	Settlement boundary definitions remain fixed at 2011 census boundaries, which may not reflect actual urban expansion that involves merging of multiple smaller settlements and sometimes splitting as well.
*	The PPS sampling method for Statutory Town identification provides statistical approximation rather than actual administrative records
*	Non-agricultural workforce projections assume uniform state-level trends apply to individual settlements
*	This estimate uses the Census definition of urban, which is known to be an underestimate in many geographies.

