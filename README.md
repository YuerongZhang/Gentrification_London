# Gentrification_London
Table 1. Data collection with sources

| Indicators      | Sources with links        | Note             |
| :----------------    |:------------------------| :----------------| 
| population    | LSOA usual resident population (ONS) from https://www.nomisweb.co.uk/census/2011/ks101ew| 2001 and 2011 LOSA usual resident population data is used.|
| Low-income       |Household income estimates for small areas (GLA) from:  https://data.london.gov.uk/dataset/household-income-estimates-small-areas      |  The low-income group is defined as the household income less than 80% of the median of household income. | 
| Property sales price | Average house prices by LSOA (Land Registry) from:  https://data.london.gov.uk/dataset/average-house-prices  | Using the median value   |
| Hot market         |Change in median property price > change of regional median |1 indicate it is hot market, i.e. has a higher increase rate.|
| % higher education  |Qualification and students census (ONS) from https://www.nomisweb.co.uk/census/2011/ks501ew | The percentage of residents achieving NVQ level 4 or above. |
| % non-white        |Ethnic group census (ONS) https://www.nomisweb.co.uk/census/2011/ks201ew | 1-%white popultion|
| % renters          |Tenure census (ONS) from https://www.nomisweb.co.uk/census/2011/ks402ew | The % renters are the sum of % social housing renters and % private housing renters.|



Table 2. Gentrification and displacement Census Typologies

| Typology      | Typology criteria       |
| :--------------|:-------------------|
| Not losing low-income households | *Low income group in 2011 (see definition in footnote 1); *Not classified as the At risk of gentrification or Ongoing gentrification or Displacement. |
| At risk of gentrification   |*Low income group in 2011 Vulnerable in 2011 (see in footnote 2);*‘Hot market’ from 2001 to 2011 (see in footnote 3);*Not currently undergoing displacement or ongoing gentrification|
| Ongoing displacement   |*Low income group in 2011 Vulnerable in 2011 (see in footnote 2);*Median income decreased from 2001 to 2011;*Few signs of gentrification|
| Ongoing gentrification   |*Low income group in 2011 Vulnerable in 2011 (see in footnote 2);*Gentrified in 2001-2011 (defined in footnote 4)|
| Moderate- to high-income households   |*Groups which are not recognised as low-income group.|

1.Low income group in 2011

If the median household income is lower or equal to 0.95*median_2011, then it will be identified as moderate-to high-income group. Conversely, if the median household income is higher than 0.95*median_2011, then it will be identified as moderate-to high-income group. 
There are two official ways in defining the low-income group in UK: 1) Households are classed as being in low income if they live on less than 60% of the UK's median income (https://www.ethnicity-facts-figures.service.gov.uk/work-pay-and-benefits/pay-and-income/low-income/latest). As the data is at aggregated level, if we apply london[which(london$Median_2011<=0.6*median (london$Median_2011)),], there is no LSOA identified as low-income. The other definition is from Greater London Authority (http://content.tfl.gov.uk/people-on-low-incomes-summary.pdf). It reports 41% of Londoners can be classed as having a low annual household income.  We have tried different ratio and found 0.95*median is the benchmark for differentiating the low-income group and moderate- to high-income groups. 
```diff
-lsoa_attr$low_income <-ifelse(lsoa_attr$Median_2011<=0.95*median(lsoa_attr$Median_2011), 1,0)
-sum(lsoa_attr$pop_2011*lsoa_attr$low_income)/sum(lsoa_attr$pop_2011)
```
In the case of London, 3025 LSOAs are identified as the moderate- to high-income households, and 1810 LSOAs are identified as the low-income households.

2. Vulnerable in 2011
Housing affordability indicates if housing price increasing rate is below median speed
```diff
-lsoa_attr$if_affor <-ifelse(lsoa_attr$pr_m_2011<=median(sort (lsoa_attr$pr_m_2011, decreasing=FALSE)), 1,0)
```
and (any 2 of 3) 
% higher education < London median 
% renters > London median
% non-white > London median
```diff
-lsoa_attr$if_edu <-ifelse(lsoa_attr$qua4_2011<= median(lsoa_attr$qua4_2011), 1,0)
-lsoa_attr$if_rent <-ifelse(lsoa_attr$sp_rented2011 > median (lsoa_attr$sp_rented2011), 1,0)
-lsoa_attr$if_nonwhite <-ifelse(lsoa_attr$non_white > median (lsoa_attr$non_white), 1,0)
```
 3. ‘Hot market’ from 2001 to 2011
Change in median real rent (social and private) > London median 
```diff
- lsoa_attr$hot_market <-ifelse(lsoa_attr$ch_price>111500, 1, 0)
```
4. Gentrification from 2001 to 2011
Vulnerable in 2001
‘Hot market’ from 2001 to 2011
Demographic change from 2001 to 2011: a) household income increase from 2001 to 2011; and b) difference in % education  > London region

