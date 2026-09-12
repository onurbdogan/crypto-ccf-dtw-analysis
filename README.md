# Cryptocurrency Time Series Analysis using CCF and DTW

This repository contains the dataset and R scripts used for analyzing the lead-lag relationships and structural similarities between various cryptocurrencies. The methodology employs the Cross-Correlation Function (CCF) and Dynamic Time Warping (DTW) to discover market dynamics.

## Project Overview
The primary goal of this research is to identify leading and lagging cryptocurrencies within specific market cycles. By utilizing daily OHLCV data from Binance, the study calculates a custom volatility/price-change metric and systematically compares coin pairs to find correlations.

## Methodology & Data Preprocessing
* **Data Source:** Binance daily historical data (Open, High, Low, Close, Volume).
* **Data Cleaning:** 
  * Cryptocurrencies that were delisted from the exchange were completely removed to maintain time-series integrity.
  * Missing dates were filled with `NA` values to align the time series.
  * Coins with more than **80% missing data** in a given period were excluded from that specific period's analysis.
* **Lag Window:** The CCF analysis scans a lag window of **±10 days** (`lag.max = 10`).
* **Metrics Used:** 
  * **CCF (Cross-Correlation Function):** Measures linear correlation and identifies lead/lag times between coin pairs.
  * **DTW (Dynamic Time Warping):** Measures the similarity between the shapes of two time series, accommodating for phase shifts.

## Analyzed Market Periods
The analysis is divided into three distinct market periods to observe changing dynamics over time:
1. **Period 1:** March 12, 2020 – November 10, 2021
2. **Period 2:** November 11, 2021 – October 12, 2023
3. **Period 3:** October 13, 2023 – April 15, 2024

## Repository Structure
* `binance/` : Directory containing the raw daily JSON data for each cryptocurrency.
* `binance_ccf_dtw_analysis.R` : The main R script containing data ingestion, cleaning, CCF/DTW calculations, and visualization functions.
* `df_with_na_cleaned.rds` : The cleaned and time-aligned main dataset.
* `result_per1st.rds`, `result_per2nd.rds`, `result_per3rd.rds` : The final computed correlation and DTW distance results for each respective period.
