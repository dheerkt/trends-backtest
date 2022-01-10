COMM4710 Active Management Strategy
================
Dheer Toprani
11/18/2021

## Fetch and process data

The Google trends data has been downloaded from trends.google.com, and
the csv files have been placed in the rawdata directory. The following
tickers were chosen: AMZN, AAPL, TSLA, NVDA. Monthly worldwide trend
data for each ticker was collected between the time period of 2011-01-01
to 2020-12-31.

Stock price data was collected using the getSymbols function in the
tidyquant package. The data is sourced from Yahoo Finance.

``` r
# Setting dates, tickers, etc.
tickers <- c("AMZN", "AAPL", "TSLA", "NVDA")
start_date <- "2010-12-01"
end_date <- "2020-12-31"
e = new.env()

# Read all ticker files simultaneously into "trends" dataframe
myfiles <- paste0("rawdata/", tickers, ".csv")
temp <- lapply(myfiles, read.csv, skip=1)
trends <- temp %>% reduce(inner_join, by = "Month")

# Process trends dataframe
colnames(trends) <- str_remove(colnames(trends), "...Worldwide.") # Fix column names
trends$Month <- paste0(trends$Month, "-01") # Make "Month" column YYYY-mm-dd format
trends[trends == "<1"] <- 1 # Change all "<1" results to 1.
trends[-1] <- trends[-1] %>% mutate_if(is.character, as.integer) # Change all character type columns to integers
trendChange <- trends %>% mutate_at(tickers, function(x) { return ((x-lag(x))/lag(x)) }) # Create a new df to calculate change
colnames(trendChange) <- c("Month", paste0(tickers, ".Change")) # Rename columns of new df

# Fetch monthly price data for all tickers
sapply(tickers, getSymbols, periodicity="monthly", from=start_date, to=end_date, env=e) # Store data in environment e
```

    ##   AMZN   AAPL   TSLA   NVDA 
    ## "AMZN" "AAPL" "TSLA" "NVDA"

After cleaning the data, we combined it into a single dataframe and
processed it further. The first few rows of the processed table are
shown below.

``` r
# Merge data into single table
data <- Reduce(merge, lapply(ls(name=e), get, envir=e))
combined <- as.xts(trendChange[,-1], order.by = as.Date(trendChange[,1])) %>% 
  merge(data, all=T)

# Calculate Rate of Change for Close prices for each stock ticker
temp <- combined %>% 
  Cl() %>% 
  ROC()
colnames(temp) <- paste0(tickers, ".ClROC") # Rename column
combined2 <- merge(combined, temp) %>% na.omit() # Merge to data table and omit NA values

head(combined2)
```

    ##            AMZN.Change AAPL.Change TSLA.Change NVDA.Change AAPL.Open AAPL.High
    ## 2011-02-01  -0.1666667 -0.24444444           0 -0.20000000  12.18929  13.03214
    ## 2011-03-01   0.2000000 -0.02941176           0  0.00000000  12.69536  12.91679
    ## 2011-04-01   0.1666667  0.00000000           0 -0.25000000  12.53964  12.68321
    ## 2011-05-01  -0.1428571 -0.36363636           0  0.00000000  12.49071  12.56536
    ## 2011-06-01  -0.3333333  0.23809524           0 -0.08333333  12.45964  12.57607
    ## 2011-07-01   0.5000000  0.46153846           0 -0.18181818  11.99821  14.44643
    ##            AAPL.Low AAPL.Close AAPL.Volume AAPL.Adjusted AMZN.Open AMZN.High
    ## 2011-02-01 12.06143   12.61464  9295949600      10.81535    170.52    191.40
    ## 2011-03-01 11.65214   12.44679 11306458800      10.67144    173.53    181.57
    ## 2011-04-01 11.43429   12.50464  9253829200      10.72105    181.58    197.80
    ## 2011-05-01 11.76500   12.42250  6912060400      10.65062    196.57    206.39
    ## 2011-06-01 11.08929   11.98821  9263850400      10.27828    196.06    206.25
    ## 2011-07-01 11.93571   13.94571 10653946800      11.95657    205.55    227.20
    ##            AMZN.Low AMZN.Close AMZN.Volume AMZN.Adjusted NVDA.Open NVDA.High
    ## 2011-02-01   169.51     173.29    95776400        173.29    6.0325    6.5425
    ## 2011-03-01   160.59     180.13   118979100        180.13    5.6750    5.7475
    ## 2011-04-01   175.37     195.81   116749400        195.81    4.6750    5.1100
    ## 2011-05-01   190.88     196.69   106274500        196.69    5.1050    5.1300
    ## 2011-06-01   181.59     204.49    95563700        204.49    4.9875    4.9950
    ## 2011-07-01   203.61     222.52    92808500        222.52    3.9850    4.0700
    ##            NVDA.Low NVDA.Close NVDA.Volume NVDA.Adjusted TSLA.Open TSLA.High
    ## 2011-02-01   5.4450     5.6650  2402576400      5.203927     4.862     5.098
    ## 2011-03-01   4.2525     4.6150  2759758000      4.239387     4.810     5.742
    ## 2011-04-01   4.2075     5.0000  1532207200      4.593053     5.490     5.636
    ## 2011-05-01   4.2800     5.0100  1716251600      4.602238     5.520     6.056
    ## 2011-06-01   3.7150     3.9850  1595378400      3.660663     6.000     6.300
    ## 2011-07-01   3.3975     3.4575  1455165600      3.176096     5.814     6.088
    ##            TSLA.Low TSLA.Close TSLA.Volume TSLA.Adjusted   AMZN.ClROC
    ## 2011-02-01    4.222      4.778   146517500         4.778  0.040119213
    ## 2011-03-01    4.354      5.550   162816500         5.550 -0.013395845
    ## 2011-04-01    4.840      5.520   149474500         5.520  0.004637578
    ## 2011-05-01    5.104      6.028   151744000         6.028 -0.006590671
    ## 2011-06-01    5.100      5.826   198636500         5.826 -0.035585344
    ## 2011-07-01    5.326      5.634   105510500         5.634  0.151248220
    ##             AAPL.ClROC   TSLA.ClROC   NVDA.ClROC
    ## 2011-02-01 0.021287913 -0.054113673 -0.008751879
    ## 2011-03-01 0.038712389 -0.204995027  0.149775879
    ## 2011-04-01 0.083465951  0.080126044 -0.005420067
    ## 2011-05-01 0.004484104  0.001998003  0.088037420
    ## 2011-06-01 0.038890204 -0.228898603 -0.034084622
    ## 2011-07-01 0.084497904 -0.141991528 -0.033510989

## Create a trading signal/indicator

After the data was sufficiently processed, we formulated our trading
strategy described below. We normalized the month-over-month change in
trend data to create monthly portfolio weights that sum up to 1.

``` r
# Select column names with the ".Change" suffix
ChangeSuffix <- colnames(combined2)[colnames(combined2) %>% endsWith(".Change")]

# Strategy 2: Normalize ".Change" columns to sum to 1, and allocate according to normalized weights
StrategyRule2 <- function(df, tickers) {
  ChangeSuffix <- paste0(tickers, ".Change")  # Fetch all MoM change in "Interest over time" trend data from Google
  normalizer <- 1/rowSums(df[,ChangeSuffix])  # Find a normalizing coefficient so that the portfolio weight will add up to 1
  temp <- df %>% mutate_at(ChangeSuffix, function(x) { x*normalizer })  # Multiply the MoM change in trends with the normalizer coefficient
  # Change column names, and merge with the input dataframe
  colnames(temp) <- paste0(tickers, ".NormSignal") %>% na.omit()
  return(cbind(df, temp[,1:length(tickers)]))
}

combined3 <- StrategyRule2(as.data.frame(combined2), tickers)
```

## Use the trading signal to create an equity curve

Once the trading signal and strategy was established, we calculated our
returns by multiplying portfolio weights with corresponding returns.

``` r
# Find Signal and ClRoc columns
SignalSuffix <- colnames(combined3)[colnames(combined3) %>% endsWith(".NormSignal")]
ClRocSuffix <- colnames(combined3)[colnames(combined3) %>% endsWith(".ClROC")]

# Multiply the values and merge the new columns with the combined dataset
temp <- combined3[,SignalSuffix]*combined3[,ClRocSuffix]
colnames(temp) <- paste0(tickers, ".Multiple")
combined4 <- cbind(temp, combined3)
```

We also created benchmark passive portfolios to compare our strategy
against. One benchmark portfolio represented all the stocks weighted
equally over the 10-year period. The other “Weighted Benchmark”
represents the average portfolio weights of our trading strategy (shown
in the table below) held passively over the 10-year period.

``` r
# Add portfolio multiple, average benchmark, and weighted benchmark to the combined dataset
avgPortfolioWeights <- as.matrix(colMeans(combined4[,paste0(tickers, ".NormSignal")]))
temp <- cbind(
  rowSums(combined4[,paste0(tickers, ".Multiple")]), 
  rowMeans(combined4[,ClRocSuffix]), 
  rowSums(avgPortfolioWeights*combined4[,ClRocSuffix])
)
colnames(temp) <- c("Portfolio.Multiple", "Benchmark.AvgClROC", "Benchmark.WeightedClROC")
combined5 <- cbind(combined4, temp)

# Format and display Weighted Benchmark weights
colnames(avgPortfolioWeights) <- "Weights"
rownames(avgPortfolioWeights) <- paste0(tickers, ".WeightedBenchmarkWeights")
avgPortfolioWeights
```

    ##                                 Weights
    ## AMZN.WeightedBenchmarkWeights 0.3779985
    ## AAPL.WeightedBenchmarkWeights 0.2864872
    ## TSLA.WeightedBenchmarkWeights 0.1455330
    ## NVDA.WeightedBenchmarkWeights 0.1899813

## Evaluate strategy

Finally, we compared the performance of our strategy to our benchmarks
using various tables and charts.

``` r
perf <- combined5 %>% subset(select = c(Portfolio.Multiple, Benchmark.AvgClROC, Benchmark.WeightedClROC))
colnames(perf) <- c("Google Trends-based Strategy", "Benchmark", "Weighted Benchmark")
perf <- xts(perf, order.by = as.Date(rownames(perf)))

table.DownsideRisk(perf)
```

    ##                              Google Trends-based Strategy Benchmark
    ## Semi Deviation                                     0.1207    0.0513
    ## Gain Deviation                                     0.2063    0.0535
    ## Loss Deviation                                     0.1432    0.0421
    ## Downside Deviation (MAR=10%)                       0.1062    0.0408
    ## Downside Deviation (Rf=0%)                         0.1032    0.0369
    ## Downside Deviation (0%)                            0.1032    0.0369
    ## Maximum Drawdown                                   0.8406    0.3566
    ## Historical VaR (95%)                              -0.1599   -0.0848
    ## Historical ES (95%)                               -0.3698   -0.1224
    ## Modified VaR (95%)                                -0.0256   -0.0896
    ## Modified ES (95%)                                 -0.4571   -0.1222
    ##                              Weighted Benchmark
    ## Semi Deviation                           0.0520
    ## Gain Deviation                           0.0541
    ## Loss Deviation                           0.0453
    ## Downside Deviation (MAR=10%)             0.0416
    ## Downside Deviation (Rf=0%)               0.0379
    ## Downside Deviation (0%)                  0.0379
    ## Maximum Drawdown                         0.3561
    ## Historical VaR (95%)                    -0.0806
    ## Historical ES (95%)                     -0.1296
    ## Modified VaR (95%)                      -0.0900
    ## Modified ES (95%)                       -0.1296

``` r
table.Drawdowns(perf)
```

    ## Warning in table.Drawdowns(perf): Only 2 available in the data.

    ##         From     Trough         To   Depth Length To Trough Recovery
    ## 1 2011-03-01 2012-10-01 2019-12-01 -0.8406    106        20       86
    ## 2 2020-09-01 2020-10-01 2020-12-01 -0.1190      4         2        2

``` r
table.Stats(perf)
```

    ##                 Google Trends-based Strategy Benchmark Weighted Benchmark
    ## Observations                        119.0000  119.0000           119.0000
    ## NAs                                   0.0000    0.0000             0.0000
    ## Minimum                              -0.8285   -0.1905            -0.2169
    ## Quartile 1                           -0.0332   -0.0146            -0.0153
    ## Median                                0.0311    0.0260             0.0252
    ## Arithmetic Mean                       0.0439    0.0282             0.0293
    ## Geometric Mean                        0.0216    0.0255             0.0266
    ## Quartile 3                            0.0893    0.0727             0.0712
    ## Maximum                               1.5160    0.2667             0.2502
    ## SE Mean                               0.0196    0.0068             0.0069
    ## LCL Mean (0.95)                       0.0051    0.0147             0.0157
    ## UCL Mean (0.95)                       0.0827    0.0417             0.0430
    ## Variance                              0.0457    0.0055             0.0056
    ## Stdev                                 0.2139    0.0745             0.0751
    ## Skewness                              2.6707    0.1567             0.0947
    ## Kurtosis                             21.0889    0.5699             1.0869

``` r
charts.PerformanceSummary(perf)
```

![](backtest_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
chart.RelativePerformance(perf[,1], perf[,2])
```

![](backtest_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
chart.RiskReturnScatter(perf, add.sharpe = c(1), xlim = c(0.1, 1.1))
```

![](backtest_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->
