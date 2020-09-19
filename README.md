# Estimate VaR of Crude Oil  by GARCH

 Estimate risk of WTI and Brent oil and the spillover effect between these two markets 

##Data

Data: WTI and Brent,

Period: 1987/5/20-2019/10/7

Source: U.S. Energy Information Administration

Figure 1: DailyPrices of WTI and Brent from 1987 to 2019

![image-20200919172409304](/Users/chenjiahao/Library/Application Support/typora-user-images/image-20200919172409304.png)

Figure 2: Daily Return of WTI and Brent from 1987 to 2019

![image-20200919172640527](/Users/chenjiahao/Library/Application Support/typora-user-images/image-20200919172640527.png)

## Model:

Assume return of WTI and Brent oil price fit SGED distribution and use ARMA(1,1)+SGED-EGARCH model to get the mean and conditional variance of  return data. Use these information to estimate the VaR of WTI and Brent Oil return.

Figure 3: Up and down side VaR prediction of Brent from 2018 till present

![image-20200919173302484](/Users/chenjiahao/Library/Application Support/typora-user-images/image-20200919173302484.png)

Figure 4: Up and down side VaR prediction of WTI from 2018 till present

![image-20200919173438673](/Users/chenjiahao/Library/Application Support/typora-user-images/image-20200919173438673.png)

