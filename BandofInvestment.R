BandofInvestment <- function(Equity, Return, InterestRate, Amortization) {
        
        #Define variables
        a <- Equity #Percent funded with equity
        b <- Return #Expected rate of return
        c <- (1 - Equity) #Percent funded with debt
        d <- InterestRate #Interest rate on debt
        e <- Amortization #Amortization period of debt

        #Calculate loan constant (cost of capital)
        LoanConstant <- (d / 12) / (1 - (1 / (1 + (d / 12)) ^ (e * 12)))*12
        
        #Calculate weighted average of equity
        EquityWeightedAvg <- a * b
        
        #Calculate weighted average of debt
        DebtWeightedAvg <- c * LoanConstant
        
        #Calculate overall capitalization rate
        CapRate <- EquityWeightedAvg + DebtWeightedAvg 
        
        return(CapRate)
        
}