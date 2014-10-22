DCFValuation <- function(CashFlows, NumPeriods, DiscountFactor, CapRate,
                              TransCostsMult) {
        
        #Define variables
        a <- CashFlows #Vector of annual cash flows
        b <- seq(1, NumPeriods) #Number of periods (must equal length(CashFlows))
        c <- DiscountFactor #Discount factor
        d <- CapRate #Reversionary captalization rate
        e <- TransCostsMult #Sales transaction costs multiplier
        
        #Calculate discount factor vector for discrete compounding
        DiscountFactorVector <- (1 - c)^b
        
        #Calculate discounted cash flows
        DiscountedCashFlows <- a * DiscountFactorVector #Vector of discounted                 
                                                                #cash flows
        
        #Calculate total sales proceeds
        TotalSalesProceeds <- CashFlows[NumPeriods] / d
        
        #Calculate transaction costs
        TransCosts <- (CashFlows[NumPeriods] / d) * e
        
        #Calculate net sales proceeds
        NetSalesProceeds <- sum(TotalSalesProceeds, -TransCosts)
        
        #Calculate valuation
        Valuation <- sum(DiscountedCashFlows[1:(NumPeriods - 2)]) + 
                sum(CashFlows[NumPeriods-1], NetSalesProceeds)/
                (1 + c)^(NumPeriods - 1)        
        
        #Print desired output to CLI              
        print(c("The estimated market value is:", round(Valuation)), quote = 
                FALSE)
        
        print("", quote = FALSE)
        
        
        print("The discounted cash flows are:", quote = FALSE)
        
        print(matrix(round(DiscountedCashFlows), dimnames = list(paste(rep("Period:",
                times = NumPeriods), seq(1, NumPeriods)), "DCF")))
        
        print("", quote = FALSE)
        
        print(paste("The net sales proceeds in period", tail(NumPeriods), "are:",
                round(NetSalesProceeds)), quote = FALSE)
                
}