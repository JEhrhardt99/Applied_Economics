#%%%%%%%% This is the textbook code example %%%%%%%%

od <- causaldata::organ_donations

# Treatment variable
od <- od %>% mutate(California = State == 'California')

# Interact quarter with being in the treated group using
# the fixest i() function, which also lets us specify
# a reference period (using the numeric version of Quarter)
clfe <- feols(Rate ~ i(Quarter_Num, California, ref = 3) | 
                State + Quarter_Num, data = od)

# And use coefplot() for a graph of effects
coefplot(clfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%% Some Testing with function i() %%%%%%%%%%%%%%%%%%%%%%%%
test1 <- feols(avg_diesel ~ dummy_GER:dummy_FTD |
                 Station + date_only,
               data = df_period)


test2 <- feols(avg_diesel ~ i(dummy_GER, dummy_FTD, ref = 0) |
                 Station + date_only,
               data = df_period)


test_list <- list()
test_list[["test1"]] <- test1
test_list[["test2"]] <- test2

modelsummary(test_list,
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
             gof_omit = "^(?!R2|Num)")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%