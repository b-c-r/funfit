
## The Functional Response model code written in cpp as simple string:
FR <- 'dxdt[0] = -Fmax * pow(x[0],1+q) / (pow(N0,1+q) + pow(x[0],1+q)) * P;'

## Automatic creation of the cpp source code using odeintr:
fr_code <- odeintr::compile_sys("FR", FR,
                                 pars = c("Fmax","N0","q","P"),
                                 method = "rk54",
                                 compile = F)

## Save the cpp code for further documentation:
cat(fr_code, file = "srcdev/odeintr_fr_code_raw.cpp")
