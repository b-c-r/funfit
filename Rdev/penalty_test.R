
library(foreach)

penalty <- 10000
h <- seq(0.9, 3.1, 0.001)
h_low <- 1
h_up <- 3

out <- foreach(i = 1:length(h), .combine = "c") %do% {
  if(h[i] < h_low){
    penalty*(h[i]-h_low)^2
  } else{
    if(h[i] > h_up){
      penalty*(h[i]-h_up)^2
    } else{
      0
    }
  }
}

plot(h, out, type = "l")



h <- 100
n_in <- 0:100
f_max <- 30
n_0 <- 20
n_eat <- f_max * n_in^h / (n_0^h + n_in^h)

plot(n_in, n_eat, type = "l")


