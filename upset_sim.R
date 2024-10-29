library(tidyverse)

s = replicate(10^5, sample(letters, 3, replace = T) %>% str_flatten())
v = str_subset(s, "[aeiou]")
# a = sample(letters, 10)
# a %>% str_flatten()
x = str_subset(s, "[qvbrugmcyx]")

N_s = length(s)
N_v = length(v)
N_x = length(x)
N_sv = length(intersect(s,v))
N_sx = length(intersect(s,x))
N_vx = length(intersect(v,x))
N_sxv = length(intersect(intersect(s,v),x))


