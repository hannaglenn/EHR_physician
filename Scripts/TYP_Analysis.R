library(lfe)


rel.event.reg <- felm(mainhosp_share~ rel_m6+ rel_m5+ rel_m4+ rel_m3+ rel_m2+ rel_0+ rel_p1+
                      rel_p2+ rel_p3+ rel_p4+ rel_p5+ rel_p6 | DocNPI+HospNPI+year, data=Final_Pairs_Variables)

rel.point.est <- as_tibble(rel.event.reg$coefficients, rownames="terms")

