
library("sets")

sets_options("universe", seq(1, 1000, 0.5))
variables <- set(
  Cooler_storage_volume = fuzzy_partition(varnames = c(small = 1, medium = 200, large = 260), sd = 1.0),
  Freezer_storage_volume = fuzzy_partition(varnames = c(small = 1, medium = 90, large = 120), sd = 1.0),
  Temp_keep_in_h_if_power_outage = fuzzy_partition(varnames = c(poor = 0, sufficient = 16, excellent = 23), sd = 2.0),
  Price = fuzzy_partition(varnames = c(low = 0, medium = 1500, high = 3000), sd = 500),
  Rank = fuzzy_partition(varnames = c(poor = 1, mediocre = 2, alright = 3, nice = 4, great = 5), FUN = fuzzy_cone, radius = 0.5)
)

# Fuzzy rules
rules <- set(
  fuzzy_rule(Cooler_storage_volume %is% small && Freezer_storage_volume %is% small && Temp_keep_in_h_if_power_outage %is% poor, Rank %is% poor),
  fuzzy_rule(Cooler_storage_volume %is% small && Freezer_storage_volume %is% small, Rank %is% mediocre),
  fuzzy_rule(Cooler_storage_volume %is% medium && Freezer_storage_volume %is% medium, Rank %is% nice),
  fuzzy_rule(Cooler_storage_volume %is% large && Freezer_storage_volume %is% large, Rank %is% great),
  fuzzy_rule(Price %is% low && Cooler_storage_volume %is% small && Freezer_storage_volume %is% small, Rank %is% alright),
  fuzzy_rule(Price %is% medium && Cooler_storage_volume %is% medium && Freezer_storage_volume %is% medium, Rank %is% nice),
  fuzzy_rule(Price %is% large && Cooler_storage_volume %is% large && Freezer_storage_volume %is% large, Rank %is% great),
  fuzzy_rule(Price %is% high && Temp_keep_in_h_if_power_outage %is% excellent, Rank %is% alright),
  fuzzy_rule(Price %is% low && Temp_keep_in_h_if_power_outage %is% poor, Rank %is% alright),
  fuzzy_rule(Price %is% low && Temp_keep_in_h_if_power_outage %is% excellent, Rank %is% great),
  fuzzy_rule(Price %is% high && Temp_keep_in_h_if_power_outage %is% poor, Rank %is% poor)
)
model <- fuzzy_system(variables, rules)

print(model)
plot(model)
example.1 <- fuzzy_inference(model, list(Cooler_storage_volume = 196, Freezer_storage_volume = 75, Temp_keep_in_h_if_power_outage = 28, Price = 899))
gset_defuzzify(example.1, "centroid")
dev.new()
plot(example.1)