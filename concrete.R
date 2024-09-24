concrete_ln <- concrete %>%
+     + mutate(ln_Blast.Furnace.Slag=log(Blast.Furnace.Slag),
+              + ln_Fly.Ash=log(Fly.Ash)
