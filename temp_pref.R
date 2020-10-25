# Redband Temperature Preference


LJ15 <- read_tsv("C:/Users/Jawn/Desktop/Pilot_Data/Little_Jacks_15_TD.txt")

start <- LJ15[1,1] 


LJ15_test <- LJ15 %>% mutate(duration = as.duration((LJ15$Time) - start$Time) / dhours(1)) %>% 
                        rename(pref_temp = `Preferred temperature [?C]`, INCR_temp = `INCR temperature [?C]`, 
                               DECR_temp = `DECR temperature [?C]`) %>% 
                        select(27, 2, 4:6) %>% 
                      mutate(occ_temp = if_else(Zone == "INCR", INCR_temp, 
                        if_else(Zone == "DECR", DECR_temp,
                        rowMeans(select(.,4:5)))))
                      
LJ15_tp <- ggplot(LJ15_test, aes(x = duration, y = occ_temp)) +
  geom_line() +
  labs(title = "Little Jacks 15 Degrees Temperature Preference",
       x = "Duration (Hours)",
       y = "Temperature (C)") +
  theme_classic()

LJ15_tp

UM15 <- read_tsv("C:/Users/Jawn/Desktop/Pilot_Data/Upper_Mann_15_TD.txt")

start <- UM15[1,1]


UM15_test <- UM15 %>% mutate(duration = as.duration((UM15$Time) - start$Time) / dhours(1)) %>% 
  rename(pref_temp = `Preferred temperature [?C]`, INCR_temp = `INCR temperature [?C]`, 
         DECR_temp = `DECR temperature [?C]`) %>% 
  select(27, 2, 4:6) %>% 
  mutate(occ_temp = if_else(Zone == "INCR", INCR_temp, 
                            if_else(Zone == "DECR", DECR_temp,
                                    rowMeans(select(.,4:5)))))

runmed <- data.frame()

for (i in 1:length(UM15_test[1:i,])) {
     m <- as.data.frame(runmed(m$occ_temp, i, endrule = "keep"))
     runningmed <- bind_rows(median)
}

UM15_tp <- ggplot(UM15_test, aes(x = duration, y = pref_temp)) +
  geom_line() +
  labs(title = "Upper Mann 15 Degrees Temperature Preference",
       x = "Duration (Hours)",
       y = "Temperature (C)") +
  theme_classic()

UM15_tp
                       