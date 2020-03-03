library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(ggrepel)


df = read.csv(file="E:/PROJECT/R/pokemon/pokemon.csv")
df = tbl_df(df)
colnames(df)[25] <- "classification"
df$capture_rate <- as.numeric(df$capture_rate)
head(df)


df = select(df, name, classification, hp, weight_kg, 
            height_m, speed, attack, defense,
            sp_attack, sp_defense, type1, type2, 
            abilities, generation,is_legendary, 
            capture_rate)
head(df)


speed_attack_legendary <- ggplot(na.omit(df), aes(attack, speed)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (attack > 170 | attack < 50 & speed >150 | speed < 50) & is_legendary == 1 | speed > 145), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")
weight_attack_legendary <- ggplot(na.omit(df), aes(attack, weight_kg)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (attack > 170 | attack < 50 | weight_kg > 650) & (is_legendary == 1)), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")
height_attack_legendary <- ggplot(na.omit(df), aes(attack, height_m)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, ((attack > 170 | attack < 50 | height_m > 7.5) & is_legendary == 1) | height_m > 5 & is_legendary == 0), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")
hp_attack_legendary <- ggplot(na.omit(df), aes(attack, hp)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, ((attack > 170 | hp > 190 | attack < 50) & is_legendary == 1) | hp >160), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50')  + geom_smooth(method = "lm")
grid.arrange(speed_attack_legendary, weight_attack_legendary, height_attack_legendary, hp_attack_legendary, ncol = 2)


speed_defense_legendary <- ggplot(na.omit(df), aes(defense, speed)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (defense > 130 | defense < 50| speed > 140 | speed < 50) & is_legendary == 1), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")
weight_defense_legendary <- ggplot(na.omit(df), aes(defense, weight_kg)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (defense > 160 | defense < 50 | weight_kg > 600) & is_legendary == 1), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")
height_defense_legendary <- ggplot(na.omit(df), aes(defense, height_m)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (defense > 150 | defense < 50 | height_m > 6) & is_legendary == 1 | height_m >5 & is_legendary ==0), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")
hp_defense_legendary <- ggplot(na.omit(df), aes(defense, hp)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, (defense > 150 | defense < 50 | hp > 150) & is_legendary == 1 | hp > 200), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + geom_smooth(method = "lm")
grid.arrange(speed_defense_legendary, weight_defense_legendary, height_defense_legendary, hp_defense_legendary, ncol = 2)


#ggplot(na.omit(df), aes(attack, weight_kg)) + geom_point(aes(color=is_legendary)) + geom_label_repel(data=subset(df, attack < 50 & defense < 50 & is_legendary == 1 ), aes(label=name), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50')


#ggplot(df, aes(attack, defense)) + geom_jitter(aes(col=speed)) + scale_color_gradient(low="blue", high="darkorange") + ggtitle("Defense vs Attack wrt Speed")


df_fight_against <- select(df, type1, against_bug:against_water)
head(df_fight_against)

df_fight_against_g <- group_by(df_fight_against, type1)
df_fight_against_summ <- summarise(df_fight_against_g, 
                                   against_bug = median(against_bug), 
                                   against_dark = median(against_dark), 
                                   against_dragon = median(against_dragon),
                                   against_electric = median(against_electric),
                                   against_fairy = median(against_fairy),
                                   against_fight = median(against_fight),
                                   against_fire = median(against_fire),
                                   against_flying = median(against_flying),
                                   against_ghost = median(against_ghost),
                                   against_grass = median(against_grass),
                                   against_ground = median(against_ground),
                                   against_ice = median(against_ice), 
                                   against_normal = median(against_normal),
                                   against_poison  = median(against_poison),
                                   against_psychic = median(against_psychic),
                                   against_rock = median(against_rock),
                                   against_steel = median(against_steel),
                                   against_water = median(against_water))

df_fight_against_long <- melt(df_fight_against_summ)
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'RdYlBu')), space='Lab')
ggplot(data=df_fight_against_long, aes(type1, variable)) + geom_tile(aes(fill=value)) + scale_fill_gradientn(colours = hm.palette(100)) + coord_equal() + theme(axis.text.x=element_text(angle=90, hjust=0)) + ggtitle("Effectiveness of different types of Pokemon")


