# ____ ____ ____ ____ ____ ____ 
# ||W |||O |||N |||T |||O |||N ||
# ||__|||__|||__|||__|||__|||__||
# |/__\|/__\|/__\|/__\|/__\|/__\|
# 2020 June 26, v1, @馄饨
# 跑团数据可视化工具
# Contact：qq2558045098
# 使用前请确保
# 1. csv文件和functions.R文件在同一目录下
# 2. csv文件变量名同示例一致
# 3. 在此界面导航栏Session -> Set Working Directory -> To Source File Location

df <- read.csv("loulan.csv", stringsAsFactors = FALSE) # 将loulan.csv替换为csv文件名
# 如果是第一次运行本程序，运行这两行（之后就不用了）
# install.packages("dplyr")
# install.packages("ggplot2")

library("dplyr")
library("ggplot2")

mynamestheme <-
  theme(plot.title = element_text(family = "ZCOOL QingKe HuangYou",
                                  face = "bold", size = (20)),
        legend.title = element_text(colour = "steelblue4",
                                    face = "bold.italic",
                                    family = "ZCOOL QingKe HuangYou",
                                    size = (15)),
        legend.text = element_text(face = "italic",
                                   colour = "steelblue4",
                                   family = "ZCOOL QingKe HuangYou"),
        axis.title = element_text(family = "ZCOOL QingKe HuangYou",
                                  size = (20),
                                  colour = "steelblue4"),
        axis.text = element_text(family = "ZCOOL QingKe HuangYou",
                                 colour = "steelblue4",
                                 size = (15)))

# Based on different player
  player_df <- df %>%
    select("character", "dice", "skill") %>%
    filter(character != "") %>%
    mutate(result = dice - skill) %>%
    group_by(character) %>%
    summarise(
      "sum" = length(result),
      "fail" = length(dice[result > 0]),
      "rate" = round(length(dice[result > 0]) / length(result), 2),
      "extremeF" = round(length(dice[dice >= 95]) / length(result), 2),
      "extremeS" = round(length(dice[dice <= 5]) / length(result), 2)
    )
  
  # Failure Rate 计算角色掷骰失败率
  failplot <- ggplot(data = player_df) +
    geom_bar(mapping = aes(x = character, y = rate, 
                           fill = character), stat = "identity") +
    ggtitle("角色掷骰失败率") +
    labs(y = "失败率", x = "角色", fill = "角色") +
    mynamestheme +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.key.height = unit(0.5, "cm")
    ) +
    scale_fill_brewer(palette="YlGnBu")
  
  # Extreme Failure Rate 计算角色掷骰大失败率
  extremeF_plot <- ggplot(data = player_df) +
    geom_bar(mapping = aes(x = character, y = extremeF, 
                           fill = character), stat = "identity") +
    ggtitle("角色掷骰大失败率") +
    labs(y = "失败率", x = "角色", fill = "角色") +
    mynamestheme +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.key.height = unit(0.5, "cm")
    ) +
    scale_fill_brewer(palette="YlGnBu")
  
  # Extreme Success Rate 计算角色掷骰大成功率
  extremeS_plot <- ggplot(data = player_df) +
    geom_bar(mapping = aes(x = character, y = extremeS, 
                           fill = character), stat = "identity") +
    ggtitle("角色掷骰大成功率") +
    labs(y = "成功率", x = "角色", fill = "角色") +
    mynamestheme +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.key.height = unit(0.5, "cm")
    ) +
    scale_fill_brewer(palette="YlGnBu")
  
  # Number of Dices 计算角色掷骰数
  dices_num <- ggplot(data = player_df) +
    geom_bar(mapping = aes(x = character, y = sum, 
                           fill = character), stat = "identity") +
    ggtitle("角色掷骰数") +
    labs(y = "掷骰数", x = "角色", fill = "角色") +
    mynamestheme +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.key.height = unit(0.5, "cm")
    ) +
    scale_fill_brewer(palette="YlGnBu")

# Based on Skill
  skill_df <- df %>%
    select("type", "dice", "skill") %>%
    filter(type != "") %>%
    mutate(result = dice - skill) %>%
    group_by(type) %>%
    summarise("sum" = length(result),
              "fail" = length(dice[result <= 0]),
              "rate" = round(length(dice[result <= 0]) / length(result), 2)
    ) %>%
    arrange(desc(sum)) %>%
    head(9)
  
  # Failure Rate 计算使用率前九的技能成功率
  skillplot <- ggplot(data = skill_df) +
    geom_bar(mapping = aes(x = type, y = rate, 
                           fill = type), stat = "identity") +
    ggtitle("技能成功率") +
    labs(y = "成功率", x = "", fill = "技能") +
    mynamestheme +
    scale_fill_brewer(palette="YlGnBu") +
    coord_flip()

