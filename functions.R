# ____ ____ ____ ____ ____ ____ 
# ||W |||O |||N |||T |||O |||N ||
# ||__|||__|||__|||__|||__|||__||
# |/__\|/__\|/__\|/__\|/__\|/__\|
# 2020 June 26, v1, @馄饨
# 跑团数据可视化工具
# Contact：qq 2558045098
# 使用前请确保
# 1. csv文件和functions.R文件在同一目录下
# 2. csv文件变量名同示例一致
# 3. 在此界面导航栏Session -> Set Working Directory -> To Source File Location

df <- read.csv("loulan.csv", stringsAsFactors = FALSE) # 将loulan.csv替换为csv文件名
# 如果是第一次运行本程序，运行这两行（之后不用了）
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
  ggplot(data = player_df) +
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
  ggplot(data = player_df) +
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
  ggplot(data = player_df) +
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
  ggplot(data = player_df) +
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
              "success" = length(dice[result <= 0]),
              "rate" = round(length(dice[result <= 0]) / length(result), 2)
    ) %>%
    arrange(desc(sum)) %>%
    head(9)
  
  # Failure Rate 计算使用率前九的技能成功率
  ggplot(data = skill_df) +
    geom_bar(mapping = aes(x = type, y = rate, 
                           fill = type), stat = "identity") +
    ggtitle("技能成功率") +
    labs(y = "成功率", x = "", fill = "技能") +
    mynamestheme +
    scale_fill_brewer(palette="YlGnBu") +
    coord_flip()

# v1.2 新增
# Single Player Report
  name <- "伟大的天父和救主克苏鲁" # 更换为意向角色名
  
  single_player_df <- df %>%
    filter(character == name) %>%
    mutate(result = dice - skill) %>%
    summarise("sum" = length(result),
              "success" = length(dice[result <= 0]),
              "rate" = round(length(dice[result <= 0]) / length(result), 4) * 100,
              "extremeF" = length(dice[dice >= 95]),
              "extremeS" = length(dice[dice <= 5])
    )
  
  single_player_skill_df <- df %>%
    filter(character == name) %>%
    mutate(result = dice - skill) %>%
    group_by(type) %>%
    summarise("sum" = length(result),
              "success_rate" = round(length(dice[result <= 0]) / length(result), 4) * 100) %>%
    arrange(desc(sum)) %>%
    head(3) # 使用频率最高的N个技能
  
  skill_helper <- function(df) {
    ret = ""
    for(a in 1:length(df$type)) {
      ret = paste0(ret, df$type[[a]], "(使用", df$sum[[a]], "次，成功率",
                   df$success_rate[[a]], "%), ")
    }
    return(ret)
  }
  ret <- skill_helper(single_player_skill_df)
    
  # 玩家掷骰总结
  print(paste0("玩家", name, "在本次事件中进行了", single_player_df$sum, "次检定，成功"
        , single_player_df$success, "次，成功率为", single_player_df$rate,
          "%。其中大失败", single_player_df$extremeF, "次，大成功", single_player_df$extremeS,
        "次。使用频率最高的", length(single_player_skill_df$type), "个技能为", ret, "最终结局为____。"))


