library(ggplot2)
library(tidyverse)
library(tidyr)

file1 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\lanzhou.csv"
file2 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\haerbin.csv"
file3 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\wulumuqi.csv"
file4 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\xian.csv"
file5 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\tianjin.csv"
file6 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\shanghai.csv"
file7 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\guangzhou.csv"
file8 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\chengdu.csv"
file9 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\nanjing.csv"
file10 <- "D:\\CODE\\Rpro\\DataAnalysis\\DataFile\\hangzhou.csv"
lanzhou <- read.csv(file1, encoding = "UTF-8")
haerbin <- read.csv(file2, encoding = "UTF-8")
wulumuqi <- read.csv(file3, encoding = "UTF-8")
xian <- read.csv(file4, encoding = "UTF-8")
tianjin <- read.csv(file5, encoding = "UTF-8")
shanghai <- read.csv(file6, encoding = "UTF-8")
guangzhou <- read.csv(file7, encoding = "UTF-8")
chengdu <- read.csv(file8, encoding = "UTF-8")
nanjing <- read.csv(file9, encoding = "UTF-8")
hangzhou <- read.csv(file10, encoding = "UTF-8")

# 数据预处理
nd <- function(df) {
    rm_df <- df[, -4] # 删除列 AQI排名
    rename_df <- dplyr::rename(rm_df, level = "质量等级", AQI = "AQI指数") # 修改列名
    # 分隔年月日
    new_df <- separate(data = rename_df, col = 日期, into = c("year", "month", "day"), sep = "-") # # nolint
    # 合并月日
    combine_data <- tidyr::unite(new_df, "date", month, day, sep = "-") # nolint

    return(combine_data)
}

# 全部数据
all_data <- function(flag) {
    df <- NULL
    if (flag == "true") {
        df <- rbind(
            nd(lanzhou), nd(haerbin), nd(wulumuqi), nd(xian), nd(tianjin),
            nd(shanghai), nd(guangzhou), nd(chengdu), nd(nanjing), nd(hangzhou)
        )
    } else {
        df <- rbind(
            lanzhou, haerbin, wulumuqi, xian, tianjin,
            shanghai, guangzhou, chengdu, nanjing, hangzhou
        )
    }

    return(df)
}

# 折线图
# line_chart <- function(df, name) {
#     day_count <- data.frame(1:220)$X1.220
#     ggplot(df, aes(x = day_count, y = AQI, group = year, color = year)) +
#         geom_line(size = 1) +
#         labs(title = name)
# }
# line_chart(nd(lanzhou), "兰州")
# line_chart(nd(haerbin), "哈尔滨")
# line_chart(nd(wulumuqi), "乌鲁木齐")
# line_chart(nd(xian), "西安")
# line_chart(nd(tianjin), "天津")
# line_chart(nd(shanghai), "上海")
# line_chart(nd(guangzhou), "广州")
# line_chart(nd(chengdu), "成都")
# line_chart(nd(nanjing), "南京")
# line_chart(nd(hangzhou), "杭州")

# # 柱状图
# histogram <- function(city, name, time) {
#     df <- subset(nd(city), nd(city)$year == time)
#     ggplot(df, aes(x = date, y = AQI, fill = date)) +
#         geom_bar(stat = "identity") +
#         geom_text(aes(label = AQI), vjust = -0.2) +
#         ggtitle(paste(time, "年", name, "春节期间前后AQI指数变化", sep = "")) +
#         theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
#         labs(x = "日期", y = "AQI值")
# }
# histogram(nanjing, "南京", "2018")
# histogram(nanjing, "南京", "2019")
# histogram(nanjing, "南京", "2020")
# histogram(nanjing, "南京", "2021")
# histogram(nanjing, "南京", "2022")
# df <- subset(nd(lanzhou), nd(lanzhou)$year == "2018")
# # cor(df[, 4:10])
# ggplot() +
#     geom_line(df, mapping = aes(x = date, y = PM2.5, group = 1, color = "PM2.5"), size = 1) +
#     geom_line(df, mapping = aes(x = date, y = PM10, group = 1, color = "PM10"), size = 1) +
#     geom_line(df, mapping = aes(x = date, y = AQI, group = 1, color = "AQI"), size = 1) +
#     theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
#     labs(title = "2018年春节期间AQI、PM2.5和PM10参数变化趋势", x = "日期", y = "数值")

# ggplot() +
#     geom_point(df,
#         mapping = aes(AQI, PM10, color = "PM10", shape = "PM10"),
#         size = 4
#     ) +
#     geom_point(df,
#         mapping = aes(AQI, PM2.5, color = "PM2.5", shape = "PM2.5"),
#         size = 4
#     ) +
#     geom_point(df,
#         mapping = aes(AQI, SO2, color = "SO2", shape = "SO2"),
#         size = 1
#     ) +
#     geom_point(df,
#         mapping = aes(AQI, NO2, color = "NO2", shape = "NO2"),
#         size = 1
#     ) +
#     geom_point(df,
#         mapping = aes(AQI, CO, color = "CO", shape = "CO"),
#         size = 1
#     ) +
#     geom_point(df,
#         mapping = aes(AQI, O3, color = "O3", shape = "O3"),
#         size = 1
#     ) +
#     labs(title = "散点图", x = "AQI", y = "其余六项参数")

# # 回归分析
fm <- lm(AQI ~ PM2.5 + PM10 + SO2 + NO2 + CO + O3, data = all_data("true"))
summary(fm)
coef.sd <- function(fm) {
    b <- fm$coef
    si <- apply(fm$model, 2, sd)
    bs <- b[-1] * si[-1] / si[1]
    return(bs)
}

# 箱尾图
# ggplot(all_data("true"), aes(x = level, y = AQI)) +
#     geom_boxplot(outlier.colour = "black", outlier.size = 2, notch = TRUE)

# 取五年数据做折线图
# five_line <- function(y) {
#     lanzhou_df <- subset(nd(lanzhou), nd(lanzhou)$year == y)
#     haerbin_df <- subset(nd(haerbin), nd(haerbin)$year == y)
#     wulumuqi_df <- subset(nd(wulumuqi), nd(wulumuqi)$year == y)
#     xian_df <- subset(nd(xian), nd(xian)$year == y)
#     tianjin_df <- subset(nd(tianjin), nd(tianjin)$year == y)
#     shanghai_df <- subset(nd(shanghai), nd(shanghai)$year == y)
#     guangzhou_df <- subset(nd(guangzhou), nd(guangzhou)$year == y)
#     chengdu_df <- subset(nd(chengdu), nd(chengdu)$year == y)
#     nanjing_df <- subset(nd(nanjing), nd(nanjing)$year == y)
#     hangzhou_df <- subset(nd(hangzhou), nd(hangzhou)$year == y)
#     day_count <- data.frame(1:44)$X1.44
#     ggplot() +
#         geom_line(lanzhou_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "兰州"), size = 1) +
#         geom_line(haerbin_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "哈尔滨"), size = 1) +
#         geom_line(wulumuqi_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "乌鲁木齐"), size = 1) +
#         geom_line(xian_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "西安"), size = 1) +
#         geom_line(tianjin_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "天津"), size = 1) +
#         geom_line(shanghai_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "上海"), size = 1) +
#         geom_line(guangzhou_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "广州"), size = 1) +
#         geom_line(chengdu_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "成都"), size = 1) +
#         geom_line(nanjing_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "南京"), size = 1) +
#         geom_line(hangzhou_df, mapping = aes(x = day_count, y = AQI, group = 1, color = "杭州"), size = 1) +
#         labs(title = paste(y, "年春节期间AQI指数变化趋势", sep = ""))
# }

# five_line(2018)
# five_line(2019)
# five_line(2020)
# five_line(2021)
# five_line(2022)