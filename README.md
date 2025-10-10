# Melbourne Vibe Finder - 墨尔本氛围探索器

一个基于R Shiny的交互式数据可视化应用，帮助游客发现墨尔本的有趣地点和规划个性化行程。

## 功能特色

### 🎭 氛围标签系统
- **文艺午后**: 博物馆、美术馆、剧场 + 附近咖啡馆
- **美食探索**: 餐厅咖啡馆 + 便利设施保障
- **历史漫步**: 历史建筑、纪念馆、文化场所
- **夜生活热点**: 酒吧夜店 + 高容量场所
- **亲子友好**: 适合家庭的地点 + 便利设施

### 🗺️ 交互式地图
- Leaflet地图显示所有POI
- 实时热力图显示人流密度
- 点击标记查看详细信息
- 拖拽式行程规划

### 📊 数据分析
- 基于真实墨尔本开放数据
- 智能氛围标签自动分类
- 街区便利性评分
- 步行距离和时间估算

## 数据来源

应用使用以下墨尔本开放数据集：

1. **cafes-and-restaurants-with-seating-capacity.csv** - 咖啡馆和餐厅数据
2. **bars-and-pubs-with-patron-capacity.csv** - 酒吧和夜店数据  
3. **landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.csv** - 地标和兴趣点
4. **venues-for-event-bookings.csv** - 活动场馆
5. **public-toilets.csv** - 公共厕所
6. **drinking-fountains.csv** - 饮水点
7. **street-furniture-including-bollards-bicycle-rails-bins-drinking-fountains-horse-.csv** - 街道家具
8. **on-street-parking-bay-sensors.csv** - 停车位传感器
9. **pedestrian-counting-system-past-hour-counts-per-minute.csv** - 行人计数数据

## 安装和运行

### 系统要求
- R 4.0+
- 以下R包：
  - shiny
  - shinyjs
  - dplyr
  - readr
  - stringr
  - tidyr
  - lubridate
  - sf
  - leaflet
  - leaflet.extras
  - htmltools
  - FNN
  - geosphere
  - ggplot2
  - plotly

### 安装依赖
```r
install.packages(c("shiny", "shinyjs", "dplyr", "readr", "stringr", "tidyr", 
                   "lubridate", "sf", "leaflet", "leaflet.extras", "htmltools", 
                   "FNN", "geosphere", "ggplot2", "plotly"))
```

### 运行应用
```r
# 确保所有CSV文件在项目目录中
# 然后运行：
shiny::runApp("app.R")
```

## 使用指南

### 1. 选择氛围标签
在左侧面板选择你感兴趣的氛围类型，可以多选。

### 2. 设置时间偏好
使用时间滑块选择计划游玩的时间段，应用会显示该时段的人流热力图。

### 3. 添加便利设施要求
勾选"需要附近有饮水点/公厕"来确保行程的便利性。

### 4. 规划行程
- 点击地图上的标记查看详细信息
- 点击"添加到行程"按钮将地点加入行程
- 使用"优化路线"按钮自动优化访问顺序
- 查看右侧的行程列表和统计信息

### 5. 查看分析
底部的Tableau嵌入区域显示选中地点的周边设施分析（需要替换为实际的Tableau Public链接）。

## 技术架构

### 前端 (UI)
- **Shiny UI**: 响应式界面设计
- **Leaflet**: 交互式地图可视化
- **自定义CSS**: 现代化UI样式
- **Tableau嵌入**: 高级数据分析图表

### 后端 (Server)
- **数据清洗**: 统一不同数据源的格式
- **空间分析**: 基于FNN的近邻搜索
- **氛围规则**: 可配置的标签分类算法
- **路线优化**: 简单的TSP近似算法

### 数据流程
1. 读取CSV文件并统一格式
2. 应用氛围标签规则
3. 计算空间关系和密度
4. 生成交互式地图
5. 响应用户交互更新显示

## 扩展功能

### 已实现
- ✅ 基础POI显示和过滤
- ✅ 氛围标签自动分类
- ✅ 交互式地图和热力图
- ✅ 行程规划和路线显示
- ✅ 步行距离和时间估算
- ✅ 街区便利性评分

### 可扩展
- 🔄 实时交通数据集成
- 🔄 用户评价和推荐系统
- 🔄 社交媒体数据集成
- 🔄 多语言支持
- 🔄 移动端优化

## 项目结构

```
Assignment3/
├── app.R                    # 主应用文件
├── README.md               # 项目说明
├── cafes-and-restaurants-with-seating-capacity.csv
├── bars-and-pubs-with-patron-capacity.csv
├── landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.csv
├── venues-for-event-bookings.csv
├── public-toilets.csv
├── drinking-fountains.csv
├── street-furniture-including-bollards-bicycle-rails-bins-drinking-fountains-horse-.csv
├── on-street-parking-bay-sensors.csv
└── pedestrian-counting-system-past-hour-counts-per-minute.csv
```

## 贡献

这个项目是GEOM90007可视化课程的Assignment 3作业。

## 许可证

本项目仅用于学术目的。
