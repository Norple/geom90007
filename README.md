# Melbourne Vibe Finder - 墨尔本氛围探索器

## 📁 项目结构

```
Assignment3/
├── app.R                    # 主应用程序文件
├── Assignment3.Rproj        # R项目文件
└── 数据文件/
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

## 🚀 快速启动

1. **安装依赖包**（如果尚未安装）：
   ```r
   install.packages(c("shiny", "shinyjs", "dplyr", "readr", "stringr", "tidyr", 
                      "lubridate", "sf", "leaflet", "leaflet.extras", "htmltools", 
                      "FNN", "geosphere", "ggplot2", "plotly"))
   ```

2. **运行应用程序**：
   ```r
   shiny::runApp("app.R", port = 3838)
   ```

3. **访问应用**：
   打开浏览器访问 http://127.0.0.1:3838

## 🎯 功能特性

- **🗺️ 区域探索**: 选择墨尔本不同区域进行探索
- **🎭 氛围标签**: 文艺午后、美食探索、历史漫步、夜生活热点、亲子友好
- **⏰ 时间规划**: 选择计划时段
- **🏪 便利设施**: 筛选有便利设施的POI
- **📊 数据分析**: 实时图表和Tableau集成
- **📍 行程规划**: 添加地点到行程并优化路线

## 📊 数据说明

- **POI数据**: 包含咖啡馆、酒吧、地标、场馆等302个兴趣点
- **氛围标签**: 基于位置和设施特征自动生成
- **行人数据**: 用于人流分析和热力图显示
- **便利设施**: 公厕、饮水点、街道家具等

## 🔧 技术栈

- **R Shiny**: 交互式Web应用框架
- **Leaflet**: 交互式地图可视化
- **Plotly**: 动态图表
- **SF**: 空间数据处理
- **Dplyr**: 数据操作
- **Tableau**: 高级数据分析集成
