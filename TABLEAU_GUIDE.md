# 📊 Tableau集成使用指南

## 🎯 概述

墨尔本氛围探索器现在专注于数据过滤和导出功能，将地图可视化交给Tableau来处理。这种架构设计让您能够：

- 在Shiny中设置过滤条件
- 导出精确的数据集到Tableau
- 在Tableau中创建专业的地图可视化

## 🚀 使用流程

### 1. 设置过滤条件
在Shiny应用的左侧面板中：
- **主要POI类型**: 6种核心类型（基于Tableau图例）
  - 🍽️ 咖啡馆/餐厅 (Café/Restaurant)
  - 🍺 酒吧/夜店 (Bar/Nightclub) 
  - 🏛️ 地标 (Landmark)
  - 🎪 活动场馆 (Event Venue)
  - 🚰 饮水点 (Drinking Fountain)
  - 🚻 公厕 (Toilet)
- **氛围标签**: 文艺午后、美食探索、历史漫步、夜生活热点、亲子友好
- **选择探索区域**: 整个墨尔本、CBD、南岸区等
- **设置时间偏好**: 6:00-23:00
- **便利设施要求**: 是否需要附近有饮水点/公厕
- **特殊地点类型**: 饮水点、垃圾桶、自行车架、座椅、护柱等12种类型
- **搜索功能**: 按名称、类型、子类型搜索地点
- **显示选项**: 调整POI数量限制

### 2. Tableau集成方式

#### 方式1: 嵌入Tableau仪表板
- 在左侧输入Tableau仪表板URL
- 点击"🔗 连接Tableau"按钮
- 仪表板将在右侧工作区显示

#### 方式2: 导出数据
- 点击"📥 导出数据到Tableau"按钮
- 下载包含所有过滤条件的CSV文件
- 文件包含：名称、类型、坐标、氛围标签、便利性评分等

### 3. 在Tableau中分析
导入CSV文件后，您可以创建：

#### 🗺️ 地图可视化
- 使用经纬度字段创建符号地图
- 按氛围标签设置不同颜色
- 使用便利性评分调整标记大小
- 添加筛选器进行交互式探索

#### 📊 数据分析图表
- 氛围标签分布饼图
- POI类型统计柱状图
- 区域便利性评分对比
- 时间偏好分析

## 📁 数据字段说明

| 字段名 | 类型 | 说明 |
|--------|------|------|
| name | 文本 | POI名称 |
| type | 文本 | POI类型（cafe_restaurant, bar_pub, landmark等） |
| subtype | 文本 | 子类型 |
| lon | 数值 | 经度 |
| lat | 数值 | 纬度 |
| capacity | 数值 | 容量信息 |
| vibe_artistic | 布尔 | 文艺午后标签 |
| vibe_foodie | 布尔 | 美食探索标签 |
| vibe_historical | 布尔 | 历史漫步标签 |
| vibe_nightlife | 布尔 | 夜生活热点标签 |
| vibe_family | 布尔 | 亲子友好标签 |
| street_density | 数值 | 街区便利性评分 |
| special_place_type | 文本 | 特殊地点类型（饮水点、垃圾桶等） |
| export_timestamp | 日期时间 | 导出时间戳 |
| selected_main_poi_types | 文本 | 选择的主要POI类型 |
| selected_vibes | 文本 | 选择的氛围标签 |
| selected_special_places | 文本 | 选择的特殊地点类型 |
| search_term | 文本 | 搜索关键词 |
| time_preference | 数值 | 时间偏好 |
| explore_region | 文本 | 探索区域 |

## 🎨 Tableau可视化建议

### 地图创建步骤
1. 将`lat`和`lon`字段拖拽到地图
2. 设置地理角色为"纬度"和"经度"
3. 将`type`字段拖拽到"颜色"
4. 将`street_density`字段拖拽到"大小"
5. 添加氛围标签筛选器

### 仪表板设计
- **地图视图**: 主要分析区域
- **筛选器面板**: 氛围标签、区域、时间
- **统计图表**: 支持性分析
- **详细信息**: 点击地图标记显示

## 💡 高级分析技巧

### 计算字段
```tableau
// 氛围标签汇总
IF [vibe_artistic] THEN "文艺午后"
ELSEIF [vibe_foodie] THEN "美食探索"
ELSEIF [vibe_historical] THEN "历史漫步"
ELSEIF [vibe_nightlife] THEN "夜生活热点"
ELSEIF [vibe_family] THEN "亲子友好"
ELSE "其他"
END
```

### 空间分析
- 创建缓冲区分析
- 计算POI密度
- 识别热点区域
- 分析空间聚集模式

## 🔧 技术优势

### Shiny + Tableau 架构
- **Shiny**: 提供灵活的过滤界面和实时数据统计
- **Tableau**: 提供专业的地图可视化和高级分析功能
- **数据流**: 无缝的数据传递和格式转换

### 性能优化
- 减少Shiny应用的复杂度
- 利用Tableau的优化渲染引擎
- 支持大数据集的可视化

## 📈 预期效果

通过这种集成方式，您将获得：
- 更专业的地图可视化效果
- 更强大的数据分析能力
- 更灵活的交互式探索体验
- 更高效的数据处理流程

这种设计完美结合了Shiny的交互性和Tableau的专业性，为墨尔本氛围探索提供了最佳的数据分析解决方案。
