# Melbourne Vibe Finder - 墨尔本氛围探索器
# A3 Assignment: Interactive Data Visualization

library(shiny)
library(shinyjs)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(FNN)
library(geosphere)
library(ggplot2)
library(plotly)

# ---------- 工具函数：鲁棒获取经纬度 ----------
pick_lonlat <- function(df) {
  nms <- names(df)
  # 常见列名
  lon_candidates <- c("lon","longitude","x","X","LONGITUDE","LONG","Long","Easting","easting")
  lat_candidates <- c("lat","latitude","y","Y","LATITUDE","LAT","Lat","Northing","northing")
  lon_col <- lon_candidates[lon_candidates %in% nms][1]
  lat_col <- lat_candidates[lat_candidates %in% nms][1]
  validate <- function(x) is.numeric(x) && all(is.finite(x))
  if (is.na(lon_col) || is.na(lat_col)) return(NULL)
  lon <- suppressWarnings(as.numeric(df[[lon_col]]))
  lat <- suppressWarnings(as.numeric(df[[lat_col]]))
  if (!validate(lon) || !validate(lat)) return(NULL)
  tibble(lon = lon, lat = lat)
}

# ---------- 安全读取数据函数 ----------
safe_read <- function(path, type_tag, subtype_tag = NULL) {
  df <- suppressMessages(suppressWarnings(readr::read_csv(path, show_col_types = FALSE)))
  ll <- pick_lonlat(df)
  if (is.null(ll)) return(NULL)
  
  # 尝试获取名称字段
  name_cols <- c("name", "Name", "TITLE", "title", "place_name", "Location Name", 
                 "Trading name", "Feature Name", "full_name", "Description")
  nm <- NULL
  for (col in name_cols) {
    if (col %in% names(df)) {
      nm <- df[[col]]
      break
    }
  }
  
  # 获取容量信息
  capacity_cols <- c("Number of seats", "Number of patrons", "Seating type")
  capacity <- NULL
  for (col in capacity_cols) {
    if (col %in% names(df)) {
      capacity <- df[[col]]
      break
    }
  }
  
  result <- tibble(
    id = coalesce(df$id, df$ID, df$`OBJECTID`, df$prop_id, df$KerbsideID, 1:nrow(df)),
    name = as.character(coalesce(nm, paste0(type_tag, " #", 1:nrow(df)))),
    type = type_tag,
    subtype = subtype_tag %||% "",
    capacity = capacity,
    lon = ll$lon, 
    lat = ll$lat
  ) |> filter(is.finite(lon), is.finite(lat))
  
  # 重新分配ID以确保唯一性
  result$id <- 1:nrow(result)
  result
}

# ---------- 读取所有数据 ----------
cat("正在加载数据...\n")

# 基础POI数据
cafes <- safe_read("cafes-and-restaurants-with-seating-capacity.csv", "cafe_restaurant", "cafe")
pubs <- safe_read("bars-and-pubs-with-patron-capacity.csv", "bar_pub", "bar")
landmarks <- safe_read("landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.csv", "landmark", "cultural")
venues <- safe_read("venues-for-event-bookings.csv", "venue", "event")

# 便利设施
toilets <- safe_read("public-toilets.csv", "amenity", "toilet")
fountains <- safe_read("drinking-fountains.csv", "amenity", "fountain")

# 街道家具（用于计算街区便利性）
street_furniture <- safe_read("street-furniture-including-bollards-bicycle-rails-bins-drinking-fountains-horse-.csv", "street_furniture", "furniture")

# 停车数据
parking <- safe_read("on-street-parking-bay-sensors.csv", "parking", "bay")

# 行人计数数据
pedestrian_data <- tryCatch({
  readr::read_csv("pedestrian-counting-system-past-hour-counts-per-minute.csv", show_col_types = FALSE) |>
    rename_with(~str_to_lower(.x)) |>
    mutate(
      ts = ymd_hms(sensing_datetime, quiet = TRUE),
      hour = floor_date(ts, "hour"),
      count = total_of_directions
    ) |>
    filter(!is.na(hour), !is.na(count)) |>
    group_by(sensor_id = location_id, hour) |>
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    filter(count > 0)
}, error = function(e) {
  cat("行人计数数据加载失败:", e$message, "\n")
  NULL
})

# 合并所有POI数据
poi <- bind_rows(cafes, pubs, landmarks, venues, toilets, fountains, street_furniture, parking) |> 
  distinct(id, .keep_all = TRUE) |>
  filter(!is.na(lon), !is.na(lat))

# 转换为sf对象用于空间计算
poi_sf <- st_as_sf(poi, coords = c("lon","lat"), crs = 4326)

cat("数据加载完成，共", nrow(poi), "个POI\n")

# ---------- 氛围标签规则 ----------
as_mat <- function(sfobj) sf::st_coordinates(sfobj)

# 近邻搜索函数
has_nearby <- function(src_sf, tgt_sf, radius_m = 300) {
  if (nrow(src_sf) == 0 || nrow(tgt_sf) == 0) return(rep(FALSE, nrow(src_sf)))
  # 粗略：经纬度近似换算（墨尔本一度约111km）
  km_per_deg <- 111
  src <- as_mat(st_transform(src_sf, 4326)) / km_per_deg
  tgt <- as_mat(st_transform(tgt_sf, 4326)) / km_per_deg
  nn <- FNN::get.knnx(tgt, src, k = 1)
  (nn$nn.dist * 1000 * km_per_deg) <= radius_m
}

# 计算密度
calculate_density <- function(src_sf, tgt_sf, radius_m = 500) {
  if (nrow(src_sf) == 0 || nrow(tgt_sf) == 0) return(rep(0, nrow(src_sf)))
  km_per_deg <- 111
  src <- as_mat(st_transform(src_sf, 4326)) / km_per_deg
  tgt <- as_mat(st_transform(tgt_sf, 4326)) / km_per_deg
  nn <- FNN::get.knnx(tgt, src, k = min(10, nrow(tgt_sf)))
  distances <- nn$nn.dist * 1000 * km_per_deg
  rowSums(distances <= radius_m)
}

# 应用氛围标签规则
is_cafe <- poi$type == "cafe_restaurant"
is_pub <- poi$type == "bar_pub"
is_landmark <- poi$type == "landmark"
is_toilet <- poi$subtype == "toilet"
is_fountain <- poi$subtype == "fountain"
is_furniture <- poi$type == "street_furniture"

# 初始化氛围标签列
poi$vibe_artistic <- FALSE
poi$vibe_foodie <- FALSE
poi$vibe_historical <- FALSE
poi$vibe_nightlife <- FALSE
poi$vibe_family <- FALSE
poi$street_density <- 0

# 文艺午后：地标 + 附近有咖啡馆
if (sum(is_landmark) > 0 && sum(is_cafe) > 0) {
  artistic_result <- has_nearby(poi_sf[is_landmark,], poi_sf[is_cafe,], 300)
  poi$vibe_artistic[is_landmark] <- artistic_result
}

# 美食探索：咖啡馆 + 附近有便利设施
if (sum(is_cafe) > 0) {
  near_toilet <- if (sum(is_toilet) > 0) has_nearby(poi_sf[is_cafe,], poi_sf[is_toilet,], 500) else rep(FALSE, sum(is_cafe))
  near_fountain <- if (sum(is_fountain) > 0) has_nearby(poi_sf[is_cafe,], poi_sf[is_fountain,], 500) else rep(FALSE, sum(is_cafe))
  poi$vibe_foodie[is_cafe] <- near_toilet | near_fountain
}

# 历史漫步：地标 + 包含历史关键词
if (sum(is_landmark) > 0) {
  poi$vibe_historical[is_landmark] <- str_detect(str_to_lower(poi$name[is_landmark]), 
                                                 "museum|gallery|heritage|memorial|historic|art")
}

# 夜生活热点：酒吧 + 高容量
if (sum(is_pub) > 0) {
  capacity_numeric <- suppressWarnings(as.numeric(poi$capacity[is_pub]))
  poi$vibe_nightlife[is_pub] <- !is.na(capacity_numeric) & capacity_numeric > 50
}

# 亲子友好：地标 + 附近有便利设施
if (sum(is_landmark) > 0) {
  near_toilet <- if (sum(is_toilet) > 0) has_nearby(poi_sf[is_landmark,], poi_sf[is_toilet,], 400) else rep(FALSE, sum(is_landmark))
  near_fountain <- if (sum(is_fountain) > 0) has_nearby(poi_sf[is_landmark,], poi_sf[is_fountain,], 400) else rep(FALSE, sum(is_landmark))
  poi$vibe_family[is_landmark] <- near_toilet | near_fountain
}

# 计算街道家具密度（用于街区便利性评分）
if (sum(is_furniture) > 0) {
  poi$street_density <- calculate_density(poi_sf, poi_sf[is_furniture,], 300)
}

# 氛围标签定义
vibe_cols <- c("vibe_artistic", "vibe_foodie", "vibe_historical", "vibe_nightlife", "vibe_family")
vibe_labels <- c("文艺午后", "美食探索", "历史漫步", "夜生活热点", "亲子友好")
names(vibe_labels) <- vibe_cols

# ---------- UI界面 ----------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Melbourne Vibe Finder - 墨尔本氛围探索器"),
  
  # 自定义CSS
  tags$head(
    tags$style(HTML("
      .vibe-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 15px;
        border-radius: 10px;
        margin: 10px 0;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .info-card {
        background: #f8f9fa;
        padding: 15px;
        border-radius: 8px;
        border-left: 4px solid #007bff;
        margin: 10px 0;
      }
      .route-item {
        background: #e3f2fd;
        padding: 10px;
        margin: 5px 0;
        border-radius: 5px;
        border-left: 3px solid #2196f3;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # 氛围选择
      h4("🎭 选择氛围标签", style = "color: #2c3e50;"),
      checkboxGroupInput("vibes", "", 
                        choices = vibe_labels, 
                        selected = c("vibe_artistic", "vibe_foodie"),
                        width = "100%"),
      
      # 时间选择
      h4("⏰ 计划时段", style = "color: #2c3e50; margin-top: 20px;"),
      sliderInput("hour", "", 
                 min = 6, max = 23, value = 14, step = 1,
                 ticks = TRUE),
      
      # 便利设施要求
      h4("🏪 便利设施要求", style = "color: #2c3e50; margin-top: 20px;"),
      checkboxInput("need_facilities", "需要附近有饮水点/公厕", value = FALSE),
      checkboxInput("avoid_crowd", "避开拥挤时段", value = FALSE),
      
      # 距离限制
      h4("🚶 步行距离", style = "color: #2c3e50; margin-top: 20px;"),
      sliderInput("max_distance", "最大步行距离 (米)", 
                 min = 200, max = 1000, value = 500, step = 100),
      
      # 控制按钮
      actionButton("clear_route", "🗑️ 清空行程", 
                  class = "btn-warning", style = "width: 100%; margin-top: 10px;"),
      actionButton("optimize_route", "🎯 优化路线", 
                  class = "btn-success", style = "width: 100%; margin-top: 5px;")
    ),
    
    mainPanel(
      width = 9,
      
      # 地图区域
      div(style = "height: 500px; border-radius: 10px; overflow: hidden; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          leafletOutput("map", height = "100%")
      ),
      
      # 信息面板
      fluidRow(
        column(6,
          h4("📍 行程列表", style = "color: #2c3e50; margin-top: 20px;"),
          div(id = "route_list", style = "max-height: 300px; overflow-y: auto;")
        ),
        column(6,
          h4("📊 地点详情", style = "color: #2c3e50; margin-top: 20px;"),
          div(id = "poi_details", class = "info-card", 
              "点击地图上的标记查看详细信息")
        )
      ),
      
      # 数据分析区域
      h4("📈 周边设施分析", style = "color: #2c3e50; margin-top: 20px;"),
      div(style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
          plotlyOutput("facility_chart", height = "400px"),
          p("💡 显示选中地点的周边设施分布情况", 
            style = "text-align: center; color: #6c757d; margin-top: 10px;")
      )
    )
  )
)

# ---------- 服务器逻辑 ----------
server <- function(input, output, session) {
  
  # 过滤POI数据
  filtered_poi <- reactive({
    req(input$vibes)
    dat <- poi
    
    # 根据选择的氛围标签过滤
    if (length(input$vibes) > 0) {
      keep <- Reduce("|", lapply(input$vibes, function(v) {
        if (v %in% names(dat)) {
          dat[[v]]
        } else {
          rep(FALSE, nrow(dat))
        }
      }))
      if (length(keep) > 0) {
        dat <- dat[keep,]
      }
    }
    
    # 便利设施要求
    if (isTRUE(input$need_facilities)) {
      near_toilet <- has_nearby(st_as_sf(dat, coords = c("lon","lat"), crs = 4326), 
                               poi_sf[is_toilet,], 400)
      near_fountain <- has_nearby(st_as_sf(dat, coords = c("lon","lat"), crs = 4326), 
                                 poi_sf[is_fountain,], 400)
      dat <- dat[near_toilet | near_fountain,]
    }
    
    # 避开拥挤时段（如果有行人数据）
    if (isTRUE(input$avoid_crowd) && !is.null(pedestrian_data)) {
      # 这里可以添加基于行人数据的过滤逻辑
      # 暂时跳过，因为需要传感器位置数据
    }
    
    dat
  })
  
  # 生成热力图数据
  heatmap_data <- reactive({
    if (is.null(pedestrian_data)) return(NULL)
    
    # 根据选择的时间过滤行人数据
    target_hour <- input$hour
    ped_filtered <- pedestrian_data |>
      filter(hour(hour) == target_hour) |>
      group_by(sensor_id) |>
      summarise(avg_count = mean(count, na.rm = TRUE), .groups = "drop")
    
    # 这里需要传感器位置数据来生成热力图
    # 暂时使用POI位置作为示例
    if (nrow(ped_filtered) > 0) {
      sample_poi <- poi[sample(nrow(poi), min(50, nrow(poi))),]
      return(sample_poi[, c("lon", "lat")])
    }
    return(NULL)
  })
  
  # 行程管理
  route <- reactiveVal(data.frame(
    name = character(), 
    lon = numeric(), 
    lat = numeric(), 
    type = character(),
    stringsAsFactors = FALSE
  ))
  
  # 清空行程
  observeEvent(input$clear_route, {
    route(data.frame(name = character(), lon = numeric(), lat = numeric(), type = character()))
  })
  
  # 优化路线（简单的距离优化）
  observeEvent(input$optimize_route, {
    r <- route()
    if (nrow(r) <= 2) return()
    
    # 简单的TSP近似算法
    n <- nrow(r)
    if (n > 1) {
      # 计算距离矩阵
      dist_matrix <- matrix(0, n, n)
      for (i in 1:n) {
        for (j in 1:n) {
          if (i != j) {
            dist_matrix[i, j] <- geosphere::distHaversine(
              c(r$lon[i], r$lat[i]), 
              c(r$lon[j], r$lat[j])
            )
          }
        }
      }
      
      # 简单的最近邻算法
      if (n > 2) {
        new_order <- c(1)
        remaining <- 2:n
        
        while (length(remaining) > 0) {
          last <- new_order[length(new_order)]
          next_idx <- remaining[which.min(dist_matrix[last, remaining])]
          new_order <- c(new_order, next_idx)
          remaining <- remaining[remaining != next_idx]
        }
        
        route(r[new_order,])
      }
    }
  })
  
  # 渲染地图
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(144.9631, -37.8136, zoom = 14) |>
      addControl(
        html = "<div style='background: white; padding: 5px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.2);'>
                <strong>Melbourne Vibe Finder</strong><br>
                <small>点击标记添加到行程</small>
                </div>",
        position = "topright"
      )
  })
  
  # 更新地图标记
  observe({
    dat <- filtered_poi()
    if (nrow(dat) == 0) return()
    
    # 创建颜色映射
    type_colors <- c(
      "cafe_restaurant" = "#e74c3c",
      "bar_pub" = "#8e44ad", 
      "landmark" = "#f39c12",
      "venue" = "#2ecc71",
      "amenity" = "#3498db",
      "street_furniture" = "#95a5a6",
      "parking" = "#34495e"
    )
    
    pal <- colorFactor(type_colors, levels = unique(dat$type))
    
    # 创建弹出窗口内容
    popup_content <- paste0(
      "<div style='min-width: 200px;'>",
      "<h4 style='color: #2c3e50; margin: 0 0 10px 0;'>", htmlEscape(dat$name), "</h4>",
      "<p><strong>类型:</strong> ", htmlEscape(dat$type), "</p>",
      if (!is.na(dat$capacity[1])) paste0("<p><strong>容量:</strong> ", dat$capacity, "</p>") else "",
      "<p><strong>氛围标签:</strong><br>",
      paste(ifelse(dat$vibe_artistic, "🎨 文艺午后", ""), 
            ifelse(dat$vibe_foodie, "🍽️ 美食探索", ""),
            ifelse(dat$vibe_historical, "🏛️ 历史漫步", ""),
            ifelse(dat$vibe_nightlife, "🌃 夜生活热点", ""),
            ifelse(dat$vibe_family, "👨‍👩‍👧‍👦 亲子友好", ""), 
            sep = " "),
      "</p>",
      "<button onclick='Shiny.setInputValue(\"add_to_route\", \"", dat$id, "\", {priority: \"event\"})' 
              class='btn btn-primary btn-sm' style='width: 100%; margin-top: 10px;'>
       添加到行程
      </button>",
      "</div>"
    )
    
    leafletProxy("map") |>
      clearMarkers() |>
      clearHeatmap() |>
      addMarkers(
        data = dat, 
        lng = ~lon, lat = ~lat, 
        icon = makeIcon(
          iconUrl = "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjQiIGhlaWdodD0iMjQiIHZpZXdCb3g9IjAgMCAyNCAyNCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPGNpcmNsZSBjeD0iMTIiIGN5PSIxMiIgcj0iMTAiIGZpbGw9IiM0Q0E1RjUiIHN0cm9rZT0iI0ZGRkZGRiIgc3Ryb2tlLXdpZHRoPSIyIi8+Cjwvc3ZnPg==",
          iconWidth = 24, iconHeight = 24
        ),
        popup = popup_content,
        layerId = ~paste0("poi_", id)
      )
    
    # 添加热力图
    heat_data <- heatmap_data()
    if (!is.null(heat_data)) {
      leafletProxy("map") |>
        addHeatmap(
          lng = heat_data$lon, 
          lat = heat_data$lat, 
          blur = 20, 
          max = 0.8, 
          radius = 15,
          gradient = c("blue", "cyan", "lime", "yellow", "red")
        )
    }
  })
  
  # 处理添加到行程
  observeEvent(input$add_to_route, {
    poi_id <- as.numeric(input$add_to_route)
    selected_poi <- poi[poi$id == poi_id, c("name", "lon", "lat", "type")]
    
    if (nrow(selected_poi) > 0) {
      current_route <- route()
      # 避免重复添加
      if (!selected_poi$name %in% current_route$name) {
        route(bind_rows(current_route, selected_poi))
      }
    }
  })
  
  # 渲染行程列表
  observe({
    r <- route()
    
    if (nrow(r) == 0) {
      html("route_list", "<p style='color: #6c757d; text-align: center; padding: 20px;'>尚未添加地点<br>点击地图标记可加入行程</p>")
      return()
    }
    
    # 计算步行距离和时间
    items <- list()
    total_distance <- 0
    
    for (i in seq_len(nrow(r))) {
      if (i > 1) {
        dist_m <- geosphere::distHaversine(
          c(r$lon[i-1], r$lat[i-1]), 
          c(r$lon[i], r$lat[i])
        )
        walk_time <- round(dist_m / 80) # 假设步行速度80米/分钟
        total_distance <- total_distance + dist_m
        
        items[[i]] <- div(
          class = "route-item",
          h5(paste(i, ".", r$name[i]), style = "margin: 0; color: #2c3e50;"),
          p(paste("步行距离:", round(dist_m), "米 (约", walk_time, "分钟)"), 
            style = "margin: 5px 0; color: #6c757d; font-size: 0.9em;"),
          p(paste("类型:", r$type[i]), 
            style = "margin: 0; color: #6c757d; font-size: 0.8em;")
        )
      } else {
        items[[i]] <- div(
          class = "route-item",
          h5(paste(i, ".", r$name[i]), style = "margin: 0; color: #2c3e50;"),
          p(paste("类型:", r$type[i]), 
            style = "margin: 5px 0; color: #6c757d; font-size: 0.8em;")
        )
      }
    }
    
    # 添加总计信息
    if (nrow(r) > 1) {
      total_time <- round(total_distance / 80)
      summary <- div(
        class = "vibe-card",
        h5("📊 行程统计", style = "margin: 0 0 10px 0;"),
        p(paste("总距离:", round(total_distance), "米"), style = "margin: 0;"),
        p(paste("预计步行时间:", total_time, "分钟"), style = "margin: 0;"),
        p(paste("地点数量:", nrow(r), "个"), style = "margin: 0;")
      )
      items <- c(list(summary), items)
    }
    
    # 在地图上绘制路线
    if (nrow(r) > 1) {
      leafletProxy("map") |>
        clearGroup("route") |>
        addPolylines(
          lng = r$lon, 
          lat = r$lat, 
          color = "#e74c3c", 
          weight = 4, 
          opacity = 0.8, 
          group = "route"
        ) |>
        addMarkers(
          lng = r$lon, 
          lat = r$lat,
          label = paste(seq_len(nrow(r)), r$name),
          labelOptions = labelOptions(noHide = TRUE, direction = "top"),
          group = "route"
        )
    }
    
    html("route_list", items)
  })
  
  # 处理地图点击
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id)) return()
    
    if (startsWith(click$id, "poi_")) {
      poi_id <- as.numeric(sub("poi_", "", click$id))
      selected_poi <- poi[poi$id == poi_id,]
      
      if (nrow(selected_poi) > 0) {
        # 显示详细信息
        details <- div(
          class = "info-card",
          h4(selected_poi$name, style = "color: #2c3e50; margin: 0 0 15px 0;"),
          p(paste("📍 类型:", selected_poi$type), style = "margin: 5px 0;"),
          if (!is.na(selected_poi$capacity)) p(paste("👥 容量:", selected_poi$capacity), style = "margin: 5px 0;") else "",
          p(paste("🏷️ 氛围标签:"), style = "margin: 10px 0 5px 0; font-weight: bold;"),
          div(
            if (selected_poi$vibe_artistic) span("🎨 文艺午后 ", style = "background: #e3f2fd; padding: 2px 8px; border-radius: 12px; margin: 2px; display: inline-block;") else "",
            if (selected_poi$vibe_foodie) span("🍽️ 美食探索 ", style = "background: #f3e5f5; padding: 2px 8px; border-radius: 12px; margin: 2px; display: inline-block;") else "",
            if (selected_poi$vibe_historical) span("🏛️ 历史漫步 ", style = "background: #fff3e0; padding: 2px 8px; border-radius: 12px; margin: 2px; display: inline-block;") else "",
            if (selected_poi$vibe_nightlife) span("🌃 夜生活热点 ", style = "background: #fce4ec; padding: 2px 8px; border-radius: 12px; margin: 2px; display: inline-block;") else "",
            if (selected_poi$vibe_family) span("👨‍👩‍👧‍👦 亲子友好 ", style = "background: #e8f5e8; padding: 2px 8px; border-radius: 12px; margin: 2px; display: inline-block;") else ""
          ),
          p(paste("🏪 街区便利性评分:", round(selected_poi$street_density), "/10"), 
            style = "margin: 10px 0 5px 0; color: #6c757d;"),
          actionButton("add_selected_to_route", "➕ 添加到行程", 
                      class = "btn btn-primary", style = "width: 100%; margin-top: 10px;")
        )
        
        html("poi_details", details)
        
        # 自动添加到行程
        current_route <- route()
        if (!selected_poi$name %in% current_route$name) {
          route(bind_rows(current_route, selected_poi[, c("name", "lon", "lat", "type")]))
          showNotification(paste("已添加", selected_poi$name, "到行程"), type = "success")
        } else {
          showNotification("该地点已在行程中", type = "warning")
        }
      }
    }
  })
  
  # 处理添加选中地点到行程
  observeEvent(input$add_selected_to_route, {
    # 这里需要获取当前选中的POI信息
    # 由于UI限制，这里简化处理
    showNotification("请直接点击地图上的标记添加到行程", type = "info")
  })
  
  # 渲染设施分析图表
  output$facility_chart <- renderPlotly({
    # 获取当前过滤的POI数据
    dat <- filtered_poi()
    
    if (nrow(dat) == 0) {
      # 如果没有数据，显示空图表
      p <- plot_ly() |>
        add_annotations(
          text = "请选择氛围标签查看数据",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "#6c757d")
        ) |>
        layout(
          xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
      return(p)
    }
    
    # 统计各类型POI数量
    type_counts <- dat |>
      count(type) |>
      arrange(desc(n))
    
    # 创建柱状图
    p <- plot_ly(
      data = type_counts,
      x = ~type,
      y = ~n,
      type = "bar",
      marker = list(
        color = c("#e74c3c", "#8e44ad", "#f39c12", "#2ecc71", "#3498db", "#95a5a6", "#34495e"),
        line = list(color = "white", width = 1)
      )
    ) |>
    layout(
      title = list(
        text = "POI类型分布",
        font = list(size = 16, color = "#2c3e50")
      ),
      xaxis = list(
        title = "类型",
        tickangle = -45,
        titlefont = list(size = 12, color = "#2c3e50")
      ),
      yaxis = list(
        title = "数量",
        titlefont = list(size = 12, color = "#2c3e50")
      ),
      margin = list(l = 50, r = 50, t = 50, b = 100),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
    
    p
  })
}

# ---------- 启动应用 ----------
shinyApp(ui, server)
