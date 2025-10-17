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
library(htmltools)
library(FNN)
library(geosphere)

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
    id = coalesce(df$id, df$ID, df$`OBJECTID`, df$prop_id, df$KerbsideID, seq_len(nrow(df))),
    name = as.character(coalesce(nm, paste0(type_tag, " #", seq_len(nrow(df))))),
    type = type_tag,
    subtype = subtype_tag %||% "",
    capacity = capacity,
    lon = ll$lon, 
    lat = ll$lat
  ) |> filter(is.finite(.data$lon), is.finite(.data$lat))
  
  # 重新分配ID以确保唯一性
  result$id <- seq_len(nrow(result))
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

# 为街道家具添加特殊地点类型信息
if (!is.null(street_furniture)) {
  # 读取原始数据获取ASSET_TYPE信息
  street_furniture_raw <- suppressMessages(suppressWarnings(readr::read_csv("street-furniture-including-bollards-bicycle-rails-bins-drinking-fountains-horse-.csv", show_col_types = FALSE)))
  
  # 合并ASSET_TYPE信息
  street_furniture <- street_furniture |>
    mutate(
      special_place_type = if (nrow(street_furniture_raw) >= nrow(street_furniture)) {
        street_furniture_raw$ASSET_TYPE[seq_len(nrow(street_furniture))]
      } else {
        rep("Unknown", nrow(street_furniture))
      }
    )
}

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
    group_by(sensor_id = .data$Location_ID, hour) |>
    summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop") |>
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
  
  # 添加应用描述
  div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
      h3("🗺️ 探索墨尔本全城氛围", style = "margin: 0 0 10px 0;"),
      p("发现墨尔本各区域的独特氛围，从CBD的繁华到圣基尔达的海滨风情。选择您感兴趣的氛围标签，探索整个城市的精彩地点！", 
        style = "margin: 0; font-size: 1.1em;")
  ),
  
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
    ")),
    tags$script(src = "https://public.tableau.com/javascripts/api/tableau-2.min.js"),
    tags$script(src = "tableau_shiny.js")
  ),
  
  fluidRow(
    # 左侧过滤器面板
    column(3,
      div(style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); height: 100vh; overflow-y: auto;",
        
        # 主要POI类型选择（基于Tableau图例）
        h4("📍 Main POI Types", style = "color: #2c3e50; margin-bottom: 15px;"),
        checkboxGroupInput("main_poi_types", "Select Main POI Types", 
                          choices = list(
                            "Cafe/Restaurant" = "cafe_restaurant",
                            "Bar/Nightclub" = "bar_pub", 
                            "Landmark" = "landmark",
                            "Event Venue" = "venue",
                            "Drinking Fountain" = "drinking_fountain",
                            "Toilet" = "toilet"
                          ),
                          selected = c("cafe_restaurant", "bar_pub", "landmark"),
                          width = "100%"),
      
      # 氛围选择
        h4("🎭 氛围标签", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
        checkboxGroupInput("vibes", "选择氛围标签", 
                        choices = vibe_labels, 
                        selected = c("vibe_artistic", "vibe_foodie"),
                        width = "100%"),
      
      # 时间选择
        h4("⏰ 计划时段", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
      sliderInput("hour", "", 
                 min = 6, max = 23, value = 14, step = 1,
                 ticks = TRUE),
      
        # 区域选择
        h4("🗺️ Explore Region", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
        selectInput("explore_region", "Select Explore Region", 
                   choices = list(
                     "Entire Melbourne" = "all",
                     "Melbourne CBD" = "cbd", 
                     "Southbank" = "southbank",
                     "Carlton" = "carlton",
                     "Fitzroy" = "fitzroy",
                     "St Kilda" = "st_kilda"
                   ),
                   selected = "all"),
        
      # 便利设施要求
        h4("🏪 便利设施要求", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
      checkboxInput("need_facilities", "需要附近有饮水点/公厕", value = FALSE),
      checkboxInput("avoid_crowd", "避开拥挤时段", value = FALSE),
      
        
        
        # 显示选项
        h4("👁️ 显示选项", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
        checkboxInput("show_heatmap", "显示人流热力图", value = FALSE),
        checkboxInput("show_density", "显示POI密度", value = TRUE),
        sliderInput("poi_limit", "显示POI数量限制", 
                   min = 50, max = 500, value = 200, step = 50),
        
        # Tableau集成
        h4("📊 Tableau集成", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
        actionButton("connect_tableau", "🔗 连接Tableau仪表板", 
                   class = "btn btn-info", style = "width: 100%; margin-bottom: 10px;"),
        
        # 数据导出按钮
        actionButton("export_data", "📥 导出数据到Tableau", 
                    class = "btn btn-success", style = "width: 100%; margin-bottom: 10px;"),
        actionButton("generate_tableau", "📈 生成Tableau报告", 
                    class = "btn btn-primary", style = "width: 100%; margin-bottom: 10px;"),
        
        # 数据统计
        h4("📊 数据统计", style = "color: #2c3e50; margin-top: 25px; margin-bottom: 15px;"),
        div(id = "data_stats", class = "info-card", 
            "选择过滤器查看数据统计")
      )
    ),
    
    # 右侧Tableau工作区
    column(9,
      div(style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); height: 100vh;",
        
        # Tableau工作区标题
        h3("📊 Tableau数据分析工作区", style = "color: #2c3e50; margin-bottom: 20px; text-align: center;"),
        
        # Tableau仪表板嵌入区域
        div(
          id = "tableau_dashboard", 
          style = "height: 80vh; border: 2px dashed #ddd; border-radius: 10px; background: #f8f9fa; position: relative;",
          div(id = "tableauViz", style = "height: 100%; width: 100%; display: none;"),
          HTML('
            <div id="tableau_placeholder"
                 style="position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center; padding: 20px;">
              <div style="margin-bottom: 30px;">
                <h4 style="color: #6c757d; margin-bottom: 15px;">🔗 Tableau仪表板工作区</h4>
                <p style="color: #6c757d; margin-bottom: 20px; font-size: 1.1em;">
                  首次加载可能需要几秒钟，请稍候…
                </p>
              </div>
              <div style="background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); max-width: 420px;">
                <h5 style="color: #2c3e50; margin-bottom: 15px;">📋 使用说明</h5>
                <div style="text-align: left; color: #6c757d;">
                  <p style="margin: 10px 0;"><strong>自动加载:</strong></p>
                  <ol style="margin: 5px 0 15px 20px;">
                    <li>页面打开后会自动连接Tableau仪表板</li>
                    <li>左侧筛选会通过API自动同步到仪表板</li>
                  </ol>
                  <p style="margin: 10px 0;"><strong>手动刷新:</strong></p>
                  <ol style="margin: 5px 0 15px 20px;">
                    <li>若仪表板未显示，可点击“连接Tableau仪表板”按钮</li>
                    <li>稍等片刻直至仪表板加载完成</li>
                  </ol>
                </div>
              </div>
            </div>'
          )
        ),
        
        # 底部说明
        div(style = "margin-top: 15px; text-align: center;",
            p("💡 提示：导出数据后，您可以在Tableau中创建专业的地图可视化、热力图和交互式仪表板", 
              style = "color: #6c757d; font-style: italic;")
        )
      )
    )
  )
)

# ---------- 服务器逻辑 ----------
server <- function(input, output, session) {
  session$onFlushed(function() {
    session$sendCustomMessage("tableauInit", list(forceReload = FALSE))
  }, once = TRUE)
  
  # 定义墨尔本各区域边界
  region_bounds <- list(
    all = list(lat_min = -37.9, lat_max = -37.7, lon_min = 144.9, lon_max = 145.0),
    cbd = list(lat_min = -37.82, lat_max = -37.80, lon_min = 144.95, lon_max = 144.98),
    southbank = list(lat_min = -37.83, lat_max = -37.81, lon_min = 144.96, lon_max = 144.99),
    carlton = list(lat_min = -37.81, lat_max = -37.79, lon_min = 144.96, lon_max = 144.99),
    fitzroy = list(lat_min = -37.80, lat_max = -37.78, lon_min = 144.97, lon_max = 145.00),
    st_kilda = list(lat_min = -37.87, lat_max = -37.85, lon_min = 144.97, lon_max = 145.00)
  )
  
  
  
  # 过滤POI数据
  filtered_poi <- reactive({
    req(input$vibes)
    
    
    dat <- poi
    
    # 根据选择的区域过滤
    if (input$explore_region != "all") {
      bounds <- region_bounds[[input$explore_region]]
      dat <- dat |>
        filter(
          .data$lat >= bounds$lat_min & .data$lat <= bounds$lat_max &
          .data$lon >= bounds$lon_min & .data$lon <= bounds$lon_max
        )
    }
    
    # 根据主要POI类型过滤
    if (length(input$main_poi_types) > 0) {
      # 应用类型过滤
      if (any(input$main_poi_types %in% c("cafe_restaurant", "bar_pub", "landmark", "venue", "drinking_fountain", "toilet"))) {
        dat <- dat |>
          filter(
            Reduce("|", lapply(input$main_poi_types, function(t) {
              case_when(
                t == "cafe_restaurant" ~ .data$type %in% c("cafe", "restaurant"),
                t == "bar_pub" ~ .data$type %in% c("bar", "pub"),
                t == "landmark" ~ .data$type == "landmark",
                t == "venue" ~ .data$type == "venue",
                t == "drinking_fountain" ~ .data$type == "fountain",
                t == "toilet" ~ .data$type == "toilet",
                TRUE ~ FALSE
              )
            }))
          )
      }
    }
    
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
    
    # 限制显示数量
    if (nrow(dat) > input$poi_limit) {
      # 按便利性评分排序，优先显示评分高的
      dat <- dat |>
        arrange(desc(.data$street_density)) |>
        head(input$poi_limit)
    }
    
    dat
  })
  
  # 数据统计显示
  observe({
    dat <- filtered_poi()
    
    if (nrow(dat) == 0) {
      html("data_stats", "<p style='color: #6c757d; text-align: center; padding: 10px;'>请选择氛围标签查看数据</p>")
      return()
    }
    
    # 计算统计数据
    total_pois <- nrow(dat)
    type_counts <- table(dat$type)
    vibe_counts <- c(
      sum(dat$vibe_artistic, na.rm = TRUE),
      sum(dat$vibe_foodie, na.rm = TRUE),
      sum(dat$vibe_historical, na.rm = TRUE),
      sum(dat$vibe_nightlife, na.rm = TRUE),
      sum(dat$vibe_family, na.rm = TRUE)
    )
    names(vibe_counts) <- c("文艺午后", "美食探索", "历史漫步", "夜生活热点", "亲子友好")
    
    
    # 创建统计显示
    stats_html <- paste0(
      "<div style='padding: 10px;'>",
      "<h5 style='color: #2c3e50; margin-bottom: 10px;'>📊 当前数据统计</h5>",
      "<p style='margin: 5px 0;'><strong>总POI数量:</strong> ", total_pois, "</p>",
      "<p style='margin: 5px 0;'><strong>选择区域:</strong> ", 
      ifelse(input$explore_region == "all", "整个墨尔本", 
             names(region_bounds)[region_bounds == input$explore_region]), "</p>",
      "<p style='margin: 5px 0;'><strong>计划时间:</strong> ", input$hour, ":00</p>",
      "<hr style='margin: 10px 0;'>",
      "<h6 style='color: #34495e; margin-bottom: 5px;'>🏷️ 氛围标签分布:</h6>",
      paste(sapply(names(vibe_counts), function(vibe) {
        if (vibe_counts[vibe] > 0) {
          paste0("<p style='margin: 2px 0; font-size: 0.9em;'>", vibe, ": ", vibe_counts[vibe], "</p>")
        } else ""
      }), collapse = ""),
      "<hr style='margin: 10px 0;'>",
      "<h6 style='color: #34495e; margin-bottom: 5px;'>📍 类型分布:</h6>",
      paste(sapply(names(type_counts), function(type) {
        paste0("<p style='margin: 2px 0; font-size: 0.9em;'>", type, ": ", type_counts[type], "</p>")
      }), collapse = ""),
      "</div>"
    )
    
    html("data_stats", stats_html)
  })
  
  # 连接或重载Tableau仪表板
  observeEvent(input$connect_tableau, {
    session$sendCustomMessage("tableauInit", list(forceReload = TRUE))
    showNotification("正在连接Tableau仪表板…", type = "message")
  })
  
  # 将Shiny筛选同步到Tableau
  observeEvent(
    list(input$main_poi_types, input$explore_region),
    {
      session$sendCustomMessage(
        "tableauFilter",
        list(
          poi_types = input$main_poi_types,
          region = input$explore_region
        )
      )
    },
    ignoreNULL = FALSE
  )
  
  # 生成Tableau分析报告
  observeEvent(input$generate_tableau, {
    dat <- filtered_poi()
    
    if (nrow(dat) == 0) {
      showNotification("请先选择氛围标签查看数据", type = "warning")
      return()
    }
    
    # 创建Tableau分析报告
    html("tableau_dashboard", 
         HTML(paste0(
           '<div style="padding: 20px; background: white; border-radius: 5px; height: 100%; overflow-y: auto;">
              <h4 style="color: #2c3e50; margin-bottom: 20px;">📊 Tableau分析报告</h4>
              <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px;">
                <div>
                  <h5 style="color: #34495e;">📈 数据概览</h5>
                  <ul style="list-style: none; padding: 0;">
                    <li style="margin: 8px 0;"><strong>总POI数量:</strong> ', nrow(dat), '</li>
                    <li style="margin: 8px 0;"><strong>选择区域:</strong> ', 
                    ifelse(input$explore_region == "all", "整个墨尔本", input$explore_region), '</li>
                    <li style="margin: 8px 0;"><strong>计划时间:</strong> ', input$hour, ':00</li>
                    <li style="margin: 8px 0;"><strong>选择氛围:</strong> ', length(input$vibes), ' 种</li>
                  </ul>
                </div>
                <div>
                  <h5 style="color: #34495e;">🏷️ 类型分布</h5>
                  <ul style="list-style: none; padding: 0;">',
                  paste(sapply(names(table(dat$type)), function(type) {
                    paste0('<li style="margin: 8px 0;"><strong>', type, ':</strong> ', table(dat$type)[type], '</li>')
                  }), collapse = ''),
                  '</ul>
                </div>
              </div>
              <div style="margin-top: 20px; padding: 15px; background: #e3f2fd; border-radius: 5px;">
                <h5 style="color: #1976d2; margin-bottom: 10px;">💡 Tableau分析建议</h5>
                <p style="margin: 5px 0; color: #424242;">• 使用经纬度字段创建交互式地图</p>
                <p style="margin: 5px 0; color: #424242;">• 按氛围标签分类显示不同颜色</p>
                <p style="margin: 5px 0; color: #424242;">• 使用便利性评分调整标记大小</p>
                <p style="margin: 5px 0; color: #424242;">• 创建区域对比分析仪表板</p>
              </div>
            </div>'
         )))
    
    showNotification("Tableau分析报告已生成", type = "success")
  })
  
  # 数据导出功能
  observeEvent(input$export_data, {
    dat <- filtered_poi()
    
    if (nrow(dat) == 0) {
      showNotification("没有数据可导出", type = "warning")
      return()
    }
    
    # 准备导出数据
    export_data <- dat |>
      select(.data$name, .data$type, .data$subtype, .data$lon, .data$lat, .data$capacity, 
             .data$vibe_artistic, .data$vibe_foodie, .data$vibe_historical, 
             .data$vibe_nightlife, .data$vibe_family, .data$street_density) |>
      mutate(
        # 添加POI类型标识，便于Tableau筛选
        poi_category = case_when(
          .data$type == "Cafe" | .data$type == "Restaurant" ~ "Cafe/Restaurant",
          .data$type == "Bar" | .data$type == "Pub" | .data$type == "Nightclub" ~ "Bar/Nightclub", 
          .data$type == "Landmark" | .data$type == "Museum" | .data$type == "Gallery" ~ "Landmark",
          .data$type == "Venue" | .data$type == "Event Space" ~ "Event Venue",
          .data$type == "Drinking Fountain" ~ "Drinking Fountain",
          .data$type == "Toilet" ~ "Toilet",
          TRUE ~ "Other"
        ),
        # 添加筛选条件信息
        export_timestamp = Sys.time(),
        selected_main_poi_types = paste(input$main_poi_types, collapse = ";"),
        selected_vibes = paste(input$vibes, collapse = ";"),
        time_preference = input$hour,
        explore_region = input$explore_region,
        # 添加Tableau筛选标识
        tableau_filter_active = TRUE,
        tableau_poi_types = paste(input$main_poi_types, collapse = ",")
      )
    
    # 创建下载链接
    output$download_tableau_data <- downloadHandler(
      filename = function() {
        paste0("melbourne_vibe_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # 显示导出成功信息
    html("tableau_dashboard", 
         HTML(paste0(
           '<div style="padding: 20px; background: white; border-radius: 5px; height: 100%; text-align: center;">
              <h4 style="color: #2c3e50; margin-bottom: 20px;">✅ 数据导出成功</h4>
              <div style="background: #e8f5e8; padding: 15px; border-radius: 5px; margin: 20px 0;">
                <h5 style="color: #2e7d32; margin-bottom: 10px;">📁 导出文件信息</h5>
                <p style="margin: 5px 0; color: #424242;"><strong>文件名:</strong> melbourne_vibe_data_', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv</p>
                <p style="margin: 5px 0; color: #424242;"><strong>记录数:</strong> ', nrow(export_data), ' 条</p>
                <p style="margin: 5px 0; color: #424242;"><strong>筛选的POI类型:</strong> ', paste(input$main_poi_types, collapse = ", "), '</p>
                <p style="margin: 5px 0; color: #424242;"><strong>探索区域:</strong> ', input$explore_region, '</p>
              </div>
              <div style="background: #fff3e0; padding: 15px; border-radius: 5px; margin: 20px 0;">
                <h5 style="color: #f57c00; margin-bottom: 10px;">🔗 Tableau集成步骤</h5>
                <ol style="text-align: left; color: #424242; margin: 10px 0;">
                  <li>下载CSV数据文件</li>
                  <li>在Tableau中导入CSV文件</li>
                  <li>使用<strong>poi_category</strong>字段筛选POI类型</li>
                  <li>使用<strong>tableau_poi_types</strong>字段匹配筛选条件</li>
                  <li>创建地图可视化，使用经纬度字段</li>
                  <li>根据氛围标签设置颜色和大小</li>
                </ol>
              </div>
              <div style="background: #e3f2fd; padding: 15px; border-radius: 5px; margin: 20px 0;">
                <h5 style="color: #1976d2; margin-bottom: 10px;">🎯 筛选建议</h5>
                <p style="color: #424242; margin: 5px 0;">在Tableau中使用以下字段进行筛选：</p>
                <ul style="text-align: left; color: #424242; margin: 10px 0;">
                  <li><strong>poi_category</strong>: 匹配主要POI类型</li>
                  <li><strong>tableau_filter_active</strong>: 确保为TRUE</li>
                  <li><strong>explore_region</strong>: 按区域筛选</li>
                </ul>
              </div>
              <button onclick="Shiny.setInputValue(\'download_tableau_data\', \'download\', {priority: \'event\'})" 
                      class="btn btn-success" style="margin-top: 10px;">
                📥 下载数据文件
              </button>
            </div>'
         )))
    
    showNotification("数据已准备就绪，可下载导入Tableau", type = "success")
  })

}

# ---------- 启动应用 ----------
shinyApp(ui, server)
