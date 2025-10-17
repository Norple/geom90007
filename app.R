# app.R — 固定字段 POI_Type × 初次缓存值域 × 稳定映射 × 去抖去重 × 干净日志
library(shiny)
library(shinyjs)
library(jsonlite)

# 嵌入桥（老师提供）
source("tableau-in-shiny-v1.2.R")

# ===== 配置 =====
TABLEAU_URL     <- "https://public.tableau.com/views/AS3_17606721865090/Table1?:language=zh-CN&:display_count=n&:origin=viz_share_link"
SHEET_NAME_LIKE <- "Table1"       # Dashboard 时用于定位子工作表（包含匹配）
FIELD_NAME      <- "POI_Type"     # 确认字段名固定

# UI 复选框文案（与真实值不必一致；首次会做映射并缓存）
POI_TYPES <- c(
  "Cafe/Restaurant",
  "Bar/Nightclub",
  "Landmark",
  "Event Venue",
  "Drinking Fountain",
  "Toilet"
)

# ===== UI =====
ui <- navbarPage(
  title = "POI Types ↔ Tableau（缓存值域·稳定联动）",
  header = tagList(
    setUpTableauInShiny(),
    
    # === JS：缓存值域 / 等就绪 / 映射 / 去抖去重 / 干净日志 ===
    tags$script(HTML('
      // -------- 日志回流（统一成字符串）
      window.logToR = (channel, payload) => {
        try {
          const msg = (typeof payload === "string") ? payload : JSON.stringify(payload);
          console.log("[JS->R]", channel, msg);
          Shiny.setInputValue(channel, msg, {priority: "event"});
        } catch(e) { console.error("logToR error", e); }
      };

      // -------- 规范化 & 去变音符（映射用）
      const stripDiacritics = s => {
        try { return s.normalize("NFD").replace(/[\\u0300-\\u036f]/g, ""); }
        catch(_) { return s; }
      };
      const norm = s => {
        let t = (s||"").toString().trim().toLowerCase();
        t = stripDiacritics(t);
        return t
          .replace(/[\\s_\\-]+/g, "")
          .replace(/[，,。.;；:：/\\\\()（）\\[\\]{}]/g, "");
      };

      // -------- 目标 worksheet（Dashboard 取子表）
      const pickTargetSheet = (activeSheet, likeName) => {
        if (!activeSheet.worksheets) return activeSheet;
        const all = activeSheet.worksheets;
        if (!likeName) return all[0];
        return all.find(w => (w.name||"").includes(likeName)) || all[0];
      };

      // -------- 取筛选字段名（探测按钮用）
      const listWorksheetFilters = async ws => {
        try { return (await ws.getFiltersAsync()).map(f => f.fieldName); }
        catch(_) { return []; }
      };
      window.listAvailableFilters = async targetSheet => {
        const names = await listWorksheetFilters(targetSheet);
        window.logToR("js_diag", "Available filters on \\"" + (targetSheet.name||"(unnamed)") + "\\": " + names.join(", "));
        return names;
      };

      // -------- 用 SummaryData 拉“显示值域”（优先 formattedValue）
      const getDomainValues = async (ws, fieldName) => {
        try {
          const summary = await ws.getSummaryDataAsync({maxRows: 0});
          const cols = summary.columns;
          const idx = cols.findIndex(c =>
            norm(c.fieldName) === norm(fieldName) || norm(c.columnName) === norm(fieldName)
          );
          if (idx < 0) {
            window.logToR("js_diag", "Summary columns: " + cols.map(c => c.fieldName || c.columnName).join(", "));
            window.logToR("js_diag", "Cannot find domain column for field " + fieldName);
            return [];
          }
          const vals = Array.from(new Set(summary.data.map(row => {
            const cell = row[idx];
            return (cell.formattedValue ?? cell.value ?? "").toString();
          }).filter(x => x !== "")));
          window.logToR("js_diag", `Domain for "${fieldName}": ` + vals.join(", "));
          return vals;
        } catch(e) {
          window.logToR("js_diag", "getSummaryDataAsync failed: " + (e?.message||e));
          return [];
        }
      };

      // -------- 全局缓存（只扩不缩）
      const cache = {
        domain: null,   // ["Cafe/Restaurant", ...]
        dict:   null    // norm(val) -> canonical val
      };
      const buildCacheFrom = vals => {
        cache.domain = vals.slice();
        cache.dict   = new Map(vals.map(d => [norm(d), d]));
      };
      const hydrateCacheIfBetter = vals => {
        if (!Array.isArray(vals) || vals.length === 0) return;
        if (!cache.domain || vals.length > cache.domain.length) buildCacheFrom(vals);
      };

      // -------- UI 文案 → 真实显示值（使用缓存；若缓存未就绪，先拉一次）
      const mapUiToDomain = async (ws, fieldName, uiVals) => {
        if (!cache.domain) {
          const first = await getDomainValues(ws, fieldName);
          hydrateCacheIfBetter(first);
        }
        if (!cache.domain) return []; // 拉不到就放弃映射
        const mapped  = (uiVals||[]).map(v => cache.dict.get(norm(v))).filter(Boolean);
        const missing = (uiVals||[]).filter(v => !cache.dict.has(norm(v)));
        if (missing.length) window.logToR("js_diag", "Unmatched UI values: " + missing.join(", "));
        window.logToR("js_diag", `[${new Date().toLocaleTimeString()}] UI -> mapped values: [` + mapped.join(", ") + "]");
        return mapped;
      };

      // -------- 去重：相同 payload 不重复 apply
      let lastKey = null;

      // -------- 等 viz 就绪（低噪音重试）
      const waitForWorkbook = (id, cb, tries=0) => {
        try {
          const viz = document.getElementById(id);
          if (!viz || !viz.workbook || !viz.workbook.activeSheet) {
            if (tries === 0 || tries === 20 || tries === 60)
              window.logToR("js_diag", `[${new Date().toLocaleTimeString()}] waiting viz workbook... (#${tries})`);
            return setTimeout(()=>waitForWorkbook(id, cb, tries+1), 120);
          }
          cb(viz, viz.workbook.activeSheet);
        } catch(e) {
          if (tries === 0 || tries % 20 === 0)
            window.logToR("js_diag", `waitForWorkbook error: ${e?.message||e} (#${tries})`);
          setTimeout(()=>waitForWorkbook(id, cb, tries+1), 120);
        }
      };

      // -------- 总控：缓存值域 + 稳定映射 + 应用
      window.applyTableauFilterAuto = function(id, sheetNameLike, fieldName, values) {
        const stamp = () => new Date().toLocaleTimeString();
        const prettyVals = Array.isArray(values) ? values.join(", ") : String(values);
        window.logToR("js_diag", `[${stamp()}] applyTableauFilterAuto called with field="${fieldName}", values=[${prettyVals}]`);

        waitForWorkbook(id, async (viz, active) => {
          const target = pickTargetSheet(active, sheetNameLike);
          window.logToR("js_diag", `[${stamp()}] target sheet = ${target.name||"(unnamed)"}`);
          window.logToR("js_diag", `[${stamp()}] resolved field = "${fieldName}"`);

          // 若缓存还没建好（通常首次全选时），拉一次完整值域并缓存；之后只扩不缩
          const domainNow = await getDomainValues(target, fieldName);
          hydrateCacheIfBetter(domainNow);

          // 用缓存映射 UI 值
          const mapped = await mapUiToDomain(target, fieldName, values||[]);

          // 全选时：如果 mapped 与缓存等长 → 直接 clear 为 All，提高稳定性
          const allSelected = cache.domain && mapped.length === cache.domain.length;

          // 去重 key（allSelected 记为 "ALL"）
          const key = fieldName + "|" + (allSelected ? "ALL" : mapped.join("|"));
          if (key === lastKey) {
            window.logToR("js_diag", `[${stamp()}] skip: duplicate payload`);
            return;
          }
          lastKey = key;

          // 应用 / 清除
          if (allSelected) {
            target.clearFilterAsync(fieldName).then(() => {
              window.logToR("js_diag", `[${stamp()}] clearFilterAsync OK for "${fieldName}" (ALL)`);
            }).catch(err => {
              window.logToR("js_diag", `[${stamp()}] clearFilterAsync ERROR: ${err?.message||err}`);
            });
          } else if (!mapped.length) {
            // 映射不到任何合法值 → 不动图，日志提示
            window.logToR("js_diag", `[${stamp()}] mapped empty; skip apply to avoid clearing unintendedly`);
          } else {
            target.applyFilterAsync(fieldName, mapped, FilterUpdateType.Replace).then(() => {
              window.logToR("js_diag", `[${stamp()}] applyFilterAsync OK: field="${fieldName}", values=[${mapped.join(", ")}]`);
            }).catch(err => {
              window.logToR("js_diag", `[${stamp()}] applyFilterAsync ERROR: ${err?.message||err}`);
            });
          }
        });
      };

      // -------- 附加 FilterChanged 回流（字符串）
      document.addEventListener("DOMContentLoaded", () => {
        try {
          const id = "tableauViz";
          const viz = document.getElementById(id);
          if (!viz) return;
          viz.addEventListener(TableauEventType.FilterChanged, async e => {
            try {
              const f = await e.detail.getFilterAsync();
              const info = {
                fieldName: e.detail.fieldName,
                isAllSelected: f.isAllSelected,
                appliedValues: (f.appliedValues||[]).map(v => v.value)
              };
              window.logToR("js_filter_event", info); // logToR 会 JSON.stringify
            } catch(err) {
              window.logToR("js_diag", "FilterChanged handler error: " + (err?.message||err));
            }
          });
          window.logToR("js_diag", "Extra FilterChanged logger attached");
        } catch(e) {
          window.logToR("js_diag", "Attach FilterChanged logger failed: " + (e?.message||e));
        }
      });
    '))
  ),
  
  tabPanel(
    "Explore",
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h3("Main POI Types"),
          strong("Select Main POI Types"),
          checkboxGroupInput("poi_types", NULL, choices = POI_TYPES, selected = POI_TYPES),
          div(style="display:flex; gap:10px; margin-top:8px;",
              actionButton("btn_all",  "全选"),
              actionButton("btn_none", "清空"),
              actionButton("btn_probe","探测可用筛选")
          ),
          hr(),
          h4("R Log"),
          verbatimTextOutput("log_r",  placeholder = TRUE),
          h4("JS Log"),
          verbatimTextOutput("log_js", placeholder = TRUE),
          h4("Tableau Event Log"),
          verbatimTextOutput("log_tbl",placeholder = TRUE)
        )
      ),
      column(
        width = 9,
        tableauPublicViz(
          id    = "tableauViz",
          url   = TABLEAU_URL,
          height= "640px",
          style = "width:100%;border:none;"
        )
      )
    )
  )
)

# ===== Server =====
server <- function(input, output, session) {
  # 日志累加
  stamp <- function() format(Sys.time(), "%H:%M:%S")
  rv <- reactiveValues(r=character(), js=character(), tbl=character())
  push_r   <- function(msg) { rv$r   <- c(paste0("[",stamp(),"] ", msg), rv$r)[1:300] }
  push_js  <- function(msg) { rv$js  <- c(paste0("[",stamp(),"] ", msg), rv$js)[1:300] }
  push_tbl <- function(msg) { rv$tbl <- c(paste0("[",stamp(),"] ", msg), rv$tbl)[1:300] }
  output$log_r   <- renderText(paste(rv$r,   collapse="\n"))
  output$log_js  <- renderText(paste(rv$js,  collapse="\n"))
  output$log_tbl <- renderText(paste(rv$tbl, collapse="\n"))
  
  # UI：全选/清空/探测
  observeEvent(input$btn_all,  { updateCheckboxGroupInput(session, "poi_types", selected = POI_TYPES) })
  observeEvent(input$btn_none, { updateCheckboxGroupInput(session, "poi_types", selected = character(0)) })
  observeEvent(input$btn_probe, {
    push_r("Probe filters requested")
    runjs(sprintf(
      '(() => {
          const viz = document.getElementById(%s);
          if (!viz || !viz.workbook) { window.logToR("js_diag", "viz not ready"); return; }
          const active = viz.workbook.activeSheet;
          const target = %s ? (active.worksheets ? (active.worksheets.find(w => (w.name||"").includes(%s)) || active.worksheets[0]) : active) : active;
          (async () => { await window.listAvailableFilters(target); })();
      })();',
      toJSON("tableauViz", auto_unbox=TRUE),
      ifelse(nchar(SHEET_NAME_LIKE)>0, "true", "false"),
      toJSON(SHEET_NAME_LIKE, auto_unbox=TRUE)
    ))
  })
  
  # R -> JS：应用筛选（固定字段 + 使用缓存映射）
  apply_filter <- function(vals) {
    push_r(sprintf('R→JS apply filter field="%s" values=[%s]',
                   FIELD_NAME, paste(vals, collapse=", ")))
    runjs(sprintf(
      'window.applyTableauFilterAuto(%s, %s, %s, %s);',
      toJSON("tableauViz",     auto_unbox=TRUE),
      toJSON(SHEET_NAME_LIKE,  auto_unbox=TRUE),
      toJSON(FIELD_NAME,       auto_unbox=TRUE),
      toJSON(vals,             auto_unbox=FALSE)
    ))
  }
  
  # 初始化：只跑一次（默认全选 → 建缓存）
  observeEvent(TRUE, {
    apply_filter(POI_TYPES)
  }, once = TRUE, ignoreInit = FALSE)
  
  # 勾选变化：400ms 去抖
  debounced_vals <- debounce(reactive(input$poi_types), 400)
  observeEvent(debounced_vals(), {
    apply_filter(debounced_vals())
  }, ignoreInit = TRUE)
  
  # JS 诊断日志
  observeEvent(input$js_diag, {
    push_js(input$js_diag)  # 已是字符串
  }, ignoreInit = FALSE)
  
  # Tableau FilterChanged（落地确认）
  observeEvent(input$tableauViz_filter_changed, {
    info <- input$tableauViz_filter_changed
    msg <- sprintf('FilterChanged: field="%s", isAll=%s, values=[%s]',
                   info$fieldName, info$isAllSelected,
                   paste(if (is.null(info$appliedValues)) character(0) else info$appliedValues, collapse=", "))
    push_tbl(msg)
  }, ignoreInit = TRUE)
  
  # 附加 JS FilterChanged 回声（字符串，避免 NA）
  observeEvent(input$js_filter_event, {
    push_tbl(paste0("JS(FilterChanged echo): ", input$js_filter_event))
  }, ignoreInit = TRUE)
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
