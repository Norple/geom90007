let viz = null;
let worksheet = null;
let isVizReady = false;
let pendingFilters = null;

const TABLEAU_URL = "https://public.tableau.com/views/AS3_17606721865090/Table1?:showVizHome=n&:embed=true&:language=zh-CN";
const TYPE_LABEL_MAP = {
  cafe_restaurant: "Cafe/Restaurant",
  bar_pub: "Bar/Nightclub",
  landmark: "Landmark",
  venue: "Event Venue",
  drinking_fountain: "Drinking Fountain",
  toilet: "Toilet"
};
const REGION_LABEL_MAP = {
  cbd: "Melbourne CBD",
  southbank: "Southbank",
  carlton: "Carlton",
  fitzroy: "Fitzroy",
  st_kilda: "St Kilda"
};
const WORKSHEET_NAME = "Table1"; // 请与Tableau仪表板中的工作表名称保持一致

function initTableauViz(forceReload = false) {
  const container = document.getElementById("tableauViz");
  if (!container) {
    console.warn("[Tableau] 找不到 #tableauViz 容器。");
    return;
  }

  if (viz && forceReload) {
    try {
      viz.dispose();
    } catch (err) {
      console.error("[Tableau] 释放旧viz失败:", err);
    }
    viz = null;
    worksheet = null;
    isVizReady = false;
  }

  if (viz) {
    return;
  }

  const options = {
    hideTabs: true,
    onFirstInteractive: function () {
      try {
        const sheet = viz.getWorkbook().getActiveSheet();
        if (sheet.getSheetType && sheet.getSheetType() === "dashboard") {
          const children = sheet.getWorksheets();
          worksheet = children.find((ws) => ws.getName() === WORKSHEET_NAME) || children[0];
        } else {
          worksheet = sheet;
        }
        isVizReady = true;
        const placeholder = document.getElementById("tableau_placeholder");
        if (placeholder) {
          placeholder.style.display = "none";
        }
        const vizElement = document.getElementById("tableauViz");
        if (vizElement) {
          vizElement.style.display = "block";
        }
        if (pendingFilters) {
          applyFilters(pendingFilters);
          pendingFilters = null;
        }
      } catch (err) {
        console.error("[Tableau] 初始化工作表失败:", err);
      }
    }
  };

  viz = new tableau.Viz(container, TABLEAU_URL, options);
}

function applyFilters(payload) {
  if (!payload) {
    return;
  }

  if (!isVizReady || !worksheet) {
    pendingFilters = payload;
    initTableauViz();
    return;
  }

  const promises = [];
  const poiTypes = Array.isArray(payload.poi_types) ? payload.poi_types : [];
  const region = payload.region;

  const poiField = "POI_Type"; // 请替换为Tableau中实际字段名
  const regionField = "Explore_Region"; // 同上

  if (poiTypes.length > 0) {
    const mapped = poiTypes
      .map((code) => TYPE_LABEL_MAP[code])
      .filter(Boolean);
    if (mapped.length > 0) {
      promises.push(
        worksheet.applyFilterAsync(
          poiField,
          mapped,
          tableau.FilterUpdateType.REPLACE
        )
      );
    } else {
      promises.push(worksheet.clearFilterAsync(poiField));
    }
  } else {
    promises.push(worksheet.clearFilterAsync(poiField));
  }

  if (region && region !== "all") {
    const mappedRegion = REGION_LABEL_MAP[region];
    if (mappedRegion) {
      promises.push(
        worksheet.applyFilterAsync(
          regionField,
          mappedRegion,
          tableau.FilterUpdateType.REPLACE
        )
      );
    } else {
      promises.push(worksheet.clearFilterAsync(regionField));
    }
  } else {
    promises.push(worksheet.clearFilterAsync(regionField));
  }

  Promise.all(promises).catch((err) => {
    console.error("[Tableau] 应用过滤失败:", err);
  });
}

function registerShinyHandlers() {
  if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) {
    setTimeout(registerShinyHandlers, 100);
    return;
  }

  Shiny.addCustomMessageHandler("tableauInit", function (payload) {
    initTableauViz(payload && payload.forceReload);
  });

  Shiny.addCustomMessageHandler("tableauFilter", function (payload) {
    applyFilters(payload);
  });
}

document.addEventListener("DOMContentLoaded", function () {
  initTableauViz();
  registerShinyHandlers();
});

window.initTableauViz = initTableauViz;
