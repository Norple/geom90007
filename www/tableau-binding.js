(() => {
  const MAX_WAIT_ATTEMPTS = 100;
  const WAIT_INTERVAL_MS = 200;

  function log(message, ...rest) {
    if (window.console && console.debug) {
      console.debug(`[tableauBinding] ${message}`, ...rest);
    }
  }

  function warn(message, ...rest) {
    if (window.console && console.warn) {
      console.warn(`[tableauBinding] ${message}`, ...rest);
    }
  }

  function error(message, ...rest) {
    if (window.console && console.error) {
      console.error(`[tableauBinding] ${message}`, ...rest);
    }
  }

  function waitForViz(vizId, attempt = 0) {
    return new Promise((resolve, reject) => {
      const element = document.getElementById(vizId);
      if (!element) {
        reject(new Error(`未找到 ID 为 ${vizId} 的 tableau-viz 元素。`));
        return;
      }

      const viz = element.viz;
      if (viz && typeof viz.ready === "function") {
        viz.ready().then(() => resolve(viz)).catch(reject);
        return;
      }

      if (attempt >= MAX_WAIT_ATTEMPTS) {
        reject(new Error(`等待 Tableau 可视化 ${vizId} 就绪超时。`));
        return;
      }

      setTimeout(() => {
        waitForViz(vizId, attempt + 1).then(resolve).catch(reject);
      }, WAIT_INTERVAL_MS);
    });
  }

  function filterUpdateType(type) {
    const normalized = (type || "replace").toString().toLowerCase();
    const FilterTypes = (window.tableau && window.tableau.FilterUpdateType) || window.FilterUpdateType;
    if (!FilterTypes) {
      return normalized === "add" ? "add" : normalized === "remove" ? "remove" : "replace";
    }
    switch (normalized) {
      case "add":
        return FilterTypes.ADD;
      case "remove":
        return FilterTypes.REMOVE;
      case "replace":
      default:
        return FilterTypes.REPLACE;
    }
  }

  function resolveSheet(workbook, sheetName) {
    const targetName = sheetName && sheetName.length ? sheetName : null;
    const activeSheet = workbook.activeSheet;

    const matchSheetByName = (sheets) => {
      if (!Array.isArray(sheets)) {
        return null;
      }
      if (!targetName) {
        return sheets[0] || null;
      }
      const matched = sheets.find((sheet) => sheet.name === targetName);
      if (!matched) {
        warn(`未在工作簿中找到名为 ${targetName} 的 worksheet，将默认使用第一个。`);
      }
      return matched || sheets[0] || null;
    };

    if (activeSheet.sheetType === "worksheet") {
      if (!targetName || activeSheet.name === targetName) {
        return activeSheet;
      }
      return matchSheetByName(workbook.worksheets || []);
    }

    if (activeSheet.sheetType === "dashboard") {
      return matchSheetByName(activeSheet.worksheets);
    }

    if (activeSheet.sheetType === "story") {
      const activePoint = activeSheet.activeStoryPoint;
      if (activePoint) {
        const storySheets = activePoint.worksheets || [];
        return matchSheetByName(storySheets);
      }
    }

    return activeSheet;
  }

  function cleanValues(values) {
    if (!Array.isArray(values)) {
      return [];
    }
    return values
      .map((v) => (typeof v === "string" ? v.trim() : v))
      .filter((v) => v !== "" && v !== null && v !== undefined);
  }

  function applyFilter(message) {
    const { vizId, field, values, filterType, sheetName } = message;
    waitForViz(vizId)
      .then((viz) => {
        const workbook = viz.workbook;
        const targetSheet = resolveSheet(workbook, sheetName);
        if (!targetSheet) {
          warn(`无法定位需要筛选的 worksheet`, { vizId, sheetName });
          return;
        }

        const clean = cleanValues(values);
        if (!clean.length) {
          targetSheet.clearFilterAsync(field).catch((err) => {
            error(`清除字段 ${field} 筛选失败`, err);
          });
          return;
        }

        targetSheet
          .applyFilterAsync(field, clean, filterUpdateType(filterType))
          .catch((err) => {
            error(`应用字段 ${field} 筛选失败`, err);
          });
      })
      .catch((err) => {
        error(err.message);
      });
  }

  function clearFilter(message) {
    const { vizId, field, sheetName } = message;
    waitForViz(vizId)
      .then((viz) => {
        const workbook = viz.workbook;
        const targetSheet = resolveSheet(workbook, sheetName);
        if (!targetSheet) {
          warn(`无法定位需要清除筛选的 worksheet`, { vizId, sheetName });
          return;
        }

        targetSheet.clearFilterAsync(field).catch((err) => {
          error(`清除字段 ${field} 筛选失败`, err);
        });
      })
      .catch((err) => {
        error(err.message);
      });
  }

  const hasTableau = Boolean(window.tableau || window.Viz || window.tableauViz);

  if (window.Shiny && hasTableau) {
    Shiny.addCustomMessageHandler("tableauApplyFilter", applyFilter);
    Shiny.addCustomMessageHandler("tableauClearFilter", clearFilter);
    log("已加载 Tableau v3 绑定脚本。");
  } else {
    error("Shiny 或 Tableau JS API 未正确加载，无法启用 Tableau 绑定。");
  }
})();
