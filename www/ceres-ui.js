(function () {
  "use strict";
  if (!window.XMLHttpRequest || !window.File || !window.Blob) return;
  var ua = navigator.userAgent || "";
  if (!(/AppleWebKit/.test(ua) && !/Chrome\/|Chromium\/|Edg\//.test(ua))) return;
  var send = XMLHttpRequest.prototype.send;
  XMLHttpRequest.prototype.send = function (body) {
    var sw = navigator.serviceWorker && navigator.serviceWorker.controller;
    if (!sw || !(body instanceof File) || typeof body.arrayBuffer !== "function") return send.apply(this, arguments);
    var xhr = this;
    body.arrayBuffer().then(function (buf) {
      send.call(xhr, new Blob([buf], { type: body.type || "application/octet-stream" }));
    }, function () { send.call(xhr, body); });
  };
})();

(function () {
  "use strict";

  var link = document.getElementById("ceres-ui-css");
  if (!link) {
    link = document.createElement("link");
    link.rel = "stylesheet";

    var me = document.currentScript && document.currentScript.src;
    var ver = me && me.indexOf("?v=") > -1 ? me.slice(me.indexOf("?v=")) : "";
    link.href = "static/ceres-ui.css" + ver;
    link.id = "ceres-ui-css";
  }

  function isSheet(n) {
    if (n.tagName === "STYLE") return /\S/.test(n.textContent || "");
    return n.tagName === "LINK" && /stylesheet/i.test(n.rel || "");
  }
  function putBefore(n) {
    if (n !== link && n.parentNode === document.head && isSheet(n) &&
        (link.compareDocumentPosition(n) & Node.DOCUMENT_POSITION_FOLLOWING))
      document.head.insertBefore(n, link);
  }
  function keepLast(muts) {
    if (!link.parentNode) document.head.appendChild(link);
    if (muts) muts.forEach(function (m) { m.addedNodes.forEach(putBefore); });
    else Array.prototype.slice.call(document.head.children).forEach(putBefore);
  }

  if (document.readyState !== "complete") {
    document.documentElement.classList.add("c-preload");
    window.addEventListener("load", function () {
      setTimeout(function () { document.documentElement.classList.remove("c-preload"); }, 120);
    });
  }

  var shiftTimer = null;
  function applyTheme(next) {
    var html = document.documentElement;
    html.setAttribute("data-ceres-theme", next);
    html.setAttribute("data-bs-theme", next);
    try { rethemePlots(); } catch (e) {   }
    try { msRedrawAll(false); } catch (e) {   }
  }

  var wantTheme = null;
  function setTheme(dark) {
    var html = document.documentElement;
    var next = dark ? "dark" : "light";
    if (wantTheme === next) return;
    var first = wantTheme === null;
    wantTheme = next;
    if (first) { applyTheme(next); return; }
    var apply = function () {
      if (html.getAttribute("data-ceres-theme") !== wantTheme) applyTheme(wantTheme);
    };
    var reduce = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    if (document.startViewTransition && !reduce) {
      try { document.startViewTransition(apply); } catch (e) { apply(); }

      setTimeout(apply, 350);
      return;
    }

    html.classList.add("c-theme-shift");
    clearTimeout(shiftTimer);
    shiftTimer = setTimeout(function () { html.classList.remove("c-theme-shift"); }, 450);
    applyTheme(next);
  }
  setTheme(false);

  document.addEventListener("change", function (e) {
    if (e.target && e.target.id === "dark_mode") setTheme(e.target.checked);
  }, true);

  function selectizeOnBody() {
    var $ = window.jQuery;
    if ($ && $.fn && $.fn.selectize && $.fn.selectize.defaults) $.fn.selectize.defaults.dropdownParent = "body";
  }
  selectizeOnBody();
  document.addEventListener("DOMContentLoaded", selectizeOnBody);

  var NAV_SEL = "ul.nav-tabs, ul.navbar-nav.nav-underline";

  function activeLink(nav) {
    return nav.querySelector(":scope > li > a.active, :scope > li.active > a, :scope > li > .nav-link.active, :scope > .nav-link.active");
  }

  function placeIndicator(nav, animate) {
    var a = activeLink(nav);
    var ind = nav.querySelector(":scope > .c-ind");
    if (!a || nav.offsetParent === null) return;
    if (!ind) {
      ind = document.createElement("span");
      ind.className = "c-ind";
      ind.setAttribute("aria-hidden", "true");
      nav.insertBefore(ind, nav.firstChild);
      nav.classList.add("c-has-ind");
      animate = false;
    }
    var nr = nav.getBoundingClientRect();
    var ar = a.getBoundingClientRect();
    if (ar.width === 0) return;
    var x = ar.left - nr.left + nav.scrollLeft - nav.clientLeft;
    var y = ar.top - nr.top + nav.scrollTop - nav.clientTop;
    if (!animate) ind.classList.add("c-ind-instant");
    ind.style.width = ar.width + "px";
    ind.style.height = ar.height + "px";
    ind.style.transform = "translate(" + x + "px," + y + "px)";
    ind.style.opacity = "1";
    if (!animate) {
      void ind.offsetWidth;
      ind.classList.remove("c-ind-instant");
    }
  }

  var pending = false;
  function placeAll(animate) {
    if (pending) return;
    pending = true;
    requestAnimationFrame(function () {
      pending = false;
      document.querySelectorAll(NAV_SEL).forEach(function (nav) { placeIndicator(nav, animate); });
    });
  }

  function linkIndex(a) {
    var li = a.closest("li") || a;
    return Array.prototype.indexOf.call(li.parentNode.children, li);
  }

  document.addEventListener("show.bs.tab", function (e) {
    var from = e.relatedTarget, to = e.target;
    if (!to) return;
    var dir = from ? (linkIndex(to) >= linkIndex(from) ? 1 : -1) : 1;
    var sel = to.getAttribute("data-bs-target") || to.getAttribute("href");
    if (sel && sel.charAt(0) === "#") {
      var pane = document.getElementById(sel.slice(1));
      if (pane) pane.style.setProperty("--c-dir", dir);
    }
  });

  document.addEventListener("shown.bs.tab", function () { placeAll(true); });
  document.addEventListener("shown.bs.modal", function () { placeAll(false); });

  (function () {
    var queue = [], open = false, waitingFor = null, timer = null, started = false, sawBusy = false;
    function hiddenOutputs(m) {
      return Array.prototype.filter.call(m.querySelectorAll(".shiny-bound-output[id]"), function (el) {

        for (var p = el.parentElement; p && p !== m; p = p.parentElement) {
          if (p.classList.contains("tab-pane") && !p.classList.contains("active")) return true;
          if (p.tagName === "DETAILS" && !p.open) return true;
        }
        return false;
      }).map(function (el, k) {

        var top = null;
        for (var p = el.parentElement; p && p !== m; p = p.parentElement)
          if (p.classList.contains("tab-pane")) top = p;
        var link = top && top.id ? m.querySelector('a[data-bs-target="#' + top.id + '"], a[href="#' + top.id + '"]') : null;
        var first = !!(link && /^\s*Predictions\s*$/.test(link.textContent));
        return { id: el.id, k: k, first: first };
      }).sort(function (a, b) { return (a.first === b.first) ? a.k - b.k : (a.first ? -1 : 1); })
        .map(function (o) { return o.id; });
    }
    function send() {
      clearTimeout(timer);
      waitingFor = queue.shift(); started = true; sawBusy = false;
      Shiny.setInputValue("ceres_prefetch", { op: "one", id: waitingFor, n: Date.now() }, { priority: "event" });
      timer = setTimeout(function () { waitingFor = null; pump(); }, 1500);
    }

    function pump() {
      clearTimeout(timer);
      if (!open || waitingFor || !queue.length || !window.Shiny || !Shiny.setInputValue) return;
      if (document.documentElement.classList.contains("shiny-busy")) return;
      send();
    }
    if (window.jQuery) jQuery(document)

      .on("shiny:value shiny:error", function (e) {
        if (waitingFor && e.name === waitingFor) { waitingFor = null; if (open && queue.length) send(); }
      })
      .on("shiny:busy", function () { if (waitingFor) sawBusy = true; })
      .on("shiny:idle", function () {
        if (!open) return;

        if (waitingFor && sawBusy) waitingFor = null;
        setTimeout(pump, 0);
      });

    var $doc = window.jQuery && jQuery(document);
    if (!$doc) return;
    var openedFor = null;

    function onShown(e) {
      var m = e.target; if (!m || !m.querySelector || !m.classList.contains("modal")) return;
      if (open && openedFor === m) {

        if (!queue.length && !waitingFor && !started) { queue = hiddenOutputs(m); clearTimeout(timer); timer = setTimeout(pump, 30); }
        return;
      }
      open = true; openedFor = m; waitingFor = null; started = false; queue = hiddenOutputs(m);
      clearTimeout(timer); timer = setTimeout(pump, 30);
    }
    $doc.on("show.bs.modal shown.bs.modal", onShown);
    document.addEventListener("show.bs.modal", onShown);
    document.addEventListener("shown.bs.modal", onShown);
    $doc.on("hidden.bs.modal", function (e) {

      var app = window.Shiny && Shiny.shinyapp, m = e.target;

      if (window.Plotly && Plotly.purge && m && m.id === "shiny-modal" && m.querySelectorAll)
        Array.prototype.forEach.call(m.querySelectorAll(".js-plotly-plot"), function (gd) { try { Plotly.purge(gd); } catch (err) {   } });

      if (app && app.$values && m && m.querySelector && m.querySelector("#fit_summary, #fit_summary_second"))
        Array.prototype.forEach.call(m.querySelectorAll("[id]"), function (el) {
          if (Object.prototype.hasOwnProperty.call(app.$values, el.id)) delete app.$values[el.id];
          if (app.$errors && Object.prototype.hasOwnProperty.call(app.$errors, el.id)) delete app.$errors[el.id];
        });
      if (!open) return;
      open = false; openedFor = null; queue = []; waitingFor = null; clearTimeout(timer);
      if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("ceres_prefetch", { op: "close", n: Date.now() }, { priority: "event" });
    });
  })();

  (function () {
    var $ = window.jQuery;
    if (!$) return;
    if (window.matchMedia && window.matchMedia("(hover: none) and (pointer: coarse)").matches) return;
    var queue = [], waitingFor = null, sawBusy = false, timer = null, idleTimer = null;
    var look = false, asking = false, built = 0, rounds = 0, pending = {}, lastSeen = 0;

    var QUIET_MS = 700;
    function screenBusy() {
      return Array.prototype.some.call(document.querySelectorAll(".recalculating"), function (el) { return el.offsetParent !== null && !el.closest(".modal"); });
    }
    function modalOpen() { return !!document.querySelector(".modal.show, .modal.in"); }
    function busy() { return document.documentElement.classList.contains("shiny-busy"); }
    function ready() { return !!(window.Shiny && Shiny.setInputValue); }
    function topTabset() {
      var nav = document.getElementById("tabs");
      return nav && nav.getAttribute("data-tabsetid");
    }

    function candidates() {
      var ids = [], top = topTabset();
      document.querySelectorAll(".tab-content").forEach(function (tc) {
        if (tc.closest(".modal") || tc.offsetParent === null) return;
        if (top && tc.getAttribute("data-tabsetid") === top) return;
        var panes = Array.prototype.filter.call(tc.children, function (p) { return p.classList.contains("tab-pane"); });
        var at = panes.findIndex(function (p) { return p.classList.contains("active"); });
        panes.map(function (p, k) { return { p: p, d: at < 0 ? k : (k > at ? k - at : panes.length + at - k) }; })
          .filter(function (o) { return !o.p.classList.contains("active"); })
          .sort(function (a, b) { return a.d - b.d; })
          .forEach(function (o) {
            o.p.querySelectorAll(".shiny-bound-output[id]").forEach(function (el) {
              if (el.matches(".shiny-plot-output, .shiny-image-output")) return;
              for (var q = el.parentElement; q && q !== o.p; q = q.parentElement) {
                if (q.classList.contains("tab-pane") && !q.classList.contains("active")) return;
                if (q.tagName === "DETAILS" && !q.open) return;
                if (q.style && q.style.display === "none") return;
              }
              ids.push(el.id);
            });
          });
      });
      return ids;
    }
    function ask() {
      var ids = candidates();
      if (!ids.length || !ready()) return;
      asking = true;
      Shiny.setInputValue("ceres_prefetch", { op: "stale", ids: ids, n: Date.now() }, { priority: "event" });
      clearTimeout(timer);
      timer = setTimeout(function () { asking = false; }, 1500);
    }
    function onStale(msg) {
      clearTimeout(timer);
      asking = false;
      queue = (msg && msg.ids) ? [].concat(msg.ids) : [];
      built = 0;

      if (queue.length && !waitingFor && ready() && !modalOpen()) send();
    }
    function send() {
      clearTimeout(timer);
      waitingFor = queue.shift(); pending[waitingFor] = true; sawBusy = false;
      Shiny.setInputValue("ceres_prefetch", { op: "once", id: waitingFor, n: Date.now() }, { priority: "event" });
      timer = setTimeout(function () { waitingFor = null; pump(); }, 1500);
    }
    function pump() {
      clearTimeout(timer);
      if (waitingFor || asking || !ready() || modalOpen() || busy() || screenBusy()) return;
      if (!queue.length) {

        if (built && rounds < 2) { rounds++; built = 0; ask(); }
        return;
      }
      send();
    }
    var registered = false;
    function register() {
      if (registered || !window.Shiny || !Shiny.addCustomMessageHandler) return;
      registered = true;
      Shiny.addCustomMessageHandler("ceres_prefetch_stale", onStale);
    }
    register();
    $(document)
      .on("shiny:connected", register)
      .on("shown.bs.tab", function (e) { if (!$(e.target).closest(".modal").length) look = true; })
      .on("shiny:value", function (e) {
        if (pending[e.name]) {
          delete pending[e.name]; built++;
          if (e.name === waitingFor) { waitingFor = null; if (queue.length && !modalOpen() && !screenBusy()) send(); }
          return;
        }

        var el = e.target;
        if (el && el.offsetParent !== null && !$(el).closest(".modal").length) { look = true; lastSeen = Date.now(); }
      })
      .on("shiny:error", function (e) {
        if (pending[e.name]) delete pending[e.name];
        if (waitingFor && e.name === waitingFor) { waitingFor = null; if (queue.length && !modalOpen() && !screenBusy()) send(); }
      })
      .on("shiny:busy", function () { if (waitingFor) sawBusy = true; })
      .on("shiny:idle", function () {
        clearTimeout(idleTimer);

        idleTimer = setTimeout(function onQuiet() {
          if (busy() || modalOpen()) return;
          if (waitingFor && sawBusy) waitingFor = null;
          if (screenBusy()) return;
          var wait = QUIET_MS - (Date.now() - lastSeen);
          if (look && wait > 0 && !waitingFor) { idleTimer = setTimeout(onQuiet, wait); return; }
          if (look && !waitingFor && !asking) { look = false; rounds = 0; ask(); return; }
          pump();
        }, waitingFor ? 80 : 250);
      });
  })();

  document.addEventListener("toggle", function (e) {
    var el = e.target;
    if (!el || el.tagName !== "DETAILS" || !window.jQuery || !el.querySelector(".shiny-bound-output")) return;
    var $el = jQuery(el);
    if (el.open) { $el.trigger("shown.sendOutputHiddenState"); $el.trigger("shown.sendImageSize"); $el.trigger("shown.htmlwidgets"); }
    else $el.trigger("hidden.sendOutputHiddenState");
  }, true);
  window.addEventListener("resize", function () { placeAll(false); });

  var phoneMenu = window.matchMedia && window.matchMedia("(max-width: 991.98px) and (hover: none) and (pointer: coarse)");
  document.addEventListener("shown.bs.collapse", function () { if (phoneMenu && phoneMenu.matches) placeAll(false); });
  document.addEventListener("show.bs.collapse", function () { if (phoneMenu && phoneMenu.matches) setTimeout(function () { placeAll(false); }, 0); });
  if (document.fonts && document.fonts.ready) document.fonts.ready.then(function () { placeAll(false); });

  function tok(name) {
    return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
  }

  function plotTheme() {
    var font = tok("--c-font") || "system-ui, sans-serif";
    var ink = tok("--c-ink-2"), muted = tok("--c-muted");
    var grid = tok("--c-line"), axis = tok("--c-line-strong");
    var dark = document.documentElement.getAttribute("data-ceres-theme") === "dark";
    return {
      font: font, ink: ink, muted: muted, grid: grid, axis: axis,
      hoverBg: dark ? "rgba(26,33,47,0.96)" : "rgba(255,255,255,0.97)",
      hoverInk: tok("--c-ink")
    };
  }

  function styleAxis(ax, t) {
    if (!ax || typeof ax !== "object") return;
    ax.gridcolor = t.grid;
    ax.zerolinecolor = t.axis;
    ax.linecolor = t.axis;
    ax.tickfont = Object.assign({}, ax.tickfont, { family: t.font, color: t.muted, size: (ax.tickfont && ax.tickfont.size) || 11 });
    if (ax.title && typeof ax.title === "object") {
      ax.title.font = Object.assign({}, ax.title.font, { family: t.font, color: t.ink });
    } else if (typeof ax.title === "string") {
      ax.title = { text: ax.title, font: { family: t.font, color: t.ink } };
    }
  }

  function themeLayout(layout) {
    layout = layout || {};
    var t = plotTheme();
    layout.font = Object.assign({}, layout.font, { family: t.font, color: t.ink });
    layout.paper_bgcolor = "rgba(0,0,0,0)";
    layout.plot_bgcolor = "rgba(0,0,0,0)";
    layout.hoverlabel = Object.assign({}, layout.hoverlabel, {
      bgcolor: t.hoverBg, bordercolor: t.axis,
      font: { family: t.font, color: t.hoverInk, size: 12 }
    });
    if (layout.legend !== false) {
      layout.legend = Object.assign({}, layout.legend, {
        bgcolor: "rgba(0,0,0,0)",
        font: Object.assign({}, layout.legend && layout.legend.font, { family: t.font, color: t.ink })
      });
    }

    if (typeof layout.title === "string" && layout.title.length) layout.title = { text: layout.title };
    if (layout.title && typeof layout.title === "object" && layout.title.text) {
      layout.title.font = Object.assign({ size: 14 }, layout.title.font, { family: t.font, color: t.ink });
      if (!/<b>/i.test(layout.title.text)) layout.title.text = "<b>" + layout.title.text + "</b>";
      if (layout.title.x == null) { layout.title.x = 0; layout.title.xanchor = "left"; layout.title.xref = "paper"; }
      if (layout.title.y == null) { layout.title.y = 0.985; layout.title.yanchor = "top"; }
      layout.margin = Object.assign({}, layout.margin);
      if (layout.margin.t == null || layout.margin.t < 58) layout.margin.t = 58;
    }
    if (!layout.xaxis) layout.xaxis = {};
    if (!layout.yaxis) layout.yaxis = {};
    Object.keys(layout).forEach(function (k) {
      if (/^[xy]axis\d*$/.test(k)) styleAxis(layout[k], t);
    });
    return layout;
  }

  function markerEdge() {
    return document.documentElement.getAttribute("data-ceres-theme") === "dark"
      ? "rgba(11,15,23,0.85)" : "rgba(255,255,255,0.9)";
  }
  function styleTraces(data) {
    if (!Array.isArray(data)) return data;
    var edge = markerEdge();
    data.forEach(function (tr) {
      if (tr && tr.type === "waterfall") {

        tr.increasing = tr.increasing || { marker: { color: "rgba(22,163,74,0.82)" } };
        tr.decreasing = tr.decreasing || { marker: { color: "rgba(224,69,123,0.82)" } };
        if (tr.connector && tr.connector.line) {
          tr.connector.line.color = "rgba(100,116,139,0.55)";
          tr.connector.line.width = 1.2;
        }
        return;
      }

      var BLACK = /^(#000000|#000|black|rgb\(0,\s*0,\s*0\)|rgba\(0,\s*0,\s*0,\s*1\))$/i;
      if (tr && tr.line && typeof tr.line.color === "string" && BLACK.test(tr.line.color)) tr.line.color = tok("--c-ink-2") || "#334155";

      var accent = tok("--c-accent") || "#2f6fe4";
      var DEFAULT_BLUE = /^(rgba\(31,\s*119,\s*180,\s*1\)|rgb\(31,\s*119,\s*180\)|#1f77b4)$/i;
      if (tr && tr.marker && typeof tr.marker.color === "string" && DEFAULT_BLUE.test(tr.marker.color)) tr.marker.color = accent;
      if (tr && tr.line && typeof tr.line.color === "string" && DEFAULT_BLUE.test(tr.line.color)) tr.line.color = accent;
      if (!tr || (tr.type && tr.type !== "scatter" && tr.type !== "scattergl")) return;
      var mode = tr.mode || "";
      if (mode.indexOf("markers") === -1) return;
      tr.marker = tr.marker || {};
      if (!tr.marker.line || tr.marker.line.__ceres) {
        tr.marker.line = { color: edge, width: 1, __ceres: true };
      }
    });
    return data;
  }

  function stripTags(s) { return String(s || "").replace(/<[^>]*>/g, ""); }
  function legendOnTitleRow(gd, data, layout) {
    var lg = (layout && layout.legend) || {}, t = layout && layout.title;
    if (!layout || layout.showlegend === false) return;

    var above = lg.orientation === "h" && typeof lg.y === "number" && lg.y >= 1;
    var unplaced = lg.orientation == null && lg.x == null && lg.y == null;
    if (lg.yref === "container" || !(above || unplaced)) return;
    if (!t || typeof t !== "object" || !t.text) return;
    var items = (Array.isArray(data) ? data : []).filter(function (tr) {
      return tr && tr.showlegend !== false && tr.name != null && String(tr.name).length && tr.type !== "heatmap" &&
             !(tr.type === "pie");
    });

    if (!items.length || items.length > 6 || (unplaced && items.length < 2 && layout.showlegend !== true)) return;
    var el = typeof gd === "string" ? document.getElementById(gd) : gd;
    var w = (el && el.offsetWidth) || layout.width || 900;
    var m = layout.margin || {};
    var plotW = w - (m.l != null ? m.l : 80) - (m.r != null ? m.r : 80);
    var size = (lg.font && lg.font.size) || 12;
    var legendW = items.reduce(function (s, tr) { return s + stripTags(tr.name).length * size * 0.56 + 44; }, 0);
    var titleW = stripTags(t.text).length * ((t.font && t.font.size) || 14) * 0.6;
    if (titleW + legendW + 32 > plotW) return;
    layout.legend = Object.assign({}, lg, { orientation: "h", xref: "paper", x: 1, xanchor: "right",
                                            yref: "container", y: t.y != null ? t.y : 0.985, yanchor: "top" });

    if (m.t != null && m.t > 62) layout.margin = Object.assign({}, m, { t: 62 });
  }

  function wrapPlotly(P) {
    if (!P || P.__ceresThemed) return;
    ["newPlot", "react"].forEach(function (fn) {
      var orig = P[fn];
      if (typeof orig !== "function") return;
      P[fn] = function (gd, data, layout, config) {
        if (data && typeof data === "object" && !Array.isArray(data) && data.data) {
          data.layout = themeLayout(data.layout);
          styleTraces(data.data);
          legendOnTitleRow(gd, data.data, data.layout);
        } else {
          layout = themeLayout(layout);
          styleTraces(data);
          legendOnTitleRow(gd, data, layout);
        }
        config = Object.assign({ displaylogo: false }, config);
        return orig.call(this, gd, data, layout, config);
      };
    });
    P.__ceresThemed = true;
  }

  (function catchPlotly() {
    var wrapAll = function (P) {
      try { wrapPlotly(P); } catch (e) {   }
      try { wrapPlotlySizing(); } catch (e) {   }
    };
    var cur = window.Plotly;
    if (cur) wrapAll(cur);
    try {
      Object.defineProperty(window, "Plotly", {
        configurable: true, enumerable: true,
        get: function () { return cur; },
        set: function (v) { cur = v; if (v) wrapAll(v); }
      });
    } catch (e) {
      (function waitForPlotly(n) {
        if (window.Plotly) { wrapAll(window.Plotly); return; }
        if (n > 600) return;
        setTimeout(function () { waitForPlotly(n + 1); }, 50);
      })(0);
    }
  })();

  function rethemePlots() {
    if (!window.Plotly) return;
    document.querySelectorAll(".js-plotly-plot").forEach(function (gd) {
      if (!gd.layout) return;
      var upd = themeLayout(JSON.parse(JSON.stringify(gd.layout)));
      try {
        window.Plotly.relayout(gd, upd);
        var idx = [];
        (gd.data || []).forEach(function (tr, i) {
          if (tr.marker && tr.marker.line && tr.marker.line.__ceres) idx.push(i);
        });
        if (idx.length) window.Plotly.restyle(gd, { "marker.line.color": markerEdge() }, idx);
      } catch (e) {   }
    });
  }

  function refitPlots() {
    if (!window.Plotly || !window.Plotly.Plots) return;
    document.querySelectorAll(".js-plotly-plot").forEach(function (gd) {
      if (gd.offsetParent !== null) {
        try { window.Plotly.Plots.resize(gd); } catch (e) {   }
      }
    });
  }
  var refitTimer = null;
  window.addEventListener("resize", function () {
    clearTimeout(refitTimer);
    refitTimer = setTimeout(refitPlots, 150);
  });

  document.addEventListener("shown.bs.tab", function () { setTimeout(refitPlots, 40); });

  function fitShownPlots(e) {
    if (!window.Plotly || !window.Plotly.relayout || !e || !e.target || !e.target.getAttribute) return;
    var sel = e.target.getAttribute("data-bs-target") || e.target.getAttribute("href");
    var pane = null;
    try { pane = sel && sel.charAt(0) === "#" ? document.querySelector(sel) : null; } catch (err) { pane = null; }
    if (!pane) return;
    pane.querySelectorAll(".js-plotly-plot").forEach(function (gd) {
      if (gd.offsetParent === null || !gd._fullLayout) return;
      var box = gd.getBoundingClientRect().width;
      if (!box || Math.abs((gd._fullLayout.width || 0) - box) < 2) return;
      gd.style.visibility = "hidden";
      var done = function () { gd.style.visibility = ""; };
      try { Promise.resolve(window.Plotly.relayout(gd, { autosize: true })).then(done, done); } catch (err) { done(); }
    });
  }
  document.addEventListener("shown.bs.tab", fitShownPlots);

  function unhideTabsAround(el) {
    var hidden = [];
    for (var p = el && el.parentElement; p && p !== document.body; p = p.parentElement) {
      if (p.classList.contains("tab-pane") && !p.classList.contains("active")) hidden.push([p, p.getAttribute("style")]);
    }
    if (!hidden.length) return null;
    var restore = function () { hidden.forEach(function (h) { if (h[1] === null) h[0].removeAttribute("style"); else h[0].setAttribute("style", h[1]); }); };

    var lay = function (adj) {
      hidden.slice().reverse().forEach(function (h, k) {
        var box = h[0].parentElement, cs = getComputedStyle(box);
        var wpx = box.clientWidth - (parseFloat(cs.paddingLeft) || 0) - (parseFloat(cs.paddingRight) || 0) + (k === 0 ? adj : 0);
        h[0].style.cssText += ";display:block !important;visibility:hidden !important;position:absolute !important;top:0;left:0;" +
          (wpx > 0 ? "width:" + wpx + "px !important;" : "right:0;");
      });
    };
    lay(0);
    return restore;
  }

  var hiddenChartWidth = {};

  var CERES_GL_POINTS = 1000;
  function useWebGL(fig) {
    var data = Array.isArray(fig) ? fig : (fig && Array.isArray(fig.data) ? fig.data : null);
    if (!data) return;
    data.forEach(function (t) {
      if (!t || (t.type && t.type !== "scatter") || !/markers/.test(t.mode || "") || (t.fill && t.fill !== "none")) return;
      var n = Math.max((t.x && t.x.length) || 0, (t.y && t.y.length) || 0);
      if (n >= CERES_GL_POINTS) t.type = "scattergl";
    });
  }

  function wrapPlotlySizing() {
    var P = window.Plotly;
    if (!P || P.__ceresWrapped) return;
    P.__ceresWrapped = true;
    ["newPlot", "react"].forEach(function (fn) {
      var orig = P[fn];
      if (typeof orig !== "function") return;
      P[fn] = function (gd, fig) {
        try { useWebGL(fig); } catch (err) {   }
        if (!gd || gd.nodeType !== 1 || !gd.closest || !gd.closest(".tab-pane:not(.active)")) return orig.apply(this, arguments);
        var key = gd.id, vw = window.innerWidth, known = key && hiddenChartWidth[key];
        if (known && known.vw === vw) {
          var oldW = gd.style.width;
          gd.style.width = known.w + "px";
          try { return orig.apply(this, arguments); } finally { gd.style.width = oldW; }
        }
        var restore = unhideTabsAround(gd);
        try {
          if (restore && key) { var w = gd.getBoundingClientRect().width; if (w > 50) hiddenChartWidth[key] = { w: w, vw: vw }; }
          return orig.apply(this, arguments);
        } finally { if (restore) restore(); }
      };
    });
  }

  function presizeHiddenPlot(gd) {
    if (!window.Plotly || !window.Plotly.relayout || !gd || !gd._fullLayout) return;
    var restore = unhideTabsAround(gd);
    if (!restore) return;
    var box = gd.getBoundingClientRect().width;
    restore();
    if (box > 50 && Math.abs((gd._fullLayout.width || 0) - box) >= 2) {
      try { window.Plotly.relayout(gd, { width: Math.round(box) }); } catch (err) {   }
    }
  }
  if (window.jQuery) jQuery(document).on("shiny:value", function (e) {
    wrapPlotlySizing();
    var el = e.target;

    if (!el || !el.closest || !el.closest(".tab-pane:not(.active)")) return;
    var tries = 0;
    (function wait() {

      var gd = el.classList && el.classList.contains("js-plotly-plot") ? el : (el.querySelector && el.querySelector(".js-plotly-plot"));
      if (gd && gd._fullLayout) presizeHiddenPlot(gd);
      else if (++tries < 25) setTimeout(wait, 20);
    })();
  });
  if (window.jQuery) jQuery(document).on("shown.bs.tab", function (e) { fitShownPlots(e.originalEvent && e.originalEvent.target ? e.originalEvent : e); });

  var MARK = { cx: 32, cy: 32, R: 27, r: 8.5, th: -18, pr: 17, rest: 30,
               ringW: 3, dotR: 4, glow: 0.475, minOp: 0.03, gamma: 1.4, n: 120 };
  var markSeq = 0;
  var reduceMotion = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function mPt(tDeg) {
    var t = tDeg * Math.PI / 180, a = MARK.th * Math.PI / 180;
    return [MARK.cx + MARK.R * Math.cos(t) * Math.cos(a) - MARK.r * Math.sin(t) * Math.sin(a),
            MARK.cy + MARK.R * Math.cos(t) * Math.sin(a) + MARK.r * Math.sin(t) * Math.cos(a)];
  }
  function mArc(t1, t2) {
    var p1 = mPt(t1), p2 = mPt(t2);
    return "M" + p1[0].toFixed(2) + " " + p1[1].toFixed(2) + "A" + MARK.R + " " + MARK.r + " " +
           MARK.th + " 0 1 " + p2[0].toFixed(2) + " " + p2[1].toFixed(2);
  }
  function isDark() { return document.documentElement.getAttribute("data-ceres-theme") === "dark"; }

  function buildMark(host) {
    var u = "cm" + (++markSeq), step = 360 / MARK.n, segs = "", front = "";
    var mids = [];
    for (var i = 0; i < MARK.n; i++) {
      var t1 = i * step, t2 = t1 + step + 1.0, mid = t1 + step / 2;
      var path = '<path d="' + mArc(t1, t2) + '" stroke="#fff"/>';
      segs += path;
      if (Math.sin(mid * Math.PI / 180) >= 0) front += path;
      mids.push(mid);
    }
    var ring = '<ellipse cx="32" cy="32" rx="27" ry="8.5" transform="rotate(-18 32 32)" fill="none" stroke-width="3" class="c-lm-ring"/>';
    var pr = MARK.pr;

    var vb = host.classList.contains("c-welcome-mark") ? "-6 0 76 64" : "0 0 64 64";
    host.innerHTML =
      '<svg viewBox="' + vb + '" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">' +
      '<defs>' +
      '<radialGradient id="' + u + 'o" cx=".34" cy=".28" r=".9"><stop offset="0" stop-color="#a9c6ff"/><stop offset=".55" stop-color="#4f7dff"/><stop offset="1" stop-color="#5b3fd9"/></radialGradient>' +
      '<radialGradient id="' + u + 'h" cx=".5" cy=".5" r=".5"><stop offset="0" stop-color="#fff" stop-opacity=".95"/><stop offset="1" stop-color="#fff" stop-opacity="0"/></radialGradient>' +
      '<radialGradient id="' + u + 'f" cx=".5" cy="1" r=".6"><stop offset="0" stop-color="#d6e4ff" stop-opacity=".55"/><stop offset="1" stop-color="#d6e4ff" stop-opacity="0"/></radialGradient>' +
      '<radialGradient id="' + u + 'g" cx=".5" cy=".5" r=".5"><stop offset="0" stop-color="#fff" stop-opacity=".9"/><stop offset=".35" class="c-lm-glowstop" stop-opacity=".55"/><stop offset="1" class="c-lm-glowstop" stop-opacity="0"/></radialGradient>' +
      '<radialGradient id="' + u + 's" cx=".5" cy=".5" r=".5"><stop offset="0" stop-color="#fff" stop-opacity=".95"/><stop offset="1" stop-color="#fff" stop-opacity="0"/></radialGradient>' +
      '<mask id="' + u + 'ma" maskUnits="userSpaceOnUse" x="0" y="0" width="64" height="64"><g fill="none" stroke-width="5" class="c-lm-all">' + segs + '</g></mask>' +
      '<mask id="' + u + 'mf" maskUnits="userSpaceOnUse" x="0" y="0" width="64" height="64"><g fill="none" stroke-width="5" class="c-lm-front">' + front + '</g></mask>' +
      '<clipPath id="' + u + 'c"><circle cx="32" cy="32" r="' + pr + '"/></clipPath>' +
      '</defs>' +
      '<g mask="url(#' + u + 'ma)">' + ring + '</g>' +
      '<g class="c-lm-dotback"></g>' +
      '<circle cx="32" cy="32" r="' + pr + '" fill="url(#' + u + 'o)"/>' +
      '<ellipse cx="32" cy="' + (32 + pr * .62).toFixed(2) + '" rx="' + (pr * .7).toFixed(2) + '" ry="' + (pr * .32).toFixed(2) + '" fill="url(#' + u + 'f)"/>' +
      '<ellipse cx="' + (32 - pr * .32).toFixed(2) + '" cy="' + (32 - pr * .46).toFixed(2) + '" rx="' + (pr * .46).toFixed(2) + '" ry="' + (pr * .26).toFixed(2) + '" fill="url(#' + u + 'h)" transform="rotate(-24 ' + (32 - pr * .32).toFixed(2) + ' ' + (32 - pr * .46).toFixed(2) + ')"/>' +
      '<circle cx="32" cy="32" r="' + (pr - .7) + '" fill="none" stroke="#fff" stroke-opacity=".38" stroke-width="1.2"/>' +
      '<g clip-path="url(#' + u + 'c)"><g mask="url(#' + u + 'mf)">' + ring + '</g></g>' +
      '<g class="c-lm-dotfront"><g class="c-lm-dot">' +
        '<circle class="c-lm-halo" r="' + (MARK.dotR * (1.6 + 1.2 * MARK.glow)).toFixed(2) + '" fill="url(#' + u + 'g)" opacity="' + (0.35 + 0.65 * MARK.glow).toFixed(2) + '"/>' +
        '<circle class="c-lm-core" r="' + MARK.dotR + '"/>' +

        '<ellipse class="c-lm-shine" cx="' + (-MARK.dotR * .28).toFixed(2) + '" cy="' + (-MARK.dotR * .36).toFixed(2) + '" rx="' + (MARK.dotR * .62).toFixed(2) + '" ry="' + (MARK.dotR * .4).toFixed(2) + '" fill="url(#' + u + 's)" transform="rotate(-24 ' + (-MARK.dotR * .28).toFixed(2) + ' ' + (-MARK.dotR * .36).toFixed(2) + ')"/>' +
      '</g></g>' +
      '</svg>';
    var svg = host.firstChild;
    return {
      host: host, svg: svg, mids: mids,
      all: svg.querySelectorAll(".c-lm-all path"),
      front: svg.querySelectorAll(".c-lm-front path"),
      frontMids: mids.filter(function (m) { return Math.sin(m * Math.PI / 180) >= 0; }),
      dot: svg.querySelector(".c-lm-dot"),
      back: svg.querySelector(".c-lm-dotback"),
      fore: svg.querySelector(".c-lm-dotfront"),
      angle: MARK.rest, inFront: true, dark: null
    };
  }

  function grey(mid, dotAngle) {
    var d = ((mid - dotAngle) % 360 + 360) % 360;
    var k = 1 - d / 360;
    var v = Math.round(255 * (MARK.minOp + (1 - MARK.minOp) * Math.pow(k, MARK.gamma)));
    return "rgb(" + v + "," + v + "," + v + ")";
  }

  function paintMark(m, angle) {
    m.angle = angle;
    var dark = isDark();
    if (dark !== m.dark) {
      m.dark = dark;
      var ringCol = dark ? "#94afff" : "#4f7dff";
      m.svg.querySelectorAll(".c-lm-ring").forEach(function (e) { e.setAttribute("stroke", ringCol); });
      m.svg.querySelector(".c-lm-core").setAttribute("fill", ringCol);
      m.svg.querySelectorAll(".c-lm-glowstop").forEach(function (e) { e.setAttribute("stop-color", dark ? "#9cc0ff" : "#7aa2ff"); });
    }
    for (var i = 0; i < m.all.length; i++) m.all[i].setAttribute("stroke", grey(m.mids[i], angle));
    for (var j = 0; j < m.front.length; j++) m.front[j].setAttribute("stroke", grey(m.frontMids[j], angle));
    var p = mPt(angle);
    m.dot.setAttribute("transform", "translate(" + p[0].toFixed(2) + " " + p[1].toFixed(2) + ")");
    var front = Math.sin(angle * Math.PI / 180) >= 0;
    if (front !== m.inFront) { (front ? m.fore : m.back).appendChild(m.dot); m.inFront = front; }
  }

  var marks = [];
  function mountMarks() {
    document.querySelectorAll(".c-live-mark:not([data-lm])").forEach(function (host) {
      host.setAttribute("data-lm", "1");
      var m = buildMark(host);
      m.mode = host.getAttribute("data-mode") || "busy";
      m.running = false; m.last = 0; m.visible = true; m.hover = false;
      paintMark(m, MARK.rest);
      if (m.mode === "busy") {
        var hoverTarget = host.closest("a") || host;
        hoverTarget.addEventListener("mouseenter", function () { m.hover = true; m.lapUntil = performance.now() + 1700; kick(); });
        hoverTarget.addEventListener("mouseleave", function () { m.hover = false; });
      }
      if ("IntersectionObserver" in window) {
        new IntersectionObserver(function (es) { m.visible = es[0].isIntersecting; kick(); }).observe(host);
      }
      marks.push(m);
    });
    marks = marks.filter(function (m) { return document.body.contains(m.host); });
    kick();
  }

  function wants(m, now) {
    if (reduceMotion || document.hidden || !m.visible) return false;
    if (m.mode === "always") return true;
    return document.documentElement.classList.contains("shiny-busy") || (m.lapUntil && now < m.lapUntil);
  }

  var rafOn = false;
  function frame(now) {
    var any = false;
    marks.forEach(function (m) {
      var period = m.mode === "always" ? 6500 : 1600;
      var dt = m.last ? Math.min(now - m.last, 64) : 16;
      m.last = now;
      var want = wants(m, now);
      if (!want && !m.running) return;
      var next = m.angle - 360 * dt / period;
      if (!want) {

        var before = ((m.angle - MARK.rest) % 360 + 360) % 360;
        var after = ((next - MARK.rest) % 360 + 360) % 360;
        if (after > before) { paintMark(m, MARK.rest); m.running = false; return; }
      }
      m.running = true; any = true;
      paintMark(m, next % 360);
    });

    marks.forEach(function (m) { if (!m.running && m.dark !== isDark()) paintMark(m, m.angle); });
    if (any) requestAnimationFrame(frame);
    else { rafOn = false; marks.forEach(function (m) { m.last = 0; }); }
  }
  function kick() {
    if (rafOn || !marks.length) return;
    rafOn = true;
    requestAnimationFrame(frame);
  }
  new MutationObserver(kick).observe(document.documentElement,
    { attributes: true, attributeFilter: ["class", "data-ceres-theme"] });
  document.addEventListener("visibilitychange", kick);

  function mountBrandMark() {
    var a = document.querySelector(".navbar .navbar-brand a, .navbar a.navbar-brand");
    if (!a || a.querySelector(".c-live-mark")) return;
    var s = document.createElement("span");
    s.className = "c-live-mark c-brand-mark";
    s.setAttribute("data-mode", "busy");
    a.insertBefore(s, a.firstChild);
    a.classList.add("c-has-live-mark");
  }

  function sessionMenu(open) {
    var wrap = document.querySelector(".c-session");
    if (!wrap) return;
    var btn = wrap.querySelector(".c-session-btn");
    var isOpen = wrap.classList.contains("c-open");
    if (open === undefined) open = !isOpen;
    if (open === isOpen) return;
    wrap.classList.toggle("c-open", open);
    if (btn) btn.setAttribute("aria-expanded", open ? "true" : "false");
    if (open) {
      var first = wrap.querySelector(".c-session-item");
      if (first) setTimeout(function () { first.focus({ preventScroll: true }); }, 30);
    }
  }

  document.addEventListener("click", function (e) {
    var t = e.target;
    if (!t || !t.closest) return;
    if (t.closest(".c-session-btn")) { sessionMenu(); return; }
    var item = t.closest("[data-c-session]");
    if (item) {
      sessionMenu(false);
      if (item.getAttribute("data-c-session") === "save") {
        var a = document.getElementById("session_save");
        if (a) a.click();
      } else if (item.getAttribute("data-c-session") === "new") {
        askStartFresh(false);
      } else {
        var inp = document.getElementById("session_open");
        if (inp) { try { inp.value = ""; } catch (err) {   } inp.click(); }
      }
      return;
    }
    if (!t.closest(".c-session-menu")) sessionMenu(false);
  });

  document.addEventListener("keydown", function (e) {
    var wrap = document.querySelector(".c-session.c-open");
    if (!wrap) return;
    if (e.key === "Escape") {
      sessionMenu(false);
      var b = wrap.querySelector(".c-session-btn");
      if (b) b.focus();
    } else if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      var items = Array.prototype.slice.call(wrap.querySelectorAll(".c-session-item"));
      var i = items.indexOf(document.activeElement);
      i = e.key === "ArrowDown" ? (i + 1) % items.length : (i <= 0 ? items.length - 1 : i - 1);
      if (items[i]) items[i].focus();
      e.preventDefault();
    }
  });

  function hasData() {
    return ["data_select", "data_select_cer_generation"].some(function (id) {
      var el = document.getElementById(id);
      return !!(el && el.value);
    });
  }
  function cerDialog(opts) {
    var old = document.querySelector(".c-dialog-wrap");
    if (old) old.remove();
    var wrap = document.createElement("div");
    wrap.className = "c-dialog-wrap";
    var box = document.createElement("div");
    box.className = "c-dialog";
    box.setAttribute("role", "dialog");
    box.setAttribute("aria-modal", "true");
    box.setAttribute("aria-labelledby", "c_dialog_title");
    var h = document.createElement("h3"); h.id = "c_dialog_title"; h.textContent = opts.title;
    var p = document.createElement("p"); p.textContent = opts.body;
    var row = document.createElement("div"); row.className = "c-dialog-actions";
    var prev = document.activeElement;
    var acted = false;
    function close() {
      document.removeEventListener("keydown", onKey, true);
      wrap.remove();
      if (!acted && opts.onDismiss) opts.onDismiss();
      acted = true;
      if (prev && prev.focus) try { prev.focus({ preventScroll: true }); } catch (e) {}
    }
    var primary = null;
    opts.buttons.forEach(function (b) {
      var el = document.createElement("button");
      el.type = "button";
      el.className = "btn " + (b.primary ? "btn-primary" : "btn-default");
      el.textContent = b.label;
      el.addEventListener("click", function () { acted = true; close(); if (b.action) b.action(); });
      if (b.primary) primary = el;
      row.appendChild(el);
    });
    function onKey(e) {
      if (e.key === "Escape") { e.preventDefault(); e.stopPropagation(); close(); }
      else if (e.key === "Tab") {
        var f = Array.prototype.slice.call(row.querySelectorAll("button"));
        var i = f.indexOf(document.activeElement);
        e.preventDefault();
        f[(i + (e.shiftKey ? f.length - 1 : 1)) % f.length].focus();
      }
    }
    document.addEventListener("keydown", onKey, true);
    wrap.addEventListener("mousedown", function (e) { if (e.target === wrap) close(); });
    box.appendChild(h); box.appendChild(p); box.appendChild(row);
    wrap.appendChild(box);
    document.body.appendChild(wrap);
    (primary || row.querySelector("button")).focus();
  }
  function startFresh() {
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("session_new", Date.now(), { priority: "event" });
  }
  function askStartFresh(fromReload) {
    var fresh = { label: "Start fresh", primary: true, action: startFresh };
    var cancel = { label: "Cancel" };
    var reload = { label: "Reload page", action: function () { window.ceresReloading = true; (window.top || window).location.reload(); } };
    if (fromReload && !hasData()) {
      cerDialog({
        title: "Reload CERES?",
        body: "Reload page restarts CERES, which takes a few seconds.",
        buttons: [cancel, Object.assign({ primary: true }, reload)]
      });
    } else if (fromReload) {
      cerDialog({
        title: "Start over?",
        body: "Start fresh clears every dataset and result right away. Reload page restarts CERES (a few seconds), then offers to pick up where you left off.",
        buttons: [cancel, reload, fresh]
      });
    } else {
      cerDialog({
        title: "Start fresh?",
        body: "This clears every dataset, model and result loaded now. Anything not saved as a session is lost.",
        buttons: [cancel, fresh]
      });
    }
  }

  document.addEventListener("keydown", function (e) {
    var reloadKey = e.key === "F5" || ((e.ctrlKey || e.metaKey) && !e.altKey && (e.key === "r" || e.key === "R"));
    if (!reloadKey || document.querySelector(".c-dialog-wrap")) return;
    e.preventDefault();
    askStartFresh(true);
  });
  window.ceresAskReload = function () { if (!document.querySelector(".c-dialog-wrap")) askStartFresh(true); };

  window.ceresStackOverflow = function () {
    cerDialog({
      title: "This step ran out of room",
      body: "Browsers on iPhone and iPad give CERES far less working room than a computer, and this step needed more. Reload to carry on: CERES then offers to pick up where you left off.",
      buttons: [{ label: "Close" },
                { label: "Reload page", primary: true, action: function () { window.ceresReloading = true; (window.top || window).location.reload(); } }]
    });
  };

  function onStartFresh(msg) {

    ["data_select", "data_select_cer_generation"].forEach(function (id) {
      Shiny.setInputValue(id, "");
    });
    var up = document.getElementById("data_upload");
    if (!up) return;
    try { up.value = ""; } catch (e) {}
    var grp = up.closest(".shiny-input-container");
    if (!grp) return;
    var txt = grp.querySelector("input[type=text]");
    if (txt) txt.value = "";
    var bar = grp.querySelector(".progress");
    if (bar) {
      bar.style.visibility = "hidden";
      var b = bar.querySelector(".progress-bar");
      if (b) { barMax.delete(b); upLoad.phase = "idle"; setBar(b, 0); b.textContent = ""; }
    }
  }
  (function hookFresh() {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler("ceres_start_fresh", onStartFresh);
    else setTimeout(hookFresh, 50);
  })();

  var AS_MAX_MS = 7 * 864e5;
  function asDb() {
    return new Promise(function (res, rej) {
      var r = indexedDB.open("ceres", 1);
      r.onupgradeneeded = function () { r.result.createObjectStore("autosave"); };
      r.onsuccess = function () { res(r.result); };
      r.onerror = function () { rej(r.error); };
    });
  }
  function asTx(mode, fn) {
    return asDb().then(function (db) {
      return new Promise(function (res, rej) {
        var t = db.transaction("autosave", mode), req = fn(t.objectStore("autosave"));
        t.oncomplete = function () { res(req && req.result); };
        t.onerror = function () { rej(t.error); };
      });
    });
  }
  function asPut(v) { return asTx("readwrite", function (s) { return v == null ? s.delete("last") : s.put(v, "last"); }).catch(function () {}); }
  function asGet() { return asTx("readonly", function (s) { return s.get("last"); }); }
  function onAutosave(msg) {
    if (msg.clear) asPut(null);
    else asPut({ text: msg.text, datasets: msg.datasets || [], at: msg.at || Date.now() });
  }
  (function hookAutosave() {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler("ceres-autosave", onAutosave);
    else setTimeout(hookAutosave, 50);
  })();
  function asAgo(ms) {
    var m = Math.round(ms / 60000);
    if (m < 1) return "just now";
    if (m < 60) return m + (m === 1 ? " minute ago" : " minutes ago");
    var h = Math.round(m / 60);
    if (h < 24) return h + (h === 1 ? " hour ago" : " hours ago");
    var d = Math.round(h / 24); return d + (d === 1 ? " day ago" : " days ago");
  }
  var asAsked = false;
  function asAsk() {
    if (asAsked) return; asAsked = true;
    var ready = function () { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("autosave_ready", true); };
    if (!window.indexedDB) return ready();
    asGet().then(function (v) {
      var age = v && v.at ? Date.now() - v.at : Infinity;
      if (!v || !v.text || age > AS_MAX_MS || hasData()) {
        if (v && age > AS_MAX_MS) asPut(null);
        return ready();
      }
      var names = (v.datasets || []).join(", ");
      cerDialog({
        title: "Pick up where you left off?",
        body: (names ? names + " · " : "") + "last change " + asAgo(age) + ". Continue reopens it as it was; Start fresh opens CERES empty.",
        onDismiss: ready,
        buttons: [
          { label: "Start fresh", action: function () { asPut(null); ready(); } },
          { label: "Continue", primary: true, action: function () {
              Shiny.setInputValue("session_autorestore", v.text, { priority: "event" }); ready(); } }
        ]
      });
    }).catch(ready);
  }
  (function hookAsk() {
    if (!window.jQuery) return setTimeout(hookAsk, 50);
    window.jQuery(document).on("shiny:connected", function () { setTimeout(asAsk, 400); });
    if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected()) setTimeout(asAsk, 400);
  })();

  var CB_FUNS = { exp: 1, log: 1, log10: 1, log2: 1, sqrt: 1, abs: 1, sin: 1, cos: 1, tan: 1, pmin: 1, pmax: 1 };
  var cbCaret = null;
  var cbComposing = false;
  function cbEsc(s) { return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"); }
  function cbEd() { return document.getElementById("nl_cb_editor"); }
  function cbText(ed) { return (ed.textContent || "").replace(/ /g, " ").replace(/​/g, ""); }
  function cbGetCaret(ed) {
    var sel = window.getSelection();
    if (!sel || !sel.rangeCount || !ed.contains(sel.anchorNode)) return null;
    var r = sel.getRangeAt(0), pre = r.cloneRange();
    pre.selectNodeContents(ed); pre.setEnd(r.endContainer, r.endOffset);
    return pre.toString().replace(/​/g, "").length;
  }
  function cbSetCaret(ed, off) {
    var w = document.createTreeWalker(ed, NodeFilter.SHOW_TEXT), n, left = off, last = null;
    while ((n = w.nextNode())) {
      last = n;
      if (left <= n.length) { var r = document.createRange(); r.setStart(n, left); r.collapse(true);
        var s = window.getSelection(); s.removeAllRanges(); s.addRange(r); return; }
      left -= n.length;
    }
    if (last) { var r2 = document.createRange(); r2.setStart(last, last.length); r2.collapse(true);
      var s2 = window.getSelection(); s2.removeAllRanges(); s2.addRange(r2); }
  }
  function cbHtml(text) {
    var drivers = {};
    document.querySelectorAll(".c-cb-driver[data-name]").forEach(function (b) { drivers[b.getAttribute("data-name")] = 1; });
    return text.replace(/([A-Za-z_.][A-Za-z0-9_.]*)|(\d+\.?\d*(?:[eE][-+]?\d+)?)|([^A-Za-z0-9_.\s]+)|(\s+)/g, function (m, name, num, op, ws) {
      if (ws) return '<span class="c-cb-ws">' + ws + "</span>";
      if (num) return '<span class="c-cb-tok-num">' + cbEsc(num) + "</span>";
      if (op) return op.split("").map(function (ch) {
        var cls = ch === "*" ? "c-cb-tok-op is-mul" : (ch === "^" ? "c-cb-tok-op is-pow" : (ch === "(" || ch === ")" || ch === "," ? "c-cb-tok-op is-paren" : "c-cb-tok-op"));
        return '<span class="' + cls + '">' + cbEsc(ch) + "</span>";
      }).join("");
      if (CB_FUNS[name]) return '<span class="c-cb-tok-fn">' + name + "</span>";
      if (name === "pi") return '<span class="c-cb-tok-num">pi</span>';
      if (drivers[name]) return '<span class="c-cb-tok-drv">' + cbEsc(name) + "</span>";

      if ((name.length > 2 || /^[A-Z]/.test(name)) && cbNear(name, Object.keys(drivers)))
        return '<span class="c-cb-tok-bad">' + cbEsc(name) + "</span>";
      return '<span class="c-cb-tok-par">' + cbEsc(name) + "</span>";
    });
  }
  function cbDist(a, b) {
    var p = [], i, j;
    for (j = 0; j <= b.length; j++) p[j] = j;
    for (i = 1; i <= a.length; i++) {
      var prev = p[0]; p[0] = i;
      for (j = 1; j <= b.length; j++) { var t = p[j]; p[j] = Math.min(p[j] + 1, p[j - 1] + 1, prev + (a[i - 1] === b[j - 1] ? 0 : 1)); prev = t; }
    }
    return p[b.length];
  }
  function cbNear(x, pool) {
    x = x.toLowerCase();
    for (var k = 0; k < pool.length; k++) if (pool[k].toLowerCase().indexOf(x) === 0) return true;
    for (k = 0; k < pool.length; k++) if (cbDist(x, pool[k].toLowerCase()) <= Math.max(1, Math.floor(x.length / 3))) return true;
    return false;
  }

  function cbPaint(caret) {
    var ed = cbEd(); if (!ed) return;
    var text = cbText(ed);
    var at = caret != null ? caret : (document.activeElement === ed ? cbGetCaret(ed) : null);
    ed.innerHTML = cbHtml(text);
    ed.classList.toggle("is-empty", !text.length);
    cbLabelNew();
    if (at != null && document.activeElement === ed) { cbSetCaret(ed, at); cbCaret = at; }
  }
  function cbSync(ed) {
    var ta = document.getElementById("nl_custom_eq"), text = cbText(ed);
    if (ta) ta.value = text;
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("nl_custom_eq", text, { priority: "event" });
  }
  function cbSet(text, caret) {
    var ed = cbEd(); if (!ed) return;
    ed.textContent = text; ed.focus();
    cbPaint(caret == null ? text.length : caret);
    cbSync(ed);
  }
  window.nlInsertAtCursor = function (text) {
    var ed = cbEd(); if (!ed) return;
    var v = cbText(ed), s = cbCaret != null ? Math.min(cbCaret, v.length) : v.length;
    var at = s + text.length;
    var open = text.indexOf("(");
    if (open >= 0 && /\)$/.test(text) && text.length - open <= 4) at = s + open + 1;
    cbSet(v.substring(0, s) + text + v.substring(s), at);
  };

  function cbNextParam() {
    var ed = cbEd(), used = {};
    (ed ? cbText(ed) : "").replace(/[A-Za-z_.][A-Za-z0-9_.]*/g, function (m) { used[m] = 1; return m; });
    for (var c = 97; c <= 122; c++) { var l = String.fromCharCode(c); if (!used[l]) return l; }
    return "k";
  }
  function cbLabelNew() {
    var b = document.querySelector(".c-cb-param.is-new");
    if (b) b.textContent = "+ " + cbNextParam();
  }
  window.nlInsertNewParam = function () { window.nlInsertAtCursor(cbNextParam()); };
  window.ceresCbTemplate = function (btn) {
    document.querySelectorAll(".c-cb-shape.is-on").forEach(function (b) { b.classList.remove("is-on"); });
    btn.classList.add("is-on");
    cbSet(btn.getAttribute("data-eq") || "");
  };
  document.addEventListener("input", function (e) {
    if (e.target && e.target.id === "nl_cb_editor" && !cbComposing) { cbPaint(); cbSync(e.target); }
  });
  document.addEventListener("compositionstart", function (e) { if (e.target && e.target.id === "nl_cb_editor") cbComposing = true; });
  document.addEventListener("compositionend", function (e) {
    if (e.target && e.target.id === "nl_cb_editor") { cbComposing = false; cbPaint(); cbSync(e.target); }
  });
  document.addEventListener("keydown", function (e) {
    if (e.target && e.target.id === "nl_cb_editor" && e.key === "Enter") e.preventDefault();
  });
  document.addEventListener("paste", function (e) {
    if (!e.target || !e.target.closest || !e.target.closest("#nl_cb_editor")) return;
    e.preventDefault();
    var t = ((e.clipboardData && e.clipboardData.getData("text/plain")) || "").replace(/\s*\n\s*/g, " ");
    var ed = cbEd(), v = cbText(ed), s = cbGetCaret(ed); if (s == null) s = v.length;
    cbSet(v.substring(0, s) + t + v.substring(s), s + t.length);
  });
  document.addEventListener("selectionchange", function () {
    var ed = cbEd(); if (ed && document.activeElement === ed) { var c = cbGetCaret(ed); if (c != null) cbCaret = c; }
  });

  function cbInit() {
    var ed = cbEd(), ta = document.getElementById("nl_custom_eq");
    if (!ed || !ta) return;
    if (cbText(ed) !== ta.value) ed.textContent = ta.value;
    cbPaint(null);
  }
  (function hookCbPaint() {
    if (!window.jQuery) return setTimeout(hookCbPaint, 50);
    window.jQuery(document).on("shiny:value", function (e) {
      if (e.name === "nl_custom_builder_ui" || e.name === "nl_cb_drivers") setTimeout(cbInit, 0);
      if (e.name === "nl_cb_paramchips") setTimeout(cbLabelNew, 0);
    });
  })();

  var cbLast = null;
  (function hookCbValue() {
    if (!window.jQuery) return setTimeout(hookCbValue, 50);
    window.jQuery(document).on("shiny:value", function (e) {
      if (e.name === "nl_cb_params" && cbLast) setTimeout(function () { onCbFit(cbLast); }, 0);
    });
  })();
  function onCbFit(msg) {
    cbLast = msg;
    (msg.rows || []).forEach(function (r) {
      var set = function (id, t) { var el = document.getElementById(id); if (el) el.textContent = t; };
      set("nl_cb_v_" + r.p, r.value);
      set("nl_cb_r_" + r.p, r.range + (r.note ? " · " + r.note : ""));
      set("nl_cb_m_" + r.p, r.meaning || "");
      var row = document.getElementById("nl_cb_row_" + r.p);
      if (row) row.classList.toggle("is-shaky", !!r.shaky);
      var bar = document.getElementById("nl_cb_b_" + r.p);
      if (bar) { bar.style.left = r.left + "%"; bar.style.width = r.width + "%"; }
    });
  }
  (function hookCb() {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler("ceres-cb-fit", onCbFit);
    else setTimeout(hookCb, 50);
  })();

  var barMax = new WeakMap();
  var upLoad = { phase: "idle" };
  function barPct(bar) {
    var w = bar.style.width;
    return /%$/.test(w || "") ? parseFloat(w) : null;
  }
  function setBar(bar, pct) {
    bar.dataset.cerW = pct + "%";
    bar.style.setProperty("width", pct + "%", bar.style.getPropertyPriority("width"));
  }
  function upBar() {
    var el = document.getElementById("data_upload_progress");
    return el ? el.querySelector(".progress-bar") : null;
  }
  function guardBar(bar) {
    var p = barPct(bar);
    if (p === null || isNaN(p)) return;
    if (upLoad.phase === "transfer" && bar === upBar() && bar.dataset.cerW !== bar.style.width) {
      p = Math.round(p * 4) / 10;
      setBar(bar, p);
    }
    var m = barMax.get(bar) || 0;
    if (p < m) setBar(bar, m);
    else barMax.set(bar, p);
  }
  new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      var t = muts[i].target;
      if (t.classList && t.classList.contains("progress-bar")) guardBar(t);
    }
  }).observe(document.documentElement, { subtree: true, attributes: true, attributeFilter: ["style"] });
  document.addEventListener("change", function (e) {
    if (e.target && e.target.id === "data_upload") {
      var bar = upBar();
      if (bar) { barMax.delete(bar); delete bar.dataset.cerW; }
      upLoad.phase = "transfer"; upLoad.since = Date.now();
    }
  }, true);
  function onUploadStage(m) {
    var el = document.getElementById("data_upload_progress");
    var bar = upBar();
    if (!el || !bar) return;
    if (m.reset) {
      upLoad.phase = "idle";
      barMax.delete(bar);
      setBar(bar, 0);
      bar.textContent = "";
      el.style.visibility = "hidden";
      return;
    }
    if (upLoad.phase === "idle") { barMax.delete(bar); upLoad.since = Date.now(); }
    upLoad.phase = m.pct >= 100 ? "idle" : "stages";
    el.style.display = "block";
    el.style.visibility = "visible";
    bar.style.transition = "width 0.3s ease";
    setBar(bar, m.pct);
    guardBar(bar);
    if (m.text) bar.textContent = m.text;
  }
  (function hookUploadStage() {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler("ceres_upload_stage", onUploadStage);
    else setTimeout(hookUploadStage, 50);
  })();

  var preIdleMs = 1200, preLastUser = Date.now();
  ["pointerdown", "keydown", "wheel", "input", "change"].forEach(function (ev) {
    document.addEventListener(ev, function () { preLastUser = Date.now(); }, true);
  });
  function onPreload(m) {
    if (!m || m.nxt > m.n) return;
    (function wait() {

      var busy = document.documentElement.classList.contains("shiny-busy") ||
                 (upLoad.phase !== "idle" && Date.now() - (upLoad.since || 0) < 30000);
      if (busy || Date.now() - preLastUser < preIdleMs) { setTimeout(wait, 250); return; }
      Shiny.setInputValue("ceres_preload", m.nxt, { priority: "event" });
    })();
  }
  (function hookPreload() {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler("ceres_preload", onPreload);
    else setTimeout(hookPreload, 50);
  })();

  var MS_CFG = {
    "# Variables":            { s: "Variables",   hi: false, int: true },
    "R-Squared":              { s: "R²",      hi: true,  pct: true },
    "Adj. R-Squared":         { s: "Adj. R²", hi: true,  pct: true },
    "Standard Error":         { s: "Std error",   hi: false },
    "Coeff Significance (%)": { s: "Coeff. sig.", hi: true,  pct: true },
    "Geometric CV":           { s: "Geo. CV",     hi: false, pct: true },
    "Multiplicative Error":   { s: "Mult. error", hi: false, pct: true },
    "CV (%)":                 { s: "CV",          hi: false, pct: true },
    "MAPE (%)":               { s: "MAPE",        hi: false, pct: true },
    "Max VIF":                { s: "Max VIF",     hi: false },
    "Weighted IC (%)":        { s: "Weighted IC", hi: true,  wic: true }
  };

  var MS_RULES = [
    { id: "r2",  col: "Adj. R-Squared",         op: ">=", pct: true, s: "Adj. R²", v: 70,  on: false },
    { id: "sig", col: "Coeff Significance (%)", op: ">=", pct: true, s: "Coeff. sig.", v: 75,  on: false },
    { id: "vif", col: "Max VIF",                op: "<",             s: "Max VIF",     v: 5,   on: false },
    { id: "nv",  col: "# Variables",            op: "<=",            s: "Variables",   v: 3,   on: false },
    { id: "se",  col: "Standard Error",         op: "<=",            s: "Std error",   v: 0.5, on: false }
  ];
  var MS_PENCIL = '<span class="c-ms-edit" title="Change the value" aria-hidden="true"><svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><path d="M11.5 2.5l2 2L6 12l-3 1 1-3z"/><path d="M10 4l2 2"/></svg></span>';
  var MS_OPS = { ">=": "≥", "<": "<", "<=": "≤" };
  var MS_SLIDER_MAX = 50;
  var MS_CONTEXT_MAX = 3000;
  var MS_SOLID_MAX = 25;
  var MS = {};
  var MS_PENDING_UI = {};

  function msArr(x) { return x == null ? [] : (Array.isArray(x) ? x : [x]); }
  function msFmt(cfg, v) {
    if (v == null || !isFinite(v)) return "—";
    if (cfg.pct) return (100 * v).toFixed(1) + "%";
    if (cfg.int) return String(Math.round(v * 10) / 10);
    if (cfg.wic) return v.toFixed(1);
    if (Math.abs(v) >= 1000) return Math.round(v).toLocaleString();
    return String(Number(v.toPrecision(4)));
  }
  function msEsc(s) {
    return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; });
  }

  function msData(msg) {
    var cols = msArr(msg.cols), vals = msArr(msg.values).map(msArr), names = msArr(msg.names).map(String);
    var N = names.length, meas = [], byCol = {};

    var capped = msg.total != null, total = capped ? +msg.total : N;
    var ctx = capped ? msArr(msg.ctx).map(function (v) { return !!v; }) : [];
    var at = function (key, j) { var a = msArr(msg[key]); return a[j]; };
    cols.forEach(function (c, j) {
      var x = (vals[j] || []).map(function (v) { return (v == null || !isFinite(+v)) ? null : +v; });
      if (!x.some(function (v) { return v != null; })) return;
      var cfg = Object.assign({ s: c, hi: true }, MS_CFG[c] || {});
      var idx = [];
      for (var i = 0; i < N; i++) if (x[i] != null) idx.push(i);
      idx.sort(function (a, b) { return cfg.hi ? x[b] - x[a] : x[a] - x[b]; });
      var rank = new Array(N).fill(idx.length + 1);
      idx.forEach(function (i, k) {
        rank[i] = (k > 0 && x[i] === x[idx[k - 1]]) ? rank[idx[k - 1]] : k + 1;
      });
      var m = { col: c, cfg: cfg, x: x, rank: rank, order: idx,
                lo: x[idx[cfg.hi ? idx.length - 1 : 0]], hi: x[idx[cfg.hi ? 0 : idx.length - 1]] };
      if (capped && at("ranks", j) != null) {
        m.rank = msArr(at("ranks", j)).map(Number);
        var rg = msArr(at("range", j)).map(Number); m.lo = rg[0]; m.hi = rg[1];
        m.best = +at("best", j); m.worst = +at("worst", j); m.worstRank = +at("worst_rank", j);
        m.bandRank = msArr(at("band_rank", j)).map(Number); m.bandRaw = msArr(at("band_raw", j)).map(Number);
      }
      meas.push(m); byCol[c] = m;
    });
    var wic = byCol["Weighted IC (%)"] || byCol["Adj. R-Squared"] || meas[meas.length - 1];
    return { N: N, total: total, capped: capped, cap: capped ? +msg.cap : null, ctx: ctx,
             names: names, meas: meas, byCol: byCol, wic: wic };
  }

  function msDefaults(d) {
    var ui = { n: Math.min(50, d.total), basis: "wic", mode: "rank", ctx: "bands", rules: {} };
    MS_RULES.forEach(function (r) { ui.rules[r.id] = { on: r.on, v: r.v }; });

    var se = d.byCol["Standard Error"];
    if (se && se.order.length && se.x[se.order[se.order.length - 1]] > 5)
      ui.rules.se.v = Number(se.x[se.order[Math.floor(se.order.length / 2)]].toPrecision(2));
    return ui;
  }
  function msMergeUi(base, ui) {
    if (!ui || typeof ui !== "object") return base;
    var out = { n: ui.n != null ? ui.n : base.n,
                basis: ui.basis === "all" ? "all" : "wic",
                mode: ui.mode === "raw" ? "raw" : "rank",
                ctx: ui.ctx === "lines" ? "lines" : "bands",
                rules: Object.assign({}, base.rules) };
    Object.keys((ui && ui.rules) || {}).forEach(function (k) {
      if (out.rules[k]) out.rules[k] = { on: !!ui.rules[k].on, v: +ui.rules[k].v };
    });
    if (out.n !== "all") out.n = Math.max(1, Math.round(+out.n || 50));
    return out;
  }

  function msPass(w, i) {
    return MS_RULES.every(function (r) {
      var u = w.ui.rules[r.id], m = w.data.byCol[r.col];
      if (!u || !u.on || !m) return true;
      var x = m.x[i];
      if (x == null) return false;
      var t = r.pct ? u.v / 100 : u.v;
      return r.op === ">=" ? x >= t - 1e-9 : (r.op === "<" ? x < t : x <= t + 1e-9);
    });
  }
  function msPick(w) {
    var d = w.data, ui = w.ui;
    var want = ui.n === "all" ? d.total : ui.n;
    if (d.capped) want = Math.min(want, d.cap);
    var all = want >= d.total, n = all ? d.total : want;
    var pass = [], cand = [];
    for (var i = 0; i < d.N; i++) {
      pass.push(!d.ctx[i] && msPass(w, i));
      if (pass[i] && (ui.basis !== "all" || all || d.meas.every(function (m) { return m.rank[i] <= n; }))) cand.push(i);
    }
    var wx = d.wic ? d.wic.x : [];
    cand.sort(function (a, b) {
      var va = wx[a], vb = wx[b];
      if (va == null) return vb == null ? a - b : 1;
      if (vb == null) return -1;
      return d.wic.cfg.hi ? vb - va : va - vb;
    });
    return { pass: pass, top: (ui.basis === "wic" && !all) ? cand.slice(0, n) : cand };
  }

  function msTheme() {
    var dark = document.documentElement.getAttribute("data-ceres-theme") === "dark";
    var acc = (tok("--c-accent-rgb") || "47, 111, 228").split(",").map(function (s) { return +s; });
    return {
      dark: dark, blue: acc,
      greyEnd: dark ? [110, 122, 140] : [148, 163, 184],
      context: dark ? "rgba(138,150,168,0.30)" : "rgba(148,163,184,0.45)",
      failing: dark ? "rgba(93,104,120,0.22)" : "rgba(203,213,225,0.35)",
      axis: tok("--c-line-strong") || "rgba(15,23,42,0.14)",
      tick: tok("--c-faint") || "#94a3b8", tickStrong: tok("--c-ink-2") || "#334155",
      edge: dark ? "rgba(11,15,23,0.9)" : "#ffffff"
    };
  }

  function msSend(w, top) {
    if (!window.Shiny || !window.Shiny.setInputValue) return;
    window.Shiny.setInputValue(w.id + "_state", {
      v: w.v, keep: top.map(function (i) { return i + 1; }).sort(function (a, b) { return a - b; }), ui: w.ui
    });
  }

  function msHeader(w) {
    var d = w.data, ui = w.ui;
    var smax = d.total > MS_SLIDER_MAX ? MS_SLIDER_MAX + 1 : d.total;
    var sval = ui.n === "all" ? smax : Math.min(ui.n, smax);
    var rules = MS_RULES.filter(function (r) { return d.byCol[r.col]; }).map(function (r) {
      return '<button type="button" class="c-ms-rule" data-r="' + r.id + '" title="Click: on / off \u00b7 pencil (or hover / long-press): change the value">' +
             '<span class="c-ms-rule-txt"></span>' + MS_PENCIL + '</button>';
    }).join("");
    w.el.innerHTML =
      '<div class="c-ms-head">' +
        '<span class="c-ms-k">Top</span>' +
        '<input type="range" class="c-ms-range" min="1" max="' + smax + '" value="' + sval + '" aria-label="Number of models to keep">' +
        '<b class="c-ms-n"></b>' +
        '<span class="c-ms-seg" data-k="basis">' +
          '<button type="button" data-v="wic" title="The best N models by Weighted IC">WIC</button>' +
          '<button type="button" data-v="all" title="Models ranked in the top N on every measure">All measures</button></span>' +
        '<span class="c-ms-sep"></span>' +
        '<span class="c-ms-seg" data-k="mode">' +
          '<button type="button" data-v="rank" title="Lines placed by rank on each measure (1 = best, at the top)">Rank</button>' +
          '<button type="button" data-v="raw" title="Lines placed by value, each axis from its lowest to its highest value">Values</button></span>' +
        '<span class="c-ms-sep"></span>' +
        '<span class="c-ms-k">Others as</span>' +
        '<span class="c-ms-seg" data-k="ctx">' +
          '<button type="button" data-v="bands" title="The models beyond the solid lines as shaded bands: where the middle 50% (darker) and 90% (lighter) sit on each axis">Bands</button>' +
          '<button type="button" data-v="lines" title="The models beyond the solid lines as faint lines (fainter the more there are)">Lines</button></span>' +

        '<span class="c-ms-info" tabindex="0" role="button" aria-label="What the chart is showing">' +
          '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" aria-hidden="true">' +
          '<circle cx="8" cy="8" r="6.6"/><path d="M8 7.3v4"/><circle cx="8" cy="4.9" r="0.45" fill="currentColor"/></svg>' +
          '<span class="c-ms-cap" role="tooltip"></span></span>' +
      '</div>' +
      '<div class="c-ms-head c-ms-head-rules"><span class="c-ms-k">Rules</span><span class="c-ms-rules">' + rules + '</span></div>' +
      '<div class="c-ms-plot"></div>';
    w.plot = w.el.querySelector(".c-ms-plot");
    msSyncHeader(w);
    msWire(w, smax);
  }
  function msRuleText(r, u) {

    return r.s + " " + MS_OPS[r.op] + " " + u.v + (r.pct ? "%" : "");
  }
  function msSyncHeader(w) {
    var ui = w.ui, el = w.el;
    el.querySelector(".c-ms-n").textContent = ui.n === "all" ? "All" : String(ui.n);
    el.querySelectorAll('.c-ms-seg[data-k="basis"] button').forEach(function (b) { b.classList.toggle("c-on", b.dataset.v === ui.basis); });
    el.querySelectorAll('.c-ms-seg[data-k="mode"] button').forEach(function (b) { b.classList.toggle("c-on", b.dataset.v === ui.mode); });
    el.querySelectorAll('.c-ms-seg[data-k="ctx"] button').forEach(function (b) { b.classList.toggle("c-on", b.dataset.v === (ui.ctx || "bands")); });
    el.querySelectorAll(".c-ms-rule").forEach(function (b) {
      var r = MS_RULES.filter(function (x) { return x.id === b.dataset.r; })[0], u = ui.rules[r.id];
      var txt = b.querySelector(".c-ms-rule-txt");
      if (txt) txt.textContent = msRuleText(r, u); else b.textContent = msRuleText(r, u);
      b.classList.toggle("c-on", !!u.on);
    });
  }

  var msPopEl = null, msPopFor = null;
  function msPop() {
    if (msPopEl) return msPopEl;
    msPopEl = document.createElement("div");
    msPopEl.className = "c-ms-pop";
    document.body.appendChild(msPopEl);
    msPopEl.addEventListener("mouseleave", function () { msPopEl._t = setTimeout(msHidePop, 260); });
    msPopEl.addEventListener("mouseenter", function () { clearTimeout(msPopEl._t); });
    document.addEventListener("pointerdown", function (e) {
      if (msPopEl.classList.contains("c-open") && !msPopEl.contains(e.target) && !(e.target.closest && e.target.closest(".c-ms-rule"))) msHidePop();
    });
    document.addEventListener("keydown", function (e) { if (e.key === "Escape" || (e.key === "Enter" && msPopEl.contains(e.target))) msHidePop(); });
    return msPopEl;
  }
  function msHidePop() { if (msPopEl) msPopEl.classList.remove("c-open"); msPopFor = null; }
  function msShowPop(w, chip) {
    var r = MS_RULES.filter(function (x) { return x.id === chip.dataset.r; })[0], u = w.ui.rules[r.id];
    var p = msPop();
    clearTimeout(p._t);
    if (msPopFor === chip && p.classList.contains("c-open")) return;
    msPopFor = chip;
    p.innerHTML = '<div class="c-ms-pop-k">' + msEsc(r.s) + '</div><label>' + MS_OPS[r.op] +
      ' <input type="number" step="any" value="' + u.v + '">' + (r.pct ? " %" : "") + "</label>";
    var b = chip.getBoundingClientRect();
    p.style.left = (b.left + window.scrollX) + "px";
    p.style.top = (b.bottom + window.scrollY + 6) + "px";
    p.classList.add("c-open");
    var inp = p.querySelector("input");
    inp.addEventListener("input", function () {
      if (inp.value === "" || !isFinite(+inp.value)) return;
      u.v = +inp.value; u.on = true;
      msSyncHeader(w); msDraw(w);
    });
    setTimeout(function () { try { inp.focus({ preventScroll: true }); inp.select(); } catch (e) {   } }, 0);
  }

  function msWire(w, smax) {
    var el = w.el, range = el.querySelector(".c-ms-range");

    range.addEventListener("input", function () {
      w.ui.n = (w.data.total > MS_SLIDER_MAX && +range.value === smax) ? "all" : +range.value;
      msSyncHeader(w);
    });
    range.addEventListener("change", function () { msDraw(w); });
    el.querySelectorAll(".c-ms-seg").forEach(function (seg) {
      seg.addEventListener("click", function (e) {
        var b = e.target.closest("button"); if (!b) return;
        var k = seg.dataset.k;
        w.ui[k] = b.dataset.v;
        msSyncHeader(w); msDraw(w);
      });
    });
    var rules = el.querySelector(".c-ms-rules"), hoverT = null, lpT = null, lp = false;
    rules.addEventListener("mouseover", function (e) {
      var c = e.target.closest(".c-ms-rule"); if (!c) return;
      clearTimeout(hoverT); hoverT = setTimeout(function () { msShowPop(w, c); }, 450);
    });
    rules.addEventListener("mouseout", function (e) {
      var c = e.target.closest(".c-ms-rule"); if (!c || c.contains(e.relatedTarget)) return;
      clearTimeout(hoverT);
      if (msPopEl && msPopEl.classList.contains("c-open") && !msPopEl.contains(e.relatedTarget))
        msPopEl._t = setTimeout(msHidePop, 300);
    });
    rules.addEventListener("pointerdown", function (e) {
      var c = e.target.closest(".c-ms-rule"); if (!c) return;
      lp = false; lpT = setTimeout(function () { lp = true; msShowPop(w, c); }, 450);
    });
    rules.addEventListener("pointerup", function () { clearTimeout(lpT); });
    rules.addEventListener("pointerleave", function () { clearTimeout(lpT); });
    rules.addEventListener("click", function (e) {
      var c = e.target.closest(".c-ms-rule"); if (!c) return;
      if (lp) { lp = false; return; }
      if (e.target.closest(".c-ms-edit")) { clearTimeout(hoverT); msShowPop(w, c); return; }
      clearTimeout(hoverT); msHidePop();
      var u = w.ui.rules[c.dataset.r]; u.on = !u.on;

      c.classList.add("c-ms-clicked");
      c.addEventListener("mouseleave", function () { c.classList.remove("c-ms-clicked"); }, { once: true });
      if (e.detail > 0) c.blur();
      msSyncHeader(w); msDraw(w);
    });
    rules.addEventListener("contextmenu", function (e) {
      var c = e.target.closest(".c-ms-rule"); if (!c) return;
      e.preventDefault(); msShowPop(w, c);
    });
  }

  function msSample(list, max) {
    if (list.length <= max) return list;
    var out = [], step = list.length / max;
    for (var k = 0; k < max; k++) out.push(list[Math.floor(k * step)]);
    return out;
  }

  function msDraw(w) {
    if (!w.data) return;
    var pick = msPick(w);
    msSend(w, pick.top);
    if (!w.plot) return;

    if (!window.Plotly || !window.Plotly.react) { w.dirty = true; msWaitPlotly(); return; }
    if (w.plot.offsetParent === null) { w.dirty = true; return; }
    w.dirty = false;
    var d = w.data, ui = w.ui, th = msTheme(), N = d.total, NR = d.N, isRank = ui.mode !== "raw";
    var xs = d.meas.map(function (m) { return m.cfg.s; });
    var rankY = function (r) { return N > 1 ? 1 - (r - 1) / (N - 1) : 0.5; };
    var yOf = function (m, i) {
      if (isRank) return rankY(m.rank[i]);
      var v = m.x[i];
      if (v == null) return null;
      var lo = Math.min(m.lo, m.hi), hi = Math.max(m.lo, m.hi);
      return hi > lo ? (v - lo) / (hi - lo) : 0.5;
    };
    var cell = function (m, i) { return "<b>" + msFmt(m.cfg, m.x[i]) + "</b>  ·  rank " + m.rank[i] + " of " + N; };
    var pos = new Map(pick.top.map(function (i, k) { return [i, k]; })), T = pick.top.length;

    var blue = th.blue.join(","), grey = th.greyEnd.join(",");
    var solidN = Math.min(T, MS_SOLID_MAX);
    var solid = pick.top.slice(0, solidN), cloud = pick.top.slice(solidN);
    var others = [];
    for (var q = 0; q < NR; q++) if (!pos.has(q)) others.push(q);
    var colOf = function (k) {
      var f = solidN > 1 ? k / (solidN - 1) : 0;
      return { col: "rgb(" + blue + ")", w: 4.2 - 1.4 * f };
    };
    var smooth = isRank ? 0.6 : 0.45;
    var merged = function (ids, col, width, hover) {
      var x = [], y = [], text = [];
      ids.forEach(function (i) {
        d.meas.forEach(function (m, j) {
          x.push(xs[j]); y.push(yOf(m, i));
          text.push("<b>" + msEsc(d.names[i]) + "</b><br>" + m.cfg.s + ": " + cell(m, i));
        });
        x.push(null); y.push(null); text.push(null);
      });
      return { type: "scatter", mode: "lines", x: x, y: y, text: text, connectgaps: false,
               hoverinfo: hover ? "text" : "skip", hovertemplate: hover ? "%{text}<extra></extra>" : undefined,
               line: { width: width, color: col, shape: "spline", smoothing: smooth } };
    };
    var hazeAlpha = function (n, base) { return Math.max(0.012, Math.min(0.35, base / Math.max(1, n))); };
    var quant = function (arr, p) {
      var a = arr.filter(function (v) { return v != null && isFinite(v); }).sort(function (u, v) { return u - v; });
      if (!a.length) return null;
      var k = (a.length - 1) * p, f = Math.floor(k), c = Math.min(f + 1, a.length - 1);
      return a[f] + (a[c] - a[f]) * (k - f);
    };
    var BAND_P = [0.05, 0.25, 0.5, 0.75, 0.95];
    var bandTraces = function (ids, rgb, strong, whole) {

      var at = whole
        ? function (p) { var k = BAND_P.indexOf(p); return d.meas.map(function (m) { var b = isRank ? m.bandRank : m.bandRaw; return b && b.length === 5 ? b[k] : null; }); }
        : function (p) { return d.meas.map(function (m) { return quant(ids.map(function (i) { return yOf(m, i); }), p); }); };
      var pair = function (lo, hi, alpha) {
        return [
          { type: "scatter", mode: "lines", x: xs, y: at(lo), line: { width: 0, shape: "spline", smoothing: smooth }, hoverinfo: "skip" },
          { type: "scatter", mode: "lines", x: xs, y: at(hi), fill: "tonexty", fillcolor: "rgba(" + rgb + "," + alpha + ")",
            line: { width: 0, shape: "spline", smoothing: smooth }, hoverinfo: "skip" }];
      };
      return pair(0.05, 0.95, strong ? 0.12 : 0.08).concat(pair(0.25, 0.75, strong ? 0.22 : 0.12), [
        { type: "scatter", mode: "lines", x: xs, y: at(0.5), hoverinfo: "skip",
          line: { color: "rgba(" + rgb + "," + (strong ? 0.7 : 0.5) + ")", width: 1.5, dash: "dash", shape: "spline", smoothing: smooth } }]);
    };
    var useBands = (ui.ctx || "bands") === "bands";
    var traces = [];

    [[others, grey, false, 14], [cloud, blue, true, 40]].forEach(function (g) {
      var ids = g[0]; if (!ids.length) return;
      var whole = d.capped && g[0] === others;
      if (useBands && (whole || ids.length >= 5)) traces = traces.concat(bandTraces(ids, g[1], g[2], whole));
      else traces.push(merged(msSample(ids, MS_CONTEXT_MAX), "rgba(" + g[1] + "," + hazeAlpha(ids.length, g[3]) + ")", g[2] ? 1.2 : 1, g[2]));
    });
    solid.slice().reverse().forEach(function (i) {
      var k = pos.get(i), c = colOf(k);
      traces.push({ type: "scatter", mode: "lines+markers", x: xs, y: d.meas.map(function (m) { return yOf(m, i); }),
        connectgaps: false, line: { width: c.w, color: c.col, shape: "spline", smoothing: smooth },
        marker: { size: c.w + 3, color: c.col, line: { color: th.edge, width: 1.5 } },
        text: d.meas.map(function (m) { return cell(m, i); }),
        hovertemplate: "<b>#" + (k + 1) + " " + msEsc(d.names[i]) + "</b><br>%{x}: %{text}<extra></extra>" });
    });

    var capEl = w.el.querySelector(".c-ms-cap");
    if (capEl) {
      var fmtN = function (n) { return Number(n).toLocaleString(); };
      var row = function (h) { return "<span class=\"c-ms-cap-row\">" + h + "</span>"; };

      var allN = !d.capped && (w.ui.n === "all" || w.ui.n >= d.N);
      var what = allN ? "Every model that passes the rules"
               : w.ui.basis === "all" ? "Models in the top " + fmtN(w.ui.n) + " on every measure that pass the rules"
               : "The best " + fmtN(w.ui.n === "all" ? Math.min(N, d.cap || N) : w.ui.n) + " by Weighted IC that pass the rules";
      capEl.innerHTML = row("<b>" + fmtN(T) + "</b> selected") +
        "<span class=\"c-ms-cap-note\">" + what + "; the table below lists them.</span>" +
        (cloud.length ? row("<b>" + fmtN(solidN) + "</b> best drawn solid") +
                        row("<b>" + fmtN(cloud.length) + "</b> more selected, in blue " + (useBands ? "bands" : "lines"))
                      : row(T ? "all drawn solid" : "none drawn")) +
        row("<b>" + fmtN(d.capped ? N - T : others.length) + "</b> not selected, in grey") +

        "<span class=\"c-ms-cap-tip\">Click a model on the map, or a row in the table, to see its statistics and residual plots.</span>";
    }

    var shapes = xs.map(function (x) {
      return { type: "line", xref: "x", yref: "y", x0: x, x1: x, y0: -0.02, y1: 1.02, layer: "below", line: { color: th.axis, width: 1 } };
    });
    var ann = [];

    var tick = function (x, y, txt, strong, top) {
      ann.push({ x: x, y: y, xref: "x", yref: "y", text: txt, showarrow: false, xanchor: "right", xshift: -5,
                 yanchor: top ? "bottom" : "middle", yshift: top ? 4 : -5,
                 font: { size: strong ? 10.5 : 9.5, color: strong ? th.tickStrong : th.tick } });
    };
    d.meas.forEach(function (m, j) {
      var k = m.order.length;

      if (isRank && m.best != null) {
        tick(xs[j], rankY(1), msFmt(m.cfg, m.best), true, true);
        if (m.worstRank > 1) tick(xs[j], rankY(m.worstRank), msFmt(m.cfg, m.worst), true, false);
      } else if (isRank) {
        tick(xs[j], rankY(1), msFmt(m.cfg, m.x[m.order[0]]), true, true);
        if (k > 1) tick(xs[j], rankY(m.rank[m.order[k - 1]]), msFmt(m.cfg, m.x[m.order[k - 1]]), true, false);
      } else {
        var lo = Math.min(m.lo, m.hi), hi = Math.max(m.lo, m.hi);
        [0, 1].forEach(function (y) { tick(xs[j], y, msFmt(m.cfg, lo + y * (hi - lo)), true, y === 1); });
      }
    });
    window.Plotly.react(w.plot, traces, {
      showlegend: false, hovermode: "closest", shapes: shapes, annotations: ann, height: 470,

      xaxis: { type: "category", categoryorder: "array", categoryarray: xs, showgrid: false, zeroline: false,
               range: [-0.12, xs.length - 0.88], fixedrange: true, ticks: "", tickfont: { size: 12 } },
      yaxis: { visible: false, range: [-0.07, 1.07], fixedrange: true },
      margin: { t: 14, l: 52, r: 16, b: 40 }
    }, { displayModeBar: false, responsive: true });
  }

  function msOnData(msg) {
    var id = msg && msg.id; if (!id) return;
    var el = document.getElementById(id); if (!el) return;
    var w = MS[id] || (MS[id] = { id: id });
    w.el = el;
    if (msg.clear) {
      w.data = null; w.v = msg.v;
      try { if (w.plot && window.Plotly) window.Plotly.purge(w.plot); } catch (e) {   }
      el.innerHTML = ""; w.plot = null;
      return;
    }
    w.v = msg.v;
    w.data = msData(msg);
    if (!w.data.N || !w.data.meas.length) { el.innerHTML = ""; w.plot = null; return; }
    w.ui = msMergeUi(msDefaults(w.data), MS_PENDING_UI[id] || w.ui);
    delete MS_PENDING_UI[id];
    if (w.ui.n !== "all") w.ui.n = Math.min(w.ui.n, w.data.total);
    msHeader(w);
    msDraw(w);
  }
  function msOnUi(msg) {
    var id = msg && msg.id; if (!id || !msg.ui) return;
    var w = MS[id];
    if (w && w.data) { w.ui = msMergeUi(msDefaults(w.data), msg.ui); msHeader(w); msDraw(w); }
    else MS_PENDING_UI[id] = msg.ui;
  }
  function msRedrawAll(onlyDirty) {
    Object.keys(MS).forEach(function (id) {
      var w = MS[id];
      if (w.data && w.plot && (!onlyDirty || w.dirty)) msDraw(w);
    });
  }
  var msWaiting = false;
  function msWaitPlotly() {
    if (msWaiting) return;
    msWaiting = true;
    (function go(n) {
      if (window.Plotly && window.Plotly.react) { msWaiting = false; msRedrawAll(true); return; }
      if (n > 1200) { msWaiting = false; return; }
      setTimeout(function () { go(n + 1); }, 50);
    })(0);
  }
  document.addEventListener("shown.bs.tab", function () { setTimeout(function () { msRedrawAll(true); }, 30); });
  (function waitForShiny(n) {
    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler("ceres-measures", msOnData);
      window.Shiny.addCustomMessageHandler("ceres-measures-ui", msOnUi);
      return;
    }
    if (n > 600) return;
    setTimeout(function () { waitForShiny(n + 1); }, 25);
  })(0);

  function hintBoundary(el) {

    var chain = [];
    for (var p = el.closest(".tab-pane"); p; p = p.parentElement && p.parentElement.closest(".tab-pane")) chain.push(p);
    return chain.length > 1 ? chain[chain.length - 2] : chain[0] || null;
  }
  function hintAnchor(hint) {
    var stop = hintBoundary(hint);
    for (var el = hint; el && el !== stop && el !== document.body; el = el.parentElement) {
      for (var s = el.previousElementSibling; s; s = s.previousElementSibling) {
        if (s.classList.contains("tab-pane")) continue;
        if (s.tagName === "H4") return s;
        var hs = Array.prototype.filter.call(s.querySelectorAll("h4"), function (h) {
          var pane = h.closest(".tab-pane");
          return !pane || !s.contains(pane);
        });
        if (hs.length) return hs[hs.length - 1];
      }
    }
    for (var n = hint.nextElementSibling; n; n = n.nextElementSibling) {
      var nav = n.matches("ul.nav") ? n : n.querySelector("ul.nav");
      if (nav) return nav;
    }
    return null;
  }
  function hintRelevant(hint) {
    if (hint.closest(".tab-pane:not(.active)")) return false;
    return getComputedStyle(hint).display !== "none";
  }
  var hintIcons = [];
  function tidyHints() {
    Array.prototype.forEach.call(document.querySelectorAll("p.c-hint:not(.c-hint-moved)"), function (hint) {
      if (hint.closest(".well, .modal")) return;

      if (hint.closest(".shiny-html-output")) return;
      var a = hintAnchor(hint);
      if (!a) return;
      var icon = a.querySelector(":scope > .c-info");
      if (!icon) {
        icon = document.createElement("span");
        icon.className = "c-info";

        icon.innerHTML = '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" aria-hidden="true">' +
          '<circle cx="8" cy="8" r="6.6"/><path d="M8 7.3v4"/><circle cx="8" cy="4.9" r="0.45" fill="currentColor"/></svg>';
        icon.tabIndex = 0;
        icon.setAttribute("role", "img");
        icon.setAttribute("aria-label", "About this section");
        icon.ceresHints = [];
        a.appendChild(icon);
        hintIcons.push(icon);
      }
      icon.ceresHints.push(hint);
      hint.classList.add("c-hint-moved");
    });
    hintIcons.forEach(function (icon) {
      icon.ceresHints = icon.ceresHints.filter(function (h) { return document.body.contains(h); });
      var text = icon.ceresHints.filter(hintRelevant).map(function (h) { return h.textContent.trim(); }).filter(Boolean);
      icon.setAttribute("data-tip", text.join("\n\n"));
      icon.classList.toggle("is-empty", !text.length);
    });
  }
  document.addEventListener("shown.bs.tab", function () { setTimeout(tidyHints, 30); });

  var pop = null, popFor = null;
  function showPop(icon) {
    var text = icon.getAttribute("data-tip");
    if (!text) return;
    if (!pop) { pop = document.createElement("div"); pop.className = "c-info-pop"; pop.setAttribute("role", "tooltip"); document.body.appendChild(pop); }
    popFor = icon;
    pop.textContent = text;
    var r = icon.getBoundingClientRect(), w = pop.offsetWidth, h = pop.offsetHeight, m = 8;
    var x = Math.min(Math.max(m, r.left - 10), window.innerWidth - w - m);
    var y = r.bottom + 8;
    if (y + h > window.innerHeight - m) y = Math.max(m, r.top - h - 8);
    pop.style.left = x + "px"; pop.style.top = y + "px";
    pop.classList.add("is-on");
  }
  function hidePop(icon) { if (pop && (!icon || icon === popFor)) { pop.classList.remove("is-on"); popFor = null; } }
  document.addEventListener("mouseover", function (e) { var i = e.target.closest && e.target.closest(".c-info"); if (i) showPop(i); });
  document.addEventListener("mouseout", function (e) { var i = e.target.closest && e.target.closest(".c-info"); if (i && !i.contains(e.relatedTarget)) hidePop(i); });
  document.addEventListener("focusin", function (e) { if (e.target.classList && e.target.classList.contains("c-info")) showPop(e.target); });
  document.addEventListener("focusout", function (e) { if (e.target.classList && e.target.classList.contains("c-info")) hidePop(e.target); });
  window.addEventListener("scroll", function () { hidePop(); }, true);

  document.addEventListener("shown.bs.tab", function () { hidePop(); });
  document.addEventListener("mousedown", function (e) { if (!(e.target.closest && e.target.closest(".c-info"))) hidePop(); });

  var EMPTY = ".tab-pane > .datatables.html-widget-output:empty, .tab-pane > div > .datatables.html-widget-output:empty:only-child, " +
              ".tab-pane .plotly.html-widget-output:empty";
  var emptyTimer = null;
  function tidyEmpties() {
    var seen = [], now = Date.now(), recheck = false;
    Array.prototype.forEach.call(document.querySelectorAll(".c-empty-quiet"), function (el) { el.classList.remove("c-empty-quiet"); });

    Array.prototype.forEach.call(document.querySelectorAll(".plotly.html-widget-output"), function (el) {
      if (el.firstChild || el.offsetParent === null) { delete el.ceresEmptyAt; el.classList.remove("c-empty-ph"); }
    });
    Array.prototype.forEach.call(document.querySelectorAll(EMPTY), function (el) {
      if (el.closest(".well")) return;
      var pane = el.closest(".tab-pane");
      if (seen.indexOf(pane) > -1) { el.classList.add("c-empty-quiet"); return; }
      seen.push(pane);
      if (!el.classList.contains("plotly") || el.offsetParent === null) return;

      if (!el.ceresEmptyAt) el.ceresEmptyAt = now;
      if (now - el.ceresEmptyAt >= 700) el.classList.add("c-empty-ph");
      else recheck = true;
    });
    if (recheck && !emptyTimer) emptyTimer = setTimeout(function () { emptyTimer = null; tidyEmpties(); }, 750);
  }
  document.addEventListener("shown.bs.tab", function () { setTimeout(tidyEmpties, 30); });

  function start() {
    keepLast();
    new MutationObserver(keepLast).observe(document.head, { childList: true });

    var t = null;
    new MutationObserver(function () {
      clearTimeout(t);

      t = setTimeout(function () { placeAll(true); mountMarks(); tidyHints(); tidyEmpties(); }, 60);
    }).observe(document.body, { childList: true, subtree: true });
    placeAll(false);
    mountBrandMark();
    mountMarks();
    tidyHints();
    tidyEmpties();
  }

  var HELD = {
    plotly: function () { return !!window.Plotly; },
    datatables: function () { var jq = window.jQuery; return !!(jq && jq.fn && jq.fn.DataTable); }
  };
  function holdWidget(name) {
    var reg = window.Shiny && Shiny.outputBindings && Shiny.outputBindings.bindingNames;
    var b = reg && reg[name] && reg[name].binding;
    if (!b) return false;
    if (b.ceresHeld) return true;
    b.ceresHeld = true;
    var render = b.renderValue, ready = HELD[name];
    if (!/renderDependenciesAsync/.test(String(render)) || !Shiny.renderDependenciesAsync) return true;
    var seq = 0;
    b.renderValue = function (el, data) {
      var self = this, args = arguments, key = "ceresHeldSeq_" + name;
      var mine = el[key] = ++seq;
      if (ready() || !data || !data.deps) return render.apply(self, args);
      var draw = function () { if (el[key] === mine) render.apply(self, args); };
      Shiny.renderDependenciesAsync(data.deps).then(function () {
        var t0 = Date.now();
        (function go() {
          if (ready() || Date.now() - t0 > 20000) draw();
          else setTimeout(go, 30);
        })();
      }, draw);
    };
    return true;
  }

  function holdAll() { Object.keys(HELD).forEach(holdWidget); }
  function hookRegister() {
    var ob = window.Shiny && Shiny.outputBindings;
    if (!ob || typeof ob.register !== "function") return false;
    if (!ob.ceresHooked) {
      ob.ceresHooked = true;
      var register = ob.register;
      ob.register = function () {
        var out = register.apply(this, arguments);
        holdAll();
        return out;
      };
    }
    holdAll();
    return true;
  }
  if (!hookRegister()) {
    var hookTries = 0;
    var hookTimer = setInterval(function () { if (hookRegister() || ++hookTries > 600) clearInterval(hookTimer); }, 50);
  }

  if (document.body) start();
  else document.addEventListener("DOMContentLoaded", start);
})();

(function () {
  var MSG = "Adjust the settings on the left to fill this in.";
  function noData() { var el = document.getElementById("data_select"); return !(el && el.value); }
  function panes() {
    var nav = document.getElementById("data_tabs"); if (!nav || !nav.parentElement) return [];
    return Array.prototype.filter.call(nav.parentElement.querySelectorAll(":scope > .tab-content > .tab-pane"),
      function (p) { return p.getAttribute("data-value") !== "Data"; });
  }
  function update() {
    panes().forEach(function (p) {
      if (p.querySelector(":scope > .c-nodata-card")) return;
      var c = document.createElement("div");
      c.className = "c-empty-card c-nodata-card";
      c.innerHTML = '<div class="c-empty-text">' + MSG + "</div>";
      p.classList.add("c-nodata-pane");
      p.insertBefore(c, p.firstChild);
    });
    var w = document.querySelector(".c-welcome");
    if (w && w.offsetHeight > 0) document.documentElement.style.setProperty("--c-welcome-h", w.offsetHeight + "px");
    document.documentElement.classList.toggle("c-nodata", noData());
  }
  function hook() {
    if (!window.jQuery) return setTimeout(hook, 50);
    var $d = window.jQuery(document);
    $d.on("shiny:connected shown.bs.tab", function () { setTimeout(update, 0); });
    $d.on("shiny:inputchanged", function (e) { if (e.name === "data_select") setTimeout(update, 0); });
    $d.on("change", "#data_select", function () { setTimeout(update, 0); });
    window.addEventListener("resize", function () { setTimeout(update, 50); });
    update();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", hook); else hook();
})();

(function () {
  function close() { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue("health_toggle", "collapse", { priority: "event" }); }
  document.addEventListener("click", function (e) {
    if (document.querySelector(".c-hp-pop") && !e.target.closest(".c-hp-anchor")) close();
  }, true);
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape" && document.querySelector(".c-hp-pop")) close();
  });
})();
