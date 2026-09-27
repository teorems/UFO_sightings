'use strict';

// Chart colours were validated against the #222222 surface (dataviz validator).
const INK = '#e9ecef';
const MUTED = '#adb5bd';
const GRID = '#3a3a3a';
const SURFACE = '#222222';
const SERIES = '#3987e5';
// the explanations chart sits next to the classification colours, so it avoids
// their blue to not read as "class B"
const SERIES_ALT = '#199e70';

const GEIPAN_CLASSES = {
  A: 'A: identified',
  B: 'B: probably identified',
  C: 'C: not enough information',
  D: 'D: unexplained after investigation',
};
// one-hue ordinal ramp: dimmest for identified cases, brightest for unexplained
const GEIPAN_COLORS = { A: '#1c5cab', B: '#3987e5', C: '#86b6ef', D: '#cde2fb' };
const UNINFORMATIVE = 'Phénomène identifié non indiqué';

// Points are drawn on a canvas, where a tap only counts inside the circle
// unless the renderer widens it; fingers need a ~40px target, mice much less.
const TAP_TOLERANCE = window.matchMedia('(pointer: coarse)').matches ? 14 : 4;

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
const collator = new Intl.Collator('en', { numeric: true, sensitivity: 'base' });

const $ = (sel, root = document) => root.querySelector(sel);
const esc = (s) => (s == null ? '' : String(s)).replace(/[&<>"']/g, (c) => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
}[c]));
const fmt = (n) => n.toLocaleString('en-US');

async function getJSON(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${url} (${res.status})`);
  return res.json();
}

function fmtDate(t) {
  if (!t) return '';
  const [d, time] = t.split(' ');
  const [y, m, day] = d.split('-');
  const date = `${+day} ${MONTHS[+m - 1]} ${y}`;
  return time && time !== '00:00' ? `${date}, ${time}` : date;
}

function setStatus(el, text, isError = false) {
  el.textContent = text;
  el.classList.toggle('error', isError);
}

// ---- charts ---------------------------------------------------------------

const PLOT_CONFIG = { displayModeBar: false, responsive: true };

function chartLayout(title, subtitle, extra = {}) {
  const text = subtitle
    ? `${esc(title)}<br><span style="font-size:13px;color:${MUTED}">${esc(subtitle)}</span>`
    : esc(title);
  return {
    title: { text, x: 0, xanchor: 'left', font: { size: 16, color: '#ffffff' } },
    paper_bgcolor: SURFACE,
    plot_bgcolor: SURFACE,
    font: { color: INK, family: 'system-ui, -apple-system, "Segoe UI", sans-serif', size: 12 },
    margin: { l: 60, r: 20, t: subtitle ? 76 : 56, b: 48 },
    hoverlabel: { bgcolor: '#333333', bordercolor: '#555555', font: { color: '#ffffff' } },
    xaxis: { gridcolor: GRID, zeroline: false, linecolor: GRID, fixedrange: true },
    yaxis: { gridcolor: GRID, zeroline: false, linecolor: GRID, fixedrange: true },
    ...extra,
  };
}

// horizontal bars, largest at the top
function barChart(el, counts, { title, subtitle, color, unit }) {
  const entries = [...counts].sort((a, b) => a[1] - b[1]);
  const height = Math.max(360, entries.length * 24 + 130);
  el.style.height = `${height}px`;
  Plotly.react(el, [{
    type: 'bar',
    orientation: 'h',
    x: entries.map((e) => e[1]),
    y: entries.map((e) => e[0]),
    marker: { color },
    hovertemplate: `%{y}: %{x:,} ${unit}<extra></extra>`,
  }], chartLayout(title, subtitle, {
    height,
    bargap: 0.3,
    margin: { l: 170, r: 20, t: subtitle ? 76 : 56, b: 48 },
    xaxis: { gridcolor: GRID, zeroline: false, fixedrange: true, title: { text: unit[0].toUpperCase() + unit.slice(1) } },
    yaxis: { showgrid: false, fixedrange: true, automargin: true },
  }), PLOT_CONFIG);
}

// ---- table ----------------------------------------------------------------

function makeTable(el, columns, { pageSize = 25, sortCol = 0, sortDir = -1 } = {}) {
  el.innerHTML = `
    <div class="table-tools">
      <input type="search" placeholder="Search the table" aria-label="Search the table">
      <span class="table-info"></span>
    </div>
    <div class="table-wrap"><table>
      <thead><tr>${columns.map((c, k) => (c.sortable === false
        ? `<th scope="col">${esc(c.label)}</th>`
        : `<th scope="col" class="sortable" tabindex="0" data-k="${k}">${esc(c.label)}</th>`)).join('')}</tr></thead>
      <tbody></tbody>
    </table></div>
    <div class="pager">
      <button type="button" data-step="-1">Previous</button>
      <span class="page-info"></span>
      <button type="button" data-step="1">Next</button>
    </div>`;
  const tbody = $('tbody', el);
  const info = $('.table-info', el);
  const pageInfo = $('.page-info', el);
  const [prev, next] = el.querySelectorAll('.pager button');
  let rows = [];
  let view = [];
  let query = '';
  let page = 0;
  let col = sortCol;
  let dir = sortDir;

  const value = (c, i) => c.get(i);

  function compare(a, b) {
    const c = columns[col];
    const va = value(c, a);
    const vb = value(c, b);
    if (va == null || va === '') return vb == null || vb === '' ? 0 : 1;
    if (vb == null || vb === '') return -1;
    return (typeof va === 'number' ? va - vb : collator.compare(va, vb)) * dir;
  }

  function refresh() {
    const q = query.trim().toLowerCase();
    view = q
      ? rows.filter((i) => columns.some((c) => c.searchable !== false
        && String(value(c, i) ?? '').toLowerCase().includes(q)))
      : rows.slice();
    view.sort(compare);
    page = 0;
    draw();
  }

  function draw() {
    const pages = Math.max(1, Math.ceil(view.length / pageSize));
    page = Math.min(page, pages - 1);
    const start = page * pageSize;
    const slice = view.slice(start, start + pageSize);
    tbody.innerHTML = slice.map((i) => `<tr>${columns.map((c) => {
      const cls = c.nowrap ? ' class="nowrap"' : '';
      const html = c.html ? c.html(i) : esc(c.format ? c.format(i) : value(c, i));
      return `<td${cls}>${html}</td>`;
    }).join('')}</tr>`).join('');
    info.textContent = view.length
      ? `Showing ${fmt(start + 1)}–${fmt(start + slice.length)} of ${fmt(view.length)}`
      : 'No matching rows';
    pageInfo.textContent = `Page ${fmt(page + 1)} of ${fmt(pages)}`;
    prev.disabled = page === 0;
    next.disabled = page >= pages - 1;
    el.querySelectorAll('th.sortable').forEach((th) => {
      const k = +th.dataset.k;
      th.setAttribute('aria-sort', k === col ? (dir > 0 ? 'ascending' : 'descending') : 'none');
    });
  }

  let timer;
  $('input', el).addEventListener('input', (e) => {
    clearTimeout(timer);
    timer = setTimeout(() => { query = e.target.value; refresh(); }, 200);
  });

  function sortBy(th) {
    const k = +th.dataset.k;
    if (k === col) dir = -dir; else { col = k; dir = 1; }
    refresh();
  }
  el.querySelectorAll('th.sortable').forEach((th) => {
    th.addEventListener('click', () => sortBy(th));
    th.addEventListener('keydown', (e) => {
      if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); sortBy(th); }
    });
  });

  el.querySelectorAll('.pager button').forEach((b) => b.addEventListener('click', () => {
    page += +b.dataset.step;
    draw();
  }));

  return { setRows(r) { rows = r; refresh(); } };
}

// ---- shared page plumbing -------------------------------------------------

function darkTiles() {
  return L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 18,
    className: 'dark-tiles',
    attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
  });
}

// wires the Map/Chart/Table tabs of a page; views render lazily when shown
function setupTabs(root, onShow) {
  const buttons = [...root.querySelectorAll('.tabs button')];
  const panels = [...root.querySelectorAll('.panel')];
  let current = 'map';
  function show(name) {
    current = name;
    buttons.forEach((b) => b.setAttribute('aria-selected', String(b.dataset.tab === name)));
    panels.forEach((p) => { p.hidden = p.dataset.panel !== name; });
    onShow(name);
  }
  buttons.forEach((b) => b.addEventListener('click', () => show(b.dataset.tab)));
  return { current: () => current };
}

// ---- NUFORC ---------------------------------------------------------------

function nuforcPage() {
  const root = $('#page-nuforc');
  const status = $('#n-status');
  const country = $('#n-country');
  const from = $('#n-from');
  const to = $('#n-to');
  let D = null;
  let summaries = null;
  let selection = [];
  let shown = null;
  const markers = [];
  const features = [];
  const stale = { chart: true, table: true };
  let map;
  let layer;
  let index = null;
  let table;
  let tabs;

  const place = (i) => [D.city[i], D.state[i], D.country[i]].filter(Boolean).join(', ');
  const reportUrl = (i) => `https://nuforc.org/sighting/?id=${D.id[i]}`;

  function popup(i) {
    const summary = summaries ? summaries[i] : null;
    return `<b>${esc(fmtDate(D.t[i]))}</b><br>${esc(place(i))}<br>`
      + `<i>${esc(D.shape[i] || 'Unknown shape')}</i>${summary ? `<br>${esc(summary)}` : ''}`;
  }

  function showReport(i) {
    shown = i;
    const summary = summaries ? summaries[i] : null;
    const meta = [fmtDate(D.t[i]), D.shape[i] || 'Unknown shape', D.duration[i]].filter(Boolean).join(' · ');
    $('#n-detail').innerHTML = `
      <h3>${esc(place(i) || 'Unknown place')}</h3>
      <p class="meta">${esc(meta)}</p>
      <p>${summary ? esc(summary) : (summaries ? '<span class="hint">No summary.</span>' : '<span class="hint">Loading summary…</span>')}</p>
      <p><a href="${reportUrl(i)}" target="_blank" rel="noopener">Open the full report on NUFORC</a></p>`;
  }

  function marker(i, latlng) {
    if (!markers[i]) {
      const m = L.circleMarker(latlng, {
        radius: 5, color: SURFACE, weight: 1, fillColor: SERIES, fillOpacity: 0.9,
      });
      m.bindPopup(() => popup(i));
      m.on('click', () => showReport(i));
      markers[i] = m;
    }
    return markers[i].setLatLng(latlng);
  }

  // reports sharing one spot (every report of a city has the same coordinates)
  // can't be split by zooming, so they are listed instead, most recent first
  function listPlace(indices) {
    const shownMax = 100;
    const sorted = indices.slice().sort((a, b) => (D.t[b] > D.t[a] ? 1 : -1));
    const where = place(sorted[0]) || 'This place';
    $('#n-detail').innerHTML = `
      <h3>${esc(where)}</h3>
      <p class="meta">${fmt(indices.length)} reports here${indices.length > shownMax ? `, the ${shownMax} most recent below; search the table for the rest` : ''}</p>
      <ul class="report-list">${sorted.slice(0, shownMax).map((i) => `
        <li><button type="button" data-i="${i}">${esc(fmtDate(D.t[i]))} · ${esc(D.shape[i] || 'Unknown shape')}</button></li>`).join('')}
      </ul>`;
    $('#n-detail').querySelectorAll('button[data-i]').forEach((b) => b.addEventListener('click', () => {
      showReport(+b.dataset.i);
      $('#n-detail').scrollIntoView({ block: 'nearest' });
    }));
  }

  function clusterMarker(f, latlng) {
    const n = f.properties.point_count;
    const size = n < 10 ? 'small' : n < 100 ? 'medium' : 'large';
    const m = L.marker(latlng, {
      icon: L.divIcon({
        html: `<div><span>${f.properties.point_count_abbreviated}</span></div>`,
        className: `marker-cluster marker-cluster-${size}`,
        iconSize: L.point(40, 40),
      }),
      keyboard: true,
      title: `${fmt(n)} reports`,
    });
    m.on('click', () => {
      const id = f.properties.cluster_id;
      const zoom = index.getClusterExpansionZoom(id);
      if (zoom <= map.getMaxZoom()) map.setView(latlng, zoom);
      else listPlace(index.getLeaves(id, Infinity).map((leaf) => leaf.properties.i));
    });
    return m;
  }

  // Supercluster groups the whole selection once; only the clusters and points
  // in view become map layers, which keeps 140,000 reports fast on phones
  function drawView() {
    layer.clearLayers();
    if (!index) return;
    const b = map.getBounds();
    const centerLng = map.getCenter().lng;
    for (const f of index.getClusters([b.getWest(), b.getSouth(), b.getEast(), b.getNorth()], Math.round(map.getZoom()))) {
      const [lng, lat] = f.geometry.coordinates;
      // draw on the world copy being viewed
      const latlng = [lat, lng + 360 * Math.round((centerLng - lng) / 360)];
      layer.addLayer(f.properties.cluster ? clusterMarker(f, latlng) : marker(f.properties.i, latlng));
    }
  }

  function drawMap() {
    const points = [];
    let south = 90; let north = -90; let west = 180; let east = -180;
    for (const i of selection) {
      if (D.lat[i] == null || D.lng[i] == null) continue;
      features[i] = features[i] || { type: 'Feature', properties: { i }, geometry: { type: 'Point', coordinates: [D.lng[i], D.lat[i]] } };
      points.push(features[i]);
      south = Math.min(south, D.lat[i]); north = Math.max(north, D.lat[i]);
      west = Math.min(west, D.lng[i]); east = Math.max(east, D.lng[i]);
    }
    index = new Supercluster({ radius: 60, maxZoom: map.getMaxZoom() }).load(points);
    if (country.value !== 'World' && points.length) {
      map.fitBounds([[south, west], [north, east]], { maxZoom: 7, padding: [20, 20] });
    }
    drawView();
  }

  function drawChart() {
    stale.chart = false;
    const el = $('#n-chart');
    $('#n-chart-empty').hidden = selection.length > 0;
    el.hidden = selection.length === 0;
    if (!selection.length) return;
    const counts = new Map();
    for (const i of selection) {
      const s = D.shape[i] || 'Unknown';
      counts.set(s, (counts.get(s) || 0) + 1);
    }
    barChart(el, counts, {
      title: `UFO sightings in ${country.value}`,
      subtitle: `${fmtDate(from.value)} – ${fmtDate(to.value)} · by reported shape`,
      color: SERIES,
      unit: 'reports',
    });
  }

  function drawTable() {
    stale.table = false;
    table.setRows(selection);
  }

  function apply() {
    const c = country.value;
    const f = from.value || '0000-00-00';
    const t = to.value || '9999-99-99';
    selection = [];
    for (let i = 0; i < D.id.length; i += 1) {
      const d = D.t[i].slice(0, 10);
      if (d >= f && d <= t && (c === 'World' || D.country[i] === c)) selection.push(i);
    }
    $('#n-count').textContent = `${fmt(selection.length)} report${selection.length === 1 ? '' : 's'}`;
    stale.chart = true;
    stale.table = true;
    drawMap();
    if (tabs.current() === 'chart') drawChart();
    if (tabs.current() === 'table') drawTable();
  }

  async function init() {
    setStatus(status, 'Loading about 140,000 reports…');
    try {
      D = await getJSON('data/nuforc.json');
    } catch (err) {
      setStatus(status, `Couldn't load the reports: ${err.message}`, true);
      return;
    }
    setStatus(status, '');

    getJSON('data/nuforc_summaries.json')
      .then((s) => { summaries = s; if (shown != null) showReport(shown); })
      .catch(() => { summaries = []; });

    const countries = [...new Set(D.country.filter(Boolean))].sort(collator.compare);
    country.innerHTML = ['World', ...countries].map((c) => `<option>${esc(c)}</option>`).join('');
    const first = D.t[0].slice(0, 10);
    const last = D.t[D.t.length - 1].slice(0, 10);
    [from, to].forEach((el) => { el.min = first; el.max = last; });
    from.value = first;
    to.value = last;
    [country, from, to].forEach((el) => el.addEventListener('change', apply));

    map = L.map('n-map', {
      renderer: L.canvas({ tolerance: TAP_TOLERANCE }), minZoom: 1, maxZoom: 18, worldCopyJump: true,
    }).setView([30, 0], 2);
    darkTiles().addTo(map);
    layer = L.layerGroup().addTo(map);
    map.on('moveend', drawView);

    table = makeTable($('#n-table'), [
      { label: 'Date', get: (i) => D.t[i], format: (i) => fmtDate(D.t[i]), nowrap: true },
      { label: 'City', get: (i) => D.city[i] },
      { label: 'State', get: (i) => D.state[i] },
      { label: 'Country', get: (i) => D.country[i] },
      { label: 'Shape', get: (i) => D.shape[i] },
      { label: 'Duration', get: (i) => D.duration[i] },
      {
        label: 'Report',
        get: (i) => D.id[i],
        html: (i) => `<a href="${reportUrl(i)}" target="_blank" rel="noopener">link</a>`,
        sortable: false,
        searchable: false,
      },
    ]);

    apply();
  }

  tabs = setupTabs(root, (name) => {
    if (!D) return;
    if (name === 'map') map.invalidateSize();
    if (name === 'chart' && stale.chart) drawChart();
    if (name === 'table' && stale.table) drawTable();
  });

  let started = false;
  return {
    show() {
      if (!started) { started = true; init(); } else if (map) map.invalidateSize();
    },
  };
}

// ---- GEIPAN ---------------------------------------------------------------

function francePage() {
  const root = $('#page-france');
  const status = $('#f-status');
  const from = $('#f-from');
  const to = $('#f-to');
  const region = $('#f-region');
  let G = null;
  let selection = [];
  const markers = [];
  const stale = { chart: true, table: true };
  let map;
  let layer;
  let table;
  let tabs;

  const classBoxes = () => [...root.querySelectorAll('#f-classes input')];

  function showCase(i) {
    const cls = GEIPAN_CLASSES[G.class[i]] || G.class[i];
    const expl = G.explanation[i] ? ` (${esc(G.explanation[i])})` : '';
    $('#f-detail').innerHTML = `
      <h3>${esc(G.place[i])}</h3>
      <p class="meta">${esc([G.date[i], G.region[i]].filter(Boolean).join(' · '))}</p>
      <p><strong>${esc(cls)}</strong>${expl}</p>
      <p><em>${esc(G.summary[i])}</em></p>
      <p>${esc(G.details[i])}</p>`;
  }

  function marker(i) {
    if (!markers[i]) {
      const m = L.circleMarker([G.lat[i], G.lng[i]], {
        radius: 6, color: SURFACE, weight: 1, fillColor: GEIPAN_COLORS[G.class[i]] || MUTED, fillOpacity: 0.95,
      });
      m.bindPopup(() => `<b>${esc(G.place[i])}</b><br>${esc(G.date[i])}<br>`
        + `${esc(GEIPAN_CLASSES[G.class[i]] || G.class[i])}`
        + `${G.explanation[i] ? `<br><i>${esc(G.explanation[i])}</i>` : ''}`);
      m.on('click', () => showCase(i));
      markers[i] = m;
    }
    return markers[i];
  }

  function drawMap() {
    layer.clearLayers();
    // D cases last so the rare unexplained ones sit on top of dense areas
    const order = selection.slice().sort((a, b) => G.class[a].localeCompare(G.class[b]));
    for (const i of order) if (G.lat[i] != null && G.lng[i] != null) layer.addLayer(marker(i));
  }

  function drawCharts() {
    stale.chart = false;
    const yearsEl = $('#f-years');
    const explEl = $('#f-explanations');
    const empty = selection.length === 0;
    $('#f-chart-empty').hidden = !empty;
    yearsEl.hidden = empty;
    explEl.hidden = empty;
    if (empty) return;

    const y0 = +from.value;
    const y1 = +to.value;
    const years = [];
    for (let y = y0; y <= y1; y += 1) years.push(y);
    const traces = Object.keys(GEIPAN_CLASSES).map((k) => {
      const counts = new Map(years.map((y) => [y, 0]));
      for (const i of selection) if (G.class[i] === k) counts.set(G.year[i], counts.get(G.year[i]) + 1);
      return {
        type: 'bar',
        name: GEIPAN_CLASSES[k],
        x: years,
        y: years.map((y) => counts.get(y)),
        marker: { color: GEIPAN_COLORS[k], line: { color: SURFACE, width: 0.6 } },
        hovertemplate: `%{x} · ${GEIPAN_CLASSES[k]}: %{y:,} cases<extra></extra>`,
        visible: selection.some((i) => G.class[i] === k) ? true : 'legendonly',
      };
    });
    yearsEl.style.height = '420px';
    Plotly.react(yearsEl, traces, chartLayout('GEIPAN cases per year', `${y0} – ${y1} · by classification`, {
      barmode: 'stack',
      bargap: 0.15,
      height: 420,
      legend: { orientation: 'h', traceorder: 'normal', x: 0, y: -0.12, font: { color: INK } },
      margin: { l: 60, r: 20, t: 76, b: 70 },
      xaxis: { showgrid: false, zeroline: false, linecolor: GRID, fixedrange: true },
      yaxis: { gridcolor: GRID, zeroline: false, fixedrange: true, title: { text: 'Cases' } },
    }), PLOT_CONFIG);

    const counts = new Map();
    for (const i of selection) {
      const e = G.explanation[i];
      if ((G.class[i] === 'A' || G.class[i] === 'B') && e && e !== UNINFORMATIVE) counts.set(e, (counts.get(e) || 0) + 1);
    }
    const top = new Map([...counts].sort((a, b) => b[1] - a[1]).slice(0, 15));
    $('#f-expl-empty').hidden = top.size > 0;
    explEl.hidden = top.size === 0;
    if (!top.size) return;
    barChart(explEl, top, {
      title: 'Most common explanations',
      subtitle: 'Identified cases (classes A and B), top 15',
      color: SERIES_ALT,
      unit: 'cases',
    });
  }

  function drawTable() {
    stale.table = false;
    table.setRows(selection);
  }

  function apply() {
    const y0 = +from.value;
    const y1 = +to.value;
    const classes = new Set(classBoxes().filter((b) => b.checked).map((b) => b.value));
    const r = region.value;
    selection = [];
    for (let i = 0; i < G.id.length; i += 1) {
      if (G.year[i] >= y0 && G.year[i] <= y1 && classes.has(G.class[i]) && (r === 'All' || G.region[i] === r)) {
        selection.push(i);
      }
    }
    $('#f-count').textContent = `${fmt(selection.length)} case${selection.length === 1 ? '' : 's'}`;
    stale.chart = true;
    stale.table = true;
    drawMap();
    if (tabs.current() === 'chart') drawCharts();
    if (tabs.current() === 'table') drawTable();
  }

  async function init() {
    setStatus(status, 'Loading GEIPAN cases…');
    try {
      G = await getJSON('data/geipan.json');
    } catch (err) {
      setStatus(status, `Couldn't load the cases: ${err.message}`, true);
      return;
    }
    setStatus(status, '');

    const years = [...new Set(G.year)].sort((a, b) => a - b);
    const options = years.map((y) => `<option>${y}</option>`).join('');
    from.innerHTML = options;
    to.innerHTML = options;
    from.value = years[0];
    to.value = years[years.length - 1];

    $('#f-classes').innerHTML = Object.entries(GEIPAN_CLASSES).map(([k, label]) => `
      <label class="check"><input type="checkbox" value="${k}" checked>
        <span class="swatch" style="background:${GEIPAN_COLORS[k]}"></span>${esc(label)}</label>`).join('');

    const regions = [...new Set(G.region.filter(Boolean))].sort(collator.compare);
    region.innerHTML = ['All', ...regions].map((x) => `<option>${esc(x)}</option>`).join('');

    [from, to, region, ...classBoxes()].forEach((el) => el.addEventListener('change', apply));

    // mainland France fills the map whatever the screen size; overseas cases
    // are there when zooming out
    map = L.map('f-map', { renderer: L.canvas({ tolerance: TAP_TOLERANCE }), minZoom: 1 }).fitBounds([[41.3, -5.2], [51.1, 9.6]]);
    darkTiles().addTo(map);
    layer = L.layerGroup().addTo(map);
    const legend = L.control({ position: 'bottomright' });
    legend.onAdd = () => {
      const div = L.DomUtil.create('div', 'legend');
      div.innerHTML = `<b>GEIPAN class</b>${Object.entries(GEIPAN_CLASSES).map(([k, label]) => `
        <div><span class="swatch" style="background:${GEIPAN_COLORS[k]}"></span>${esc(label)}</div>`).join('')}`;
      return div;
    };
    legend.addTo(map);

    table = makeTable($('#f-table'), [
      { label: 'Date', get: (i) => G.date[i], nowrap: true },
      { label: 'Place', get: (i) => G.place[i] },
      { label: 'Region', get: (i) => G.region[i] },
      { label: 'Class', get: (i) => G.class[i] },
      { label: 'Explanation', get: (i) => G.explanation[i] },
      { label: 'Summary', get: (i) => G.summary[i] },
    ]);

    apply();
  }

  tabs = setupTabs(root, (name) => {
    if (!G) return;
    if (name === 'map') map.invalidateSize();
    if (name === 'chart' && stale.chart) drawCharts();
    if (name === 'table' && stale.table) drawTable();
  });

  let started = false;
  return {
    show() {
      if (!started) { started = true; init(); } else if (map) map.invalidateSize();
    },
  };
}

// ---- routing ----------------------------------------------------------------

const pages = { nuforc: nuforcPage(), france: francePage(), sky: skyPage() };
const HASH_TO_PAGE = { '#france': 'france', '#worldwide': 'nuforc', '#sky': 'sky' };

function route() {
  const name = HASH_TO_PAGE[location.hash] || 'nuforc';
  Object.keys(pages).forEach((key) => {
    $(`#page-${key}`).hidden = key !== name;
    if (key !== name && pages[key].hide) pages[key].hide();
  });
  document.querySelectorAll('.pages a').forEach((a) => {
    if (a.dataset.page === name) a.setAttribute('aria-current', 'page');
    else a.removeAttribute('aria-current');
  });
  pages[name].show();
}

window.addEventListener('hashchange', route);
route();
