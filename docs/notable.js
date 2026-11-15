'use strict';

// "Notable cases": a hand-picked list of well-documented cases from
// data/notable.json, with what the investigating body concluded and links to
// the sources. Uses the helpers defined in app.js ($, esc, getJSON...).

const NOTABLE_STATUS = {
  unexplained: { label: 'Unexplained', color: '#f2c14e' },
  disputed: { label: 'Disputed', color: '#86b6ef' },
  explained: { label: 'Explained', color: '#6cc9a8' },
};

function notablePage() {
  const status = $('#c-status');
  const list = $('#c-list');
  let cases = [];
  let map;
  const markers = new Map();

  const checked = () => [...document.querySelectorAll('#c-filters input:checked')].map((b) => b.value);

  function caseDate(c) {
    return fmtDate(c.date);
  }

  function card(c) {
    const s = NOTABLE_STATUS[c.status] || { label: c.status, color: MUTED };
    const sources = (c.sources || []).filter((x) => /^https?:\/\//.test(x.url))
      .map((x) => `<a href="${esc(x.url)}" rel="noopener">${esc(x.label)}</a>`).join(' · ');
    return `
      <article class="case" id="case-${esc(c.id)}" tabindex="-1">
        <h3>${esc(c.title)}</h3>
        <p class="meta">${esc(caseDate(c))} · ${esc(c.place)}${c.country && !c.place.includes(c.country) ? `, ${esc(c.country)}` : ''}</p>
        <p class="verdict"><span class="badge" style="--badge:${s.color}">${esc(s.label)}</span> ${esc(c.verdict)}</p>
        <p>${esc(c.summary)}</p>
        <p class="why"><strong>Why it matters:</strong> ${esc(c.why)}</p>
        <p class="links">${sources}${sources ? ' · ' : ''}<button type="button" class="linklike" data-show="${esc(c.id)}">Show on map</button></p>
      </article>`;
  }

  function render() {
    const keep = new Set(checked());
    const shown = cases.filter((c) => keep.has(c.status));
    list.innerHTML = shown.length ? shown.map(card).join('') : '<p class="empty">No case with the selected status.</p>';
    $('#c-count').textContent = `${shown.length} of ${cases.length} cases`;
    markers.forEach((m, id) => {
      const c = cases.find((x) => x.id === id);
      if (keep.has(c.status)) m.addTo(map); else m.remove();
    });
  }

  function focus(id, scroll) {
    const c = cases.find((x) => x.id === id);
    const m = markers.get(id);
    if (!c || !m) return;
    if (scroll) {
      $('#c-map').scrollIntoView({ behavior: 'smooth', block: 'center' });
      map.setView([c.lat, c.lng], Math.max(map.getZoom(), 5));
    }
    m.openPopup();
  }

  async function init() {
    setStatus(status, 'Loading the cases…');
    try {
      const data = await getJSON('data/notable.json');
      cases = data.cases.slice().sort((a, b) => a.date.localeCompare(b.date));
      $('#c-updated').textContent = data.updated ? `List reviewed on ${fmtDate(data.updated)}.` : '';
    } catch (err) {
      setStatus(status, `The cases could not be loaded: ${err.message}`, true);
      return;
    }
    setStatus(status, '');

    $('#c-filters').innerHTML = Object.entries(NOTABLE_STATUS).map(([k, s]) => `
      <label class="check"><input type="checkbox" value="${k}" checked>
        <span class="swatch" style="background:${s.color}"></span>${esc(s.label)}
        (${cases.filter((c) => c.status === k).length})</label>`).join('');
    $('#c-filters').addEventListener('change', render);

    map = L.map('c-map', { renderer: L.canvas({ tolerance: TAP_TOLERANCE }), minZoom: 1, worldCopyJump: true })
      .setView([35, -20], 2);
    darkTiles().addTo(map);
    for (const c of cases) {
      const s = NOTABLE_STATUS[c.status] || { color: MUTED, label: c.status };
      const m = L.circleMarker([c.lat, c.lng], {
        radius: 8, color: SURFACE, weight: 1.5, fillColor: s.color, fillOpacity: 0.95,
      }).bindPopup(`<b>${esc(c.title)}</b><br>${esc(caseDate(c))} · ${esc(s.label)}<br>
        <button type="button" class="linklike" data-read="${esc(c.id)}">Read the case</button>`);
      markers.set(c.id, m);
    }
    const legend = L.control({ position: 'bottomright' });
    legend.onAdd = () => {
      const div = L.DomUtil.create('div', 'legend');
      div.innerHTML = `<b>Status</b>${Object.values(NOTABLE_STATUS).map((s) => `
        <div><span class="swatch" style="background:${s.color}"></span>${esc(s.label)}</div>`).join('')}`;
      return div;
    };
    legend.addTo(map);

    // "Show on map" in a card, "Read the case" in a popup
    list.addEventListener('click', (e) => {
      const b = e.target.closest('[data-show]');
      if (b) focus(b.dataset.show, true);
    });
    $('#c-map').addEventListener('click', (e) => {
      const b = e.target.closest('[data-read]');
      if (!b) return;
      const el = document.getElementById(`case-${b.dataset.read}`);
      if (el) { el.scrollIntoView({ behavior: 'smooth', block: 'start' }); el.focus({ preventScroll: true }); }
    });

    render();
  }

  let started = false;
  return {
    show() {
      if (!started) { started = true; init(); } else if (map) map.invalidateSize();
    },
  };
}
