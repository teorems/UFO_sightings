'use strict';

// "Sky tonight": what someone looking up could be seeing right now. Satellite
// positions are propagated in the browser (satellite.js) from orbital elements
// refreshed daily into data/sky/satellites.json; planets and the Sun come from
// Astronomy Engine. Uses the helpers defined in app.js ($, esc, fmt, getJSON...).

const DEFAULT_PLACE = { lat: 48.857, lon: 2.352, label: 'Paris (default)' };
const MIN_ELEVATION = 10; // degrees above the horizon to count as "up"
const DARK_SUN = -6; // Sun this far below the horizon: dark enough to see satellites
const TRACKED = [
  { norad: 25544, label: 'ISS', note: 'International Space Station' },
  { norad: 48274, label: 'Tiangong', note: 'Chinese space station' },
  { norad: 20580, label: 'Hubble', note: 'Hubble Space Telescope' },
];
const PLANETS = ['Venus', 'Jupiter', 'Mars', 'Saturn', 'Mercury'];
const SKY_COLOR = { station: '#f2c14e', fresh: '#6cc9a8', you: '#ffffff' };
const COMPASS = ['N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW'];
const PLACE_KEY = 'ufo-sky-place';

const toDeg = (r) => (r * 180) / Math.PI;
const toRad = (d) => (d * Math.PI) / 180;
const compass = (azDeg) => COMPASS[Math.round((((azDeg % 360) + 360) % 360) / 22.5) % 16];

function clock(date) {
  return date.toLocaleString(undefined, { weekday: 'short', hour: '2-digit', minute: '2-digit' });
}

function hm(date) {
  return date.toLocaleTimeString(undefined, { hour: '2-digit', minute: '2-digit' });
}

function fromNow(date, now = new Date()) {
  const min = Math.round((date - now) / 60000);
  if (min < 1) return 'now';
  if (min < 60) return `in ${min} min`;
  const h = Math.floor(min / 60);
  if (h < 48) return `in ${h} h ${String(min % 60).padStart(2, '0')}`;
  return `in ${Math.round(h / 24)} days`;
}

function sunAltitude(date, observer) {
  const eq = Astronomy.Equator(Astronomy.Body.Sun, date, observer, true, true);
  return Astronomy.Horizon(date, observer, eq.ra, eq.dec, 'normal').altitude;
}

function bodyHorizon(body, date, observer) {
  const eq = Astronomy.Equator(body, date, observer, true, true);
  return Astronomy.Horizon(date, observer, eq.ra, eq.dec, 'normal');
}

// where a satellite is at a given moment, seen from the observer
function look(satrec, date, place) {
  const pv = satellite.propagate(satrec, date);
  if (!pv || !pv.position || Number.isNaN(pv.position.x)) return null;
  const gmst = satellite.gstime(date);
  const ground = satellite.eciToGeodetic(pv.position, gmst);
  const angles = satellite.ecfToLookAngles(
    { latitude: toRad(place.lat), longitude: toRad(place.lon), height: 0.1 },
    satellite.eciToEcf(pv.position, gmst),
  );
  return {
    position: pv.position,
    velocity: pv.velocity,
    lat: satellite.degreesLat(ground.latitude),
    lon: satellite.degreesLong(ground.longitude),
    height: ground.height,
    elevation: toDeg(angles.elevation),
    azimuth: toDeg(angles.azimuth),
    range: angles.rangeSat,
  };
}

// sunlit satellite over a dark sky: the only time one can be seen
function visibleNow(l, date, observer) {
  if (!l || l.elevation < MIN_ELEVATION) return false;
  if (sunAltitude(date, observer) > DARK_SUN) return false;
  const sun = satellite.sunPos(satellite.jday(date));
  return satellite.shadowFraction(sun.rsun, l.position) < 0.5;
}

// passes above MIN_ELEVATION in the next `hours`, sampled every `step` seconds;
// a pass is visible if at some point the observer is in darkness and the
// satellite lit
function findPasses(satrec, place, observer, start, hours, step = 30) {
  const passes = [];
  let current = null;
  for (let t = 0; t <= hours * 3600; t += step) {
    const date = new Date(start.getTime() + t * 1000);
    const l = look(satrec, date, place);
    const up = l && l.elevation >= MIN_ELEVATION;
    if (up) {
      if (!current) current = { rise: date, maxEl: -90, samples: [] };
      const lit = visibleNow(l, date, observer);
      current.samples.push({ date, el: l.elevation, az: l.azimuth, lit });
      if (l.elevation > current.maxEl) current.maxEl = l.elevation;
    } else if (current) {
      passes.push(current);
      current = null;
    }
  }
  return passes.map((p) => {
    const seen = p.samples.filter((s) => s.lit);
    if (!seen.length) return { rise: p.rise, visible: false };
    return {
      visible: true,
      start: seen[0].date,
      end: seen[seen.length - 1].date,
      fromAz: seen[0].az,
      toAz: seen[seen.length - 1].az,
      maxEl: Math.max(...seen.map((s) => s.el)),
      minutes: Math.max(1, Math.round((seen[seen.length - 1].date - seen[0].date) / 60000 + step / 60)),
    };
  });
}

// the dark part of the coming night: from now (if already dark) or dusk, to dawn
function comingNight(observer, now) {
  const start = sunAltitude(now, observer) < DARK_SUN
    ? now
    : Astronomy.SearchAltitude(Astronomy.Body.Sun, observer, -1, now, 1, DARK_SUN)?.date;
  if (!start) return null;
  const end = Astronomy.SearchAltitude(Astronomy.Body.Sun, observer, +1, start, 1, DARK_SUN)?.date
    || new Date(start.getTime() + 12 * 3600 * 1000);
  return { start, end };
}

function planetsTonight(observer, now) {
  const night = comingNight(observer, now);
  if (!night) return { night: null, rows: [] };
  const rows = [...PLANETS, 'Moon'].map((name) => {
    const body = Astronomy.Body[name];
    let best = null;
    let first = null;
    let last = null;
    for (let t = night.start.getTime(); t <= night.end.getTime(); t += 10 * 60000) {
      const date = new Date(t);
      const h = bodyHorizon(body, date, observer);
      if (h.altitude > 5) {
        first = first || date;
        last = date;
        if (!best || h.altitude > best.altitude) best = { date, altitude: h.altitude, azimuth: h.azimuth };
      }
    }
    if (!best) return { name, visible: false };
    const light = Astronomy.Illumination(body, best.date);
    return { name, visible: true, best, first, last, mag: light.mag, phase: light.phase_fraction };
  });
  return { night, rows };
}

function moonPhaseName(date) {
  const a = Astronomy.MoonPhase(date);
  const names = ['New Moon', 'Waxing crescent', 'First quarter', 'Waxing gibbous', 'Full Moon', 'Waning gibbous', 'Last quarter', 'Waning crescent'];
  return names[Math.round(a / 45) % 8];
}

// a launch shortly after dusk or before dawn at the pad: the plume, lit by the
// Sun high up, can be seen from very far away as a glowing "jellyfish"
function twilightLaunch(launch) {
  if (launch.lat == null || launch.lon == null || !launch.net) return false;
  if (launch.precision && !['Second', 'Minute', 'Hour'].includes(launch.precision)) return false;
  const alt = sunAltitude(new Date(launch.net), new Astronomy.Observer(launch.lat, launch.lon, 0));
  return alt < -4 && alt > -18;
}

function skyPage() {
  const root = $('#page-sky');
  const status = $('#s-status');
  let place = DEFAULT_PLACE;
  let observer = new Astronomy.Observer(place.lat, place.lon, 0);
  let sats = null;
  let launches = null;
  let explained = null;
  let tracked = [];
  let fresh = [];
  let bright = [];
  let map;
  let youMarker;
  let trackLine;
  let freshLayer;
  const trackedMarkers = {};
  let timers = [];

  const caption = (key, what) => {
    const n = explained && explained.counts ? explained.counts[key] : null;
    return n ? `<p class="caption">GEIPAN identified ${fmt(n)} French sighting${n === 1 ? '' : 's'} as ${what}.</p>` : '';
  };

  try {
    const saved = JSON.parse(localStorage.getItem(PLACE_KEY));
    if (saved && Number.isFinite(saved.lat) && Number.isFinite(saved.lon)) place = saved;
  } catch (e) { /* storage unavailable: keep the default */ }

  function setPlace(lat, lon, label) {
    place = { lat: Math.round(lat * 100) / 100, lon: Math.round(lon * 100) / 100, label };
    observer = new Astronomy.Observer(place.lat, place.lon, 0);
    try { localStorage.setItem(PLACE_KEY, JSON.stringify(place)); } catch (e) { /* ignore */ }
    renderAll();
  }

  function renderPlace() {
    $('#s-place').textContent = `${place.label} · ${place.lat.toFixed(2)}°, ${place.lon.toFixed(2)}°`;
    if (youMarker) youMarker.setLatLng([place.lat, place.lon]);
  }

  // ---- live parts (every few seconds) ----

  function renderNow() {
    const now = new Date();
    const dark = sunAltitude(now, observer) < DARK_SUN;
    $('#s-sky-state').textContent = dark ? 'It is dark where you are.' : 'It is still daylight or twilight where you are.';

    if (!tracked.some((t) => t.norad === 25544)) {
      $('#s-iss').innerHTML = '<p class="hint">Satellite data not available yet: it is downloaded once a day.</p>';
    }
    for (const t of tracked) {
      const l = look(t.satrec, now, place);
      if (!l) continue;
      const m = trackedMarkers[t.norad];
      if (m) m.setLatLng([l.lat, l.lon]);
      if (t.norad === 25544) {
        const speed = Math.hypot(l.velocity.x, l.velocity.y, l.velocity.z) * 3600;
        const where = l.elevation > 0
          ? `<strong>above your horizon</strong>, ${Math.round(l.elevation)}° up in the ${compass(l.azimuth)}`
          : 'below your horizon';
        $('#s-iss').innerHTML = `
          <p class="big">${Math.round(l.height)} km up · ${fmt(Math.round(speed / 100) * 100)} km/h</p>
          <p>Over ${esc(`${Math.abs(l.lat).toFixed(1)}°${l.lat >= 0 ? 'N' : 'S'}, ${Math.abs(l.lon).toFixed(1)}°${l.lon >= 0 ? 'E' : 'W'}`)}, ${fmt(Math.round(l.range))} km from you, ${where}.</p>
          <p>${visibleNow(l, now, observer) ? '<strong>Visible right now</strong>: a bright, steady light crossing the sky in a few minutes, without blinking.' : 'Not visible right now.'}</p>
          ${caption('iss', 'the ISS')}`;
      }
    }

    let up = 0;
    let seen = 0;
    const seenNames = [];
    for (const s of [...bright, ...fresh]) {
      const l = look(s.satrec, now, place);
      if (!l || l.elevation < MIN_ELEVATION) continue;
      up += 1;
      if (visibleNow(l, now, observer)) {
        seen += 1;
        seenNames.push(`${s.name} (${Math.round(l.elevation)}° ${compass(l.azimuth)})`);
      }
    }
    $('#s-overhead').innerHTML = sats
      ? `<p class="big">${fmt(up)} above you · ${fmt(seen)} visible</p>
         <p>Among the ${fmt(bright.length)} brightest satellites and the ${fmt(fresh.length)} objects launched in the last 30 days.</p>
         ${seenNames.length ? `<p class="hint">${esc(seenNames.slice(0, 8).join(', '))}${seenNames.length > 8 ? '…' : ''}</p>` : ''}
         ${caption('satellites', 'satellites, Starlink trains or Iridium flares')}`
      : '<p class="hint">Satellite data not available yet: it is downloaded once a day.</p>';
  }

  function renderMapLive() {
    if (!freshLayer) return;
    const now = new Date();
    freshLayer.clearLayers();
    for (const s of fresh) {
      const l = look(s.satrec, now, place);
      if (!l) continue;
      L.circleMarker([l.lat, l.lon], {
        radius: 3, stroke: false, fillColor: SKY_COLOR.fresh, fillOpacity: 0.85, interactive: false,
      }).addTo(freshLayer);
    }
    const iss = tracked.find((t) => t.norad === 25544);
    if (iss) {
      const segments = [[]];
      let prevLon = null;
      for (let m = 0; m <= 95; m += 1) {
        const l = look(iss.satrec, new Date(now.getTime() + m * 60000), place);
        if (!l) continue;
        if (prevLon !== null && Math.abs(l.lon - prevLon) > 180) segments.push([]);
        segments[segments.length - 1].push([l.lat, l.lon]);
        prevLon = l.lon;
      }
      trackLine.setLatLngs(segments);
    }
  }

  // ---- slower parts (on location change, every 10 minutes) ----

  function renderPasses() {
    if (!tracked.length) {
      $('#s-passes').innerHTML = '<p class="hint">Satellite data not available yet: it is downloaded once a day.</p>';
      return;
    }
    const now = new Date();
    const rows = [];
    let hidden = 0;
    for (const t of tracked) {
      for (const p of findPasses(t.satrec, place, observer, now, 72)) {
        if (p.visible) rows.push({ ...p, label: t.label });
        else hidden += 1;
      }
    }
    rows.sort((a, b) => a.start - b.start);
    $('#s-passes').innerHTML = rows.length ? `
      <table class="sky-table">
        <thead><tr><th scope="col">When</th><th scope="col">What</th><th scope="col">Path</th><th scope="col">Highest</th></tr></thead>
        <tbody>${rows.slice(0, 8).map((p) => `
          <tr><td class="nowrap">${esc(clock(p.start))}<br><span class="hint">${esc(fromNow(p.start, now))}</span></td>
          <td>${esc(p.label)}</td>
          <td>${esc(compass(p.fromAz))} → ${esc(compass(p.toAz))}<br><span class="hint">${p.minutes} min</span></td>
          <td>${Math.round(p.maxEl)}°</td></tr>`).join('')}
        </tbody>
      </table>
      <p class="hint">Visible means you are in darkness while the station is still in sunlight. ${fmt(hidden)} other passes in the next 3 days happen in daylight or in the Earth's shadow.</p>`
      : `<p>No visible pass of the ISS, Tiangong or Hubble in the next 3 days from here (${fmt(hidden)} passes happen in daylight or in the Earth's shadow).</p>`;
  }

  function renderTrains() {
    if (!fresh.length) {
      $('#s-trains').innerHTML = sats ? '<p class="hint">No satellites launched in the last 30 days.</p>' : '';
      return;
    }
    // objects from one launch share the launch part of their international designator
    const groups = new Map();
    for (const s of fresh) {
      const launch = (s.id || '').slice(0, 8);
      if (!groups.has(launch)) groups.set(launch, []);
      groups.get(launch).push(s);
    }
    const now = new Date();
    const rows = [...groups.values()]
      .filter((g) => g.length >= 5)
      .map((g) => {
        const lead = g.slice().sort((a, b) => a.id.localeCompare(b.id))[0];
        // trains pass slowly: a 1-minute step over 2 days keeps this quick on phones
        const next = findPasses(lead.satrec, place, observer, now, 48, 60).find((p) => p.visible);
        const family = lead.name.replace(/[-\s]*\d+$/, '') || lead.name;
        return { family, launch: lead.id.slice(0, 8), n: g.length, next };
      })
      .sort((a, b) => (a.next ? a.next.start : Infinity) - (b.next ? b.next.start : Infinity));
    $('#s-trains').innerHTML = rows.length ? `
      <table class="sky-table">
        <thead><tr><th scope="col">Launch</th><th scope="col">Objects</th><th scope="col">Next visible pass</th></tr></thead>
        <tbody>${rows.slice(0, 8).map((r) => `
          <tr><td>${esc(r.family)}<br><span class="hint">${esc(r.launch)}</span></td>
          <td>${fmt(r.n)}</td>
          <td>${r.next ? `${esc(clock(r.next.start))}, ${esc(compass(r.next.fromAz))} → ${esc(compass(r.next.toAz))}, up to ${Math.round(r.next.maxEl)}°` : '<span class="hint">none in 2 days</span>'}</td></tr>`).join('')}
        </tbody>
      </table>
      <p class="hint">Right after launch, satellites fly in a tight line (a "train") and look like a string of lights; they spread out within weeks.</p>`
      : '<p class="hint">No large launch in the last 30 days.</p>';
  }

  function renderPlanets() {
    const now = new Date();
    const { night, rows } = planetsTonight(observer, now);
    const moon = `${moonPhaseName(now)}, ${Math.round(Astronomy.Illumination(Astronomy.Body.Moon, now).phase_fraction * 100)}% lit`;
    if (!night) {
      $('#s-planets').innerHTML = `<p>The sky does not get dark here tonight (polar summer).</p><p class="hint">Moon: ${esc(moon)}.</p>`;
      return;
    }
    const seen = rows.filter((r) => r.visible);
    const hidden = rows.filter((r) => !r.visible).map((r) => r.name);
    $('#s-planets').innerHTML = `
      <p class="hint">Dark from ${esc(clock(night.start))} to ${esc(clock(night.end))}. Moon: ${esc(moon)}.</p>
      ${seen.length ? `<table class="sky-table">
        <thead><tr><th scope="col">What</th><th scope="col">Up</th><th scope="col">Best at</th><th scope="col">Brightness</th></tr></thead>
        <tbody>${seen.map((r) => `
          <tr><td>${esc(r.name)}</td>
          <td class="nowrap">${esc(hm(r.first))}–${esc(hm(r.last))}</td>
          <td>${esc(hm(r.best.date))}, ${Math.round(r.best.altitude)}° ${esc(compass(r.best.azimuth))}</td>
          <td>${r.mag.toFixed(1)}${r.mag < -3 ? ' (dazzling)' : r.mag < 0 ? ' (very bright)' : ''}</td></tr>`).join('')}
        </tbody>
      </table>` : '<p>No bright planet is up in the dark tonight.</p>'}
      ${hidden.length ? `<p class="hint">Not visible tonight: ${esc(hidden.join(', '))} (below the horizon or too close to the Sun).</p>` : ''}
      <p class="hint">Brightness is a magnitude: lower is brighter; the brightest stars are around 0.</p>
      ${caption('venus', 'Venus')}${explained && explained.counts ? `<p class="caption">…and ${fmt(explained.counts.jupiter)} as Jupiter, ${fmt(explained.counts.moon)} as the Moon.</p>` : ''}`;
  }

  function renderLaunches() {
    if (!launches) {
      $('#s-launches').innerHTML = '<p class="hint">Launch data not available yet: it is downloaded once a day.</p>';
      return;
    }
    const now = new Date();
    const next = launches.launches.filter((l) => new Date(l.net) > now).slice(0, 8);
    $('#s-launches').innerHTML = next.length ? `<ul class="launch-list">${next.map((l) => {
      const when = new Date(l.net);
      const exact = !l.precision || ['Second', 'Minute', 'Hour'].includes(l.precision);
      return `<li>
        <strong>${esc(l.mission || l.name)}</strong> · ${esc(l.rocket || '')}<br>
        <span class="hint">${esc(exact ? clock(when) : when.toLocaleDateString())}${exact ? ` (${esc(fromNow(when, now))})` : ` (${esc((l.precision || '').toLowerCase())} not fixed yet)`} · ${esc(l.location || l.pad || '')}${l.status ? ` · ${esc(l.status)}` : ''}</span>
        ${twilightLaunch(l) ? '<br><span class="flag">Twilight launch: the lit plume can be seen hundreds of kilometres away and is often reported as a UFO.</span>' : ''}
      </li>`;
    }).join('')}</ul>${caption('rockets', 'rocket launches, spent stages or re-entries')}`
      : '<p class="hint">No upcoming launch listed.</p>';
  }

  function renderSources() {
    const age = (iso) => (iso ? `${new Date(iso).toLocaleDateString()}` : 'not yet');
    $('#s-sources').innerHTML = `Orbits: <a href="https://celestrak.org">CelesTrak</a>, updated ${esc(age(sats && sats.updated))}.
      Launches: <a href="https://thespacedevs.com">The Space Devs</a>, updated ${esc(age(launches && launches.updated))}.
      Planets: computed with <a href="https://github.com/cosinekitty/astronomy">Astronomy Engine</a>.`;
    const days = sats && sats.updated ? (Date.now() - new Date(sats.updated)) / 86400000 : 0;
    setStatus(status, days > 7 ? `The orbit data is ${Math.round(days)} days old, so satellite positions may be off.` : '');
  }

  function renderAll() {
    renderPlace();
    renderNow();
    renderMapLive();
    renderPasses();
    renderTrains();
    renderPlanets();
    renderLaunches();
    renderSources();
  }

  async function init() {
    setStatus(status, 'Loading satellite orbits and launches…');
    const [s, l, e] = await Promise.allSettled([
      getJSON('data/sky/satellites.json'),
      getJSON('data/sky/launches.json'),
      getJSON('data/geipan_explanations.json'),
    ]);
    sats = s.status === 'fulfilled' ? s.value : null;
    launches = l.status === 'fulfilled' ? l.value : null;
    explained = e.status === 'fulfilled' ? e.value : null;
    setStatus(status, '');

    const toSat = (o) => {
      try {
        return { name: o.OBJECT_NAME, id: o.OBJECT_ID, norad: +o.NORAD_CAT_ID, satrec: satellite.json2satrec(o) };
      } catch (err) {
        return null;
      }
    };
    if (sats && sats.groups) {
      const all = [...(sats.groups.stations || []), ...(sats.groups.visual || [])].map(toSat).filter(Boolean);
      tracked = TRACKED.map((t) => {
        const s = all.find((x) => x.norad === t.norad);
        return s ? { ...t, satrec: s.satrec } : null;
      }).filter(Boolean);
      bright = (sats.groups.visual || []).map(toSat).filter(Boolean);
      fresh = (sats.groups.recent || []).map(toSat).filter(Boolean);
    }

    map = L.map('s-map', { renderer: L.canvas({ tolerance: TAP_TOLERANCE }), minZoom: 1, worldCopyJump: true })
      .setView([place.lat, place.lon], 2);
    darkTiles().addTo(map);
    freshLayer = L.layerGroup().addTo(map);
    trackLine = L.polyline([], { color: SKY_COLOR.station, weight: 2, opacity: 0.7, dashArray: '6 6', interactive: false }).addTo(map);
    for (const t of tracked) {
      trackedMarkers[t.norad] = L.marker([0, 0], {
        icon: L.divIcon({ className: 'sat-icon', html: `<span>${esc(t.label)}</span>`, iconSize: null }),
        title: t.note,
      }).addTo(map);
    }
    youMarker = L.circleMarker([place.lat, place.lon], {
      radius: 7, color: SURFACE, weight: 2, fillColor: SKY_COLOR.you, fillOpacity: 1,
    }).bindTooltip('You').addTo(map);
    map.on('click', (e) => setPlace(e.latlng.lat, L.Util.wrapNum(e.latlng.lng, [-180, 180], true), 'Point on the map'));
    const legend = L.control({ position: 'bottomright' });
    legend.onAdd = () => {
      const div = L.DomUtil.create('div', 'legend');
      div.innerHTML = `
        <div><span class="swatch" style="background:${SKY_COLOR.you}"></span>You</div>
        <div><span class="swatch" style="background:${SKY_COLOR.station}"></span>Stations, Hubble; dashes: ISS orbit</div>
        <div><span class="swatch" style="background:${SKY_COLOR.fresh}"></span>Launched &lt; 30 days ago</div>`;
      return div;
    };
    legend.addTo(map);

    $('#s-locate').addEventListener('click', () => {
      const note = $('#s-loc-status');
      if (!navigator.geolocation) { note.textContent = 'Your browser cannot share its location; click on the map instead.'; return; }
      note.textContent = 'Asking your browser for your location…';
      navigator.geolocation.getCurrentPosition(
        (pos) => { note.textContent = ''; setPlace(pos.coords.latitude, pos.coords.longitude, 'Your location'); map.setView([place.lat, place.lon], 3); },
        () => { note.textContent = 'Location not shared; click on the map instead.'; },
        { timeout: 15000, maximumAge: 600000 },
      );
    });

    renderAll();
    startTimers();
  }

  // live updates only while the page is shown
  function startTimers() {
    stopTimers();
    timers = [
      setInterval(renderNow, 5000),
      setInterval(renderMapLive, 15000),
      setInterval(() => { renderPasses(); renderTrains(); renderPlanets(); renderLaunches(); }, 10 * 60000),
    ];
  }

  function stopTimers() {
    timers.forEach(clearInterval);
    timers = [];
  }

  let started = false;
  return {
    show() {
      if (!started) {
        started = true;
        init();
      } else if (map) {
        map.invalidateSize();
        renderAll();
        startTimers();
      }
    },
    hide() { stopTimers(); },
  };
}
