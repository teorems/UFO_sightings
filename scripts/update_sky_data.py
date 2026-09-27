"""Downloads the data behind the "Sky tonight" page into docs/data/sky/.

- satellites.json: orbital elements (CelesTrak, OMM JSON) for the space
  stations, the ~150 brightest satellites and everything launched in the last
  30 days (where fresh Starlink "trains" show up). The page propagates them in
  the browser with satellite.js.
- launches.json: upcoming launches from The Space Devs' Launch Library 2.

Run daily by .github/workflows/update-sky-data.yml; standard library only.
CelesTrak asks for at most one download per group every two hours, and the
Launch Library free tier allows 15 requests an hour, so once a day is well
within both. If a source fails, its previous data is kept.
"""

import json
import sys
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

OUT = Path(__file__).resolve().parent.parent / "docs" / "data" / "sky"
USER_AGENT = "UFO_sightings dashboard (https://github.com/teorems/UFO_sightings)"

CELESTRAK = "https://celestrak.org/NORAD/elements/gp.php?GROUP={group}&FORMAT=json"
GROUPS = {"stations": "stations", "visual": "visual", "recent": "last-30-days"}
# only the fields satellite.js's json2satrec reads, plus names
OMM_FIELDS = [
    "OBJECT_NAME", "OBJECT_ID", "NORAD_CAT_ID", "EPOCH", "MEAN_MOTION", "ECCENTRICITY",
    "INCLINATION", "RA_OF_ASC_NODE", "ARG_OF_PERICENTER", "MEAN_ANOMALY", "BSTAR",
    "MEAN_MOTION_DOT", "MEAN_MOTION_DDOT",
]

LAUNCHES = "https://ll.thespacedevs.com/2.2.0/launch/upcoming/?limit=25"


def fetch_json(url):
    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT, "Accept": "application/json"})
    with urllib.request.urlopen(req, timeout=60) as res:
        return json.load(res)


def load_previous(name):
    path = OUT / name
    if path.exists():
        try:
            return json.loads(path.read_text(encoding="utf-8"))
        except ValueError:
            pass
    return {}


def write(name, data):
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / name).write_text(json.dumps(data, ensure_ascii=False, separators=(",", ":")) + "\n", encoding="utf-8")


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def update_satellites():
    previous = load_previous("satellites.json").get("groups", {})
    groups, ok = {}, 0
    for key, group in GROUPS.items():
        try:
            rows = fetch_json(CELESTRAK.format(group=group))
            if not isinstance(rows, list) or not rows:
                raise ValueError("empty or unexpected response")
            groups[key] = [{f: r[f] for f in OMM_FIELDS if f in r} for r in rows]
            ok += 1
            print(f"CelesTrak {group}: {len(rows)} objects")
        except Exception as err:  # keep yesterday's elements rather than none
            groups[key] = previous.get(key, [])
            print(f"CelesTrak {group} failed ({err}); kept {len(groups[key])} previous objects", file=sys.stderr)
    if ok:
        write("satellites.json", {"updated": now_iso(), "source": "CelesTrak (celestrak.org)", "groups": groups})
    return ok


def update_launches():
    try:
        results = fetch_json(LAUNCHES).get("results", [])
    except Exception as err:
        print(f"Launch Library failed ({err}); kept previous launches", file=sys.stderr)
        return 0
    launches = []
    for r in results:
        pad = r.get("pad") or {}
        location = pad.get("location") or {}
        rocket = ((r.get("rocket") or {}).get("configuration") or {})
        mission = r.get("mission") or {}
        launches.append({
            "name": r.get("name"),
            "net": r.get("net"),
            "precision": ((r.get("net_precision") or {}).get("name")),
            "status": ((r.get("status") or {}).get("name")),
            "provider": ((r.get("launch_service_provider") or {}).get("name")),
            "rocket": rocket.get("full_name") or rocket.get("name"),
            "mission": mission.get("name"),
            "description": mission.get("description"),
            "orbit": ((mission.get("orbit") or {}).get("name")),
            "pad": pad.get("name"),
            "location": location.get("name"),
            "lat": float(pad["latitude"]) if pad.get("latitude") not in (None, "") else None,
            "lon": float(pad["longitude"]) if pad.get("longitude") not in (None, "") else None,
        })
    print(f"Launch Library: {len(launches)} upcoming launches")
    write("launches.json", {"updated": now_iso(), "source": "The Space Devs Launch Library 2 (thespacedevs.com)", "launches": launches})
    return 1


if __name__ == "__main__":
    done = update_satellites() + update_launches()
    sys.exit(0 if done else 1)
