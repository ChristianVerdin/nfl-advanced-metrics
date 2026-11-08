"""Screenshot a gt HTML table to PNG at 2x: python shoot.py <in.html> <out.png> [width]."""
import sys
from playwright.sync_api import sync_playwright

src, out = sys.argv[1], sys.argv[2]
width = int(sys.argv[3]) if len(sys.argv) > 3 else 1400
with sync_playwright() as p:
    b = p.chromium.launch()
    pg = b.new_page(viewport={"width": width, "height": 800}, device_scale_factor=2)
    pg.goto(f"file://{src}")
    pg.wait_for_load_state("networkidle")
    pg.add_style_tag(content="body{background:#f7f7f7;margin:0;padding:28px 32px;} .gt_table{margin:0!important;}")
    el = pg.query_selector("table.gt_table") or pg.query_selector("body")
    el.screenshot(path=out)
    b.close()
print(out)
