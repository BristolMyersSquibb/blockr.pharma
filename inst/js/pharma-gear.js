// Gear rows of the pharma blocks (the swimmer, the timeline), registered as
// blockr.viz gear editors (core/gear/profile.js):
//   mapping     the Tables row: what the block found in its input (R's
//               swimmer_mapping() or timeline_mapping(), sent as
//               derived.gear.info); with `parameters` it says when the
//               picked response parameter is not in ADRS (the pick itself is
//               the prepare script's `parameter` row)
//   lane_width  the swimlane's Width
//   open_rows   whether the patient rows start closed or open (the table's
//               `open`)
(function () {
  'use strict';
  const B = window.Blockr;
  if (!B || !B.viz || !B.viz.gear) return;
  B.viz.gear.editors = B.viz.gear.editors || {};

  const el = (tag, cls, parent, text) => {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text != null) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  };

  const STYLE = [
    '.vz-row-controls > .pharma-map-wrap{display:flex;flex-direction:column;align-items:stretch;',
    'width:100%;max-width:680px}',
    '.pharma-map-status{font-size:13px;padding:6px 10px;border-radius:6px;margin:0 0 8px;',
    'background:#ecfdf5;color:#047857}',
    '.pharma-map-status.is-bad{background:#fef3c7;color:#b45309}',
    '.pharma-map{border-collapse:collapse;font-size:13px;width:100%;max-width:640px;',
    'background:var(--blockr-color-bg,#fff);border:1px solid var(--blockr-color-border-default,#e5e7eb)}',
    '.pharma-map td,.pharma-map th{padding:4px 8px;text-align:left;',
    'border-bottom:1px solid var(--blockr-color-border-default,#e5e7eb)}',
    '.pharma-map th{font-weight:500;color:var(--blockr-color-text-muted,#6b7280)}',
    '.pharma-map .is-ok{color:#047857}.pharma-map .is-bad{color:#b45309;font-weight:600}',
    '.pharma-map .is-off{color:var(--blockr-color-text-muted,#9ca3af)}',
    '.pharma-map-note{font-size:12px;color:var(--blockr-color-text-muted,#6b7280);margin-top:6px}'
  ].join('');

  const style = () => {
    if (document.getElementById('pharma-map-style')) return;
    const s = el('style', null, document.head, STYLE);
    s.id = 'pharma-map-style';
  };

  /** The script's current value of `name` (R sends a one-element array). */
  const current = (ctx, name) => {
    const v = ((ctx.script && ctx.script.values) || {})['sv_' + name];
    return Array.isArray(v) ? v[0] : v;
  };

  function draw(parent, ctx) {
    style();
    const host = el('div', 'pharma-map-wrap', parent);
    const info = (ctx.profile && ctx.profile.wire && ctx.profile.wire.info) || {};
    const roles = info.roles || [];
    // A one-element vector may arrive unboxed, as a string.
    const hasParams = info.parameters != null;
    const params = [].concat(info.parameters || []);
    const param = current(ctx, 'parameter') || 'OVR';
    const err = ctx.script && ctx.script.error;
    const bad = roles.filter((r) => (r.need && !r.found) || (r.found && (r.missing || []).length));
    const noParam = hasParams && params.length > 0 && !params.includes(param);

    let msg = 'All tables and columns found.';
    if (bad.length) {
      msg = bad.map((r) => r.found
        ? r.table + ' has no ' + r.missing.join(', ') + '.'
        : 'No ' + r.table + ' in the input.').join(' ');
    } else if (noParam) {
      msg = 'ADRS has no PARAMCD "' + param + '". Pick a response parameter below.';
    } else if (err) {
      msg = err;
    }
    el('div', 'pharma-map-status' + (bad.length || noParam || err ? ' is-bad' : ''), host, msg);

    const tab = el('table', 'pharma-map', host);
    const head = el('tr', null, tab);
    for (const h of ['Role', 'Table', 'Columns', '']) el('th', null, head, h);
    for (const r of roles) {
      const tr = el('tr', null, tab);
      el('td', null, tr, r.role);
      el('td', r.found ? null : (r.need ? 'is-bad' : 'is-off'), tr,
         r.found ? r.table : r.table + (r.need ? ' missing' : ' (optional, absent)'));
      // A one-element vector may arrive as a plain string.
      const cols = [].concat(r.columns || []);
      const miss = [].concat(r.missing || []);
      el('td', null, tr, cols.join(', '));
      const ok = r.found ? !miss.length : !r.need;
      // An optional role whose columns are absent is off, not wrong.
      const off = !ok && !r.need;
      el('td', ok ? 'is-ok' : off ? 'is-off' : 'is-bad', tr, ok ? (r.found ? '✓' : '') : off ? '' : '!');
    }

    if (info.note) el('div', 'pharma-map-note', host, info.note);
  }

  B.viz.gear.editors.mapping = { label: 'Found in the input', draw };

  // The swimlane's width: the spans column's `size` (md, lg, xl), the Width
  // the table's summaries editor offers, here as a row of its own.
  const WIDTHS = [['md', 'Regular'], ['lg', 'Wide'], ['xl', 'Widest']];
  function drawWidth(parent, ctx) {
    const sums = Array.isArray(ctx.values.summaries) ? ctx.values.summaries : [];
    const lane = sums.find((s) => s && s.type === 'spans');
    if (!lane) return;
    const at = String(Math.max(0, WIDTHS.findIndex(([v]) => v === (lane.size || 'md'))));
    const pick = (i) => {
      const size = WIDTHS[Number(i)][0];
      ctx.setValues({ summaries: sums.map((s) =>
        s && s.type === 'spans' ? Object.assign({}, s, { size }) : s) });
    };
    const opts = WIDTHS.map(([, label], i) => ({ value: String(i), label }));
    parent.appendChild(B.segmented(opts, at, pick, { label: 'Width' }).el);
  }

  B.viz.gear.editors.lane_width = { label: 'Width', draw: drawWidth };

  // The patient rows at start: closed or open (the table's `open`). The
  // header's button opens and closes them all either way.
  const OPEN = [['none', 'None open'], ['all', 'All open']];
  function drawOpen(parent, ctx) {
    const at = String(Math.max(0, OPEN.findIndex(([v]) => v === (ctx.values.open || 'none'))));
    const opts = OPEN.map(([, label], i) => ({ value: String(i), label }));
    parent.appendChild(B.segmented(opts, at, (i) => ctx.setValues({ open: OPEN[Number(i)][0] }),
                                   { label: 'At start' }).el);
  }

  B.viz.gear.editors.open_rows = { label: 'At start', draw: drawOpen };
})();
