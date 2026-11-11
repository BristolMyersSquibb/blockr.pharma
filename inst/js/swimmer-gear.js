// The swimmer gear's Tables row: what the swimmer found in its input dm (R's
// swimmer_mapping(), sent as derived.gear.info) and the ADRS parameter of the
// overall response, which writes the prepare script's `parameter`.
// Registered as blockr.viz's gear editor "mapping" (core/gear/profile.js).
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
    '.pharma-map-param{margin-top:10px;display:flex;gap:8px;align-items:center;font-size:13px}',
    '.pharma-map-param select{min-width:120px}',
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
    const params = info.parameters || [];
    const param = current(ctx, 'parameter') || 'OVR';
    const err = ctx.script && ctx.script.error;
    const bad = roles.filter((r) => (r.need && !r.found) || (r.found && (r.missing || []).length));
    const noParam = params.length > 0 && !params.includes(param);

    let msg = 'All tables and columns found.';
    if (bad.length) {
      msg = bad.map((r) => r.found
        ? r.table + ' has no ' + r.missing.join(', ') + '.'
        : 'No ' + r.table + ' in the input.').join(' ');
    } else if (noParam) {
      msg = 'ADRS has no PARAMCD "' + param + '". Pick the overall response parameter.';
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
      el('td', null, tr, (r.columns || []).join(', '));
      const ok = r.found ? !(r.missing || []).length : !r.need;
      el('td', ok ? 'is-ok' : 'is-bad', tr, ok ? (r.found ? '✓' : '') : '!');
    }

    const pick = el('div', 'pharma-map-param', host);
    el('span', null, pick, 'Overall response, ADRS PARAMCD');
    const sel = el('select', null, pick);
    const opts = params.length ? params.slice() : [param];
    if (!opts.includes(param)) opts.unshift(param);
    for (const p of opts) {
      const o = el('option', null, sel, p);
      o.value = p;
      if (p === param) o.selected = true;
    }
    sel.addEventListener('change', () => {
      const held = Object.assign({}, ctx.values.script_values || {});
      held.parameter = sel.value;
      ctx.setValues({ script_values: held });
    });
    el('div', 'pharma-map-note', host,
       'Names match without case. Any of the listed alternatives will do (ARM, TRT01A or ACTARM).');
  }

  B.viz.gear.editors.mapping = { label: 'Found in the input', draw };
})();
