/* The sidebar: the panel picker (a Blockr.Pick.list) that sync_selected
 * fills and that reports back through toggle_viz and reorder_viz (one
 * change) or set_viz (several), and the
 * patients' own filter box. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const ON = ['patient_overview', 'ae_gantt', 'adlbc_all__ALB', 'adlbc_all__ALP', 'adlbc_all__ALT'];
const search = (h) => h.el('search');
const row = (h, id) => h.q(`#proxy1-pp_panels .blockr-select__option[data-value="${id}"]`);
const filterBox = (h) => h.q('#proxy1-pp_panels .blockr-select__search');

const boot = (selected) => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  if (selected) h.send('sync_selected', selected); else h.sendRecorded('sync_selected');
  assert.deepEqual(h.recorded('sync_selected').pop(), ON);
  return h;
};

/** A drag of one pick onto another, as the browser fires it. */
const dragOnto = (h, from, onto, after) => {
  const dt = { setData() {}, effectAllowed: '', dropEffect: '' };
  h.layout(onto, { top: 100, left: 0, width: 200, height: 24 });
  const fire = (type, el, y) => {
    const ev = new h.win.Event(type, { bubbles: true, cancelable: true });
    Object.defineProperty(ev, 'dataTransfer', { value: dt });
    Object.defineProperty(ev, 'clientY', { value: y });
    el.dispatchEvent(ev);
  };
  const y = after ? 120 : 104;
  fire('dragstart', from, 0);
  fire('dragover', onto, y);
  fire('drop', onto, y);
  fire('dragend', from, y);
};

test('sync_selected puts the picks on top, in profile order', () => {
  const h = boot();
  assert.deepEqual(h.onProfile(), ON);
  assert.match(h.text('#proxy1-pp_panels .blockr-pick__count'), /^5 of 9 on the profile/);
  // A parameter shows its code, with its name as meta.
  assert.equal(row(h, 'adlbc_all__ALB').querySelector('.blockr-pick__name').textContent, 'ALB');
  assert.equal(row(h, 'adlbc_all__ALB').querySelector('.blockr-select__opt-label').textContent,
    'Albumin (g/L)');
  // The rest under their groups.
  const groups = h.qa('#proxy1-pp_panels .blockr-pick__subgroup').map((g) => g.textContent);
  assert.ok(groups.includes('Vital signs'));
  // Ids the catalogue does not know are left out.
  h.send('sync_selected', ON.concat(['no_such_panel']));
  assert.deepEqual(h.onProfile(), ON);
  assert.equal(h.inputs('toggle_viz').length, 0, 'the server state is not echoed back');
  h.close();
});

test('a tick adds a panel after the pause, as one toggle_viz', () => {
  const h = boot();
  h.click(row(h, 'advs_all__TEMP'));
  assert.deepEqual(h.onProfile(), ON.concat(['advs_all__TEMP']), 'moved up at once');
  h.tick(500);
  assert.equal(h.inputs('toggle_viz').length, 0, 'nothing before the pause');
  h.tick(600);
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['advs_all__TEMP']);
  assert.equal(h.inputs('reorder_viz').length, 0);
  h.close();
});

test('quick picks are reported together, as one set_viz', () => {
  const h = boot();
  h.click(row(h, 'ae_gantt'));
  h.tick(400);
  h.click(row(h, 'advs_all__PULSE'));
  h.tick(400);
  h.click(row(h, 'advs_all__TEMP'));
  h.tick(1000);
  // One update for the three changes, so the profile redraws once.
  assert.equal(h.inputs('toggle_viz').length, 0);
  assert.equal(h.inputs('reorder_viz').length, 0);
  assert.deepEqual(h.inputs('set_viz').map((i) => i.value),
    [ON.filter((v) => v !== 'ae_gantt').concat(['advs_all__PULSE', 'advs_all__TEMP'])]);
  h.close();
});

test('None is one set_viz, and the profile has no All', () => {
  const h = boot();
  const btns = h.qa('#proxy1-pp_panels .blockr-pick__bulk-btn').map((b) => b.textContent);
  assert.deepEqual(btns, ['None']);
  h.q('#proxy1-pp_panels .blockr-pick__bulk-btn').click();
  h.tick(1100);
  assert.equal(h.inputs('toggle_viz').length, 0);
  assert.deepEqual(h.inputs('set_viz').map((i) => i.value), [[]]);
  h.close();
});

test('a tick and its untick inside the pause send nothing', () => {
  const h = boot();
  h.click(row(h, 'advs_all__TEMP'));
  h.click(row(h, 'advs_all__TEMP'));
  h.tick(1100);
  assert.equal(h.inputs().filter((i) => /toggle_viz|reorder_viz/.test(i.name)).length, 0);
  h.close();
});

test('dragging a pick sends the new order after the pause', () => {
  const h = boot();
  dragOnto(h, row(h, 'patient_overview'), row(h, 'adlbc_all__ALT'), true);
  const want = ON.slice(1).concat(['patient_overview']);
  assert.deepEqual(h.onProfile(), want);
  h.tick(1100);
  assert.deepEqual(h.lastInput('reorder_viz'), want);
  assert.equal(h.inputs('toggle_viz').length, 0);
  // The echo leaves it as it is and sends nothing.
  h.resetInputs();
  h.send('sync_selected', want);
  assert.deepEqual(h.onProfile(), want);
  h.tick(1100);
  assert.equal(h.inputs().length, 0);
  h.close();
});

test('an addition dragged up is a toggle and then a reorder', () => {
  const h = boot();
  h.click(row(h, 'advs_all__TEMP'));
  dragOnto(h, row(h, 'advs_all__TEMP'), row(h, 'patient_overview'), false);
  h.tick(1100);
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['advs_all__TEMP']);
  assert.deepEqual(h.lastInput('reorder_viz'), ['advs_all__TEMP'].concat(ON));
  const names = h.inputs().map((i) => i.name.replace('proxy1-', ''))
    .filter((n) => n === 'toggle_viz' || n === 'reorder_viz');
  assert.deepEqual(names, ['toggle_viz', 'reorder_viz'], 'the toggle first');
  h.close();
});

test('a sync during the pause sends the pending picks and keeps them on screen', () => {
  const h = boot();
  h.click(row(h, 'advs_all__TEMP'));
  // The server, not yet told, says the old state.
  h.send('sync_selected', ON);
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['advs_all__TEMP']);
  assert.deepEqual(h.onProfile(), ON.concat(['advs_all__TEMP']));
  h.send('sync_selected', ON.concat(['advs_all__TEMP']));
  h.tick(1100);
  assert.equal(h.inputs('toggle_viz').length, 1, 'nothing sent twice');
  h.close();
});

test('the panel picker rendering again keeps what is on the profile', () => {
  const h = boot();
  const host = h.el('panel_picker');
  const fresh = h.el('pp_panels').cloneNode(false);
  host.innerHTML = '';
  host.appendChild(fresh);
  h.rendered('panel_picker');
  h.tick(0);
  assert.deepEqual(h.onProfile(), ON);
  assert.equal(h.qa('#proxy1-pp_panels .blockr-pick').length, 1);
  h.close();
});

test('the picker filter narrows the panels and leaves the patients', () => {
  const h = boot();
  h.type(filterBox(h), 'temp');
  const shown = h.qa('#proxy1-pp_panels .blockr-select__option').map((r) => r.getAttribute('data-value'));
  assert.deepEqual(shown, ['advs_all__TEMP']);
  assert.equal(h.shownIds().length, 12);
  h.close();
});

// --- the patients' filter box ----------------------------------------------

test('the patient box filters the patients only', () => {
  const h = boot();
  h.type(search(h), ' 1023 ');
  assert.deepEqual(h.shownIds(), ['01-701-1023']);
  assert.equal(h.q('.pp-pt.is-enter').getAttribute('data-usubjid'), '01-701-1023');
  assert.equal(h.el('search_clear').classList.contains('is-hidden'), false);
  assert.equal(h.onProfile().length, 5, 'the panels are untouched');
  h.type(search(h), 'zzzz');
  assert.equal(h.shownIds().length, 0);
  assert.equal(h.q('.is-enter'), null);
  h.close();
});

test('Escape and the clear button empty the patient box', () => {
  const h = boot();
  h.type(search(h), 'zzzz');
  h.key(search(h), 'Escape');
  assert.equal(search(h).value, '');
  assert.equal(h.shownIds().length, 12);
  h.type(search(h), '1023');
  h.click(h.el('search_clear'));
  assert.equal(search(h).value, '');
  assert.equal(h.shownIds().length, 12);
  assert.equal(h.el('search_clear').classList.contains('is-hidden'), true);
  h.close();
});

test('the arrows walk the patients, clamped, and Enter picks one', () => {
  const h = boot();
  h.type(search(h), '01-701');
  const list = h.shownIds();
  assert.ok(list.length > 3);
  const at = () => h.q('.pp-pt.is-enter').getAttribute('data-usubjid');
  assert.equal(at(), list[0]);
  assert.equal(h.key(search(h), 'ArrowDown').defaultPrevented, true);
  assert.equal(at(), list[1]);
  h.key(search(h), 'ArrowUp');
  h.key(search(h), 'ArrowUp');
  assert.equal(at(), list[0], 'up from the top stays put');
  h.key(search(h), 'ArrowDown');
  h.key(search(h), 'Enter');
  h.tick(1000);
  assert.equal(h.lastInput('pick_subject'), list[1]);
  h.close();
});

test('the filter survives the cohort list rendering again', () => {
  const h = boot();
  h.type(search(h), '1023');
  h.qa('.pp-pt').forEach((r) => r.classList.remove('is-filtered-out'));
  h.rendered('sidebar_cohort');
  h.tick(0);
  assert.deepEqual(h.shownIds(), ['01-701-1023']);
  h.close();
});

test('"Search for one" focuses the patient box, "Add one" the picker filter', () => {
  const h = boot();
  const link = (target) => {
    const b = h.doc.createElement('button');
    b.className = 'blockr-slot pp-open-search';
    if (target) b.setAttribute('data-target', target);
    h.el('pp_layout').appendChild(b);
    return b;
  };
  h.click(link('patients'));
  assert.equal(h.doc.activeElement, search(h));
  h.click(link());
  assert.equal(h.doc.activeElement, filterBox(h));
  h.close();
});
