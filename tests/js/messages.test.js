/* The custom messages R sends, and what the header does with them. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const boot = () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  return h;
};

// The cohort's status is painted into two places: the patients' header in
// the sidebar, and the header row (shown only while the list is shut).
const statuses = (h) => h.qa(`#${h.NS}-pp_layout .pp-cohort-status`);
const statusText = (h) => statuses(h).map((el) => el.textContent.trim());
const resets = (h) => h.qa(`#${h.NS}-pp_layout .pp-drill-reset`);

test('subject_picker writes the cohort in grey, in both places', () => {
  const h = boot();
  assert.equal(statuses(h).length, 2, 'the sidebar header and the header row');
  h.sendRecorded('subject_picker');
  assert.deepEqual(statusText(h), ['12 patients', '12 patients']);
  assert.equal(resets(h).length, 0, 'no reset while nothing is drilled');
  h.send('subject_picker', { count: 1 });
  assert.deepEqual(statusText(h), ['1 patient', '1 patient']);
  h.send('subject_picker', { count: 0 });
  assert.deepEqual(statusText(h), ['', '']);
  h.close();
});

test('a drill turns the count into the reset, with what it started from', () => {
  const h = boot();
  h.send('subject_picker', { count: 6 });
  h.send('drill', { clause: 'SEX = F', before: 179 });
  assert.deepEqual(statusText(h), ['6 of 179 patients', '6 of 179 patients']);
  const r = resets(h)[0];
  // Names what is undone, and how many come back.
  assert.equal(r.getAttribute('data-blockr-tooltip'), 'Show all 179 patients, clearing SEX = F');
  h.send('drill', { clause: '' });
  assert.deepEqual(statusText(h), ['6 patients', '6 patients']);
  assert.equal(resets(h).length, 0);
  h.close();
});

test('a drill whose filter did not count says only what it kept', () => {
  // A remote dm: the drill filter skips the count.
  const h = boot();
  h.send('subject_picker', { count: 6 });
  h.send('drill', { clause: 'SEX = F' });
  assert.deepEqual(statusText(h), ['6 patients', '6 patients']);
  assert.equal(resets(h)[0].getAttribute('data-blockr-tooltip'), 'Show all patients, clearing SEX = F');
  h.close();
});

test('a drill that matches nobody still says so, and keeps the reset', () => {
  const h = boot();
  h.send('subject_picker', { count: 0 });
  assert.equal(resets(h).length, 0);
  // No patients BECAUSE of a drill: a result, and the state where the way
  // back matters most.
  h.send('drill', { clause: 'SEX = M', before: 12 });
  assert.deepEqual(statusText(h), ['0 of 12 patients', '0 of 12 patients']);
  h.close();
});

test('either message can land first; the status is painted from both', () => {
  const h = boot();
  h.send('drill', { clause: 'SEX = M', before: 40 });
  h.send('subject_picker', { count: 33 });
  assert.deepEqual(statusText(h), ['33 of 40 patients', '33 of 40 patients']);
  h.close();
});

test('the reset asks R to undrill and leaves the list alone', () => {
  const h = boot();
  const sidebar = h.el('pp_sidebar');
  h.send('subject_picker', { count: 6 });
  h.send('drill', { clause: 'SEX = M', before: 12 });
  const shutBefore = sidebar.classList.contains('collapsed');
  h.click(resets(h)[0]);
  assert.equal(h.inputs('undrill').length, 1);
  assert.equal(sidebar.classList.contains('collapsed'), shutBefore);
  h.close();
});

test('dl_menu_state names the cohort and hides the section that does not apply', () => {
  const h = boot();
  const root = h.el('pp_dl_root');
  const shown = (scope) => h.qa(`#${h.NS}-pp_dl_root [data-scope="${scope}"]`)
    .map((el) => !el.hidden);
  // What R sent once a patient was picked: single, and the cohort's size.
  h.sendRecorded('dl_menu_state');
  assert.equal(h.el('pp_dl_label_cohort').textContent, 'Cohort · 12 patients');
  assert.equal(root.hidden, false);
  assert.ok(shown('patient').every(Boolean));
  assert.ok(shown('cohort').every(Boolean));
  // Before the pick R said: nothing single, twelve in the cohort.
  h.sendRecorded('dl_menu_state', 0);
  assert.equal(root.hidden, false, 'the cohort section still applies');
  assert.ok(shown('patient').every((x) => !x));
  // One patient upstream and none picked: nothing to download.
  h.send('dl_menu_state', { single: false, picked: '', n: 1 });
  assert.equal(h.el('pp_dl_label_cohort').textContent, 'Cohort · 1 patient');
  assert.equal(root.hidden, true);
  h.close();
});

test('dl_menu_state waits for the menu to exist, up to three seconds', () => {
  const h = boot();
  const root = h.el('pp_dl_root');
  const parent = root.parentNode;
  const next = root.nextSibling;
  root.remove();
  h.send('dl_menu_state', { single: true, picked: 'A', n: 2 });
  h.tick(1000);
  parent.insertBefore(root, next);
  h.tick(100);
  assert.equal(h.el('pp_dl_label_cohort').textContent, 'Cohort · 2 patients');

  root.remove();
  h.send('dl_menu_state', { single: true, picked: 'B', n: 5 });
  h.tick(3100);
  parent.insertBefore(root, next);
  h.tick(1000);
  assert.equal(h.el('pp_dl_label_cohort').textContent, 'Cohort · 2 patients', 'gave up');
  h.close();
});

test('the gear opens its tray in flow and says so', () => {
  const h = boot();
  const gear = h.el('pp_gear_btn');
  const tray = h.el('pp_gear_tray');
  assert.equal(gear.getAttribute('aria-expanded'), 'false');
  h.click(gear);
  assert.equal(gear.getAttribute('aria-expanded'), 'true');
  assert.equal(tray.classList.contains('blockr-settings--open'), true);
  // A click inside or elsewhere leaves it open; the gear closes it.
  h.click(h.doc.body);
  assert.equal(tray.classList.contains('blockr-settings--open'), true);
  h.click(gear);
  assert.equal(gear.getAttribute('aria-expanded'), 'false');
  h.close();
});

test('gear_state builds the Display section and each control sends its input', () => {
  const h = boot();
  h.sendRecorded('gear_state');
  const display = h.el('pp_gear_display');
  const seg = display.querySelector('.blockr-segmented');
  const boxes = Array.from(display.querySelectorAll('.blockr-checkbox'));
  assert.ok(seg, 'the timeline is a segmented control');
  assert.deepEqual(boxes.map((b) => b.textContent),
    ['Hide data before day −30', 'Smooth lines']);
  // What R sent: relative day, the axis cut at day -30, smooth lines.
  assert.equal(seg.querySelector('.is-selected').textContent, 'Relative day');
  assert.deepEqual(boxes.map((b) => b.querySelector('input').checked), [true, true]);

  h.click(Array.from(seg.querySelectorAll('button')).find((b) => b.textContent === 'Date'));
  assert.equal(h.lastInput('timeline_mode'), 'date');
  const clip = boxes[0].querySelector('input');
  clip.checked = false;
  clip.dispatchEvent(new h.win.Event('change', { bubbles: true }));
  assert.equal(h.lastInput('show_prestudy'), true, 'unchecked: the full history');
  const smooth = boxes[1].querySelector('input');
  smooth.checked = false;
  smooth.dispatchEvent(new h.win.Event('change', { bubbles: true }));
  assert.equal(h.lastInput('smooth_mode'), 'off');

  // A study without a treatment start: no relative day, nothing to cut at.
  h.send('gear_state', { rday: false, mode: 'date', prestudy: false, smooth: 'auto' });
  assert.equal(display.querySelector('.blockr-segmented'), null);
  assert.deepEqual(Array.from(display.querySelectorAll('.blockr-checkbox')).map((b) => b.textContent),
    ['Smooth lines']);
  h.close();
});

test('the list toggle opens and closes the list, and says which', () => {
  const h = boot();
  const sidebar = h.el('pp_sidebar');
  const layout = h.el('pp_layout');
  const count = h.el('pp_cohort_count');
  h.click(count);
  assert.equal(sidebar.classList.contains('collapsed'), true);
  assert.equal(layout.classList.contains('sidebar-collapsed'), true);
  assert.equal(count.classList.contains('is-shut'), true);
  assert.equal(count.getAttribute('data-blockr-tooltip'), 'Show the list of patients');
  h.click(count);
  assert.equal(sidebar.classList.contains('collapsed'), false);
  assert.equal(count.classList.contains('is-shut'), false);
  assert.equal(count.getAttribute('data-blockr-tooltip'), 'Hide the list of patients');
  h.close();
});

test('sync_band tags the panel the cohort strip draws, and re-tags after a render', () => {
  const h = boot();
  h.sendRecorded('sync_band');
  assert.deepEqual(h.qa('.pp-is-band').map((e) => e.id), [`${h.NS}-viz_slot_ae_gantt`]);
  h.send('sync_band', { viz_id: 'adlbc_all__ALB' });
  assert.deepEqual(h.qa('.pp-is-band').map((e) => e.id), [`${h.NS}-viz_slot_adlbc_all__ALB`]);

  // The chart area re-renders without the class: the tag is remembered.
  h.el('viz_slot_adlbc_all__ALB').classList.remove('pp-is-band');
  h.rendered('chart_area');
  h.tick(0);
  assert.equal(h.el('viz_slot_adlbc_all__ALB').classList.contains('pp-is-band'), true);
  h.rendered('viz_slot_adlbc_all__ALB');
  h.tick(0);
  assert.equal(h.qa('.pp-is-band').length, 1);

  h.send('sync_band', { viz_id: '' });
  assert.equal(h.qa('.pp-is-band').length, 0);
  h.send('sync_band', null);
  assert.equal(h.qa('.pp-is-band').length, 0);
  h.close();
});

test('sync_params accepts what R sends, a list, and nothing', () => {
  const h = boot();
  assert.doesNotThrow(() => h.sendRecorded('sync_params'));
  assert.doesNotThrow(() => h.send('sync_params', ['adlbc_all@@ALB', 'adlbc_all@@ALT']));
  assert.doesNotThrow(() => h.send('sync_params', null));
  h.close();
});
