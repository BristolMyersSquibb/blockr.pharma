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

test('subject_picker fills the cohort count and hides the segment at zero', () => {
  const h = boot();
  const seg = h.el('pp_cohort_seg');
  const count = h.el('pp_cohort_count');
  h.sendRecorded('subject_picker');
  assert.equal(count.querySelector('.pp-cohort-count-n').textContent, '12 patients');
  assert.equal(seg.classList.contains('is-hidden'), false);
  h.send('subject_picker', { count: 254 });
  assert.equal(count.querySelector('.pp-cohort-count-n').textContent, '254 patients');
  h.send('subject_picker', { count: 1 });
  assert.equal(count.querySelector('.pp-cohort-count-n').textContent, '1 patient');
  h.send('subject_picker', { count: 0 });
  assert.equal(seg.classList.contains('is-hidden'), true);
  h.send('subject_picker', null);
  assert.equal(seg.classList.contains('is-hidden'), true);
  h.close();
});

test('a drill that matches nobody still says so, and keeps the reset', () => {
  const h = boot();
  const seg = h.el('pp_cohort_seg');
  const count = h.el('pp_cohort_count');
  const reset = h.el('pp_cohort_reset');
  // No patients and no drill: nothing loaded, so the segment stays away.
  h.send('subject_picker', { count: 0 });
  assert.equal(seg.classList.contains('is-hidden'), true);
  // No patients BECAUSE of a drill: a result, and the state where the way
  // back matters most. The segment says 0 and the reset works.
  h.send('drill', { clause: 'SEX = M' });
  assert.equal(seg.classList.contains('is-hidden'), false);
  assert.equal(count.querySelector('.pp-cohort-count-n').textContent, '0 patients');
  assert.equal(reset.disabled, false);
  assert.equal(seg.classList.contains('is-drilled'), true);
  h.send('drill', { clause: '' });
  assert.equal(seg.classList.contains('is-hidden'), true);
  assert.equal(reset.disabled, true);
  h.close();
});

test('either message can land first; the segment is painted from both', () => {
  const h = boot();
  const seg = h.el('pp_cohort_seg');
  const reset = h.el('pp_cohort_reset');
  // Drill first, count second: the count must not undo the drill.
  h.send('drill', { clause: 'SEX = M' });
  h.send('subject_picker', { count: 33 });
  assert.equal(seg.classList.contains('is-drilled'), true);
  assert.equal(reset.disabled, false);
  h.close();
});

test('the reset is disabled until a drill, names it, and asks R to undrill', () => {
  const h = boot();
  const seg = h.el('pp_cohort_seg');
  const count = h.el('pp_cohort_count');
  const reset = h.el('pp_cohort_reset');
  h.sendRecorded('subject_picker');
  // Always there, so the segment keeps its width; disabled with nothing to undo.
  assert.equal(reset.disabled, true);
  assert.equal(reset.getAttribute('data-blockr-tooltip'), null);
  h.send('drill', { clause: 'SEX = M' });
  // The class lands on the SEGMENT: it is what joins the two halves.
  assert.equal(seg.classList.contains('is-drilled'), true);
  assert.equal(reset.disabled, false);
  // Names what is undone, never where you land.
  assert.equal(reset.getAttribute('data-blockr-tooltip'), 'Reset drill-down: SEX = M');
  // The reset asks R; it does not touch the list the count toggles.
  const shutBefore = count.classList.contains('is-shut');
  h.click(reset);
  assert.equal(h.inputs('undrill').length, 1);
  assert.equal(count.classList.contains('is-shut'), shutBefore);
  h.send('drill', { clause: '' });
  assert.equal(seg.classList.contains('is-drilled'), false);
  assert.equal(reset.disabled, true);
  h.send('drill', null);
  assert.equal(seg.classList.contains('is-drilled'), false);
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

test('the cohort count toggles the list and turns its chevron', () => {
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
