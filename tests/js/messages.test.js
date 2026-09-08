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

test('subject_picker fills the cohort count and hides it at zero', () => {
  const h = boot();
  const count = h.el('pp_cohort_count');
  h.send('subject_picker', { count: 254 });
  assert.equal(count.querySelector('.pp-cohort-count-n').textContent, '254 patients');
  assert.equal(count.classList.contains('is-hidden'), false);
  h.send('subject_picker', { count: 1 });
  assert.equal(count.querySelector('.pp-cohort-count-n').textContent, '1 patient');
  h.send('subject_picker', { count: 0 });
  assert.equal(count.classList.contains('is-hidden'), true);
  h.send('subject_picker', null);
  assert.equal(count.classList.contains('is-hidden'), true);
  h.close();
});

test('dl_menu_state labels the two download scopes and hides what does not apply', () => {
  const h = boot();
  const root = h.el('pp_dl_root');
  h.send('dl_menu_state', { single: true, picked: '01-701-1015', n: 254 });
  assert.equal(h.el('pp_dl_label_patient').textContent, 'This patient: 01-701-1015');
  assert.equal(h.el('pp_dl_label_cohort').textContent, 'Cohort (254 patients)');
  assert.equal(root.classList.contains('is-hidden'), false);
  assert.equal(root.querySelector('.pp-dl-scope-patient').classList.contains('is-hidden'), false);
  assert.equal(root.querySelector('.pp-dl-scope-cohort').classList.contains('is-hidden'), false);

  // One patient upstream and none picked: nothing to download.
  h.send('dl_menu_state', { single: false, picked: '', n: 1 });
  assert.equal(h.el('pp_dl_label_patient').textContent, 'This patient');
  assert.equal(h.el('pp_dl_label_cohort').textContent, 'Cohort (1 patient)');
  assert.equal(root.classList.contains('is-hidden'), true);
  assert.equal(root.querySelector('.pp-dl-scope-patient').classList.contains('is-hidden'), true);
  assert.equal(root.querySelector('.pp-dl-scope-cohort').classList.contains('is-hidden'), true);
  h.close();
});

test('dl_menu_state waits for the header to exist, up to three seconds', () => {
  const h = boot();
  const header = h.el('header_bar');
  const html = header.innerHTML;
  header.innerHTML = '';
  h.send('dl_menu_state', { single: true, picked: 'A', n: 2 });
  h.tick(1000);
  header.innerHTML = html;
  h.tick(100);
  assert.equal(h.el('pp_dl_label_patient').textContent, 'This patient: A');

  header.innerHTML = '';
  h.send('dl_menu_state', { single: true, picked: 'B', n: 2 });
  h.tick(3100);
  header.innerHTML = html;
  h.tick(1000);
  assert.equal(h.el('pp_dl_label_patient').textContent, 'This patient', 'gave up');
  h.close();
});

test('sync_band tags the panel the cohort strip draws, and re-tags after a render', () => {
  const h = boot();
  h.send('sync_band', { viz_id: 'ae_gantt' });
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

test('sync_params accepts a list, a string or nothing', () => {
  const h = boot();
  assert.doesNotThrow(() => h.send('sync_params', ['adlbc_all@@ALB', 'adlbc_all@@ALT']));
  assert.doesNotThrow(() => h.send('sync_params', 'adlbc_all@@ALB'));
  assert.doesNotThrow(() => h.send('sync_params', null));
  h.close();
});

test('the gear opens its popover and any click elsewhere closes it', () => {
  const h = boot();
  const btn = h.el('pp_gear_btn');
  const pop = h.el('pp_gear_popover');
  h.click(btn);
  assert.equal(pop.classList.contains('is-open'), true);
  assert.equal(btn.classList.contains('is-active'), true);
  // Inside the popover: stays open.
  h.click(pop);
  assert.equal(pop.classList.contains('is-open'), true);
  h.click(h.doc.body);
  assert.equal(pop.classList.contains('is-open'), false);
  assert.equal(btn.classList.contains('is-active'), false);
  h.close();
});

test('the timeline, pre-study and smoothing toggles flip their state and send it', () => {
  const h = boot();
  const tl = h.q('.pp-popover-toggle[data-tl-mode]');
  const pre = h.q('.pp-popover-toggle[data-prestudy]');
  const smooth = h.q('.pp-popover-toggle[data-smooth]');
  assert.ok(tl && pre && smooth, 'the header has all three');

  const tl0 = tl.getAttribute('data-tl-mode');
  h.click(tl);
  const tl1 = tl0 === 'rday' ? 'date' : 'rday';
  assert.equal(tl.getAttribute('data-tl-mode'), tl1);
  assert.equal(tl.textContent, tl1 === 'rday' ? 'Relative day' : 'Date');
  assert.equal(h.lastInput('timeline_mode'), tl1);
  // Disabled: no change, nothing sent.
  tl.setAttribute('data-disabled', '1');
  h.click(tl);
  assert.equal(tl.getAttribute('data-tl-mode'), tl1);
  assert.equal(h.inputs('timeline_mode').length, 1);

  const pre0 = pre.getAttribute('data-prestudy') === '1';
  h.click(pre);
  assert.equal(pre.getAttribute('data-prestudy'), pre0 ? '0' : '1');
  assert.equal(pre.textContent, pre0 ? 'Screening only' : 'Full history');
  assert.equal(h.lastInput('show_prestudy'), !pre0);

  const s0 = smooth.getAttribute('data-smooth');
  h.click(smooth);
  const s1 = s0 === 'off' ? 'auto' : 'off';
  assert.equal(smooth.getAttribute('data-smooth'), s1);
  assert.equal(smooth.textContent, s1 === 'off' ? 'Straight' : 'Smooth');
  assert.equal(h.lastInput('smooth_mode'), s1);
  h.close();
});

test('the cohort count toggles the sidebar', () => {
  const h = boot();
  const sidebar = h.el('pp_sidebar');
  const layout = h.el('pp_layout');
  const count = h.el('pp_cohort_count');
  h.click(count);
  assert.equal(sidebar.classList.contains('collapsed'), true);
  assert.equal(layout.classList.contains('sidebar-collapsed'), true);
  assert.equal(count.classList.contains('is-shut'), true);
  h.click(count);
  assert.equal(sidebar.classList.contains('collapsed'), false);
  assert.equal(layout.classList.contains('sidebar-collapsed'), false);
  assert.equal(count.classList.contains('is-shut'), false);
  h.close();
});
