/* The words in a panel's sentence and its checkboxes, all funnelled into
 * one viz_ctrl input; the remove button; and the chart area's resize watch. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount, json } = require('./harness.js');

const boot = () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  return h;
};

const gantt = (h) => h.el('viz_slot_ae_gantt');
const word = (h, slot, param) => h.el(slot).querySelector(`.pp-slot[data-param="${param}"]`);
const menu = (h) => h.q('.blockr-select__dropdown');
const rows = (h) => h.qa('.blockr-select__option').map((e) => e.textContent);
const pick = (h, text) => {
  const opt = h.qa('.blockr-select__option').find((e) => e.textContent.startsWith(text));
  opt.dispatchEvent(new h.win.Event('click', { bubbles: true }));
};
const closeMenu = (h) => h.doc.body.dispatchEvent(new h.win.Event('click', { bubbles: true }));

test('the sentence names what the panel draws, each setting a live word', () => {
  const h = boot();
  const sentence = gantt(h).querySelector('.pp-chart-sentence').textContent;
  assert.match(sentence, /^\d+ events by preferred term, showing all$/);
  assert.equal(word(h, 'viz_slot_ae_gantt', 'lanes').className, 'blockr-slot pp-slot');
  h.close();
});

test('a single word opens its values, labels first, and a pick applies at once', () => {
  const h = boot();
  const w = word(h, 'viz_slot_ae_gantt', 'lanes');
  h.click(w);
  assert.ok(menu(h), 'the menu opened');
  assert.equal(h.q('.blockr-select__menu-title').textContent, 'Lanes');
  assert.ok(rows(h).includes('Preferred termAEDECOD'));
  assert.equal(w.classList.contains('blockr-slot--open'), true);
  // The value already shown is not a change.
  pick(h, 'Preferred term');
  h.tick(10);
  assert.equal(h.inputs('viz_ctrl').length, 0);
  h.click(word(h, 'viz_slot_ae_gantt', 'lanes'));
  pick(h, 'Body system');
  h.tick(10);
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'lanes', value: 'AEBODSYS' });
  h.close();
});

test('a findings card names its value in words', () => {
  const h = boot();
  const w = word(h, 'viz_slot_adlbc_all__ALB', 'value');
  assert.ok(w, 'the card carries a value word');
  assert.equal(w.textContent, 'Analysis value');
  h.click(w);
  assert.deepEqual(rows(h), ['Analysis valueAVAL', 'Change from baselineCHG']);
  pick(h, 'Change from baseline');
  h.tick(10);
  assert.deepEqual(h.lastInput('viz_ctrl'),
    { viz_id: 'adlbc_all__ALB', param: 'value', value: 'CHG' });
  h.close();
});

const VOCAB = {
  viz_id: 'ae_gantt', token: '7',
  groups: [{ col: 'AEDECOD', label: 'Preferred term',
    options: [{ value: 'DIARRHOEA', n: 4 }, { value: 'SYNCOPE', n: 2 }] }]
};

test('the filter asks for the cohort terms on first open and waits for them', () => {
  const h = boot();
  const w = word(h, 'viz_slot_ae_gantt', 'find');
  h.click(w);
  assert.equal(menu(h), null, 'not open yet');
  assert.deepEqual(json(h.lastInput('find_vocab')).viz_id, 'ae_gantt');
  h.send('find_vocab', VOCAB);
  assert.ok(menu(h), 'opens when the terms land');
  assert.equal(h.q('.blockr-select__menu-title').textContent, 'Show');
  // This patient's terms first, in the panel's casing, with their counts;
  // then the rest of the cohort's, once.
  const r = rows(h);
  assert.ok(r[0].startsWith('Application site erythema'));
  assert.equal(r.filter((x) => x.startsWith('Diarrhoea')).length, 1);
  assert.ok(r.includes('Syncope'));
  h.close();
});

test('without an answer the filter opens with the patient\'s terms alone', () => {
  const h = boot();
  h.click(word(h, 'viz_slot_ae_gantt', 'find'));
  h.tick(1600);
  assert.ok(menu(h));
  assert.ok(!rows(h).includes('Syncope'));
  h.close();
});

test('filter picks are sent once, on close, as the data spells them', () => {
  const h = boot();
  h.send('find_vocab', VOCAB);
  h.click(word(h, 'viz_slot_ae_gantt', 'find'));
  assert.ok(menu(h), 'held terms: opens at once');
  pick(h, 'Syncope');
  pick(h, 'Diarrhoea');
  assert.equal(h.inputs('viz_ctrl').length, 0, 'nothing while picking');
  closeMenu(h);
  h.tick(10);
  assert.deepEqual(h.lastInput('viz_ctrl'), {
    viz_id: 'ae_gantt', param: 'find',
    value: [{ col: 'AEDECOD', value: 'SYNCOPE' }, { col: 'AEDECOD', value: 'DIARRHOEA' }]
  });
  h.close();
});

test('opening and closing a filter without a change sends nothing', () => {
  const h = boot();
  h.send('find_vocab', VOCAB);
  h.click(word(h, 'viz_slot_ae_gantt', 'find'));
  closeMenu(h);
  h.tick(10);
  assert.equal(h.inputs('viz_ctrl').length, 0);
  h.close();
});

test('the remove button on a panel sends the toggle', () => {
  const h = boot();
  h.click(gantt(h).querySelector('.pp-chart-remove'));
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['ae_gantt']);
  h.close();
});

test('the chart area is watched for size and its charts resized, debounced', () => {
  const h = boot();
  const area = h.q('.pp-chart-area');
  assert.ok(area, 'the chart area exists');
  assert.ok(h.observed().resize.includes(area));
  const widget = gantt(h).querySelector('.echarts4r');
  widget.__echarts = true;
  let width = 400;
  Object.defineProperty(area, 'clientWidth', { get: () => width, configurable: true });
  h.resize(area);
  h.resize(area);
  h.tick(79);
  assert.equal(h.win.echarts.__resized.length, 0);
  h.tick(1);
  assert.equal(h.win.echarts.__resized.length, 1);
  assert.equal(h.win.echarts.__resized[0], widget);
  // 'auto', so a size htmlwidgets pinned on the instance is measured again.
  assert.deepEqual(json(h.win.echarts.__resizeOpts[0]), { width: 'auto', height: 'auto' });

  // A collapsed rail hides the area: nothing is resized to 0.
  width = 0;
  h.resize(area);
  h.tick(80);
  assert.equal(h.win.echarts.__resized.length, 1);
  width = 400;
  h.resize(area);
  h.tick(80);
  assert.equal(h.win.echarts.__resized.length, 2);

  // A render that replaces the area re-watches the new element.
  const again = h.doc.createElement('div');
  again.className = area.className;
  again.append(...Array.from(area.childNodes));
  area.replaceWith(again);
  h.rendered('chart_area');
  assert.notEqual(again, area);
  assert.ok(h.observed().resize.includes(again));
  h.close();
});
