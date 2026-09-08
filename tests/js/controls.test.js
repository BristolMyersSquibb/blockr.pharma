/* The controls in a panel's header: pills, chips, toggles, radios and the
 * find box, all funnelled into one viz_ctrl input. And the chart area's
 * resize watch. */
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

const gantt = (h) => h.el('viz_slot_ae_gantt');

test('a pill cycles its values and sends the one it shows', () => {
  const h = boot();
  const pill = gantt(h).querySelector('.pp-ctrl-pill');
  assert.equal(pill.getAttribute('data-index'), '1');
  assert.equal(pill.textContent, 'Preferred term');
  h.click(pill);
  assert.equal(pill.getAttribute('data-index'), '2');
  assert.equal(pill.textContent, 'High-level term');
  assert.equal(pill.getAttribute('title'), 'Switch to Body system');
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'lanes', value: 'AEHLT' });
  h.click(pill); h.click(pill);
  assert.equal(pill.getAttribute('data-index'), '0');
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'lanes', value: 'AETERM' });
  h.close();
});

test('the remove button on a panel sends the toggle', () => {
  const h = boot();
  h.click(gantt(h).querySelector('.pp-chart-remove'));
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['ae_gantt']);
  h.close();
});

test('chips, toggles and radios built like the header\'s are handled', () => {
  const h = boot();
  const header = gantt(h).querySelector('.pp-chart-header');
  header.insertAdjacentHTML('beforeend', `
    <div class="pp-ctrl-chips">
      <span class="pp-ctrl-chip is-active" data-viz-id="ae_gantt" data-param="sev" data-value="MILD">Mild</span>
      <span class="pp-ctrl-chip" data-viz-id="ae_gantt" data-param="sev" data-value="SEVERE">Severe</span>
    </div>
    <span class="pp-ctrl-toggle" data-viz-id="ae_gantt" data-param="ongoing">Ongoing</span>
    <span class="pp-ctrl-radio is-active" data-viz-id="ae_gantt" data-param="scale" data-value="lin">Lin</span>
    <span class="pp-ctrl-radio" data-viz-id="ae_gantt" data-param="scale" data-value="log">Log</span>`);

  h.click(header.querySelector('.pp-ctrl-chip[data-value="SEVERE"]'));
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'sev', value: ['MILD', 'SEVERE'] });
  h.click(header.querySelector('.pp-ctrl-chip[data-value="MILD"]'));
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'sev', value: ['SEVERE'] });

  const toggle = header.querySelector('.pp-ctrl-toggle');
  h.click(toggle);
  assert.equal(toggle.classList.contains('is-on'), true);
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'ongoing', value: true });
  h.click(toggle);
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'ongoing', value: false });

  const log = header.querySelector('.pp-ctrl-radio[data-value="log"]');
  h.click(log);
  assert.equal(log.classList.contains('is-active'), true);
  assert.equal(header.querySelector('.pp-ctrl-radio[data-value="lin"]').classList.contains('is-active'), false);
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'scale', value: 'log' });
  h.close();
});

test('the find box waits 400ms after the last keystroke, then sends once', () => {
  const h = boot();
  const box = gantt(h).querySelector('.pp-ctrl-search-input');
  h.type(box, 'he');
  assert.equal(box.closest('.pp-ctrl-search').classList.contains('is-active'), true);
  h.tick(300);
  h.type(box, 'head');
  h.tick(399);
  assert.equal(h.inputs('viz_ctrl').length, 0);
  h.tick(1);
  assert.deepEqual(h.inputs('viz_ctrl').map((i) => i.value),
    [{ viz_id: 'ae_gantt', param: 'search', value: 'head' }]);
  h.type(box, '');
  assert.equal(box.closest('.pp-ctrl-search').classList.contains('is-active'), false);
  h.close();
});

test('a panel re-rendering within 2.5s gets the find text and focus back', () => {
  const h = boot();
  const slot = gantt(h);
  const box = slot.querySelector('.pp-ctrl-search-input');
  h.type(box, 'head');
  h.tick(400);
  // R re-renders the panel with an empty box, as the search filters it.
  const html = slot.innerHTML;
  h.doc.body.focus();
  slot.innerHTML = html;
  const fresh = slot.querySelector('.pp-ctrl-search-input');
  fresh.value = '';
  h.rendered('viz_slot_ae_gantt');
  h.tick(0);
  assert.equal(fresh.value, 'head');
  assert.equal(h.doc.activeElement, fresh);

  // Long after, a render leaves the box alone.
  h.tick(3000);
  slot.innerHTML = html;
  const later = slot.querySelector('.pp-ctrl-search-input');
  later.value = '';
  h.rendered('viz_slot_ae_gantt');
  h.tick(0);
  assert.equal(later.value, '');
  h.close();
});

test('the clear glyph and the caption\'s find link both empty the search', () => {
  const h = boot();
  const wrap = gantt(h).querySelector('.pp-ctrl-search');
  wrap.insertAdjacentHTML('beforeend',
    '<span class="pp-ctrl-search-clear" data-viz-id="ae_gantt" data-param="search"></span>');
  h.type(wrap.querySelector('input'), 'x');
  h.click(wrap.querySelector('.pp-ctrl-search-clear'));
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'search', value: '' });
  h.tick(400);
  assert.equal(h.inputs('viz_ctrl').length, 1, 'the pending keystroke was cancelled');

  h.el('cohort_band_caption').insertAdjacentHTML('beforeend',
    '<a class="pp-cohort-bandcap-find" data-viz-id="ae_gantt"></a>');
  h.click(h.q('.pp-cohort-bandcap-find'));
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'search', value: '' });
  h.close();
});

test('the chart area is watched for size and its charts resized, debounced', () => {
  const h = boot();
  const area = h.q('.pp-chart-area');
  assert.ok(area, 'the chart area exists');
  assert.ok(h.observed().resize.includes(area));
  const widget = gantt(h).querySelector('.echarts4r');
  widget.__echarts = true;
  h.resize(area);
  h.resize(area);
  h.tick(79);
  assert.equal(h.win.echarts.__resized.length, 0);
  h.tick(1);
  assert.equal(h.win.echarts.__resized.length, 1);
  assert.equal(h.win.echarts.__resized[0], widget);

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
