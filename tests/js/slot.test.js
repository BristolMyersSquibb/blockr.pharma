/* The `slot` message: a panel brought to the current patient in place,
 * header swapped as DOM, chart updated through the live echarts instance. */
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

const widgetOf = (h, vid) => h.el(`viz_slot_${vid}`).querySelector('.pp-chart-body .echarts4r');

const msg = (over) => Object.assign({
  viz_id: 'ae_gantt',
  header: '<span class="pp-chart-grip"></span><div class="pp-chart-title">Adverse Events</div>' +
    '<button class="pp-chart-remove" data-viz-id="ae_gantt"></button>',
  opts_json: JSON.stringify({
    series: [{ type: 'custom', renderItem: 'function(p, api) { return api.value(0); }' }],
    tooltip: { formatter: 'function(x) { return x.name; }' },
    title: { text: 'function() { return 1; }' }
  }),
  evals: ['series.0.renderItem', 'tooltip.formatter', 'no.such.path'],
  height: 210
}, over || {});

test('a slot message swaps the header and updates the live chart in place', () => {
  const h = boot();
  const widget = widgetOf(h, 'ae_gantt');
  widget.__echarts = true;
  const canvasBefore = widget;
  h.send('slot', msg());

  const header = h.el('viz_slot_ae_gantt').querySelector('.pp-chart-header');
  assert.equal(header.querySelector('.pp-chart-title').textContent, 'Adverse Events');
  assert.equal(header.querySelector('.pp-ctrl-pill'), null, 'the old header is gone');
  assert.equal(h.win.__bound.map((b) => b[0]).join(), 'unbind,bind');

  assert.equal(widget, canvasBefore, 'the widget element survives');
  assert.equal(widget.style.height, '210px');
  const applied = h.win.echarts.__options;
  assert.equal(applied.length, 1);
  assert.equal(applied[0].el, widget);
  assert.deepEqual(json(applied[0].how), { notMerge: true });
  assert.equal(typeof applied[0].opts.series[0].renderItem, 'function', 'function text became code');
  assert.equal(typeof applied[0].opts.tooltip.formatter, 'function');
  assert.equal(typeof applied[0].opts.title.text, 'string', 'text not on the list stays text');
  assert.equal(h.win.echarts.__resized.length, 1);
  assert.equal(h.win.echarts.__resized[0], widget);
  h.close();
});

test('the same header again is not swapped, so the find box keeps its focus', () => {
  const h = boot();
  widgetOf(h, 'ae_gantt').__echarts = true;
  h.send('slot', msg());
  h.win.__bound.length = 0;
  h.send('slot', msg({ height: 220 }));
  assert.equal(h.win.__bound.length, 0);
  assert.equal(widgetOf(h, 'ae_gantt').style.height, '220px');
  assert.equal(h.win.echarts.__options.length, 2);
  h.close();
});

test('a message that beats the instance waits for it, a frame at a time', () => {
  const h = boot();
  const widget = widgetOf(h, 'ae_gantt');
  h.send('slot', msg());
  h.tick(16 * 5);
  assert.equal(h.win.echarts.__options.length, 0);
  widget.__echarts = true;
  h.tick(16);
  assert.equal(h.win.echarts.__options.length, 1);
  h.close();
});

test('with no instance for two seconds it gives up quietly', () => {
  const h = boot();
  h.send('slot', msg());
  h.tick(16 * 130);
  assert.equal(h.win.echarts.__options.length, 0);
  assert.equal(h.clock.pending(), 0, 'no retry left ticking');
  h.close();
});

test('a message for a panel that is not on the profile does nothing', () => {
  const h = boot();
  assert.doesNotThrow(() => h.send('slot', msg({ viz_id: 'no_such_panel' })));
  assert.doesNotThrow(() => h.send('slot', null));
  assert.equal(h.win.__bound.length, 0);
  h.close();
});

test('a swapped header gets the find box text and caret back', () => {
  const h = boot();
  const slot = h.el('viz_slot_ae_gantt');
  widgetOf(h, 'ae_gantt').__echarts = true;
  const box = slot.querySelector('.pp-ctrl-search-input');
  h.type(box, 'head');
  h.tick(400);
  h.send('slot', msg({
    header: '<div class="pp-ctrl-search"><input type="text" class="pp-ctrl-search-input" ' +
      'data-viz-id="ae_gantt" data-param="search" value=""></div>'
  }));
  const fresh = slot.querySelector('.pp-ctrl-search-input');
  assert.notEqual(fresh, box);
  assert.equal(fresh.value, 'head');
  assert.equal(h.doc.activeElement, fresh);
  h.close();
});
