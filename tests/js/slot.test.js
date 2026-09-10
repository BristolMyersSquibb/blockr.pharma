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
  // The whole header, root included, as pp_slot_header_ui() renders it.
  header: '<div class="pp-chart-header"><span class="pp-chart-grip"></span>' +
    '<div class="pp-chart-title">Adverse Events</div>' +
    '<button class="pp-chart-remove" data-viz-id="ae_gantt"></button></div>',
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
  assert.equal(header.querySelector('.pp-chart-header'), null,
    'the new header replaces the old one, it is not put inside it');
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

test('the same header again is not swapped, so an open control is left alone', () => {
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

test('a swapped header leaves an open find popover standing', () => {
  // The header is replaced wholesale on every settings change, which is why
  // the popover is parented to <body> instead of living in the control. A
  // reader mid-pick must not lose their list because the panel redrew.
  const h = boot();
  const slot = h.el('viz_slot_ae_gantt');
  widgetOf(h, 'ae_gantt').__echarts = true;
  const trigger = slot.querySelector('.pp-ctrl-find');
  h.click(trigger);
  const popover = h.q('.pp-find-pop.is-open');
  assert.ok(popover);
  h.click(popover.querySelectorAll('.pp-find-opt')[0]);

  h.send('slot', msg({
    header: '<div class="pp-chart-header">' +
      '<button class="pp-ctrl-find" type="button" data-viz-id="ae_gantt" ' +
      'data-param="find" data-picks="[]" data-options="[]"></button></div>'
  }));
  const fresh = slot.querySelector('.pp-ctrl-find');
  assert.notEqual(fresh, trigger, 'the trigger was replaced');
  assert.equal(h.q('.pp-find-pop.is-open'), popover, 'the popover was not');
  assert.equal(popover.querySelectorAll('.pp-find-tag').length, 1,
    'and it still holds the pick that has not been sent yet');
  h.close();
});

test('a swapped header takes the class of the one that came, so a legend row can come and go', () => {
  const h = boot();
  const slot = h.el('viz_slot_ae_gantt');
  widgetOf(h, 'ae_gantt').__echarts = true;
  const header = slot.querySelector('.pp-chart-header');
  h.send('slot', msg({
    header: '<div class="pp-chart-header has-legend"><div class="pp-chart-title">Adverse Events</div>' +
      '<div class="pp-chart-legend"><span class="pp-legend-item">Mild</span></div></div>'
  }));
  assert.equal(slot.querySelector('.pp-chart-header'), header, 'the element itself survives');
  assert.equal(header.classList.contains('has-legend'), true);
  assert.equal(header.querySelector('.pp-chart-legend').textContent, 'Mild');
  h.send('slot', msg());
  assert.equal(header.classList.contains('has-legend'), false);
  assert.equal(header.querySelector('.pp-chart-legend'), null);
  h.close();
});
