/* The cohort well sizes itself to the space beneath it: the first scrolling
 * ancestor when there is one, the window otherwise. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const wellOf = (h) => h.el('pp_cohort_well');

test('without a scrolling ancestor the well fills to the bottom of the window', () => {
  const h = mount();
  h.win.innerHeight = 800;
  h.layout(wellOf(h), { top: 200, left: 0, width: 240, height: 100 });
  h.renderAll();
  h.tick(0);
  assert.equal(wellOf(h).style.maxHeight, '584px', '800 - 200 - 16');
  h.close();
});

test('a scrolling ancestor is the host, measured from its top and scroll', () => {
  const h = mount();
  const host = h.el('pp_sidebar');
  host.style.overflowY = 'auto';
  h.layout(host, { top: 100, left: 0, width: 240, height: 500 }, { clientHeight: 500, scrollTop: 0 });
  h.layout(wellOf(h), { top: 200, left: 0, width: 240, height: 100 });
  h.renderAll();
  h.tick(0);
  assert.equal(wellOf(h).style.maxHeight, '384px', '500 - (200 - 100) - 16');

  // Both the host and the well's parent are watched.
  const watched = h.observed().resize;
  assert.ok(watched.includes(host));
  assert.ok(watched.includes(wellOf(h).parentElement));

  // The host shrinks: the observer fires, a frame later the well follows.
  h.layout(host, { top: 100, left: 0, width: 240, height: 300 }, { clientHeight: 300, scrollTop: 0 });
  h.resize(host);
  assert.equal(wellOf(h).style.maxHeight, '384px', 'not before the frame');
  h.tick(16);
  assert.equal(wellOf(h).style.maxHeight, '184px');

  // Scrolled down inside the host, the well has less room still.
  h.layout(host, { top: 100, left: 0, width: 240, height: 300 }, { clientHeight: 300, scrollTop: 50 });
  h.resize(host);
  h.tick(16);
  assert.equal(wellOf(h).style.maxHeight, '134px');
  h.close();
});

test('the well never goes below three rows', () => {
  const h = mount();
  const host = h.el('pp_sidebar');
  host.style.overflowY = 'auto';
  h.layout(host, { top: 100, left: 0, width: 240, height: 150 }, { clientHeight: 150 });
  h.layout(wellOf(h), { top: 200, left: 0, width: 240, height: 100 });
  h.renderAll();
  h.tick(0);
  assert.equal(wellOf(h).style.maxHeight, '132px');
  h.close();
});

test('a change under two pixels is not written, so the observer cannot loop', () => {
  const h = mount();
  h.win.innerHeight = 800;
  h.layout(wellOf(h), { top: 200, left: 0, width: 240, height: 100 });
  h.renderAll();
  h.tick(0);
  wellOf(h).style.maxHeight = '583px';
  h.win.dispatchEvent(new h.win.Event('resize'));
  assert.equal(wellOf(h).style.maxHeight, '583px');
  h.win.innerHeight = 700;
  h.win.dispatchEvent(new h.win.Event('resize'));
  assert.equal(wellOf(h).style.maxHeight, '484px');
  h.close();
});

test('a well that is not laid out is left alone', () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  assert.equal(wellOf(h).style.maxHeight, '');
  h.close();
});

test('the watch is re-attached when the cohort list or caption renders', () => {
  const h = mount();
  const host = h.el('pp_sidebar');
  host.style.overflowY = 'auto';
  h.layout(host, { top: 100, left: 0, width: 240, height: 500 }, { clientHeight: 500 });
  h.layout(wellOf(h), { top: 200, left: 0, width: 240, height: 100 });
  h.tick(0);
  assert.equal(wellOf(h).style.maxHeight, '384px');

  h.layout(wellOf(h), { top: 250, left: 0, width: 240, height: 100 });
  h.rendered('cohort_band_caption');
  assert.equal(wellOf(h).style.maxHeight, '384px', 'deferred a tick');
  h.tick(0);
  assert.equal(wellOf(h).style.maxHeight, '334px');

  // An unrelated output does not re-measure.
  h.layout(wellOf(h), { top: 300, left: 0, width: 240, height: 100 });
  h.rendered('subject_facts');
  h.tick(0);
  assert.equal(wellOf(h).style.maxHeight, '334px');
  h.close();
});
