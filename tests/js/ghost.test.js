/* The photograph a panel holds over itself while it re-renders, and the
 * four ways it comes down: the new canvas painted, no widget to wait for,
 * the deadline, a scroll. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const SLOT = 'viz_slot_ae_gantt';

const boot = () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  const slot = h.el(SLOT);
  const body = slot.querySelector('.pp-chart-body');
  h.layout(body, { top: 100, left: 50, width: 400, height: 200 });
  return { h, slot, body };
};

const recalculating = (h, slot) => {
  h.$(slot).trigger(h.$.Event('shiny:recalculating', { name: slot.id }));
};

test('recalculating photographs the chart body, once, at its box', () => {
  const { h, slot, body } = boot();
  recalculating(h, slot);
  const ghost = h.q('body > .pp-chart-ghost');
  assert.ok(ghost);
  assert.deepEqual([ghost.style.top, ghost.style.left, ghost.style.width, ghost.style.height],
    ['100px', '50px', '400px', '200px']);
  assert.equal(ghost.innerHTML, body.innerHTML, 'the body as it was');
  recalculating(h, slot);
  assert.equal(h.qa('.pp-chart-ghost').length, 1);
  h.close();
});

test('a body without a box is not photographed', () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  recalculating(h, h.el(SLOT));
  assert.equal(h.q('.pp-chart-ghost'), null);
  h.close();
});

test('the ghost holds until the new canvas exists, then cross-fades', () => {
  const { h, slot, body } = boot();
  recalculating(h, slot);
  const ghost = h.q('.pp-chart-ghost');
  h.rendered(SLOT);
  // Frames pass with no canvas: the ghost stays and is not fading.
  h.tick(16 * 5);
  assert.equal(ghost.parentNode, h.doc.body);
  assert.equal(ghost.classList.contains('is-gone'), false);

  body.querySelector('.html-widget').appendChild(h.doc.createElement('canvas'));
  h.tick(16);
  assert.equal(ghost.classList.contains('is-gone'), false, 'the paint check ran, the fade waits a frame');
  h.tick(16);
  assert.equal(ghost.classList.contains('is-gone'), true);
  h.tick(199);
  assert.equal(ghost.parentNode, h.doc.body);
  h.tick(1);
  assert.equal(ghost.parentNode, null);
  h.close();
});

test('a chart that never paints is given 1500ms', () => {
  const { h, slot } = boot();
  recalculating(h, slot);
  const ghost = h.q('.pp-chart-ghost');
  h.rendered(SLOT);
  h.tick(1500);
  assert.equal(ghost.classList.contains('is-gone'), false);
  h.tick(16 + 16);
  assert.equal(ghost.classList.contains('is-gone'), true);
  h.tick(200);
  assert.equal(ghost.parentNode, null);
  h.close();
});

test('a panel with no widget fades at once', () => {
  const { h, slot, body } = boot();
  recalculating(h, slot);
  const ghost = h.q('.pp-chart-ghost');
  body.innerHTML = '<div class="pp-empty">No adverse events</div>';
  h.rendered(SLOT);
  h.tick(16);
  assert.equal(ghost.classList.contains('is-gone'), true);
  h.tick(200);
  assert.equal(ghost.parentNode, null);
  h.close();
});

test('an error render lifts the ghost the same way', () => {
  const { h, slot, body } = boot();
  recalculating(h, slot);
  const ghost = h.q('.pp-chart-ghost');
  body.innerHTML = '';
  h.$(slot).trigger(h.$.Event('shiny:error', { name: slot.id }));
  h.tick(16 + 200);
  assert.equal(ghost.parentNode, null);
  h.close();
});

test('any scroll drops every ghost immediately', () => {
  const { h, slot } = boot();
  recalculating(h, slot);
  const other = h.el('viz_slot_patient_overview');
  h.layout(other.querySelector('.pp-chart-body'), { top: 0, left: 50, width: 400, height: 80 });
  recalculating(h, other);
  assert.equal(h.qa('.pp-chart-ghost').length, 2);
  h.doc.body.dispatchEvent(new h.win.Event('scroll', { bubbles: true }));
  assert.equal(h.qa('.pp-chart-ghost').length, 0);
  h.close();
});

test('a render after a scroll-drop does not throw or leave anything behind', () => {
  const { h, slot } = boot();
  recalculating(h, slot);
  h.doc.body.dispatchEvent(new h.win.Event('scroll', { bubbles: true }));
  h.rendered(SLOT);
  h.tick(2000);
  assert.equal(h.qa('.pp-chart-ghost').length, 0);
  h.close();
});
