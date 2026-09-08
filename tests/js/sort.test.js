/* The sort clause on the cohort caption cycles through the keys the
 * caption carries and tells R which one is up. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

test('each click moves to the next key, relabels, and sends the value', () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  const by = h.el('cohort_sort_by');
  assert.equal(by.getAttribute('data-index'), '0');
  assert.equal(by.querySelector('b').textContent, 'patient id');

  h.click(by);
  assert.equal(by.getAttribute('data-index'), '1');
  assert.equal(by.querySelector('b').textContent, 'severity');
  assert.equal(by.getAttribute('title'), 'Sort by event count');
  assert.deepEqual(h.inputs('cohort_sort').map((i) => i.value), ['worst']);

  h.click(by);
  assert.equal(by.querySelector('b').textContent, 'event count');
  assert.equal(by.getAttribute('title'), 'Sort by patient id');
  assert.equal(h.lastInput('cohort_sort'), 'ae');

  // Round the cycle and back to the id order.
  h.click(by);
  assert.equal(by.getAttribute('data-index'), '0');
  assert.equal(by.querySelector('b').textContent, 'patient id');
  assert.equal(h.lastInput('cohort_sort'), 'id');
  h.close();
});

test('a lab band offers peak and lowest', () => {
  const h = mount({ profile: 'lab' });
  h.renderAll();
  h.tick(0);
  const by = h.el('cohort_sort_by');
  h.click(by);
  assert.equal(by.querySelector('b').textContent, 'peak');
  assert.equal(h.lastInput('cohort_sort'), 'high');
  h.click(by);
  assert.equal(by.querySelector('b').textContent, 'lowest');
  assert.equal(h.lastInput('cohort_sort'), 'low');
  h.close();
});

test('a caption without keys is inert', () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  const by = h.el('cohort_sort_by');
  by.removeAttribute('data-values');
  h.click(by);
  assert.equal(h.inputs('cohort_sort').length, 0);
  h.close();
});
