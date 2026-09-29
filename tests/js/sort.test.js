/* The sort on the patients' caption is a word in its sentence: a menu of
 * the keys the caption carries, and the pick tells R which one is up. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const rows = (h) => h.qa('.blockr-select__option').map((e) => e.textContent);
const pick = (h, text) => h.qa('.blockr-select__option')
  .find((e) => e.textContent === text)
  .dispatchEvent(new h.win.Event('click', { bubbles: true }));

test('the word opens the keys and a pick sends the key', () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  const by = h.el('cohort_sort_by');
  assert.equal(by.textContent, 'patient id');
  assert.match(h.q('.pp-cohort-bandcap-what').textContent, /, by patient id$/);
  h.click(by);
  assert.equal(h.q('.blockr-select__menu-title').textContent, 'Sort patients by');
  assert.deepEqual(rows(h), ['Patient id', 'Worst severity', 'Event count']);
  pick(h, 'Event count');
  h.tick(10);
  assert.deepEqual(h.inputs('cohort_sort').map((i) => i.value), ['ae']);
  // The key already up is not a change.
  h.click(h.el('cohort_sort_by'));
  pick(h, 'Patient id');
  h.tick(10);
  assert.equal(h.inputs('cohort_sort').length, 1);
  h.close();
});

test('a lab band offers peak and lowest', () => {
  const h = mount({ profile: 'lab' });
  h.renderAll();
  h.tick(0);
  h.click(h.el('cohort_sort_by'));
  assert.deepEqual(rows(h), ['Patient id', 'Peak value', 'Lowest value']);
  pick(h, 'Lowest value');
  h.tick(10);
  assert.equal(h.lastInput('cohort_sort'), 'low');
  h.close();
});
