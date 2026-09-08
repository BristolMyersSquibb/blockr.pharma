'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

test('the block mounts on the rendered fixtures', () => {
  const h = mount();
  assert.equal(h.rows().length, 12);
  assert.equal(h.slots().length, 5);
  assert.deepEqual(h.handlers().sort(), ['dl_menu_state', 'subject_picker', 'sync_band',
    'sync_params', 'sync_selected', 'sync_subject']);
  h.renderAll();
  h.tick(0);
  h.close();
});
