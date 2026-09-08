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

test('every message R sent while rendering has a handler and the shape the client expects', () => {
  for (const profile of ['default', 'lab']) {
    const h = mount({ profile });
    h.renderAll();
    h.tick(0);
    const seen = new Set();
    for (const channel of ['sync_selected', 'sync_params', 'sync_band', 'sync_subject',
      'subject_picker', 'dl_menu_state']) {
      const xs = h.recorded(channel);
      assert.ok(xs.length >= 1, `${profile}: R sent ${channel}`);
      xs.forEach((p) => seen.add(channel + ':' + JSON.stringify(p)));
    }
    // Array channels are arrays even with one entry, and never bare strings.
    h.recorded('sync_selected').forEach((p) => assert.ok(Array.isArray(p)));
    h.recorded('sync_params').forEach((p) => assert.ok(Array.isArray(p)));
    h.recorded('sync_subject').forEach((p) => assert.equal(typeof p.id, 'string'));
    h.recorded('sync_band').forEach((p) => assert.equal(typeof p.viz_id, 'string'));
    h.recorded('subject_picker').forEach((p) => assert.equal(typeof p.count, 'number'));
    h.recorded('dl_menu_state').forEach((p) => {
      assert.equal(typeof p.single, 'boolean');
      assert.equal(typeof p.picked, 'string');
      assert.equal(typeof p.n, 'number');
    });
    assert.doesNotThrow(() => h.replay());
    // After the replay the page shows what R last said.
    assert.equal(h.selectedId(), '01-701-1015');
    assert.equal(h.onProfile().length, profile === 'lab' ? 3 : 5);
    assert.equal(h.text('.pp-cohort-count-n'), '12 patients');
    h.close();
  }
});
