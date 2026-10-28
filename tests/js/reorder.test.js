/* Reordering panels by their header in the chart area. The sidebar's drag
 * is the picker's (picker.test.js). */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const ON = ['patient_overview', 'ae_gantt', 'adlbc_all__ALB', 'adlbc_all__ALP', 'adlbc_all__ALT'];

const boot = () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  h.send('sync_selected', ON);
  return h;
};

// --- panels dragged by their header -------------------------------------

const layoutPanels = (h) => {
  let y = 0;
  h.qa('[id*=viz_slot_]').forEach((slot) => {
    Array.from(slot.children).forEach((child, i) => {
      h.layout(child, { top: y + i * 40, left: 0, width: 600, height: 40 });
    });
    y += 100;
  });
};

test('a panel dragged by its header below another lands there', () => {
  const h = boot();
  layoutPanels(h);
  const slots = h.slots();
  const header = h.el('viz_slot_patient_overview').querySelector('.pp-chart-header');
  const down = h.mouse('mousedown', header, { clientY: 10 });
  assert.equal(down.defaultPrevented, true);
  assert.equal(h.el('viz_slot_patient_overview').classList.contains('pp-is-dragging'), true);
  assert.equal(h.doc.body.classList.contains('pp-dragging'), true);
  assert.ok(h.q('.pp-drop-line'), 'the drop line is up from the first move');

  // Into the third panel's upper half: the line sits above it.
  h.mouse('mousemove', h.doc, { clientY: 210 });
  assert.equal(h.q('.pp-drop-line').style.top, (200 - 4 - 1) + 'px');
  h.mouse('mouseup', h.doc, { clientY: 210 });

  assert.deepEqual(h.lastInput('reorder_viz'), [slots[1], slots[0], slots[2], slots[3], slots[4]]);
  assert.equal(h.q('.pp-drop-line'), null);
  assert.equal(h.q('.pp-is-dragging'), null);
  assert.equal(h.doc.body.classList.contains('pp-dragging'), false);
  h.close();
});

test('a panel dropped past the end goes last; dropped on itself, nothing is sent', () => {
  const h = boot();
  layoutPanels(h);
  const slots = h.slots();
  const header = h.el('viz_slot_ae_gantt').querySelector('.pp-chart-header');
  h.mouse('mousedown', header, { clientY: 110 });
  h.mouse('mousemove', h.doc, { clientY: 900 });
  h.mouse('mouseup', h.doc, { clientY: 900 });
  assert.deepEqual(h.lastInput('reorder_viz'), [slots[0], slots[2], slots[3], slots[4], slots[1]]);

  h.resetInputs();
  h.mouse('mousedown', header, { clientY: 110 });
  h.mouse('mouseup', h.doc, { clientY: 110 });
  assert.equal(h.inputs('reorder_viz').length, 0);
  h.close();
});

test('the header\'s controls keep their clicks', () => {
  const h = boot();
  layoutPanels(h);
  const remove = h.el('viz_slot_ae_gantt').querySelector('.pp-chart-remove');
  const down = h.mouse('mousedown', remove, { clientY: 110 });
  assert.equal(down.defaultPrevented, false);
  assert.equal(h.q('.pp-is-dragging'), null);
  h.close();
});

test('with one panel there is nothing to reorder', () => {
  const h = mount({ profile: 'lab' });
  h.renderAll();
  h.tick(0);
  // The lab profile has three parameter panels; strip it to one.
  h.qa('[id*=viz_slot_]').slice(1).forEach((s) => s.remove());
  layoutPanels(h);
  const header = h.q('.pp-chart-header');
  h.mouse('mousedown', header, { clientY: 10 });
  assert.equal(h.q('.pp-is-dragging'), null);
  h.close();
});
