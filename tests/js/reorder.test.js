/* Reordering panels by mouse: the On rows in the sidebar, and the panel
 * headers in the chart area. Both send the new order as one input. */
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
  // Rows stacked 24px apart from the top of the page.
  h.qa('.pp-add-ord').forEach((r, i) => h.layout(r, { top: i * 24, left: 0, width: 200, height: 24 }));
  return h;
};

const drag = (h, from, toY) => {
  h.mouse('mousedown', from, { clientY: from.getBoundingClientRect().top + 12 });
  h.mouse('mousemove', h.doc, { clientY: toY });
  h.mouse('mouseup', h.doc, { clientY: toY });
};

test('dragging an On row below the last puts it last', () => {
  const h = boot();
  const rows = h.qa('.pp-add-ord');
  h.mouse('mousedown', rows[0], { clientY: 12 });
  assert.equal(rows[0].classList.contains('is-dragging'), true);
  h.mouse('mousemove', h.doc, { clientY: 30 });
  assert.equal(rows[1].classList.contains('is-over'), true, 'above row 1\'s middle');
  h.mouse('mousemove', h.doc, { clientY: 40 });
  assert.equal(rows[1].classList.contains('is-over'), false, 'past row 1\'s middle');
  assert.equal(rows[2].classList.contains('is-over'), true);
  h.mouse('mousemove', h.doc, { clientY: 130 });
  assert.equal(rows[4].classList.contains('is-over-last'), true);
  h.mouse('mouseup', h.doc, { clientY: 130 });

  const want = ON.slice(1).concat([ON[0]]);
  assert.deepEqual(h.onProfile(), want, 'redrawn at once');
  assert.deepEqual(h.lastInput('reorder_viz'), want);
  assert.equal(h.qa('.is-dragging, .is-over, .is-over-last').length, 0);
  h.close();
});

test('dragging a row above the first puts it first', () => {
  const h = boot();
  drag(h, h.qa('.pp-add-ord')[3], 2);
  const want = [ON[3]].concat(ON.filter((x) => x !== ON[3]));
  assert.deepEqual(h.onProfile(), want);
  assert.deepEqual(h.lastInput('reorder_viz'), want);
  h.close();
});

test('a drop where the row already is sends nothing', () => {
  const h = boot();
  drag(h, h.qa('.pp-add-ord')[1], 30);
  assert.deepEqual(h.onProfile(), ON);
  assert.equal(h.inputs('reorder_viz').length, 0);
  // Just below itself is the same place.
  drag(h, h.qa('.pp-add-ord')[1], 50);
  assert.equal(h.inputs('reorder_viz').length, 0);
  h.close();
});

test('the remove button is not a drag handle', () => {
  const h = boot();
  const x = h.q('.pp-add-ord .pp-add-ord-x');
  h.mouse('mousedown', x, { clientY: 12 });
  assert.equal(h.q('.pp-add-ord.is-dragging'), null);
  h.close();
});

test('the server\'s echo of the new order leaves the rows in place', () => {
  const h = boot();
  drag(h, h.qa('.pp-add-ord')[0], 130);
  const nodes = h.qa('.pp-add-ord');
  h.send('sync_selected', ON.slice(1).concat([ON[0]]));
  h.qa('.pp-add-ord').forEach((r, i) => assert.equal(r, nodes[i]));
  h.close();
});

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
