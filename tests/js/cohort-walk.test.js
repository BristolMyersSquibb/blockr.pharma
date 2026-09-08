/* The cohort list: clicks, the keyboard walk, the pick debounce and the
 * echo guard on the server's confirmation. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const boot = () => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  return h;
};

test('a click selects the row at once and sends the pick without waiting', () => {
  const h = boot();
  const ids = h.shownIds();
  assert.equal(ids.length, 12);
  assert.equal(h.selectedId(), null);

  h.click(h.rows()[2]);
  assert.equal(h.selectedId(), ids[2]);
  assert.deepEqual(h.inputs('pick_subject').map((i) => i.value), [ids[2]]);
  assert.deepEqual(h.inputs('pick_subject')[0].opts, { priority: 'event' });

  // A second click moves the class, and sends again; nothing is left pending.
  h.click(h.rows()[5]);
  assert.equal(h.selectedId(), ids[5]);
  assert.equal(h.inputs('pick_subject').length, 2);
  h.tick(1000);
  assert.equal(h.inputs('pick_subject').length, 2);
  h.close();
});

test('arrow keys walk the rows in DOM order and send one pick 90ms after the last', () => {
  const h = boot();
  const ids = h.shownIds();
  const well = h.el('pp_cohort_well');

  // No selection yet: Down lands on the first row.
  h.key(well, 'ArrowDown');
  assert.equal(h.selectedId(), ids[0]);
  assert.equal(h.inputs('pick_subject').length, 0);

  // A burst, 25ms apart, moves the class every time and sends nothing.
  for (let i = 0; i < 4; i++) { h.tick(25); h.key(well, 'ArrowDown'); }
  assert.equal(h.selectedId(), ids[4]);
  assert.equal(h.inputs('pick_subject').length, 0);

  // 89ms after the last press: still nothing. At 90: one pick, the last row.
  h.tick(89);
  assert.equal(h.inputs('pick_subject').length, 0);
  h.tick(1);
  assert.deepEqual(h.inputs('pick_subject').map((i) => i.value), [ids[4]]);

  // Up walks back, one pick per settled press.
  h.key(well, 'ArrowUp');
  h.tick(90);
  assert.equal(h.selectedId(), ids[3]);
  assert.deepEqual(h.inputs('pick_subject').map((i) => i.value), [ids[4], ids[3]]);
  h.close();
});

test('the walk wraps at both ends', () => {
  const h = boot();
  const ids = h.shownIds();
  const well = h.el('pp_cohort_well');
  h.key(well, 'ArrowUp');
  assert.equal(h.selectedId(), ids[11], 'Up from nothing lands on the last row');
  h.key(well, 'ArrowDown');
  assert.equal(h.selectedId(), ids[0], 'Down from the last row wraps to the first');
  h.close();
});

test('Home and End jump to the ends and send at once', () => {
  const h = boot();
  const ids = h.shownIds();
  const well = h.el('pp_cohort_well');
  h.key(well, 'End');
  assert.equal(h.selectedId(), ids[11]);
  assert.deepEqual(h.inputs('pick_subject').map((i) => i.value), [ids[11]]);
  h.key(well, 'Home');
  assert.equal(h.selectedId(), ids[0]);
  assert.deepEqual(h.inputs('pick_subject').map((i) => i.value), [ids[11], ids[0]]);
  h.close();
});

test('filtered-out rows are skipped and other keys are ignored', () => {
  const h = boot();
  const ids = h.shownIds();
  const well = h.el('pp_cohort_well');
  h.rows()[1].classList.add('is-filtered-out');
  h.rows()[2].classList.add('is-filtered-out');
  h.click(h.rows()[0]);
  h.key(well, 'ArrowDown');
  assert.equal(h.selectedId(), ids[3]);

  const ev = h.key(well, 'a');
  assert.equal(h.selectedId(), ids[3]);
  assert.equal(ev.defaultPrevented, false);
  const arrow = h.key(well, 'ArrowDown');
  assert.equal(arrow.defaultPrevented, true, 'the arrow is consumed');
  h.close();
});

test('a stale confirmation during the walk is ignored, a matching one applied', () => {
  const h = boot();
  const ids = h.shownIds();
  const well = h.el('pp_cohort_well');
  h.click(h.rows()[0]);
  h.key(well, 'ArrowDown');
  h.key(well, 'ArrowDown');
  assert.equal(h.selectedId(), ids[2]);

  // The server confirming the row two presses ago must not move the class.
  h.send('sync_subject', { id: ids[0] });
  assert.equal(h.selectedId(), ids[2]);
  // Confirming the pending row is fine.
  h.send('sync_subject', { id: ids[2] });
  assert.equal(h.selectedId(), ids[2]);

  // Once settled, any confirmation is the truth, starting with the one R
  // sent for the fixture's pick.
  h.tick(90);
  h.sendRecorded('sync_subject');
  assert.equal(h.selectedId(), '01-701-1015');
  h.send('sync_subject', { id: ids[7] });
  assert.equal(h.selectedId(), ids[7]);
  h.send('sync_subject', { id: '' });
  assert.equal(h.selectedId(), null);
  h.send('sync_subject', null);
  assert.equal(h.selectedId(), null);
  h.close();
});

test('a click during a pending walk cancels the timer', () => {
  const h = boot();
  const ids = h.shownIds();
  const well = h.el('pp_cohort_well');
  h.key(well, 'ArrowDown');
  h.key(well, 'ArrowDown');
  h.click(h.rows()[9]);
  assert.deepEqual(h.inputs('pick_subject').map((i) => i.value), [ids[9]]);
  h.tick(500);
  assert.equal(h.inputs('pick_subject').length, 1, 'the walk\'s timer did not fire');
  // And the guard is down: a confirmation applies at once.
  h.send('sync_subject', { id: ids[1] });
  assert.equal(h.selectedId(), ids[1]);
  h.close();
});
