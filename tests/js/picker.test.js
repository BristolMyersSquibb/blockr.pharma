/* The panel picker in the sidebar: the On list the server's sync_selected
 * paints, the search over panels and patients, the first hit, and the
 * optimistic move when a row is picked or removed. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const ON = ['patient_overview', 'ae_gantt', 'adlbc_all__ALB', 'adlbc_all__ALP', 'adlbc_all__ALT'];
const search = (h) => h.el('search');

// The On list as R sent it for this fixture: five rows for three panels.
const boot = (selected) => {
  const h = mount();
  h.renderAll();
  h.tick(0);
  if (selected) h.send('sync_selected', selected); else h.sendRecorded('sync_selected');
  assert.deepEqual(h.recorded('sync_selected').pop(), ON);
  return h;
};

test('sync_selected paints the On list in order, ticks the catalogue, counts', () => {
  const h = boot();
  assert.deepEqual(h.onProfile(), ON);
  assert.equal(h.text('.pp-add-n'), '5');
  const on = h.qa('.pp-add-row.is-on').map((r) => r.getAttribute('data-viz-id'));
  assert.deepEqual(on.sort(), ON.slice().sort());
  assert.equal(h.q('.pp-add-on-wrap').classList.contains('is-hidden'), false);

  // A parameter row carries its code, a panel row its colour.
  const alb = h.q('.pp-add-ord[data-viz-id="adlbc_all__ALB"]');
  assert.equal(alb.querySelector('.pp-add-ord-code').textContent, 'ALB');
  assert.equal(alb.querySelector('.pp-add-name').textContent, 'Albumin (g/L)');
  const ae = h.q('.pp-add-ord[data-viz-id="ae_gantt"]');
  assert.ok(ae.querySelector('.pp-add-dot'));
  assert.ok(ae.querySelector('.pp-add-ord-grip .grip'), 'the grip glyph from the mount config');
  assert.ok(ae.querySelector('button.pp-add-ord-x'));

  // Nothing selected: the block hides the whole On section and says nothing.
  h.send('sync_selected', []);
  assert.deepEqual(h.onProfile(), []);
  assert.equal(h.text('.pp-add-n'), '');
  assert.equal(h.q('.pp-add-on-wrap').classList.contains('is-hidden'), true);
  // One panel arrives as a one-element array (pp_send wraps it).
  h.send('sync_selected', ['ae_gantt']);
  assert.deepEqual(h.onProfile(), ['ae_gantt']);
  h.close();
});

test('an unchanged selection does not rebuild the rows', () => {
  const h = boot();
  const before = h.qa('.pp-add-ord');
  h.send('sync_selected', ON);
  const after = h.qa('.pp-add-ord');
  assert.equal(after.length, before.length);
  after.forEach((r, i) => assert.equal(r, before[i], 'same node'));
  // Ids the catalogue does not know are dropped, not drawn blank.
  h.send('sync_selected', ON.concat(['no_such_panel']));
  assert.deepEqual(h.onProfile(), ON);
  h.close();
});

test('with no query the catalogue is hidden and patients all show', () => {
  const h = boot();
  assert.ok(h.qa('.pp-add-row').every((r) => r.classList.contains('is-hidden')));
  assert.ok(h.qa('.pp-add-results .pp-add-group').every((g) => g.classList.contains('is-hidden')));
  assert.equal(h.shownIds().length, 12);
  assert.equal(h.q('.pp-add-none').classList.contains('is-shown'), false);
  assert.equal(h.q('.is-enter'), null);
  h.close();
});

test('a query filters panels and patients together and marks the first hit', () => {
  const h = boot();
  h.type(search(h), 'temp');
  const shown = h.qa('.pp-add-row:not(.is-hidden)').map((r) => r.getAttribute('data-viz-id'));
  assert.deepEqual(shown, ['advs_all__TEMP']);
  assert.ok(h.qa('.pp-add-results .pp-add-group').every((g) => g.classList.contains('is-hidden')),
    'group headings never show during a query');
  assert.equal(h.shownIds().length, 0, 'no patient matches "temp"');
  assert.equal(h.q('.pp-add-none').classList.contains('is-shown'), false, 'one panel matched');
  assert.equal(h.q('.is-enter').getAttribute('data-viz-id'), 'advs_all__TEMP');
  assert.equal(h.el('pp_sidebar').classList.contains('is-searching'), true);
  assert.equal(h.el('search_clear').classList.contains('is-hidden'), false);

  // Case and surrounding space do not matter.
  h.type(search(h), '  ALB ');
  assert.deepEqual(h.qa('.pp-add-row:not(.is-hidden)').map((r) => r.getAttribute('data-viz-id')),
    ['adlbc_all__ALB']);

  // A patient id: no panel matches, the patient is the first hit.
  h.type(search(h), '1023');
  assert.equal(h.qa('.pp-add-row:not(.is-hidden)').length, 0);
  assert.deepEqual(h.shownIds(), ['01-701-1023']);
  assert.equal(h.q('.is-enter').getAttribute('data-usubjid'), '01-701-1023');
  assert.equal(h.q('.pp-add-none').classList.contains('is-shown'), false);

  // Nothing anywhere.
  h.type(search(h), 'zzzz');
  assert.equal(h.q('.pp-add-none').classList.contains('is-shown'), true);
  assert.equal(h.q('.is-enter'), null);
  assert.equal(h.shownIds().length, 0);
  h.close();
});

test('Escape and the clear button empty the box and restore everything', () => {
  const h = boot();
  h.type(search(h), 'zzzz');
  h.key(search(h), 'Escape');
  assert.equal(search(h).value, '');
  assert.equal(h.shownIds().length, 12);
  assert.equal(h.q('.pp-add-none').classList.contains('is-shown'), false);
  assert.equal(h.el('pp_sidebar').classList.contains('is-searching'), false);

  h.type(search(h), 'alb');
  h.click(h.el('search_clear'));
  assert.equal(search(h).value, '');
  assert.equal(h.shownIds().length, 12);
  assert.ok(h.qa('.pp-add-row').every((r) => r.classList.contains('is-hidden')));
  h.close();
});

test('picking a catalogue row moves it onto the profile and resets the search', () => {
  const h = boot();
  h.type(search(h), 'temp');
  const row = h.q('.pp-add-row[data-viz-id="advs_all__TEMP"]');
  h.click(row);

  // Optimistic: the row is on, the On list has it last, the count moved.
  assert.equal(row.classList.contains('is-on'), true);
  assert.deepEqual(h.onProfile(), ON.concat(['advs_all__TEMP']));
  assert.equal(h.text('.pp-add-n'), '6');
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['advs_all__TEMP']);
  assert.equal(h.q('.pp-add-ord[data-viz-id="advs_all__TEMP"]').classList.contains('is-landed'), true);

  // The box empties and, a tick later, the filter follows it.
  assert.equal(search(h).value, '');
  h.tick(0);
  assert.equal(h.shownIds().length, 12);
  assert.ok(h.qa('.pp-add-row').every((r) => r.classList.contains('is-hidden')));

  // The landing tint outlives the move and then goes.
  h.tick(190 + 419);
  assert.equal(h.q('.pp-add-ord[data-viz-id="advs_all__TEMP"]').classList.contains('is-landed'), true);
  h.tick(1);
  assert.equal(h.q('.pp-add-ord[data-viz-id="advs_all__TEMP"]').classList.contains('is-landed'), false);

  // The server's echo, saying the same thing, leaves the rows alone.
  const nodes = h.qa('.pp-add-ord');
  h.send('sync_selected', ON.concat(['advs_all__TEMP']));
  h.qa('.pp-add-ord').forEach((r, i) => assert.equal(r, nodes[i]));
  h.close();
});

test('Enter picks the first hit, a panel before a patient', () => {
  const h = boot();
  h.type(search(h), 'temp');
  h.key(search(h), 'Enter');
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['advs_all__TEMP']);
  assert.equal(search(h).value, '');

  // A patient hit: Enter clicks the row, which picks the patient and keeps
  // the query in the box.
  h.type(search(h), '1034');
  const ev = h.key(search(h), 'Enter');
  assert.equal(ev.defaultPrevented, true);
  assert.deepEqual(h.inputs('pick_subject').map((i) => i.value), ['01-701-1034']);
  assert.equal(h.selectedId(), '01-701-1034');
  assert.equal(search(h).value, '1034');

  // No hit: Enter does nothing.
  h.resetInputs();
  h.type(search(h), 'zzzz');
  const none = h.key(search(h), 'Enter');
  assert.equal(none.defaultPrevented, false);
  assert.equal(h.inputs().length, 0);
  h.close();
});

test('the x on an On row takes it off the profile', () => {
  const h = boot();
  h.click(h.q('.pp-add-ord[data-viz-id="ae_gantt"] .pp-add-ord-x'));
  assert.deepEqual(h.onProfile(), ON.filter((x) => x !== 'ae_gantt'));
  assert.equal(h.text('.pp-add-n'), '4');
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['ae_gantt']);
  assert.equal(h.q('.pp-add-row[data-viz-id="ae_gantt"]').classList.contains('is-on'), true,
    'the catalogue tick waits for the server');
  h.send('sync_selected', ON.filter((x) => x !== 'ae_gantt'));
  assert.equal(h.q('.pp-add-row[data-viz-id="ae_gantt"]').classList.contains('is-on'), false);
  h.close();
});

test('a row that is on comes off again when clicked in the catalogue', () => {
  const h = boot();
  h.type(search(h), 'alb');
  h.click(h.q('.pp-add-row[data-viz-id="adlbc_all__ALB"]'));
  assert.deepEqual(h.onProfile(), ON.filter((x) => x !== 'adlbc_all__ALB'));
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['adlbc_all__ALB']);
  assert.equal(h.q('.pp-add-row[data-viz-id="adlbc_all__ALB"]').classList.contains('is-on'), false);
  h.close();
});

test('the panel picker rendering again re-applies the remembered selection', () => {
  const h = mount();
  const host = h.el('panel_picker');
  const fresh = host.innerHTML;
  h.renderAll();
  h.tick(0);
  h.send('sync_selected', ON);
  // R re-renders the picker (a new catalogue): the ticks and the On list
  // come back from memory, before any new sync_selected.
  host.innerHTML = fresh;
  assert.deepEqual(h.onProfile(), []);
  h.rendered('panel_picker');
  h.tick(0);
  assert.deepEqual(h.onProfile(), ON);
  assert.equal(h.qa('.pp-add-row.is-on').length, 5);
  h.close();
});

test('the FLIP move animates a row from where it was to where it is', () => {
  const h = boot();
  h.type(search(h), 'temp');
  const temp = h.q('.pp-add-row[data-viz-id="advs_all__TEMP"]');
  const still = h.q('.pp-add-row[data-viz-id="advs_all__PULSE"]');
  // The row is measured before the change and again after: it moved 48px
  // down the page (the On list above it grew by two rows).
  let calls = 0;
  temp.getBoundingClientRect = () => {
    const top = calls++ === 0 ? 200 : 248;
    return { top, left: 10, width: 200, height: 24, bottom: top + 24, right: 210 };
  };
  h.layout(still, { top: 100, left: 10, width: 200, height: 24 });

  h.click(temp);
  assert.equal(temp.style.transform, 'translate(0px,-48px)', 'held at its old place');
  assert.equal(temp.style.transition, 'none');
  assert.equal(still.style.transform, '', 'a row that did not move is left alone');

  // Next frame: the transform is released under a transition.
  h.tick(16);
  assert.equal(temp.style.transform, '');
  assert.match(temp.style.transition, /transform 190ms/);
  h.close();
});

test('reduced motion skips the travel but keeps the landing tint longer', () => {
  const h = boot();
  h.win.matchMedia = () => ({ matches: true, addEventListener() {}, addListener() {} });
  h.type(search(h), 'temp');
  const temp = h.q('.pp-add-row[data-viz-id="advs_all__TEMP"]');
  let calls = 0;
  temp.getBoundingClientRect = () => {
    const top = calls++ === 0 ? 200 : 248;
    return { top, left: 10, width: 200, height: 24, bottom: top + 24, right: 210 };
  };
  h.click(temp);
  assert.equal(temp.style.transform, '');
  const landed = h.q('.pp-add-ord[data-viz-id="advs_all__TEMP"]');
  h.tick(499);
  assert.equal(landed.classList.contains('is-landed'), true);
  h.tick(1);
  assert.equal(landed.classList.contains('is-landed'), false);
  h.close();
});

// The arrows: a cursor over the hits, and Enter takes what it is on.
//
// Enter took the FIRST hit whatever you did, so a search that found the right
// panel third gave you no way to reach it from the keyboard: you typed, then
// let go of the keyboard and used the mouse.

const hits = (h) =>
  h.qa('.pp-add-row:not(.is-hidden)').map((r) => r.getAttribute('data-viz-id'))
    .concat(h.shownIds());
const at = (h) => {
  const row = h.q('.is-enter');
  if (!row) return null;
  return row.getAttribute('data-viz-id') || row.getAttribute('data-usubjid');
};

test('the arrows walk the hits and the ring follows', () => {
  const h = boot();
  h.type(search(h), 'al');
  const list = hits(h);
  assert.ok(list.length >= 3, 'the query has several hits to walk');
  assert.equal(at(h), list[0], 'the cursor starts on the first hit');

  h.key(search(h), 'ArrowDown');
  assert.equal(at(h), list[1]);
  h.key(search(h), 'ArrowDown');
  assert.equal(at(h), list[2]);
  h.key(search(h), 'ArrowUp');
  assert.equal(at(h), list[1]);
  h.close();
});

test('the cursor clamps at both ends rather than wrapping', () => {
  // The cohort can be three hundred rows, and wrapping from its last patient
  // back to the first panel is a jump the eye cannot follow.
  const h = boot();
  h.type(search(h), 'al');
  const list = hits(h);

  h.key(search(h), 'ArrowUp');
  assert.equal(at(h), list[0], 'up from the top stays put');

  for (let i = 0; i < list.length + 3; i++) h.key(search(h), 'ArrowDown');
  assert.equal(at(h), list[list.length - 1], 'down past the end stays put');
  h.close();
});

test('the cursor crosses from the panels into the patients', () => {
  // One box, one result list: a cursor that stopped at the bottom of the
  // panels would make the patients below look like a different result set.
  const h = boot();
  h.type(search(h), 'p');
  const list = hits(h);
  const panels = h.qa('.pp-add-row:not(.is-hidden)').length;
  assert.ok(panels > 0 && list.length > panels, 'the query finds both kinds');

  for (let i = 0; i < panels; i++) h.key(search(h), 'ArrowDown');
  assert.equal(at(h), list[panels], 'the first patient follows the last panel');
  assert.ok(h.q('.is-enter').hasAttribute('data-usubjid'));
  h.close();
});

test('Enter takes the row the arrows are on, not the first hit', () => {
  const h = boot();
  h.type(search(h), 'al');
  const list = hits(h);
  h.key(search(h), 'ArrowDown');
  h.key(search(h), 'ArrowDown');
  const want = list[2];
  h.key(search(h), 'Enter');
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), [want]);
  h.close();
});

test('a new keystroke puts the cursor back on the first hit', () => {
  // The arrows move a cursor over THIS query's results. Carrying its position
  // into the next query would land it on a row nobody was looking at.
  const h = boot();
  h.type(search(h), 'al');
  h.key(search(h), 'ArrowDown');
  h.key(search(h), 'ArrowDown');
  h.type(search(h), 'alb');
  assert.equal(at(h), hits(h)[0]);
  h.close();
});

test('the arrows never move the caret in the box', () => {
  // It is a text input: the browser's own Down jumps the caret to the end of
  // the query, which is the last thing you want mid-search.
  const h = boot();
  h.type(search(h), 'al');
  assert.equal(h.key(search(h), 'ArrowDown').defaultPrevented, true);
  assert.equal(h.key(search(h), 'ArrowUp').defaultPrevented, true);

  // Still swallowed with nothing to walk, so the caret cannot jump there
  // either.
  h.type(search(h), 'zzzz');
  assert.equal(h.q('.is-enter'), null);
  assert.equal(h.key(search(h), 'ArrowDown').defaultPrevented, true);
  h.resetInputs();
  assert.equal(h.key(search(h), 'Enter').defaultPrevented, false);
  assert.equal(h.inputs().length, 0);
  h.close();
});
