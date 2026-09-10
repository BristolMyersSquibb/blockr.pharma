/* The controls in a panel's header: pills, chips, toggles, radios and the
 * find control, all funnelled into one viz_ctrl input. And the chart area's
 * resize watch. */
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

const gantt = (h) => h.el('viz_slot_ae_gantt');

test('a pill cycles its values and sends the one it shows', () => {
  const h = boot();
  const pill = gantt(h).querySelector('.pp-ctrl-pill');
  assert.equal(pill.getAttribute('data-index'), '1');
  assert.equal(pill.textContent, 'Preferred term');
  h.click(pill);
  assert.equal(pill.getAttribute('data-index'), '2');
  assert.equal(pill.textContent, 'High-level term');
  assert.equal(pill.getAttribute('title'), 'Switch to Body system');
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'lanes', value: 'AEHLT' });
  h.click(pill); h.click(pill);
  assert.equal(pill.getAttribute('data-index'), '0');
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'lanes', value: 'AETERM' });
  h.close();
});

test('the remove button on a panel sends the toggle', () => {
  const h = boot();
  h.click(gantt(h).querySelector('.pp-chart-remove'));
  assert.deepEqual(h.inputs('toggle_viz').map((i) => i.value), ['ae_gantt']);
  h.close();
});

test('chips, toggles and radios built like the header\'s are handled', () => {
  const h = boot();
  const header = gantt(h).querySelector('.pp-chart-header');
  header.insertAdjacentHTML('beforeend', `
    <div class="pp-ctrl-chips">
      <span class="pp-ctrl-chip is-active" data-viz-id="ae_gantt" data-param="sev" data-value="MILD">Mild</span>
      <span class="pp-ctrl-chip" data-viz-id="ae_gantt" data-param="sev" data-value="SEVERE">Severe</span>
    </div>
    <span class="pp-ctrl-toggle" data-viz-id="ae_gantt" data-param="ongoing">Ongoing</span>
    <span class="pp-ctrl-radio is-active" data-viz-id="ae_gantt" data-param="scale" data-value="lin">Lin</span>
    <span class="pp-ctrl-radio" data-viz-id="ae_gantt" data-param="scale" data-value="log">Log</span>`);

  h.click(header.querySelector('.pp-ctrl-chip[data-value="SEVERE"]'));
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'sev', value: ['MILD', 'SEVERE'] });
  h.click(header.querySelector('.pp-ctrl-chip[data-value="MILD"]'));
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'sev', value: ['SEVERE'] });

  const toggle = header.querySelector('.pp-ctrl-toggle');
  h.click(toggle);
  assert.equal(toggle.classList.contains('is-on'), true);
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'ongoing', value: true });
  h.click(toggle);
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'ongoing', value: false });

  const log = header.querySelector('.pp-ctrl-radio[data-value="log"]');
  h.click(log);
  assert.equal(log.classList.contains('is-active'), true);
  assert.equal(header.querySelector('.pp-ctrl-radio[data-value="lin"]').classList.contains('is-active'), false);
  assert.deepEqual(h.lastInput('viz_ctrl'), { viz_id: 'ae_gantt', param: 'scale', value: 'log' });
  h.close();
});

/* The find control. The trigger is in the header; the popover it opens is
 * parented to <body> and owned by pp-find.js, which is what lets it survive
 * the header swap a settings change performs. */
const trigger = (h) => gantt(h).querySelector('.pp-ctrl-find');
const pop = (h) => h.q('.pp-find-pop.is-open');
const optRows = (h) => Array.from(pop(h).querySelectorAll('.pp-find-opt'))
  .map((o) => o.querySelector('.pp-find-text').textContent);

test('the trigger opens a popover listing every coding level with counts', () => {
  const h = boot();
  assert.equal(pop(h), null, 'nothing is open before the click');
  h.click(trigger(h));
  const groups = Array.from(pop(h).querySelectorAll('.pp-find-group'))
    .map((g) => g.textContent);
  assert.deepEqual(groups, ['Body system', 'High level term', 'Preferred term']);
  // The dictionary shouts; the list does not.
  assert.ok(optRows(h).includes('Diarrhoea'));
  // Every row carries this patient's record count, which is what answers
  // "which of my events are these" before the panel redraws.
  assert.ok(Array.from(pop(h).querySelectorAll('.pp-find-n'))
    .every((n) => /^[0-9]+$/.test(n.textContent)));
  // Nothing is sent by opening it.
  assert.equal(h.inputs('viz_ctrl').length, 0);
  h.close();
});

test('typing filters the list without touching the server', () => {
  const h = boot();
  h.click(trigger(h));
  h.type(pop(h).querySelector('.pp-find-input'), 'diarr');
  assert.deepEqual(optRows(h), ['Diarrhoea']);
  h.tick(1000);
  assert.equal(h.inputs('viz_ctrl').length, 0, 'no round trip while typing');
  h.close();
});

test('picks batch: several ticks, one send, and only on close', () => {
  const h = boot();
  h.click(trigger(h));
  const rows = () => Array.from(pop(h).querySelectorAll('.pp-find-opt'));
  h.click(rows()[0]);
  h.click(rows()[1]);
  assert.equal(h.inputs('viz_ctrl').length, 0,
    'a change to this setting redraws 254 cohort bands; ticking must not');
  assert.equal(pop(h).querySelectorAll('.pp-find-tag').length, 2);
  h.click(pop(h).querySelector('.pp-find-done'));
  const sent = h.inputs('viz_ctrl');
  assert.equal(sent.length, 1);
  assert.equal(sent[0].value.viz_id, 'ae_gantt');
  assert.equal(sent[0].value.param, 'find');
  assert.equal(sent[0].value.value.length, 2);
  assert.equal(sent[0].value.value[0].col, 'AEBODSYS');
  h.close();
});

test('a click outside applies, Escape abandons', () => {
  const h = boot();
  h.click(trigger(h));
  h.click(pop(h).querySelectorAll('.pp-find-opt')[0]);
  h.click(h.doc.body);
  assert.equal(h.inputs('viz_ctrl').length, 1, 'clicking away applies');

  h.resetInputs();
  h.click(trigger(h));
  h.click(pop(h).querySelectorAll('.pp-find-opt')[1]);
  h.key(h.q('.pp-find-pop'), 'Escape');
  assert.equal(h.inputs('viz_ctrl').length, 0,
    'Escape abandons: the picks were never sent, so there is nothing to undo');
  h.close();
});

test('opening and closing without a change sends nothing', () => {
  const h = boot();
  h.click(trigger(h));
  h.click(pop(h).querySelector('.pp-find-done'));
  assert.equal(h.inputs('viz_ctrl').length, 0);
  h.close();
});

test('arrows walk the rows, Space ticks, Enter ticks and closes', () => {
  const h = boot();
  h.click(trigger(h));
  const box = h.q('.pp-find-pop');
  h.key(box, 'ArrowDown');
  h.key(box, ' ');
  assert.equal(pop(h).querySelectorAll('.pp-find-opt.is-picked').length, 1);
  h.key(box, 'ArrowDown');
  h.key(box, 'Enter');
  assert.equal(h.q('.pp-find-pop.is-open'), null, 'Enter closes');
  assert.equal(h.lastInput('viz_ctrl').value.length, 2);
  h.close();
});

/* The cohort's terms. The list that rides the header is this patient's, which
 * is what makes its counts mean something and what makes it useless for
 * arming a filter before paging through the cohort. */
const VOCAB = {
  viz_id: 'ae_gantt',
  token: '1',
  groups: [{
    col: 'AEDECOD', label: 'Preferred term', truncated: 0,
    options: [{ value: 'PNEUMONIA', n: 12 }, { value: 'DIARRHOEA', n: 40 }]
  }]
};

test('typing asks for the cohort terms once, and not before', () => {
  const h = boot();
  h.click(trigger(h));
  assert.equal(h.inputs('find_vocab').length, 0, 'opening asks for nothing');
  h.type(pop(h).querySelector('.pp-find-input'), 'pn');
  assert.deepEqual(h.lastInput('find_vocab'), { viz_id: 'ae_gantt', have: '' });
  // One request per popover session, however much is typed after it.
  h.type(pop(h).querySelector('.pp-find-input'), 'pneu');
  h.type(pop(h).querySelector('.pp-find-input'), 'pneumo');
  assert.equal(h.inputs('find_vocab').length, 1);
  h.close();
});

test('the cohort terms are offered under a split, counted in patients', () => {
  const h = boot();
  h.click(trigger(h));
  h.type(pop(h).querySelector('.pp-find-input'), 'pneu');
  // Until the reply lands the reader is told the cohort is being searched,
  // rather than being shown "no matches" and stopping there.
  assert.match(pop(h).textContent, /Searching the rest of the cohort/);

  h.send('find_vocab', VOCAB);
  assert.match(pop(h).textContent, /Not in this patient/);
  const row = pop(h).querySelector('.pp-find-opt.is-elsewhere');
  assert.match(row.textContent, /Pneumonia/);
  // A different unit from this patient's record counts beside it, so it is
  // spelled out rather than left as a bare number.
  assert.match(row.querySelector('.pp-find-n').textContent, /^12 patients$/);

  // Picking one is the point: it applies like any other pick, and the panel
  // goes to its empty state until a patient who has it comes up.
  h.click(row);
  h.click(pop(h).querySelector('.pp-find-done'));
  assert.deepEqual(h.lastInput('viz_ctrl').value,
    [{ col: 'AEDECOD', value: 'PNEUMONIA' }]);
  h.close();
});

test('a cohort term this patient DOES have is not listed twice', () => {
  const h = boot();
  h.click(trigger(h));
  // DIARRHOEA is in this patient's own list (the fixture's header carries it).
  h.type(pop(h).querySelector('.pp-find-input'), 'diarr');
  h.send('find_vocab', VOCAB);
  const rows = Array.from(pop(h).querySelectorAll('.pp-find-opt'))
    .map((o) => o.querySelector('.pp-find-text').textContent);
  assert.deepEqual(rows, ['Diarrhoea']);
  assert.equal(pop(h).querySelectorAll('.is-elsewhere').length, 0);
  h.close();
});

test('a second search session sends the token instead of asking again', () => {
  const h = boot();
  h.click(trigger(h));
  h.type(pop(h).querySelector('.pp-find-input'), 'pn');
  h.send('find_vocab', VOCAB);
  h.click(pop(h).querySelector('.pp-find-done'));

  h.resetInputs();
  h.click(trigger(h));
  h.type(pop(h).querySelector('.pp-find-input'), 'pn');
  assert.deepEqual(h.lastInput('find_vocab'), { viz_id: 'ae_gantt', have: '1' });
  // The server says the list has not moved and sends none of it; what the
  // client already holds is still drawn.
  h.send('find_vocab', { viz_id: 'ae_gantt', token: '1', unchanged: true });
  assert.equal(pop(h).querySelectorAll('.pp-find-opt.is-elsewhere').length, 1);
  h.close();
});

test('a new cohort replaces the terms the client was holding', () => {
  const h = boot();
  h.click(trigger(h));
  h.type(pop(h).querySelector('.pp-find-input'), 'pn');
  h.send('find_vocab', VOCAB);
  assert.equal(pop(h).querySelectorAll('.pp-find-opt.is-elsewhere').length, 1);
  // An upstream filter narrowed the cohort: a new token, a new list, and
  // the term that is no longer in it is gone.
  h.send('find_vocab', { viz_id: 'ae_gantt', token: '2', groups: [] });
  assert.equal(pop(h).querySelectorAll('.pp-find-opt.is-elsewhere').length, 0);
  h.close();
});

test('the cohort list is only offered while something is typed', () => {
  const h = boot();
  h.click(trigger(h));
  h.type(pop(h).querySelector('.pp-find-input'), 'pn');
  h.send('find_vocab', VOCAB);
  assert.equal(pop(h).querySelectorAll('.is-elsewhere').length, 1);
  // Cleared again: several hundred cohort rows would bury this patient's few.
  h.type(pop(h).querySelector('.pp-find-input'), '');
  assert.equal(pop(h).querySelectorAll('.is-elsewhere').length, 0);
  assert.equal(pop(h).textContent.indexOf('Not in this patient'), -1);
  h.close();
});

test('a query nothing codes for becomes a free-text pick', () => {
  const h = boot();
  h.click(trigger(h));
  h.type(pop(h).querySelector('.pp-find-input'), 'zzzz');
  assert.equal(pop(h).querySelectorAll('.pp-find-opt').length, 0);
  h.click(pop(h).querySelector('.pp-find-free'));
  h.click(pop(h).querySelector('.pp-find-done'));
  // Which is exactly what the box this control replaced always sent.
  assert.deepEqual(h.lastInput('viz_ctrl').value, [{ col: '*', value: 'zzzz' }]);
  h.close();
});

test('Enter on a query that matched nothing does the same', () => {
  const h = boot();
  h.click(trigger(h));
  h.type(pop(h).querySelector('.pp-find-input'), 'zzzz');
  h.key(h.q('.pp-find-pop'), 'Enter');
  assert.deepEqual(h.lastInput('viz_ctrl').value, [{ col: '*', value: 'zzzz' }]);
  h.close();
});

test('a pick this patient has none of is shown at zero, never dropped', () => {
  const h = boot();
  const t = trigger(h);
  t.setAttribute('data-picks',
    '[{"col":"AEBODSYS","value":"CARDIAC DISORDERS"}]');
  h.click(t);
  const zero = Array.from(pop(h).querySelectorAll('.pp-find-opt.is-zero'));
  assert.equal(zero.length, 1);
  assert.match(zero[0].textContent, /Cardiac disorders/);
  assert.ok(zero[0].classList.contains('is-picked'));
  // And it can be taken off again, which is the whole reason it is drawn:
  // picks survive a patient switch on purpose.
  h.click(zero[0]);
  h.click(pop(h).querySelector('.pp-find-done'));
  assert.deepEqual(h.lastInput('viz_ctrl').value, []);
  h.close();
});

test("the trigger's x and the caption's chip both clear every pick", () => {
  const h = boot();
  const header = gantt(h).querySelector('.pp-chart-header');
  header.insertAdjacentHTML('beforeend',
    '<button class="pp-ctrl-find-clear" data-viz-id="ae_gantt" ' +
    'data-param="find"></button>');
  h.click(header.querySelector('.pp-ctrl-find-clear'));
  assert.deepEqual(h.lastInput('viz_ctrl'),
    { viz_id: 'ae_gantt', param: 'find', value: [] });

  h.el('cohort_band_caption').insertAdjacentHTML('beforeend',
    '<a class="pp-cohort-bandcap-find" data-viz-id="ae_gantt"></a>');
  h.click(h.q('.pp-cohort-bandcap-find'));
  assert.deepEqual(h.lastInput('viz_ctrl'),
    { viz_id: 'ae_gantt', param: 'find', value: [] });
  h.close();
});

test('the chart area is watched for size and its charts resized, debounced', () => {
  const h = boot();
  const area = h.q('.pp-chart-area');
  assert.ok(area, 'the chart area exists');
  assert.ok(h.observed().resize.includes(area));
  const widget = gantt(h).querySelector('.echarts4r');
  widget.__echarts = true;
  h.resize(area);
  h.resize(area);
  h.tick(79);
  assert.equal(h.win.echarts.__resized.length, 0);
  h.tick(1);
  assert.equal(h.win.echarts.__resized.length, 1);
  assert.equal(h.win.echarts.__resized[0], widget);

  // A render that replaces the area re-watches the new element.
  const again = h.doc.createElement('div');
  again.className = area.className;
  again.append(...Array.from(area.childNodes));
  area.replaceWith(again);
  h.rendered('chart_area');
  assert.notEqual(again, area);
  assert.ok(h.observed().resize.includes(again));
  h.close();
});
