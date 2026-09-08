/* The band each cohort row carries: drawn from the row's own attributes
 * when the row scrolls into view, spans for events, a line for a series. */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const attrs = (el) => Object.fromEntries(
  Array.from(el.attributes).map((a) => [a.name, a.value]));

test('rows are watched once the list renders and drawn as they intersect', () => {
  const h = mount();
  h.rendered('sidebar_cohort');
  assert.equal(h.observed().intersect.length, 0, 'deferred a tick');
  h.tick(0);
  const rows = h.rows();
  assert.equal(h.observed().intersect.length, 12);
  assert.ok(rows.every((r) => !r.hasAttribute('data-band-drawn')));

  h.intersect([rows[0], rows[3]]);
  assert.equal(rows[0].getAttribute('data-band-drawn'), '1');
  assert.equal(rows[3].getAttribute('data-band-drawn'), '1');
  assert.equal(rows[1].hasAttribute('data-band-drawn'), false);
  // Drawn rows are no longer watched; a second pass draws nothing twice.
  assert.equal(h.observed().intersect.length, 10);
  const before = rows[0].querySelector('.pp-pt-band').children.length;
  h.intersect([rows[0]]);
  assert.equal(rows[0].querySelector('.pp-pt-band').children.length, before);
  h.close();
});

test('an events row draws one span per triplet over the track, and the end-of-treatment mark', () => {
  const h = mount();
  h.rendered('sidebar_cohort');
  h.tick(0);
  const row = h.q('.pp-pt[data-usubjid="01-701-1023"]');
  assert.equal(row.getAttribute('data-band'), '26.4,18.4,#CA8A04 26.4,149.6,#D97706 26.4,149.6,#CA8A04');
  h.intersect([row]);
  const svg = row.querySelector('.pp-pt-band');
  const rects = Array.from(svg.querySelectorAll('rect'));
  assert.equal(rects.length, 4, 'the track from R plus three spans');
  assert.deepEqual(attrs(rects[1]), {
    x: '26.4', y: '0', width: '18.4', height: '8', fill: '#CA8A04', opacity: '0.9'
  });
  assert.equal(rects[3].getAttribute('fill'), '#CA8A04');
  const eot = svg.querySelector('path');
  assert.equal(row.getAttribute('data-eot'), '46.4');
  // A diamond centred on the end-of-treatment x at half the band height.
  assert.equal(eot.getAttribute('d'), 'M46.4 1L49.4 4L46.4 7L43.4 4Z');
  assert.equal(eot.getAttribute('fill'), 'var(--pp-cohort-eot, #6b7280)');
  h.close();
});

test('a series row draws the reference range, the line, and no track', () => {
  const h = mount({ profile: 'lab' });
  h.rendered('sidebar_cohort');
  h.tick(0);
  const rows = h.rows();
  // Albumin's upper limit sits above the strip for most of this cohort, so
  // the fixture rows carry only the lower edge; one row is given both.
  const withLo = rows.find((r) => !r.hasAttribute('data-limit') && r.hasAttribute('data-limit-lo'));
  assert.ok(withLo, 'a row with the lower edge only');
  const withBoth = rows.find((r) => r !== withLo && r.hasAttribute('data-limit-lo'));
  withBoth.setAttribute('data-limit', '6.5');
  h.intersect(rows);

  // Both limits: a shaded rect from the upper to the lower edge.
  let svg = withBoth.querySelector('.pp-pt-band');
  assert.equal(svg.getAttribute('height'), '30');
  const hi = parseFloat(withBoth.getAttribute('data-limit'));
  const lo = parseFloat(withBoth.getAttribute('data-limit-lo'));
  const rect = svg.querySelector('rect');
  assert.equal(svg.querySelectorAll('rect').length, 1, 'no track on a series row');
  assert.equal(parseFloat(rect.getAttribute('y')), hi);
  assert.ok(Math.abs(parseFloat(rect.getAttribute('height')) - (lo - hi)) < 1e-9);
  assert.equal(rect.getAttribute('width'), '176');
  assert.match(rect.getAttribute('fill'), /--pp-cohort-ref/);
  const path = svg.querySelector('path');
  assert.equal(path.getAttribute('d'), withBoth.getAttribute('data-band'));
  assert.equal(path.getAttribute('fill'), 'none');
  assert.match(path.getAttribute('stroke'), /--pp-cohort-line/);

  // One limit: a dashed hairline at that edge instead.
  svg = withLo.querySelector('.pp-pt-band');
  assert.equal(svg.querySelectorAll('rect').length, 0);
  const line = svg.querySelector('line');
  assert.equal(line.getAttribute('y1'), withLo.getAttribute('data-limit-lo'));
  assert.equal(line.getAttribute('stroke-dasharray'), '2 2');
  h.close();
});

test('a single visit is a dot, and clipped values are ticked at the edge they left', () => {
  const h = mount({ profile: 'lab' });
  h.rendered('sidebar_cohort');
  h.tick(0);
  const row = h.rows()[0];
  row.setAttribute('data-dot', '88,15');
  row.setAttribute('data-clip', '10 20');
  row.setAttribute('data-clip-lo', '30');
  h.intersect([row]);
  const svg = row.querySelector('.pp-pt-band');
  assert.equal(svg.querySelector('path'), null, 'a dot replaces the line');
  const dot = svg.querySelector('circle');
  assert.deepEqual([dot.getAttribute('cx'), dot.getAttribute('cy'), dot.getAttribute('r')], ['88', '15', '1.6']);
  const ticks = Array.from(svg.querySelectorAll('line')).filter((l) => l.getAttribute('stroke-width') === '1.2');
  assert.equal(ticks.length, 3);
  assert.deepEqual(ticks.map((t) => [t.getAttribute('x1'), t.getAttribute('y1'), t.getAttribute('y2')]),
    [['10', '0', '2.5'], ['20', '0', '2.5'], ['30', '27.5', '30']]);
  h.close();
});

test('the tick height comes from the row\'s own svg, the config only as a fallback', () => {
  const h = mount({ profile: 'lab', bandH: 12 });
  h.rendered('sidebar_cohort');
  h.tick(0);
  const row = h.rows()[0];
  row.querySelector('.pp-pt-band').removeAttribute('height');
  row.setAttribute('data-clip-lo', '5');
  h.intersect([row]);
  const tick = Array.from(row.querySelectorAll('line')).find((l) => l.getAttribute('x1') === '5');
  assert.deepEqual([tick.getAttribute('y1'), tick.getAttribute('y2')], ['9.5', '12']);
  h.close();
});

test('without IntersectionObserver every row is drawn at once', () => {
  const h = mount();
  h.win.IntersectionObserver = undefined;
  h.rendered('sidebar_cohort');
  h.tick(0);
  assert.ok(h.rows().every((r) => r.getAttribute('data-band-drawn') === '1'));
  h.close();
});

test('a re-rendered list is watched afresh', () => {
  const h = mount();
  const host = h.el('sidebar_cohort');
  const fresh = host.innerHTML;
  h.rendered('sidebar_cohort');
  h.tick(0);
  h.intersect(h.rows());
  host.innerHTML = fresh;
  h.rendered('sidebar_cohort');
  h.tick(0);
  assert.equal(h.observed().intersect.length, 12);
  assert.ok(h.rows().every((r) => !r.hasAttribute('data-band-drawn')));
  h.close();
});
