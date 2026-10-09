/* PatientProfile.tip(): the data tooltip drawn from the `tip` R builds
 * (R/pp-tooltip.R). */
'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { mount } = require('./harness.js');

const draw = (h, tip) => {
  const box = h.doc.createElement('div');
  box.innerHTML = h.win.PatientProfile.tip({ data: { tip } });
  return box;
};

test('a tooltip is the headline with its swatch, the line under it, then rows', () => {
  const h = mount();
  const box = draw(h, {
    head: 'Agitation', color: '#dc2626', sub: 'Psychiatric disorders',
    rows: [
      { label: 'Severity', value: 'Severe' },
      { label: 'From', value: 'D32', meta: '2013-10-17' },
      { label: 'To', value: 'ongoing' }
    ]
  });
  // blockr.viz's card classes (tooltip.css), so it looks like every chart's.
  assert.equal(box.querySelector('.dd-tt-head').textContent, 'Agitation');
  assert.match(box.querySelector('.dd-tt-sw').getAttribute('style'), /#dc2626/);
  assert.equal(box.querySelector('.dd-tt-row.dd-tt-label').textContent,
    'Psychiatric disorders');
  const rows = [...box.querySelectorAll('.dd-tt-row:not(.dd-tt-label)')];
  assert.deepEqual(rows.map((r) => r.querySelector('.dd-tt-label').textContent),
    ['Severity', 'From', 'To']);
  assert.equal(rows[1].querySelector('.dd-tt-meta').textContent, '2013-10-17');
  assert.equal(rows[2].querySelector('.dd-tt-meta'), null);
  h.close();
});

test('study text is escaped, never markup', () => {
  const h = mount();
  const box = draw(h, {
    head: '<img src=x onerror=alert(1)>', color: '"><b>x</b>',
    rows: [{ label: 'Outcome', value: '<b>bold</b>' }], note: '<i>n</i>'
  });
  assert.equal(box.querySelector('img'), null);
  assert.equal(box.querySelector('b'), null);
  assert.equal(box.querySelector('i'), null);
  assert.equal(box.querySelector('.dd-tt-head').textContent, '<img src=x onerror=alert(1)>');
  h.close();
});

test('a point without a tip draws nothing', () => {
  const h = mount();
  assert.equal(h.win.PatientProfile.tip({ data: [1, 2] }), '');
  assert.equal(h.win.PatientProfile.tip(null), '');
  h.close();
});
