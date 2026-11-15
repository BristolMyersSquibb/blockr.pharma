'use strict';
/* A failing assert on a happy-dom node or window fails with a short message,
 * such as "<input#a.x> !== <button#b>". Without this, node:assert renders and
 * diffs the whole fake browser behind the node, which can take all of the
 * machine's memory before the test ends. Every test process that builds a
 * window loads this first. Asserts on anything else are untouched. */
const util = require('node:util');
const assert = require('node:assert');
const hd = require('happy-dom');

const isDom = (x) => x instanceof hd.Node || x instanceof hd.Window ||
  x instanceof hd.BrowserWindow;

const short = (n) => {
  if (n instanceof hd.Window || n instanceof hd.BrowserWindow) return '[Window]';
  if (n.nodeType === 1) {
    const id = n.id ? '#' + n.id : '';
    const cls = typeof n.className === 'string' && n.className.trim()
      ? '.' + n.className.trim().split(/\s+/).slice(0, 3).join('.') : '';
    return `<${String(n.tagName).toLowerCase()}${id}${cls}>`;
  }
  if (n.nodeType === 3) return `#text ${JSON.stringify(String(n.textContent).slice(0, 40))}`;
  if (n.nodeType === 9) return '#document';
  return `[${n.nodeName || 'node'}]`;
};

// Plain objects and arrays a few levels down; a node or window is a leaf.
const hasDom = (x, depth = 0) => {
  if (isDom(x)) return true;
  if (depth > 3 || x === null || typeof x !== 'object') return false;
  const proto = Object.getPrototypeOf(x);
  if (!Array.isArray(x) && proto !== Object.prototype && proto !== null) return false;
  return Object.values(x).some((v) => hasDom(v, depth + 1));
};
const show = (x, depth = 0) => {
  if (isDom(x)) return short(x);
  if (depth > 3 || x === null || typeof x !== 'object') return util.inspect(x);
  if (Array.isArray(x)) return '[' + x.map((v) => show(v, depth + 1)).join(', ') + ']';
  return '{ ' + Object.entries(x).map(([k, v]) => k + ': ' + show(v, depth + 1)).join(', ') + ' }';
};
// Deep equality with nodes and windows compared by identity.
const same = (a, b, depth = 0) => {
  if (isDom(a) || isDom(b)) return a === b;
  if (depth > 3 || a === null || b === null || typeof a !== 'object' || typeof b !== 'object') {
    return util.isDeepStrictEqual(a, b);
  }
  if (Array.isArray(a) !== Array.isArray(b)) return false;
  const ka = Object.keys(a), kb = Object.keys(b);
  return ka.length === kb.length && ka.every((k) => k in b && same(a[k], b[k], depth + 1));
};

const fail = (a, b, op, message, name) => {
  if (message instanceof Error) throw message;
  const what = `${show(a)} ${op} ${show(b)}`;
  throw new assert.AssertionError({
    message: message ? `${message}\n${what}` : what,
    actual: show(a), expected: show(b), operator: name
  });
};

const checks = {
  equal: [(a, b) => a == b, '!='], // eslint-disable-line eqeqeq
  strictEqual: [(a, b) => Object.is(a, b), '!=='],
  notEqual: [(a, b) => a != b, '=='], // eslint-disable-line eqeqeq
  notStrictEqual: [(a, b) => !Object.is(a, b), '==='],
  deepEqual: [same, 'not deep-equal to'],
  deepStrictEqual: [same, 'not deep-equal to'],
  notDeepEqual: [(a, b) => !same(a, b), 'deep-equal to'],
  notDeepStrictEqual: [(a, b) => !same(a, b), 'deep-equal to']
};

const wrap = (target, name, check, op) => {
  const orig = target[name];
  if (typeof orig !== 'function' || orig.__domGuard) return;
  const fn = function(actual, expected, message) {
    if (!hasDom(actual) && !hasDom(expected)) return orig.apply(this, arguments);
    if (!check(actual, expected)) fail(actual, expected, op, message, name);
  };
  fn.__domGuard = true;
  target[name] = fn;
};

for (const [name, [check, op]] of Object.entries(checks)) wrap(assert, name, check, op);
// assert/strict: its equal is strictEqual, its deepEqual deepStrictEqual.
const strict = assert.strict;
for (const [name, [check, op]] of Object.entries(checks)) {
  const s = { equal: 'strictEqual', notEqual: 'notStrictEqual', deepEqual: 'deepStrictEqual',
    notDeepEqual: 'notDeepStrictEqual' }[name] || name;
  wrap(strict, name, checks[s][0], checks[s][1]);
}
