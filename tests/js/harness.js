/* Mount the patient profile's JavaScript in a headless DOM and drive it the
 * way R and the browser do.
 *
 * inst/js/pp-*.js is the half of the block that R tests cannot
 * reach: `testServer()` sees the custom messages R sends, never what the
 * client does with them. The pick debounce, the keyboard walk, the picker's
 * remember-and-reapply, the ghost's wait for a canvas, the well's sizing:
 * all of it lives there. So the tests run the real file, against the real
 * markup (tests/js/fixtures, rendered by the block's own R), with real
 * jQuery, in happy-dom.
 *
 * What is stubbed is the browser's asynchrony and Shiny:
 *   - a fake clock owns setTimeout, setInterval, requestAnimationFrame,
 *     Date.now and performance.now, so a test says `tick(250)` instead of
 *     waiting for the debounce;
 *   - ResizeObserver and IntersectionObserver record what they watch and
 *     fire when the test says so;
 *   - `Shiny.addCustomMessageHandler` captures the handlers so `send()`
 *     dispatches on the channel names R uses, and `Shiny.setInputValue`
 *     records every input the client pushes;
 *   - happy-dom lays nothing out, so `layout(el, rect)` gives an element the
 *     box a test needs it to have.
 *
 * Usage:
 *   const h = mount();                 // the default profile, events band
 *   h.click(h.rows()[2]);              // a real click through jQuery
 *   h.tick(250);                       // the pick debounce elapses
 *   h.inputs('pick_subject');          // [{value: '01-701-1028', ...}]
 *   h.send('sync_subject', {id: '01-701-1015'});
 *   h.close();
 */
'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { execSync } = require('node:child_process');
const { Window } = require('happy-dom');

const ROOT = path.join(__dirname, '..', '..');
const FIXTURES = path.join(__dirname, 'fixtures');
const NS = 'proxy1';

const read = (file) => fs.readFileSync(file, 'utf8');
const fixture = (name) => read(path.join(FIXTURES, name + '.html'));

/* Shiny's own jQuery, found through R once per process (PP_JQUERY overrides). */
function jqueryPath() {
  if (process.env.PP_JQUERY) return process.env.PP_JQUERY;
  return execSync(
    "Rscript -e \"cat(system.file('www/shared/jquery.min.js', package = 'shiny'))\"",
    { encoding: 'utf8' }
  ).trim();
}
const JQUERY = read(jqueryPath());
const PARTS = ['pp-core.js', 'pp-header.js', 'pp-cohort.js', 'pp-picker.js', 'pp-panels.js'];
const BLOCK_JS = PARTS.map((f) => read(path.join(ROOT, 'inst', 'js', f))).join('\n');

/* Objects built inside the window's realm have that realm's prototypes;
 * JSON is the honest comparison and also what Shiny puts on the wire. */
const json = (x) => (x === undefined ? undefined : JSON.parse(JSON.stringify(x)));

/**
 * The fake clock. Timers run in order of their due time when the test
 * advances the clock; nothing runs on its own.
 */
function makeClock() {
  let now = 0;
  let seq = 0;
  const timers = [];
  const schedule = (fn, delay, repeat) => {
    const every = Math.max(0, Number(delay) || 0);
    const t = { id: ++seq, at: now + every, fn, every: repeat ? Math.max(1, every) : 0 };
    timers.push(t);
    return t.id;
  };
  const cancel = (id) => {
    const i = timers.findIndex((t) => t.id === id);
    if (i >= 0) timers.splice(i, 1);
  };
  return {
    now: () => now,
    setTimeout: (fn, d, ...a) => schedule(() => fn(...a), d, false),
    setInterval: (fn, d, ...a) => schedule(() => fn(...a), d, true),
    clearTimeout: cancel,
    clearInterval: cancel,
    requestAnimationFrame: (fn) => schedule(() => fn(now), 16, false),
    cancelAnimationFrame: cancel,
    /** Advance by `ms`, running every timer that falls due, in order. */
    tick(ms) {
      const target = now + (ms || 0);
      for (;;) {
        const due = timers.filter((t) => t.at <= target).sort((a, b) => a.at - b.at || a.id - b.id);
        if (!due.length) break;
        const t = due[0];
        now = Math.max(now, t.at);
        if (t.every) t.at = now + t.every; else cancel(t.id);
        t.fn();
      }
      now = target;
    },
    pending: () => timers.length
  };
}

/**
 * @param {{profile?: 'default'|'lab', grip?: string, bandH?: number}} [opts]
 */
function mount(opts = {}) {
  const prefix = opts.profile === 'lab' ? 'lab_' : '';
  const win = new Window({ url: 'http://localhost/' });
  const doc = win.document;
  const clock = makeClock();

  // The page as the browser assembles it: the static UI, then every
  // renderUI output dropped into its placeholder, then the slots into the
  // chart area's placeholders.
  doc.body.innerHTML = fixture(prefix + 'ui');
  const fill = (name) => {
    const host = doc.getElementById(`${NS}-${name}`);
    if (!host) throw new Error(`the UI has no placeholder for ${name}`);
    host.innerHTML = fixture(prefix + name);
    return host;
  };
  const outputs = ['sidebar_cohort', 'panel_picker', 'cohort_band_caption',
    'header_bar', 'subject_facts', 'chart_area'];
  outputs.forEach(fill);
  const slotNames = Array.from(doc.querySelectorAll('[id*=viz_slot_]'))
    .map((el) => el.id.replace(`${NS}-`, ''));
  slotNames.forEach(fill);
  // What R sent while these fixtures rendered, in order (render.R).
  const messages = JSON.parse(read(path.join(FIXTURES, prefix + 'messages.json')));

  // The clock before anything that could schedule.
  ['setTimeout', 'setInterval', 'clearTimeout', 'clearInterval',
    'requestAnimationFrame', 'cancelAnimationFrame'].forEach((k) => {
    win[k] = clock[k];
  });
  win.performance.now = clock.now;

  // Browser APIs the block reaches for, in the window's realm.
  win.eval(`
    window.__handlers = {};
    window.__inputs = [];
    window.__resize = [];
    window.__intersect = [];
    window.Shiny = {
      addCustomMessageHandler: function (n, f) { window.__handlers[n] = f; },
      setInputValue: function (n, v, o) { window.__inputs.push({name: n, value: v, opts: o || null}); }
    };
    window.ResizeObserver = function (cb) {
      this.cb = cb; this.targets = [];
      window.__resize.push(this);
    };
    window.ResizeObserver.prototype.observe = function (el) { this.targets.push(el); };
    window.ResizeObserver.prototype.unobserve = function (el) {
      this.targets = this.targets.filter(function (t) { return t !== el; });
    };
    window.ResizeObserver.prototype.disconnect = function () { this.targets = []; };
    window.IntersectionObserver = function (cb, o) {
      this.cb = cb; this.opts = o || {}; this.targets = [];
      window.__intersect.push(this);
    };
    window.IntersectionObserver.prototype.observe = function (el) { this.targets.push(el); };
    window.IntersectionObserver.prototype.unobserve = function (el) {
      this.targets = this.targets.filter(function (t) { return t !== el; });
    };
    window.IntersectionObserver.prototype.disconnect = function () { this.targets = []; };
    window.matchMedia = function () {
      return {matches: false, addEventListener: function () {}, addListener: function () {}};
    };
    window.echarts = {
      __resized: [],
      getInstanceByDom: function (el) {
        var inst = {resize: function () { window.echarts.__resized.push(el); }};
        return el && el.__echarts ? inst : null;
      }
    };
    Element.prototype.scrollIntoView = Element.prototype.scrollIntoView || function () {};
  `);

  // Date.now inside the block reads the fake clock; everything else about
  // Date is the real thing.
  win.eval(`
    (function () {
      var Real = Date;
      var Fake = function () { return new (Function.prototype.bind.apply(Real, [null].concat([].slice.call(arguments))))(); };
      Fake.now = function () { return window.performance.now(); };
      Fake.parse = Real.parse; Fake.UTC = Real.UTC; Fake.prototype = Real.prototype;
      window.Date = Fake;
    })();
  `);

  win.eval(JQUERY);
  win.eval(BLOCK_JS);
  win.eval(`window.PatientProfile.mount(${JSON.stringify({
    id: NS,
    grip: opts.grip || '<svg class="grip"></svg>',
    bandH: opts.bandH || 8
  })});`);

  const $ = win.jQuery;

  /** Fire Shiny's own output event on an element, as its render would. */
  const rendered = (name) => {
    const el = doc.getElementById(`${NS}-${name}`);
    if (!el) throw new Error(`no output ${name}`);
    $(el).trigger($.Event('shiny:value', { name: `${NS}-${name}`, value: null }));
    return el;
  };

  const api = {
    win, doc, $, clock, NS,
    tick: (ms) => clock.tick(ms),

    /** Everything the client pushed as a Shiny input, by name suffix. */
    inputs(suffix) {
      const all = json(win.__inputs);
      return suffix ? all.filter((i) => i.name === `${NS}-${suffix}`) : all;
    },
    lastInput(suffix) {
      const xs = api.inputs(suffix);
      return xs.length ? xs[xs.length - 1].value : undefined;
    },
    resetInputs() { win.__inputs.length = 0; },

    /** Dispatch a custom message on the channel R sends it on. */
    send(channel, message) {
      const h = win.__handlers[`${NS}-${channel}`];
      if (!h) throw new Error(`no handler registered for "${channel}"`);
      h(json(message));
      return api;
    },
    handlers: () => Object.keys(win.__handlers).map((k) => k.replace(`${NS}-`, '')),

    /** The payloads R sent on `channel` during the fixture render, in order. */
    recorded(channel) {
      return messages.filter((m) => m.channel === channel).map((m) => json(m.payload));
    },
    /** Send the last thing R sent on `channel`, or the nth (0-based). */
    sendRecorded(channel, n) {
      const xs = api.recorded(channel);
      if (!xs.length) throw new Error(`R sent nothing on "${channel}"`);
      return api.send(channel, xs[n === undefined ? xs.length - 1 : n]);
    },
    /** Send every recorded message in the order R sent them, as a page load does. */
    replay() {
      messages.forEach((m) => api.send(m.channel, m.payload));
      return api;
    },

    rendered,
    /** Announce every output as rendered, the way a full page render does. */
    renderAll() {
      outputs.concat(slotNames).forEach(rendered);
      return api;
    },

    q: (sel) => doc.querySelector(sel),
    qa: (sel) => Array.from(doc.querySelectorAll(sel)),
    el: (name) => doc.getElementById(`${NS}-${name}`),

    /** A real click through jQuery's delegated handlers. */
    click(target) {
      const el = typeof target === 'string' ? doc.querySelector(target) : target;
      if (!el) throw new Error(`nothing to click for ${target}`);
      el.dispatchEvent(new win.MouseEvent('click', { bubbles: true, cancelable: true }));
      return api;
    },
    key(target, key, extra) {
      const el = typeof target === 'string' ? doc.querySelector(target) : target;
      const ev = new win.KeyboardEvent('keydown', Object.assign(
        { key, bubbles: true, cancelable: true }, extra || {}));
      el.dispatchEvent(ev);
      return ev;
    },
    mouse(type, target, at) {
      const el = typeof target === 'string' ? doc.querySelector(target) : target;
      const ev = new win.MouseEvent(type, Object.assign(
        { bubbles: true, cancelable: true, button: 0 }, at || {}));
      (el || doc).dispatchEvent(ev);
      return ev;
    },
    /** Type into an input the way a keyboard does: value, then `input`. */
    type(target, text) {
      const el = typeof target === 'string' ? doc.querySelector(target) : target;
      el.value = text;
      el.dispatchEvent(new win.Event('input', { bubbles: true }));
      return api;
    },

    /**
     * Give an element the box happy-dom cannot compute. `rect` is
     * {top, left, width, height}; the rest follows. `scroll` sets
     * clientHeight / scrollHeight / scrollTop for a scrolling box.
     */
    layout(target, rect, scroll) {
      const el = typeof target === 'string' ? doc.querySelector(target) : target;
      const r = Object.assign({ top: 0, left: 0, width: 0, height: 0 }, rect);
      el.getBoundingClientRect = () => ({
        top: r.top, left: r.left, width: r.width, height: r.height,
        right: r.left + r.width, bottom: r.top + r.height, x: r.left, y: r.top
      });
      Object.defineProperty(el, 'offsetParent', { value: doc.body, configurable: true });
      Object.defineProperty(el, 'offsetHeight', { value: r.height, configurable: true });
      Object.defineProperty(el, 'offsetWidth', { value: r.width, configurable: true });
      const s = Object.assign({ clientHeight: r.height, clientWidth: r.width,
        scrollHeight: r.height, scrollTop: 0 }, scroll || {});
      ['clientHeight', 'clientWidth', 'scrollHeight'].forEach((k) => {
        Object.defineProperty(el, k, { value: s[k], configurable: true });
      });
      let top = s.scrollTop;
      Object.defineProperty(el, 'scrollTop', {
        get: () => top, set: (v) => { top = v; }, configurable: true
      });
      return el;
    },

    /** Fire every ResizeObserver watching `el` (or all of them). */
    resize(el) {
      win.__resize.forEach((ro) => {
        if (!el || ro.targets.includes(el)) ro.cb(ro.targets.map((t) => ({ target: t })), ro);
      });
      return api;
    },
    /** Bring elements into view for every IntersectionObserver watching them. */
    intersect(els) {
      win.__intersect.forEach((io) => {
        const hit = io.targets.filter((t) => !els || els.includes(t));
        if (hit.length) io.cb(hit.map((t) => ({ target: t, isIntersecting: true })), io);
      });
      return api;
    },
    observed: () => ({
      resize: win.__resize.flatMap((ro) => ro.targets),
      intersect: win.__intersect.flatMap((io) => io.targets)
    }),

    // --- what the user sees --------------------------------------------
    rows: () => api.qa('.pp-pt'),
    shownIds: () => api.qa('.pp-pt:not(.is-filtered-out)').map((r) => r.getAttribute('data-usubjid')),
    selectedId() {
      const r = api.q('.pp-pt.is-selected');
      return r ? r.getAttribute('data-usubjid') : null;
    },
    onProfile: () => api.qa('.pp-add-ord').map((r) => r.getAttribute('data-viz-id')),
    text: (sel) => (api.q(sel) ? api.q(sel).textContent.trim() : null),
    slots: () => api.qa('[id*=viz_slot_]').map((e) => e.id.split('viz_slot_')[1]),

    close() { win.close(); }
  };

  return api;
}

module.exports = { mount, NS, json };
