// @ts-check
/* Patient profile block: the client half, in parts.
 *
 * Each pp-*.js file registers one part with PatientProfile.part(); the
 * block's UI calls PatientProfile.mount(cfg) once the files are loaded, and
 * every part runs against the same context:
 *
 *   cfg  what R passed: {id, grip, bandH}
 *   ns   function(name) -> id + '-' + name, the way shiny::NS() builds ids
 *
 * Parts talk to the DOM and to Shiny, never to each other: what one part
 * needs to know about another arrives through the document (a render event,
 * a class on an element), which is also what the tests in tests/js can see.
 *
 * Load order is the order the R side lists the files in; parts register
 * document-level handlers, so it only decides which handler runs first.
 */
(function() {
  var parts = [];

  /* The charts' ink (design system, "Charts": canvas ink is read from the
   * tokens at render). The options R builds name a token wherever they mean
   * UI ink -- `"var(--blockr-color-text-muted)"` for axis labels,
   * `"var(--blockr-color-border-default)"` for split lines, the body face as
   * `"var(--bs-body-font-family)"` -- because a canvas cannot resolve a CSS
   * variable. Every setOption on this page resolves them first; a
   * renderItem function asks PatientProfile.ink() for the same values.
   * Data colours are hex and pass through untouched. */
  /** @type {Record<string, string>} */
  var inkCache = {};
  /** @param {string} name */
  function ink(name) {
    if (!(name in inkCache)) {
      var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
      if (!v && name === '--bs-body-font-family' && document.body) {
        v = getComputedStyle(document.body).fontFamily;
      }
      inkCache[name] = v;
    }
    return inkCache[name];
  }
  // The dark scheme is an attribute on <html>: its values are other values.
  if (typeof MutationObserver !== 'undefined') {
    new MutationObserver(function() { inkCache = {}; }).observe(
      document.documentElement,
      { attributes: true, attributeFilter: ['data-bs-theme', 'class', 'style'] });
  }
  var TOKEN = /^var\((--[A-Za-z0-9-]+)\)$/;
  /** Replace every `var(--token)` string in an option object, in place.
   * @param {any} o */
  function resolveInk(o) {
    if (!o || typeof o !== 'object') return o;
    var keys = Array.isArray(o) ? o.map(function(_, i) { return i; }) : Object.keys(o);
    keys.forEach(function(k) {
      var v = o[k];
      if (typeof v === 'string') {
        var m = TOKEN.exec(v);
        if (m) o[k] = ink(m[1]) || v;
      } else if (v && typeof v === 'object') {
        resolveInk(v);
      }
    });
    return o;
  }
  /* Every chart instance resolves its options on the way in, whichever path
   * sets them: htmlwidgets' first render or pp_slot_update()'s setOption. */
  function patchEcharts() {
    var ec = /** @type {any} */ (window).echarts;
    if (!ec || ec.__ppInk) return;
    var init = ec.init;
    ec.init = function() {
      var inst = init.apply(this, arguments);
      var set = inst.setOption;
      inst.setOption = function(/** @type {any} */ opt) {
        resolveInk(opt);
        return set.apply(this, arguments);
      };
      return inst;
    };
    ec.__ppInk = true;
  }
  patchEcharts();

  window.PatientProfile = {
    ink: ink,
    resolveInk: resolveInk,
    part: function(fn) { parts.push(fn); },
    mount: function(cfg) {
      patchEcharts();
      var ctx = {
        cfg: cfg,
        ns: function(name) { return cfg.id + '-' + name; }
      };
      parts.forEach(function(p) { p(ctx); });
      return ctx;
    }
  };
})();
