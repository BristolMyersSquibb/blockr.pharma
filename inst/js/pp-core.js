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
  window.PatientProfile = {
    part: function(fn) { parts.push(fn); },
    mount: function(cfg) {
      var ctx = {
        cfg: cfg,
        ns: function(name) { return cfg.id + '-' + name; }
      };
      parts.forEach(function(p) { p(ctx); });
      return ctx;
    }
  };
})();
