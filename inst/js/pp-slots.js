// @ts-check
/* The words in a panel's sentence (design system, "The sentence and its
 * slots"): each `.pp-slot` opens blockr.ui's Blockr.Select.menu listing the
 * values of the setting it names, and a pick goes to R as one `viz_ctrl`
 * input. The markup comes from R (pp_controls_ui()); the header it sits in is
 * swapped wholesale after every change, so nothing here keeps state on it.
 *
 *   data-kind="single"  one of a few values (lanes, a findings value, a
 *                        radio); the pick applies at once and the menu closes
 *   data-kind="multi"   several values (items, visits); applied when the menu
 *                        closes, because every change redraws the panel
 *   data-kind="find"    the panel's filter over the values of the level the
 *                        lanes show; this patient's terms first with their
 *                        record counts, then the rest of the cohort's, fetched
 *                        once over `find_vocab`; applied when the menu closes
 *
 * An on/off control is a blockr.ui checkbox after the sentence
 * (`.pp-ctrl-check`), sent as it changes.
 *
 * Depends on: pp-core.js, blockr.ui's blockr-ui.js and blockr-select.js
 */
PatientProfile.part(function(ctx) {
  var ns = ctx.ns;
  var layoutId = ns('pp_layout');
  var ctrlInputId = ns('viz_ctrl');
  var vocabInputId = ns('find_vocab');
  var vocabMsgId = ns('find_vocab');

  /** How long a first open waits for the cohort's terms before it opens with
   *  this patient's alone. */
  var VOCAB_WAIT = 1500;

  /** The cohort's terms per panel, as far as this client knows them.
   * @type {Object<string, {token: string, groups: Array<{col: string, options: Array<{value: string, n: number}>}>}>} */
  var vocab = {};
  /** A find word waiting for its vocabulary: open it when the reply lands.
   * @type {{vizId: string, open: () => void, timer: number} | null} */
  var waiting = null;

  /** @param {Element} el @param {string} name */
  function parseAttr(el, name) {
    var raw = el.getAttribute(name);
    if (!raw) return null;
    try { return JSON.parse(raw); } catch (e) { return null; }
  }

  /* The panel's own casing: a coding dictionary shouts, and a list of
   * shouted terms is unreadable. Same rule as pp_term_label(). */
  /** @param {string} s */
  function termLabel(s) {
    s = String(s || '');
    return s ? s.charAt(0).toUpperCase() + s.slice(1).toLowerCase() : '';
  }

  /** @param {string} vizId @param {string} param @param {unknown} value */
  function send(vizId, param, value) {
    Shiny.setInputValue(ctrlInputId, { viz_id: vizId, param: param, value: value },
                        {priority: 'event'});
  }

  /** @param {HTMLElement} word @param {object} config */
  function openMenu(word, config) {
    if (!window.Blockr || !Blockr.Select || !Blockr.Select.menu) return;
    word.classList.add('blockr-slot--open');
    word.setAttribute('aria-expanded', 'true');
    var onClose = /** @type {any} */ (config).onClose;
    Blockr.Select.menu(word, Object.assign({}, config, {
      onClose: function() {
        word.classList.remove('blockr-slot--open');
        word.setAttribute('aria-expanded', 'false');
        if (onClose) onClose();
      }
    }));
  }

  /** One of a few values: applies at once. A word may send to an input of
   *  its own (`data-input`, the patients' sort) rather than to `viz_ctrl`,
   *  and its options may carry a `key` apart from what they show.
   *  @param {HTMLElement} word */
  function openSingle(word) {
    var vizId = word.getAttribute('data-viz-id') || '';
    var param = word.getAttribute('data-param') || '';
    var input = word.getAttribute('data-input');
    var cur = parseAttr(word, 'data-value');
    /** @type {Array<{value: string, label?: string, key?: string}>} */
    var options = parseAttr(word, 'data-options') || [];
    /** @type {Record<string, string>} */
    var keyOf = {};
    options.forEach(function(o) { if (o.key !== undefined) keyOf[o.value] = o.key; });
    openMenu(word, {
      title: word.getAttribute('data-title') || undefined,
      options: options.map(function(o) {
        return o.label ? { value: o.value, label: o.label } : { value: o.value };
      }),
      selected: cur,
      labelFirst: word.getAttribute('data-label-first') !== 'false',
      search: false,
      onChange: function(/** @type {string} */ v) {
        if (!v || v === cur) return;
        var value = keyOf[v] !== undefined ? keyOf[v] : v;
        if (input) {
          Shiny.setInputValue(ns(input), value, {priority: 'event'});
        } else {
          send(vizId, param, value);
        }
      }
    });
  }

  /** Several values: applied when the menu closes. @param {HTMLElement} word */
  function openMulti(word) {
    var vizId = word.getAttribute('data-viz-id') || '';
    var param = word.getAttribute('data-param') || '';
    var before = parseAttr(word, 'data-value') || [];
    var picked = before.slice();
    openMenu(word, {
      mode: 'multi',
      title: word.getAttribute('data-title') || undefined,
      options: parseAttr(word, 'data-options') || [],
      selected: picked,
      labelFirst: true,
      onChange: function(/** @type {string[]} */ v) { picked = v.slice(); },
      onClose: function() {
        // Nothing picked is not a view of anything: keep what was there.
        if (!picked.length) return;
        if (JSON.stringify(picked) !== JSON.stringify(before)) {
          send(vizId, param, picked);
        }
      }
    });
  }

  /** The filter: this patient's terms, then the cohort's. @param {HTMLElement} word */
  function openFind(word) {
    var vizId = word.getAttribute('data-viz-id') || '';
    var param = word.getAttribute('data-param') || '';
    var col = word.getAttribute('data-col') || '';
    /** @type {Array<{value: string, n: number}>} */
    var own = parseAttr(word, 'data-options') || [];
    /** @type {Array<{col: string, value: string}>} */
    var picks = parseAttr(word, 'data-picks') || [];
    if (!Array.isArray(picks)) picks = [picks];

    // What the menu shows is the term in the panel's casing; what R filters
    // on is the term as the data spells it.
    /** @type {Record<string, string>} */
    var raw = {};
    /** @type {Array<{value: string, label?: string}>} */
    var options = [];
    /** @param {string} v @param {string} [meta] */
    var add = function(v, meta) {
      var shown = termLabel(v);
      if (raw[shown] !== undefined) return;
      raw[shown] = v;
      options.push(meta ? { value: shown, label: meta } : { value: shown });
    };
    own.slice().sort(function(a, b) { return termLabel(a.value) < termLabel(b.value) ? -1 : 1; })
      .forEach(function(o) { add(o.value, String(o.n)); });
    var cohort = vocab[vizId];
    if (cohort) {
      (cohort.groups || []).forEach(function(g) {
        if (g.col !== col) return;
        (g.options || []).forEach(function(o) { add(o.value); });
      });
    }
    var before = picks.filter(function(p) { return p.col === col; })
      .map(function(p) { add(p.value); return termLabel(p.value); });
    var picked = before.slice();

    openMenu(word, {
      mode: 'multi',
      title: word.getAttribute('data-title') || 'Show',
      options: options,
      selected: picked,
      onChange: function(/** @type {string[]} */ v) { picked = v.slice(); },
      onClose: function() {
        if (JSON.stringify(picked) === JSON.stringify(before)) return;
        send(vizId, param, picked.map(function(shown) {
          return { col: col, value: raw[shown] !== undefined ? raw[shown] : shown };
        }));
      }
    });
  }

  /** Ask R for the cohort's terms; the reply lands in the handler below. */
  function askVocab(/** @type {string} */ vizId) {
    Shiny.setInputValue(vocabInputId, {
      viz_id: vizId,
      have: (vocab[vizId] && vocab[vizId].token) || '',
      nonce: Date.now()
    }, {priority: 'event'});
  }

  Shiny.addCustomMessageHandler(vocabMsgId, function(msg) {
    if (!msg || !msg.viz_id) return;
    if (msg.unchanged) {
      if (vocab[msg.viz_id]) vocab[msg.viz_id].token = String(msg.token);
    } else {
      vocab[msg.viz_id] = {
        token: String(msg.token),
        groups: Array.isArray(msg.groups) ? msg.groups : []
      };
    }
    if (waiting && waiting.vizId === msg.viz_id) {
      var w = waiting;
      waiting = null;
      clearTimeout(w.timer);
      w.open();
    }
  });

  $(document).on('click', '#' + layoutId + ' .pp-slot', function(e) {
    e.stopPropagation();
    var word = /** @type {HTMLElement} */ (this);
    var kind = word.getAttribute('data-kind');
    if (kind === 'single') return openSingle(word);
    if (kind === 'multi') return openMulti(word);
    if (kind !== 'find') return;
    var vizId = word.getAttribute('data-viz-id') || '';
    // With the cohort's terms at hand, open now and refresh them for next
    // time; without, wait for them (briefly) on this first open.
    if (vocab[vizId]) {
      askVocab(vizId);
      return openFind(word);
    }
    if (waiting) clearTimeout(waiting.timer);
    var open = function() { if (word.isConnected) openFind(word); };
    waiting = {
      vizId: vizId,
      open: open,
      timer: window.setTimeout(function() { waiting = null; open(); }, VOCAB_WAIT)
    };
    askVocab(vizId);
  });

  $(document).on('change', '#' + layoutId + ' .pp-ctrl-check input', function(e) {
    e.stopPropagation();
    var input = /** @type {HTMLInputElement} */ (this);
    send(input.getAttribute('data-viz-id') || '', input.getAttribute('data-param') || '',
         input.checked);
  });
});
