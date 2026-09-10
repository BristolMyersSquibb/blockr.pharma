// @ts-check
/* The panel's find control: a trigger in the header, a popover on <body>.
 *
 * The header is replaced wholesale on every settings change
 * (pp_slot_update() ships it as HTML and pp-panels.js sets innerHTML), so a
 * control that holds open state cannot live inside it. The trigger does --
 * it is a button R re-renders freely -- and the popover does not: it is
 * parented to <body>, owned by this file, and re-anchored to whichever
 * trigger carries its panel's id after a swap. That is the same move the
 * chart ghost makes, and for the same reason.
 *
 * The picks are applied when the popover CLOSES, not on each tick. One
 * change to this setting redraws the panel and re-derives the cohort band
 * for every patient in the list, so ticking five terms would pay that five
 * times; the per-option counts are what give feedback in the meantime.
 *
 * Depends on: pp-core.js
 */
PatientProfile.part(function(ctx) {
  var ns = ctx.ns;
  var layoutId = ns('pp_layout');
  var ctrlInputId = ns('viz_ctrl');

  /** The free-text pick's column. Not a column name and cannot be one. */
  var FREE = '*';

  var vocabInputId = ns('find_vocab');
  var vocabMsgId = ns('find_vocab');

  /** The COHORT's terms, per panel, as far as this client knows them.
   *  Fetched on the first thing the reader types, because looking for a term
   *  THIS patient does not have is how you arm a filter before paging
   *  through the cohort -- and because it is 16kB against the few hundred
   *  bytes of the patient's own list, so it does not ride the header.
   * @type {Object<string, {token: string, groups: Array<any>}>} */
  var vocab = {};

  /** @type {HTMLElement | null} */
  var pop = null;
  /** The panel the open popover belongs to, and its state.
   * @type {{vizId: string, param: string, placeholder: string,
   *         picks: Array<{col: string, value: string}>, groups: Array<any>,
   *         query: string, cursor: number, dirty: boolean,
   *         asked: boolean} | null} */
  var open = null;

  function esc(s) {
    return String(s).replace(/[&<>"]/g, function(c) {
      return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c];
    });
  }
  /* The panel's own casing: a coding dictionary shouts, and a list of
   * shouted terms is unreadable at 12px. Same rule as pp_term_label(). */
  function termLabel(s) {
    s = String(s || '');
    if (!s) return '';
    return s.charAt(0).toUpperCase() + s.slice(1).toLowerCase();
  }
  function pickLabel(p) {
    return p.col === FREE ? '\u201c' + p.value + '\u201d' : termLabel(p.value);
  }
  function mark(text, q) {
    if (!q) return esc(text);
    var i = text.toLowerCase().indexOf(q.toLowerCase());
    if (i < 0) return esc(text);
    return esc(text.slice(0, i)) + '<mark>' + esc(text.slice(i, i + q.length)) +
      '</mark>' + esc(text.slice(i + q.length));
  }
  function parseAttr(el, name) {
    var raw = el.getAttribute(name);
    if (!raw) return [];
    try {
      var v = JSON.parse(raw);
      return Array.isArray(v) ? v : [v];
    } catch (e) { return []; }
  }
  /** The trigger for a panel, or null once its panel has left the profile. */
  function triggerFor(vizId) {
    return /** @type {HTMLElement | null} */ (document.querySelector(
      '#' + layoutId + ' .pp-ctrl-find[data-viz-id=' + JSON.stringify(vizId) + ']'
    ));
  }
  function isPicked(col, value) {
    if (!open) return false;
    for (var i = 0; i < open.picks.length; i++) {
      if (open.picks[i].col === col && open.picks[i].value === value) return true;
    }
    return false;
  }

  /* The rows the popover draws, in the order it draws them.
   *
   * A picked term the CURRENT patient does not have is appended to its group
   * with a count of zero rather than dropped. Picks survive a patient switch
   * on purpose -- tick a body system once and page through the cohort looking
   * for it -- so the one place that must never hide a pick is the control
   * that would let you take it off again. */
  function rows() {
    // const, not var: TypeScript keeps a const narrowed inside the callbacks
    // below, which is the whole reason the state is hoisted out of `open`.
    const st = open;
    if (!st) return [];
    var q = st.query.toLowerCase();
    var out = [];
    st.groups.forEach(function(g) {
      var opts = (g.options || []).filter(function(o) {
        return !q || termLabel(o.value).toLowerCase().indexOf(q) >= 0;
      });
      var seen = {};
      opts.forEach(function(o) { seen[o.value] = true; });
      st.picks.forEach(function(p) {
        if (p.col !== g.col || seen[p.value]) return;
        if (q && termLabel(p.value).toLowerCase().indexOf(q) < 0) return;
        opts.push({value: p.value, n: 0});
      });
      if (opts.length) {
        out.push({group: g.label, col: g.col, truncated: g.truncated || 0,
                  options: opts});
      }
    });
    if (q) out = out.concat(cohortRows(st, q, out));
    return out;
  }

  /* The terms the COHORT has and this patient does not, when the reader has
   * typed something. Only then: unfiltered the list runs to several hundred
   * rows and would bury the patient's eight.
   *
   * The count is PATIENTS, not records. For a term this patient has none of,
   * the useful number is how much of the cohort does -- which is also what
   * says whether arming the filter is worth it. */
  function cohortRows(st, q, mine) {
    var have = vocab[st.vizId];
    if (!have) return [];
    var seen = {};
    mine.forEach(function(g) {
      g.options.forEach(function(o) { seen[g.col + '\r' + o.value] = true; });
    });
    var out = [];
    have.groups.forEach(function(g) {
      var opts = (g.options || []).filter(function(o) {
        return !seen[g.col + '\r' + o.value] &&
          termLabel(o.value).toLowerCase().indexOf(q) >= 0;
      });
      if (opts.length) {
        out.push({group: g.label, col: g.col, truncated: g.truncated || 0,
                  options: opts, elsewhere: true});
      }
    });
    return out;
  }

  /** Ask for the cohort's terms, at most once per popover session, and never
   *  for a list this client already holds (the token says so). */
  function askVocab(st) {
    if (st.asked) return;
    st.asked = true;
    Shiny.setInputValue(vocabInputId, {
      viz_id: st.vizId,
      have: (vocab[st.vizId] && vocab[st.vizId].token) || ''
    }, {priority: 'event'});
  }
  function flatRows() {
    /** @type {Array<{col: string, value: string}>} */
    var out = [];
    rows().forEach(function(g) {
      g.options.forEach(function(o) { out.push({col: g.col, value: o.value}); });
    });
    return out;
  }

  function draw() {
    const st = open;
    if (!pop || !st) return;
    var groups = rows();
    var i = -1;
    var q = st.query;
    var html = '<div class="pp-find-search">' +
      '<svg class="pp-search-icon" width="10" height="10" fill="currentColor" ' +
      'viewBox="0 0 16 16" aria-hidden="true"><path d="M11.742 10.344a6.5 6.5 ' +
      '0 1 0-1.397 1.398h-.001q.044.06.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414' +
      'l-3.85-3.85a1 1 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 ' +
      '11 0"/></svg>' +
      '<input type="text" class="pp-find-input" placeholder="' +
      esc(st.placeholder) + '" value="' + esc(q) + '">' +
      '</div>';

    if (st.picks.length) {
      html += '<div class="pp-find-picked">' + st.picks.map(function(p, pi) {
        return '<span class="pp-find-tag" title="' + esc(pickLabel(p)) + '">' +
          '<span>' + esc(pickLabel(p)) + '</span>' +
          '<button type="button" class="pp-find-drop" data-drop="' + pi +
          '" title="Remove">&times;</button></span>';
      }).join('') + '</div>';
    }

    html += '<div class="pp-find-list" role="listbox">';
    if (q && !vocab[st.vizId]) {
      html += '<div class="pp-find-split">Searching the rest of the cohort' +
        '&hellip;</div>';
    }
    if (!groups.length) {
      html += '<div class="pp-find-empty">' +
        (q ? 'No coded term matches <b>' + esc(q) + '</b>'
           : 'Nothing to filter on') +
        (q ? '<div><button type="button" class="pp-find-free">Filter on text ' +
             'containing \u201c' + esc(q) + '\u201d</button></div>' : '') +
        '</div>';
    }
    var wasElsewhere = false;
    groups.forEach(function(g) {
      if (g.elsewhere && !wasElsewhere) {
        wasElsewhere = true;
        html += '<div class="pp-find-split">Not in this patient</div>';
      }
      html += '<div class="pp-find-group' +
        (g.elsewhere ? ' pp-find-group--elsewhere' : '') + '">' +
        esc(g.group) + '</div>';
      g.options.forEach(function(o) {
        i++;
        var on = isPicked(g.col, o.value);
        var count = g.elsewhere
          ? o.n + (o.n === 1 ? ' patient' : ' patients')
          : String(o.n);
        html += '<div class="pp-find-opt' + (on ? ' is-picked' : '') +
          ((o.n === 0 && !g.elsewhere) ? ' is-zero' : '') +
          (g.elsewhere ? ' is-elsewhere' : '') +
          (i === st.cursor ? ' is-cursor' : '') +
          '" role="option" aria-selected="' + (on ? 'true' : 'false') +
          '" data-i="' + i + '">' +
          '<span class="pp-find-box">' + (on ? '&#10003;' : '') + '</span>' +
          '<span class="pp-find-text" title="' + esc(termLabel(o.value)) + '">' +
          mark(termLabel(o.value), q) + '</span>' +
          '<span class="pp-find-n">' + esc(count) + '</span></div>';
      });
      if (g.truncated) {
        html += '<div class="pp-find-more">' + g.truncated +
          ' more, keep typing</div>';
      }
    });
    html += '</div>';

    html += '<div class="pp-find-foot"><span>' +
      (st.picks.length
        ? st.picks.length + ' filter' + (st.picks.length > 1 ? 's' : '')
        : 'No filter') +
      '</span><span class="pp-find-acts">' +
      (st.picks.length
        ? '<button type="button" class="pp-find-clearall">Clear</button>' : '') +
      '<button type="button" class="pp-find-done">Done</button>' +
      '</span></div>';

    pop.innerHTML = html;
    var inp = /** @type {HTMLInputElement | null} */
      (pop.querySelector('.pp-find-input'));
    if (inp && document.activeElement !== inp) {
      inp.focus();
      try { inp.setSelectionRange(q.length, q.length); } catch (e) { /* no-op */ }
    }
    scrollCursorIntoView();
  }

  function scrollCursorIntoView() {
    if (!pop) return;
    var el = pop.querySelector('.pp-find-opt.is-cursor');
    if (el && el.scrollIntoView) el.scrollIntoView({block: 'nearest'});
  }

  /* Anchored to the trigger and pulled back inside the window, the way
   * Blockr.Select positions a headless menu. Fixed, because every ancestor
   * between the panel and <body> is `display: contents` under Shiny's
   * output-wrapper rule and cannot contain a positioned child. */
  function place() {
    var st = open;
    if (!pop || !st) return;
    var anchor = triggerFor(st.vizId);
    if (!anchor) { close(false); return; }
    var r = anchor.getBoundingClientRect();
    var h = pop.offsetHeight || 300;
    var w = pop.offsetWidth || 320;
    var below = window.innerHeight - r.bottom - 8;
    pop.style.position = 'fixed';
    pop.style.left = Math.max(8, Math.min(r.left,
      document.documentElement.clientWidth - w - 8)) + 'px';
    if (below < h && r.top > h) {
      pop.style.top = Math.max(8, r.top - h - 4) + 'px';
    } else {
      pop.style.top = (r.bottom + 4) + 'px';
    }
  }

  function openFor(trigger) {
    var vizId = trigger.getAttribute('data-viz-id');
    var param = trigger.getAttribute('data-param');
    if (!vizId || !param) return;
    if (!pop) {
      pop = document.createElement('div');
      pop.className = 'pp-find-pop';
      document.body.appendChild(pop);
      bindPop(pop);
    }
    open = {
      vizId: vizId,
      param: param,
      // A copy: the picks are not the server's until the popover closes.
      picks: parseAttr(trigger, 'data-picks').map(function(p) {
        return {col: String(p.col), value: String(p.value)};
      }),
      groups: parseAttr(trigger, 'data-options'),
      placeholder: trigger.getAttribute('data-placeholder') || 'Search',
      query: '',
      cursor: 0,
      dirty: false,
      // One request per popover session, on the first thing typed.
      asked: false
    };
    pop.classList.add('is-open');
    trigger.classList.add('is-open');
    draw();
    place();
  }

  /** @param {boolean} apply Send the picks, or drop them on the floor. */
  function close(apply) {
    if (!open) return;
    var state = open;
    open = null;
    if (pop) pop.classList.remove('is-open');
    var trigger = triggerFor(state.vizId);
    if (trigger) trigger.classList.remove('is-open');
    if (apply && state.dirty) send(state.vizId, state.param, state.picks);
  }

  function send(vizId, param, picks) {
    Shiny.setInputValue(ctrlInputId, {
      viz_id: vizId, param: param, value: picks
    }, {priority: 'event'});
  }

  function toggleRow(row) {
    var st = open;
    if (!st || !row) return;
    var at = -1;
    for (var i = 0; i < st.picks.length; i++) {
      if (st.picks[i].col === row.col && st.picks[i].value === row.value) {
        at = i; break;
      }
    }
    if (at >= 0) st.picks.splice(at, 1);
    else st.picks.push({col: row.col, value: row.value});
    st.dirty = true;
    draw();
  }

  function bindPop(el) {
    // Keep the focus in the filter box: a mousedown on a row would otherwise
    // blur it, and the next keystroke would go to the page.
    el.addEventListener('mousedown', function(e) {
      if (!(/** @type {HTMLElement} */ (e.target)).closest('.pp-find-input')) {
        e.preventDefault();
      }
    });
    el.addEventListener('input', function(e) {
      var st = open;
      var t = /** @type {HTMLInputElement} */ (e.target);
      if (!st || !t.classList.contains('pp-find-input')) return;
      st.query = t.value;
      st.cursor = 0;
      if (st.query) askVocab(st);
      draw();
    });
    el.addEventListener('click', function(e) {
      var st = open;
      if (!st) return;
      // Every click in here is the popover's. Said now, before draw() below
      // detaches the row that was clicked: once it is out of the document
      // its `closest()` walks a tree with no popover in it, and the
      // click-outside handler on `document` would read its own list as
      // somewhere else and close.
      e.stopPropagation();
      var t = /** @type {HTMLElement} */ (e.target);
      var drop = t.closest('[data-drop]');
      if (drop) {
        st.picks.splice(Number(drop.getAttribute('data-drop')), 1);
        st.dirty = true;
        draw();
        return;
      }
      if (t.closest('.pp-find-clearall')) {
        st.picks = [];
        st.dirty = true;
        draw();
        return;
      }
      if (t.closest('.pp-find-done')) { close(true); return; }
      if (t.closest('.pp-find-free')) {
        if (st.query) {
          st.picks.push({col: FREE, value: st.query});
          st.query = '';
          st.dirty = true;
          draw();
        }
        return;
      }
      var opt = t.closest('.pp-find-opt');
      if (opt) {
        st.cursor = Number(opt.getAttribute('data-i'));
        toggleRow(flatRows()[st.cursor]);
      }
    });
    el.addEventListener('keydown', function(e) {
      var st = open;
      if (!st) return;
      var ke = /** @type {KeyboardEvent} */ (e);
      var flat = flatRows();
      if (ke.key === 'ArrowDown') {
        st.cursor = Math.min(flat.length - 1, st.cursor + 1);
        draw(); ke.preventDefault();
      } else if (ke.key === 'ArrowUp') {
        st.cursor = Math.max(0, st.cursor - 1);
        draw(); ke.preventDefault();
      } else if (ke.key === ' ' && st.query === '') {
        toggleRow(flat[st.cursor]); ke.preventDefault();
      } else if (ke.key === 'Enter') {
        // Enter on a query that matched nothing is the free-text pick: it is
        // what the box did before this control existed, and the one gesture
        // nobody should have to relearn.
        if (!flat.length && st.query) {
          st.picks.push({col: FREE, value: st.query});
          st.dirty = true;
        } else {
          toggleRow(flat[st.cursor]);
        }
        close(true); ke.preventDefault();
      } else if (ke.key === 'Escape') {
        // Escape abandons: the picks were never sent, so there is nothing to
        // undo. Done and a click outside both apply.
        close(false); ke.preventDefault();
      }
    });
  }

  // Opening, and the click that lands outside.
  document.addEventListener('click', function(e) {
    var t = /** @type {HTMLElement} */ (e.target);
    var trigger = t.closest('#' + layoutId + ' .pp-ctrl-find');
    if (trigger) {
      e.stopPropagation();
      if (open && open.vizId === trigger.getAttribute('data-viz-id')) close(true);
      else { close(true); openFor(/** @type {HTMLElement} */ (trigger)); }
      return;
    }
    if (open && !t.closest('.pp-find-pop')) close(true);
  });

  // The x beside the trigger, and the sidebar caption's echo of the picks.
  // Both clear EVERY pick through the one channel the popover uses, so there
  // is a single path back to unfiltered. The caption clears all of them
  // rather than the one it names: it shows the first pick and a count of the
  // rest, so a chip that dropped only its own would leave "+2" on screen
  // with no way to reach the two.
  document.addEventListener('click', function(e) {
    var t = /** @type {HTMLElement} */ (e.target);
    var btn = t.closest('#' + layoutId + ' .pp-ctrl-find-clear') ||
      t.closest('#' + layoutId + ' .pp-cohort-bandcap-find');
    if (btn) {
      e.stopPropagation();
      close(false);
      send(btn.getAttribute('data-viz-id'),
           btn.getAttribute('data-param') || 'find', []);
    }
  });

  Shiny.addCustomMessageHandler(vocabMsgId, function(msg) {
    if (!msg || !msg.viz_id) return;
    if (msg.unchanged) {
      // The list this client holds is still the current one; nothing came.
      if (vocab[msg.viz_id]) vocab[msg.viz_id].token = String(msg.token);
    } else {
      vocab[msg.viz_id] = {
        token: String(msg.token),
        groups: Array.isArray(msg.groups) ? msg.groups : []
      };
    }
    if (open && open.vizId === msg.viz_id) draw();
  });

  // The panel moved or the window changed: follow the trigger, or give up if
  // the panel has left the profile.
  window.addEventListener('resize', place);
  window.addEventListener('scroll', place, true);
  // A header swap replaces the trigger under an open popover. The popover is
  // on <body> and survives; re-anchor it to the new button.
  $(document).on('shiny:value', function() {
    if (open) setTimeout(place, 0);
  });
  document.addEventListener('pp-header-swapped', function() {
    if (open) place();
  });
});
