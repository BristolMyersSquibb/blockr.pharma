// @ts-check
/* The sidebar: the panel picker (a Blockr.Pick.list) and the patients'
 * filter box.
 *
 * Depends on: pp-core.js, Blockr.Pick (blockr.dplyr's blockr_pick_dep()).
 */
PatientProfile.part(function(ctx) {
  var ns = ctx.ns;
  var layoutId = ns('pp_layout');
  var sidebarId = ns('pp_sidebar');
  var searchId = ns('search');
  var clearBtnId = ns('search_clear');
  var panelsId = ns('pp_panels');
  var toggleInputId = ns('toggle_viz');
  var reorderInputId = ns('reorder_viz');
  var syncMsgId = ns('sync_selected');

  // --- The panel picker ----------------------------------------------------
  //
  // One list: what is on the profile on top, in profile order, then the
  // catalogue folded under "+ Add panels". Every row is one viz, a
  // parameter card included, so a pick is a toggle_viz and a drag a
  // reorder_viz; the server's selection logic is untouched.

  /** What the server last said is on the profile, unavailable ids included.
   *  @type {string[]} */
  var known = [];
  /** @type {any} */
  var pick = null;
  // Set while onChange runs, so a sync that flushed a pending change knows.
  var sent = false;

  /** @param {string[]} a @param {string[]} b */
  function same(a, b) {
    return a.length === b.length && a.every(function(v, i){ return v === b[i]; });
  }

  /** The picks after a pause: send what changed against what the server has.
   *  @param {string[]} values */
  function report(values) {
    var opts = pick ? pick.optionValues : [];
    var had = known.filter(function(v){ return opts.indexOf(v) >= 0; });
    var removed = had.filter(function(v){ return values.indexOf(v) < 0; });
    var added = values.filter(function(v){ return had.indexOf(v) < 0; });
    removed.concat(added).forEach(function(id){
      Shiny.setInputValue(toggleInputId, id, {priority: 'event'});
    });
    // The server appends what is added; anything else is a reorder.
    var after = had.filter(function(v){ return removed.indexOf(v) < 0; })
      .concat(added);
    if (values.length > 1 && !same(after, values)) {
      Shiny.setInputValue(reorderInputId, values.slice(), {priority: 'event'});
    }
    // As R writes it: the new order into the slots of the ids this list
    // offers, the others left where they are.
    var next = known.filter(function(v){ return removed.indexOf(v) < 0; })
      .concat(added);
    var k = 0;
    known = next.map(function(v){
      return opts.indexOf(v) >= 0 ? values[k++] : v;
    });
    sent = true;
  }

  function mountPicker() {
    var host = document.getElementById(panelsId);
    if (!host || (pick && host.contains(pick.el))) return;
    if (pick) { pick.destroy(); pick = null; }
    var options = [];
    try { options = JSON.parse(host.getAttribute('data-options') || '[]'); }
    catch (e) { options = []; }
    var Pick = /** @type {any} */ (window.Blockr).Pick;
    if (!Pick || !Pick.list) return;
    pick = Pick.list(host, {
      options: options,
      selected: known,
      reorderable: true,
      label: 'Panels on the profile',
      countLabel: 'on the profile',
      addLabel: 'Add panels',
      searchPlaceholder: 'Filter panels',
      onChange: report
    });
    pick.optionValues = options.map(function(o){ return o.value; });
  }

  $(document).on('shiny:value', function(e) {
    if (e.name === ns('panel_picker')) setTimeout(mountPicker, 0);
    if (e.name === ns('sidebar_cohort')) setTimeout(applyFilter, 0);
  });

  Shiny.addCustomMessageHandler(syncMsgId, function(selected) {
    selected = selected || [];
    // Picks still waiting for their pause go out first; the server's next
    // sync then answers them, and this one is already out of date.
    sent = false;
    if (pick) pick.flush();
    if (sent) return;
    known = selected;
    if (pick && !same(pick.getValue(), selected)) pick.setValue(selected);
  });

  // --- The patients' filter box -------------------------------------------
  //
  // Client-side over the rows the server rendered. A row is marked with a
  // class rather than tested with `:visible`, which is false inside a
  // hidden ancestor.
  function query() {
    return String($('#' + searchId).val() || '').toLowerCase().trim();
  }

  function applyFilter() {
    var q = query();
    $('#' + clearBtnId).toggleClass('is-hidden', !q);
    $('#' + sidebarId).find('.pp-pt').each(function() {
      var hay = $(this).attr('data-search-text') || '';
      $(this).toggleClass('is-filtered-out', !!q && hay.indexOf(q) === -1);
    });
    setCursor(q ? hits()[0] || null : null);
  }

  $(document).on('input', '#' + searchId, applyFilter);

  $(document).on('click', '#' + clearBtnId, function(e) {
    e.stopPropagation();
    $('#' + searchId).val('').focus();
    applyFilter();
  });

  /** @returns {HTMLElement[]} */
  function hits() {
    return [].slice.call(document.querySelectorAll(
      '#' + layoutId + ' .pp-pt:not(.is-filtered-out)'));
  }

  // The cursor: the row Enter takes. Read off the DOM, since the cohort list
  // is re-rendered under it.
  /** @returns {HTMLElement | null} */
  function cursor() {
    return /** @type {HTMLElement | null} */ (
      document.querySelector('#' + layoutId + ' .pp-pt.is-enter'));
  }

  /** @param {HTMLElement | null} row */
  function setCursor(row) {
    $('#' + layoutId + ' .pp-pt.is-enter').removeClass('is-enter');
    if (!row) return;
    row.classList.add('is-enter');
    if (row.scrollIntoView) row.scrollIntoView({ block: 'nearest' });
  }

  /** Clamped, not wrapping: the cohort can be hundreds of rows.
   *  @param {number} delta */
  function moveCursor(delta) {
    var list = hits();
    if (!list.length) return;
    var cur = cursor();
    var at = cur ? list.indexOf(cur) : -1;
    var next = at < 0 ? (delta > 0 ? 0 : list.length - 1)
                      : Math.min(list.length - 1, Math.max(0, at + delta));
    setCursor(list[next]);
  }

  $(document).on('keydown', '#' + searchId, function(e) {
    if (e.key === 'Escape') {
      if (!$(this).val()) return;
      e.stopPropagation();
      $(this).val('');
      applyFilter();
      return;
    }
    // preventDefault either way, or the caret jumps to the end of the box.
    if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
      e.preventDefault();
      moveCursor(e.key === 'ArrowDown' ? 1 : -1);
      return;
    }
    if (e.key !== 'Enter') return;
    var row = cursor() || (query() ? hits()[0] : null);
    if (!row) return;
    e.preventDefault();
    row.click();
  });
});
