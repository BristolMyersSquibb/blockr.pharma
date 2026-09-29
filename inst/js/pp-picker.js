// @ts-check
/* The panel picker in the sidebar: the On list, the search over panels and
 * patients, the first hit, the move animation, the On-row drag, and what
 * the server says is on the profile.
 *
 * Depends on: pp-core.js
 */
PatientProfile.part(function(ctx) {
  var ns = ctx.ns;
  var cfg = ctx.cfg;
  var layoutId = ns('pp_layout');
  var sidebarId = ns('pp_sidebar');
  var searchId = ns('search');
  var toggleInputId = ns('toggle_viz');
  var clearBtnId = ns('search_clear');
  var pickParamInputId = ns('pick_param');
  var addOnId = ns('pp_add_on');
  // The design system's thin x, for the remove tool on an On row.
  var X_SVG = '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" ' +
    'stroke="currentColor" stroke-width="1.3" stroke-linecap="round" ' +
    'aria-hidden="true"><path d="M3 3l6 6M9 3l-6 6"/></svg>';
  var syncMsgId = ns('sync_selected');
  var syncParamsMsgId = ns('sync_params');
  var reorderInputId = ns('reorder_viz');

  // The + button and its picker.
  //
  // Filtering is client-side over rows the server rendered once:
  // the catalogue is a few dozen rows, it is all in the DOM
  // already, and a round trip per keystroke would cost the caret
  // for nothing (the AE find box learned that the hard way).
  //
  // Clicking a row sends toggle_viz for a panel and pick_param for
  // a parameter, so the server's selection logic is untouched.
  // pick_param adds a parameter to a multi-parameter panel that is on
  // the profile, or opens that panel showing only that parameter.
  var addPopId = ns('pp_panels');
  // The sidebar's own box: there is no second search any more.
  var addInputId = ns('search');

  // Browsing: the empty box was focused, so the whole catalogue is open
  // under it. It stays open while the reader scrolls it and picks from it,
  // which is why it is a flag and not the input's focus: a click on a row
  // or on the list's scrollbar takes the focus away first. It closes on
  // Escape and on a mousedown anywhere outside the box and the panels.
  var browsing = false;

  function addRows(){
    return document.querySelectorAll('#' + addPopId + ' .pp-add-row');
  }

  // What is on the profile, REMEMBERED.
  //
  // The ticks used to be painted only when sync_selected arrived,
  // which is on change -- and the one at boot lands before the
  // picker's rows exist, so its marks went nowhere. Opening the
  // picker then showed nothing ticked, and clicking a panel that
  // WAS on the profile removed it -- the row offered to add and did
  // the opposite. Same fix the parameter check marks already use:
  // keep the last state and re-apply it whenever there is something
  // to apply it to.
  var lastSelected = [];
  var lastParamOn = [];

  // The picker's own ordered list of what is on the profile.
  //
  // Built here rather than server-side because the order changes on
  // every drag, and the catalogue below it is rendered once per
  // study. Labels come from the catalogue's own rows, so there is
  // one source for them.
  function addLabels(){
    var map = {};
    addRows().forEach(function(r){
      if (r.getAttribute('data-kind') !== 'panel') return;
      var code = r.querySelector('.pp-add-code');
      var name = r.querySelector('.pp-add-name');
      map[r.getAttribute('data-viz-id') || ''] = {
        label: (name && name.textContent) || '',
        code: code ? code.textContent : ''
      };
    });
    return map;
  }

  function renderAddOn(){
    var host = document.getElementById(addOnId);
    if (!host) return;
    var map = addLabels();
    var want = lastSelected.filter(function(id){ return !!map[id]; });
    // A repaint that would draw the same rows is skipped.
    //
    // A pick moves a row optimistically and animates it; the
    // server's sync_selected lands a tenth of a second later
    // saying the same thing, and rebuilding the list there tore
    // the row out mid-travel -- the tint measured 30ms instead of
    // 600. Comparing first costs one array walk over at most a
    // dozen ids.
    var have = Array.prototype.slice.call(host.children).map(function(r){
      return r.getAttribute('data-viz-id'); });
    var same = have.length === want.length &&
      have.every(function(v, i){ return v === want[i]; });
    // The count on the heading, set where the list is painted.
    // It was set from sync_selected, which fires on CHANGE -- so
    // at boot it landed before the heading existed and the profile
    // said nothing until you touched something.
    $('#' + layoutId + ' .pp-add-n').text(want.length || '');
    if (same) { addOnVisibility(host); return; }
    host.innerHTML = '';
    var list = host;
    want.forEach(function(id){
      var m = map[id];
      if (!m) return;
      var row = document.createElement('div');
      row.className = 'pp-add-ord';
      row.setAttribute('data-viz-id', id);
      row.setAttribute('role', 'listitem');
      // Focusable, so Alt+Up and Alt+Down can move it: the whole row drags,
      // and the keyboard needs a row to hold.
      row.tabIndex = 0;
      // A parameter shows its code, then its name as meta; a panel its name.
      if (m.code) {
        var code = document.createElement('span');
        code.className = 'pp-add-ord-code';
        code.textContent = m.code;
        row.appendChild(code);
      }
      var name = document.createElement('span');
      name.className = m.code ? 'pp-add-name is-meta' : 'pp-add-name';
      name.textContent = m.label;
      var x = document.createElement('button');
      x.className = 'pp-add-ord-x';
      x.type = 'button';
      x.setAttribute('aria-label', 'Remove ' + (m.code || m.label));
      x.setAttribute('data-blockr-tooltip', 'Remove from the profile');
      x.innerHTML = X_SVG;
      row.appendChild(name); row.appendChild(x);
      list.appendChild(row);
    });
    addOnVisibility(host);
  }

  // Hidden only when there is nothing on the profile. It used to
  // hide while searching too, when it lived in a menu and the
  // results were the whole answer; in the sidebar it is the
  // profile itself and a search is a thing happening beneath it.
  function addOnVisibility(host){
    var wrap = host.parentElement;
    if (!wrap) return;
    wrap.classList.toggle('is-hidden', !lastSelected.length);
  }

  // Picking moves a thing, it does not tick it.
  //
  // A row you pick belongs in the list above; leaving it in the
  // catalogue with a mark on it put the same parameter on screen
  // twice, and the tick then said what the list above already
  // said. So it travels, and the rows it left close the gap behind
  // it. The motion is the whole explanation.
  //
  // FLIP: measure both lists before the DOM changes, redraw, then
  // play the difference back to zero -- the browser animates a
  // transform and never the layout. The repaint is optimistic, so
  // the before-rects are taken in the same click and the server's
  // sync_selected lands later to find everything already in place.
  //
  // The tick survives for one case: a SEARCH still lists what is
  // already on the profile, and while searching the list above is
  // hidden, so nothing else would say so.
  var ADD_FLIP_MS = 190;
  function addReduceMotion(){
    return !!(window.matchMedia &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches);
  }
  function addFlipRects(){
    var m = {}, pop = document.getElementById(addPopId);
    if (!pop) return m;
    pop.querySelectorAll('.pp-add-ord, .pp-add-row')
      .forEach(function(r){
        var b = r.getBoundingClientRect();
        if (b.height) m[r.getAttribute('data-viz-id')] = b;
      });
    return m;
  }
  function addFlipPlay(before, landed){
    var pop = document.getElementById(addPopId);
    if (!pop) return;
    var reduce = addReduceMotion();
    if (!reduce) {
      pop.querySelectorAll('.pp-add-ord, .pp-add-row')
        .forEach(function(el){
          var r = /** @type {HTMLElement} */ (el);
          var b = before[r.getAttribute('data-viz-id') || ''];
          if (!b) return;
          var a = r.getBoundingClientRect();
          if (!a.height) return;
          var dy = b.top - a.top, dx = b.left - a.left;
          if (!dy && !dx) return;
          r.style.transition = 'none';
          r.style.transform =
            'translate(' + dx + 'px,' + dy + 'px)';
          requestAnimationFrame(function(){
            r.style.transition =
              'transform ' + ADD_FLIP_MS + 'ms ' +
              'cubic-bezier(.2,.7,.3,1)';
            r.style.transform = '';
          });
        });
    }
    // And a moment of tint where it landed. On a long profile the
    // list above can be scrolled past its box, and the tint is the
    // part that survives arriving off-screen.
    if (!landed) return;
    var t = pop.querySelector(
      '.pp-add-ord[data-viz-id=' + JSON.stringify(landed) + ']');
    if (!t) return;
    t.classList.add('is-landed');
    var landedRow = t;
    window.setTimeout(function(){
      landedRow.classList.remove('is-landed');
    }, reduce ? 500 : ADD_FLIP_MS + 420);
  }

  // Reordering inside the picker. The same pointer drag the panels
  // themselves use, over 28px rows instead of 400px charts -- which
  // is the whole reason to offer it here as well.
  $(document).on('mousedown', '#' + addOnId + ' .pp-add-ord',
    function(e) {
      if (e.target.closest('button')) return;
      e.preventDefault();
      var host = document.getElementById(addOnId);
      if (!host) return;
      var rows = Array.prototype.slice.call(host.children);
      var from = rows.indexOf(this);
      if (from < 0 || rows.length < 2) return;
      this.classList.add('is-dragging');
      var to = from;

      function mark(y){
        rows.forEach(function(r){
          r.classList.remove('is-over', 'is-over-last');
        });
        to = rows.length;
        for (var i = 0; i < rows.length; i++) {
          if (i === from) continue;
          var bx = rows[i].getBoundingClientRect();
          if (y < bx.top + bx.height / 2) {
            rows[i].classList.add('is-over'); to = i; return;
          }
        }
        rows[rows.length - 1].classList.add('is-over-last');
      }
      mark(e.clientY);

      function move(ev){ mark(ev.clientY); }
      function up(){
        document.removeEventListener('mousemove', move);
        document.removeEventListener('mouseup', up);
        rows.forEach(function(r){
          r.classList.remove('is-dragging', 'is-over',
                             'is-over-last');
        });
        if (to !== from && to !== from + 1) {
          var ids = rows.map(function(r){
            return r.getAttribute('data-viz-id'); });
          var t = to;
          var moved = ids.splice(from, 1)[0];
          if (from < t) t -= 1;
          ids.splice(t, 0, moved);
          lastSelected = ids;
          renderAddOn();
          Shiny.setInputValue(reorderInputId, ids,
                              {priority: 'event'});
        }
      }
      document.addEventListener('mousemove', move);
      document.addEventListener('mouseup', up);
    });

  // The keyboard's drag: Alt+Up and Alt+Down move the focused row.
  $(document).on('keydown', '#' + addOnId + ' .pp-add-ord', function(e) {
    if (!e.altKey || (e.key !== 'ArrowUp' && e.key !== 'ArrowDown')) return;
    e.preventDefault();
    var host = document.getElementById(addOnId);
    if (!host) return;
    var ids = Array.prototype.slice.call(host.children).map(function(r){
      return r.getAttribute('data-viz-id'); });
    var id = this.getAttribute('data-viz-id');
    var from = ids.indexOf(id);
    var to = from + (e.key === 'ArrowUp' ? -1 : 1);
    if (from < 0 || to < 0 || to >= ids.length) return;
    ids.splice(from, 1);
    ids.splice(to, 0, id);
    lastSelected = ids;
    renderAddOn();
    var moved = /** @type {HTMLElement | null} */ (host.querySelector(
      '.pp-add-ord[data-viz-id=' + JSON.stringify(id) + ']'));
    if (moved) moved.focus();
    Shiny.setInputValue(reorderInputId, ids, {priority: 'event'});
  });

  $(document).on('click', '#' + addOnId + ' .pp-add-ord-x',
    function(e) {
      e.stopPropagation();
      var owner = this.closest('.pp-add-ord');
      var id = owner ? owner.getAttribute('data-viz-id') : null;
      // The same move, downwards: it travels back to where the
      // catalogue keeps it, which is where you would look for it
      // if you wanted it again.
      var before = addFlipRects();
      lastSelected = lastSelected.filter(function(x){
        return x !== id; });
      $('#' + layoutId + ' .pp-add-n').text(lastSelected.length || '');
      filterAdd();
      addFlipPlay(before, null);
      Shiny.setInputValue(toggleInputId, id, {priority: 'event'});
    });

  function paintAddTicks(){
    addRows().forEach(function(r){
      var id = r.getAttribute('data-viz-id');
      var on = r.getAttribute('data-kind') === 'param'
        ? lastParamOn.indexOf(
            id + '@@' + r.getAttribute('data-paramcd')) >= 0
        : lastSelected.indexOf(id) >= 0;
      r.classList.toggle('is-on', on);
    });
    renderAddOn();
  }
  function filterAdd(){
    renderAddOn();
    var pop = document.getElementById(addPopId);
    if (!pop) return;
    var inp = /** @type {HTMLInputElement | null} */ (
      document.getElementById(addInputId));
    var q = (inp ? inp.value : '').trim().toLowerCase();
    var browse = browsing && !q;
    pop.classList.toggle('is-browsing', browse);
    var shown = 0;
    addRows().forEach(function(r){
      var hay = r.getAttribute('data-search-text') || '';
      // No query, no catalogue, unless the reader asked to browse.
      // Browsing lists what is NOT on the profile yet: the list above
      // is on screen at the same time and already names the rest.
      // Searching shows every match, on the profile or not, so a hit
      // is never missing.
      var ok = q ? hay.indexOf(q) >= 0
        : browse && !r.classList.contains('is-on');
      r.classList.toggle('is-hidden', !ok);
      if (ok) shown++;
    });
    // A group heading with nothing under it is noise. Scoped to
    // the catalogue: the ordered list's own heading is followed by
    // a container rather than by rows, and this walk would always
    // decide it was empty.
    pop.querySelectorAll('.pp-add-results .pp-add-group')
      .forEach(function(g){
      var any = false, n = g.nextElementSibling;
      while (n && n.classList.contains('pp-add-row')) {
        if (!n.classList.contains('is-hidden')) { any = true; break; }
        n = n.nextElementSibling;
      }
      g.classList.toggle('is-hidden', !any);
    });
    var none = pop.querySelector('.pp-add-none');
    // Only when the query found nothing ANYWHERE. It counted panel
    // hits alone, so searching for a patient id -- which no panel
    // matches -- printed an empty-result line directly above the
    // patient it had just found.
    if (none) {
      var pats = document.querySelectorAll(
        '#' + layoutId + ' .pp-pt:not(.is-filtered-out)').length;
      none.classList.toggle('is-shown', !!q && shown === 0 && !pats);
    }
  }

  $(document).on('click', '#' + layoutId + ' .pp-add-row', function(e){
    e.stopPropagation();
    var kind = this.getAttribute('data-kind');
    var vizId = this.getAttribute('data-viz-id');
    // Optimistic, so the tick lands at click speed; the server
    // confirms through sync_selected a flush later. The remembered
    // state moves with it, or reopening the picker would repaint
    // the row back to what it was.
    var nowOn = !this.classList.contains('is-on');
    var before = addFlipRects();
    this.classList.toggle('is-on', nowOn);
    if (kind === 'panel') {
      // The query stays, and so do its hits: "blood" finds several
      // panels, and picking one should not make you type it again to
      // pick the next. Focus goes back to the box, and the cursor to
      // this row, so typing or the arrows carry on from here. The
      // query also filters the cohort below; x or Escape clears it.
      var box = /** @type {HTMLInputElement | null} */ (
        document.getElementById(addInputId));
      if (box && (box.value || browsing)) {
        box.focus({preventScroll: true});
        setCursor(this);
      }
      var at = lastSelected.indexOf(vizId);
      if (nowOn && at < 0) lastSelected = lastSelected.concat([vizId]);
      if (!nowOn && at >= 0) lastSelected = lastSelected.filter(
        function(x){ return x !== vizId; });
      $('#' + layoutId + ' .pp-add-n').text(lastSelected.length || '');
      // Both lists, now: the catalogue drops what it just handed
      // upwards. filterAdd() repaints the list above as well.
      filterAdd();
      addFlipPlay(before, nowOn ? vizId : null);
    }
    if (kind === 'param') {
      Shiny.setInputValue(pickParamInputId, {
        viz_id: vizId, paramcd: this.getAttribute('data-paramcd')
      }, {priority: 'event'});
    } else {
      Shiny.setInputValue(toggleInputId, vizId, {priority: 'event'});
    }
  });

  // applyFilter() runs on every cohort re-render, so a live query
  // survives it -- the treatment the panel list already had, which
  // the cohort list never got. A fresh catalogue arrives with nothing
  // ticked.
  $(document).on('shiny:value', function(e) {
    if (e.name && e.name.indexOf('sidebar_cohort') >= 0) {
      setTimeout(applyFilter, 0);
    }
    if (e.name && e.name.indexOf('panel_picker') >= 0) {
      setTimeout(function(){ paintAddTicks(); filterAdd(); }, 0);
    }
  });

  // Search: client-side filtering of panels and patients alike.
  //
  // Match on a row's own data-search-text and mark it with a class,
  // never on `:visible`: that is false for every row inside a hidden
  // ancestor, so a list hidden by one keystroke could never come back
  // on the next.
  function applyFilter() {
    var $sidebar = $('#' + sidebarId);
    var query = String($('#' + searchId).val() || '').toLowerCase().trim();

    $('#' + clearBtnId).toggleClass('is-hidden', !query);
    $sidebar.toggleClass('is-searching', !!query);

    // One search box, two tenants. A query hides non-matching
    // PATIENTS in place rather than switching the cohort to a
    // results tree: the list is already flat, so there is nothing
    // to restructure, and the count in the header keeps telling
    // you the size of the cohort rather than of the query.
    $sidebar.find('.pp-pt').each(function() {
      var hay = $(this).attr('data-search-text') || '';
      $(this).toggleClass('is-filtered-out',
        !!query && hay.indexOf(query) === -1);
    });

    // One box, two tenants: the panels above and the patients
    // below. AFTER the rows above, because the empty-result line
    // has to count both lists -- run first, it counted the
    // patients the PREVIOUS query had left standing.
    filterAdd();
    markFirstHit();
  }

  $(document).on('input', '#' + searchId, applyFilter);

  $(document).on('focusin', '#' + searchId, function() {
    if (browsing || $(this).val()) return;
    browsing = true;
    filterAdd();
  });

  function endBrowse() {
    if (!browsing) return;
    browsing = false;
    if (!String($('#' + searchId).val() || '').trim()) setCursor(null);
    filterAdd();
  }

  // Outside means outside both the box and the panels. The cohort counts
  // as outside: picking a patient is the end of choosing panels.
  $(document).on('mousedown', function(e) {
    if (!browsing) return;
    var t = /** @type {Element} */ (e.target);
    if (t.closest && t.closest(
      '#' + addPopId + ', .pp-sidebar-search')) return;
    endBrowse();
  });

  // Clear (x): reset the query and hand back the full list
  $(document).on('click', '#' + clearBtnId, function(e) {
    e.stopPropagation();
    $('#' + searchId).val('').focus();
    applyFilter();
  });

  // What a query has found, in reading order: the panels the catalogue is
  // still showing, then the patients still standing. One list, because the
  // box is one box -- a cursor that stopped at the bottom of the panels
  // would make the patients below look like a different result set.
  /** @returns {HTMLElement[]} */
  function allHits(){
    var rows = [].slice.call(document.querySelectorAll(
      '#' + addPopId + ' .pp-add-row:not(.is-hidden)'));
    return rows.concat([].slice.call(document.querySelectorAll(
      '#' + layoutId + ' .pp-pt:not(.is-filtered-out)')));
  }

  /** @returns {HTMLElement | null} */
  function firstHit(){
    var hits = allHits();
    return hits.length ? hits[0] : null;
  }

  // The cursor: the row Enter would take. It starts on the first hit, so
  // Enter alone still works on the result you were typing towards, and the
  // arrows move it from there.
  //
  // Read off the DOM rather than held in a variable, because the lists are
  // rebuilt under it: every keystroke re-filters, the cohort list is
  // re-rendered by the server on a pick, and an index into a list that no
  // longer exists is how a cursor ends up on the wrong row.
  /** @returns {HTMLElement | null} */
  function cursor(){
    return /** @type {HTMLElement | null} */ (
      document.querySelector('#' + layoutId + ' .is-enter'));
  }

  /** @param {HTMLElement | null} row */
  function setCursor(row){
    $('#' + layoutId + ' .is-enter').removeClass('is-enter');
    if (!row) return;
    row.classList.add('is-enter');
    // `nearest`, so a cursor already on screen does not scroll the list
    // under the pointer just because a key was pressed.
    if (row.scrollIntoView) row.scrollIntoView({ block: 'nearest' });
  }

  function markFirstHit(){
    var q = String($('#' + searchId).val() || '').trim();
    if (!q) { setCursor(null); return; }
    setCursor(firstHit());
  }

  /** @param {number} delta */
  function moveCursor(delta){
    var hits = allHits();
    if (!hits.length) return false;
    var cur = cursor();
    var at = cur ? hits.indexOf(cur) : -1;
    // Clamped, not wrapping. The cohort can be three hundred rows, and
    // wrapping from its last patient back up to the first panel is a jump
    // the eye cannot follow.
    var next = at < 0 ? (delta > 0 ? 0 : hits.length - 1)
                      : Math.min(hits.length - 1, Math.max(0, at + delta));
    setCursor(hits[next]);
    return true;
  }

  $(document).on('keydown', '#' + searchId, function(e) {
    if (e.key === 'Escape' || e.keyCode === 27) {
      if (!$(this).val()) {
        if (!browsing) return;
        e.stopPropagation();
        endBrowse();
        return;
      }
      e.stopPropagation();
      $(this).val('');
      applyFilter();
      return;
    }
    if (e.key === 'ArrowDown' || e.keyCode === 40) {
      // preventDefault whether or not there is anywhere to go: the box is a
      // text input, and the browser's own Down would jump the caret to the
      // end of the query mid-search.
      e.preventDefault();
      moveCursor(1);
      return;
    }
    if (e.key === 'ArrowUp' || e.keyCode === 38) {
      e.preventDefault();
      moveCursor(-1);
      return;
    }
    if (e.key !== 'Enter' && e.keyCode !== 13) return;
    // The cursor if the arrows have moved it, the first hit otherwise. Never
    // recompute when a cursor is set: the arrows are the only thing that
    // could have moved it, and taking the first hit instead would ignore
    // them.
    // Browsing with nothing typed, Enter takes only a row the arrows
    // reached: the first row of a list nobody searched is not a choice.
    if (browsing && !String($(this).val() || '').trim() && !cursor()) return;
    var row = cursor() || firstHit();
    if (!row) return;
    e.preventDefault();
    row.click();
  });

  Shiny.addCustomMessageHandler(syncParamsMsgId, function(keys) {
    if (!keys) keys = [];
    lastParamOn = keys;
    paintAddTicks();
  });

  // Sync sidebar state from server
  Shiny.addCustomMessageHandler(syncMsgId, function(selected) {
    if (!selected) selected = [];

    // The heading says how many panels are on the profile, and the
    // catalogue ticks the ones that are.
    lastSelected = selected;
    $('#' + layoutId + ' .pp-add-n').text(selected.length || '');
    paintAddTicks();

    // Cards moved between the sections keep the live query honest
    applyFilter();
  });
});
